#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# 08_run_bsts_gt_MA_EMA.R
# Step 9: BSTS with Google Trends predictors, MA and EMA variants
# -----------------------------------------------------------------------------
# Purpose:
#   Fit rolling-origin Bayesian structural time-series nowcasting / forecasting
#   models that add Google Trends predictors to the frozen local-level BSTS
#   state specification selected in Step 8B.
#
#   This script is the first Google Trends modelling step. It evaluates MA and
#   EMA quarterly predictor variants separately and repeats the analysis over the
#   agreed sparsity-threshold grid.
#
# Inputs:
#   data/processed/modeling_panel_gt_overlap.csv
#   data/processed/keyword_dictionary_clean.csv
#   data/processed/validation/rolling_origins_h0_nowcast.csv
#   data/processed/validation/rolling_origins_h1_forecast.csv
#   data/processed/validation/rolling_origins_h2_forecast_exploratory.csv
#   data/processed/validation/sparsity_threshold_grid.csv
#
# Optional inputs for comparison plots:
#   data/processed/benchmarks/benchmark_h0_model_ranking.csv
#   data/processed/bsts_state_specs/bsts_state_spec_h0_ranking.csv
#
# Outputs:
#   data/processed/bsts_gt/bsts_gt_predictions_h0.csv
#   data/processed/bsts_gt/bsts_gt_predictions_h1.csv
#   data/processed/bsts_gt/bsts_gt_predictions_h2.csv
#   data/processed/bsts_gt/bsts_gt_predictions_all.csv
#   data/processed/bsts_gt/bsts_gt_metrics_by_model_horizon_threshold.csv
#   data/processed/bsts_gt/bsts_gt_h0_ranking.csv
#   data/processed/bsts_gt/bsts_gt_selected_predictors_by_fold.csv
#   data/processed/bsts_gt/bsts_gt_predictor_inclusion_by_fold.csv
#   data/processed/bsts_gt/bsts_gt_predictor_inclusion_summary.csv
#   data/processed/bsts_gt/bsts_gt_vs_benchmarks_h0.csv
#   data/processed/bsts_gt/08_run_bsts_gt_MA_EMA_config.json
#   data/processed/bsts_gt/08_run_bsts_gt_MA_EMA_manifest.csv
#   data/processed/bsts_gt/08_run_bsts_gt_MA_EMA_report.md
#   data/processed/bsts_gt/figure_captions_step9.md
#   data/processed/bsts_gt/figures/*.png
#
# Pipeline rules:
#   - Frozen state specification: local level only.
#   - No SAWA in this step.
#   - No Google Trends placeholders after 2024 Q3.
#   - No global keyword filtering.
#   - No global standardisation.
#   - Within each rolling fold, zero-variance filtering, sparsity filtering,
#     and standardisation are fit on training predictor rows only.
#   - For h = 0, target-quarter Google Trends are used only as prediction
#     newdata. They are not used for preprocessing or feature filtering.
#   - For h = 1 and h = 2, only lag-aligned Google Trends available at the
#     forecast origin are used.
#   - Figures are generated without embedded titles or subtitles. Manuscript
#     captions should be supplied separately.
#
# Usage from project root:
#   Rscript scripts/08_run_bsts_gt_MA_EMA.R
#
# Common options:
#   Rscript scripts/08_run_bsts_gt_MA_EMA.R \
#     --processed-dir=data/processed \
#     --validation-dir=data/processed/validation \
#     --out-dir=data/processed/bsts_gt \
#     --horizons=h0,h1,h2 \
#     --aggregations=MA,EMA \
#     --thresholds=0.95,0.90,0.80,0.70,0.50,0.30 \
#     --niter=3000 \
#     --burn-fraction=0.25 \
#     --expected-model-size=10 \
#     --seed=20260616
#
# Quick diagnostic run:
#   Rscript scripts/08_run_bsts_gt_MA_EMA.R \
#     --horizons=h0 \
#     --thresholds=0.90 \
#     --niter=1000
#
# Required packages:
#   install.packages(c("bsts", "ggplot2"))
# -----------------------------------------------------------------------------

options(stringsAsFactors = FALSE, warn = 1)
suppressWarnings(try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE))

# -----------------------------
# 0. Argument handling
# -----------------------------
get_arg <- function(key, default) {
  args <- commandArgs(trailingOnly = TRUE)
  key_dash <- paste0("--", key)
  key_eq <- paste0(key_dash, "=")

  hit_eq <- args[startsWith(args, key_eq)]
  if (length(hit_eq) > 0L) return(sub(key_eq, "", hit_eq[[1]], fixed = TRUE))

  hit_pos <- which(args == key_dash)
  if (length(hit_pos) > 0L && hit_pos[[1]] < length(args)) {
    return(args[[hit_pos[[1]] + 1L]])
  }
  default
}

parse_csv_arg <- function(x, default = character()) {
  if (is.null(x) || !nzchar(trimws(x))) return(default)
  trimws(unlist(strsplit(x, ",", fixed = TRUE)))
}

parse_numeric_csv_arg <- function(x, default = numeric()) {
  out <- suppressWarnings(as.numeric(parse_csv_arg(x, character())))
  out <- out[!is.na(out)]
  if (length(out) == 0L) return(default)
  out
}

parse_bool <- function(x, default = FALSE) {
  if (is.null(x) || !nzchar(x)) return(default)
  tolower(trimws(x)) %in% c("true", "t", "1", "yes", "y")
}

PROJECT_ROOT <- normalizePath(get_arg("project-root", Sys.getenv("BSTS_PROJECT_ROOT", unset = getwd())), mustWork = FALSE)
PROCESSED_DIR_DEFAULT <- file.path(PROJECT_ROOT, "data", "processed")
PROCESSED_DIR <- normalizePath(get_arg("processed-dir", Sys.getenv("BSTS_PROCESSED_DIR", unset = PROCESSED_DIR_DEFAULT)), mustWork = FALSE)
VALIDATION_DIR <- normalizePath(get_arg("validation-dir", file.path(PROCESSED_DIR, "validation")), mustWork = FALSE)
BENCHMARKS_DIR <- normalizePath(get_arg("benchmarks-dir", file.path(PROCESSED_DIR, "benchmarks")), mustWork = FALSE)
BSTS_STATE_SPECS_DIR <- normalizePath(get_arg("bsts-state-specs-dir", file.path(PROCESSED_DIR, "bsts_state_specs")), mustWork = FALSE)
OUT_DIR <- normalizePath(get_arg("out-dir", file.path(PROCESSED_DIR, "bsts_gt")), mustWork = FALSE)
FIG_DIR <- file.path(OUT_DIR, "figures")

AGGREGATIONS <- toupper(parse_csv_arg(get_arg("aggregations", "MA,EMA"), c("MA", "EMA")))
HORIZONS <- tolower(parse_csv_arg(get_arg("horizons", "h0,h1,h2"), c("h0", "h1", "h2")))
THRESHOLDS_ARG <- get_arg("thresholds", "")
NITER <- as.integer(get_arg("niter", "3000"))
BURN_FRACTION <- as.numeric(get_arg("burn-fraction", "0.25"))
INTERVAL_LEVEL <- as.numeric(get_arg("interval-level", "95")) / 100
EXPECTED_MODEL_SIZE <- as.numeric(get_arg("expected-model-size", "10"))
SEED <- as.integer(get_arg("seed", "20260616"))
SAVE_MODELS <- parse_bool(get_arg("save-models", "false"), FALSE)
MAX_PREDICTORS <- suppressWarnings(as.numeric(get_arg("max-predictors", "Inf")))
if (is.na(MAX_PREDICTORS)) MAX_PREDICTORS <- Inf

STATE_SPEC <- "local_level"
PRIMARY_THRESHOLD <- 0.90
ACTIVE_AGGREGATIONS <- c("MA", "EMA")
ALLOWED_HORIZONS <- c("h0", "h1", "h2")

if (!all(AGGREGATIONS %in% ACTIVE_AGGREGATIONS)) {
  stop(sprintf("Unsupported aggregation(s): %s. Step 9 supports MA and EMA only.", paste(setdiff(AGGREGATIONS, ACTIVE_AGGREGATIONS), collapse = ", ")), call. = FALSE)
}
if (!all(HORIZONS %in% ALLOWED_HORIZONS)) {
  stop(sprintf("Unsupported horizon(s): %s. Use h0, h1, and/or h2.", paste(setdiff(HORIZONS, ALLOWED_HORIZONS), collapse = ", ")), call. = FALSE)
}
if (!is.finite(NITER) || NITER < 100L) stop("niter must be at least 100.", call. = FALSE)
if (!is.finite(BURN_FRACTION) || BURN_FRACTION < 0 || BURN_FRACTION >= 0.8) stop("burn-fraction must be in [0, 0.8).", call. = FALSE)
if (!is.finite(INTERVAL_LEVEL) || INTERVAL_LEVEL <= 0 || INTERVAL_LEVEL >= 1) stop("interval-level must be between 0 and 100.", call. = FALSE)

# -----------------------------
# 1. Package checks
# -----------------------------
required_packages <- c("bsts", "ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "Missing required package(s): ", paste(missing_packages, collapse = ", "),
      ". Install with install.packages(c(\"bsts\", \"ggplot2\")) before running Step 9."
    ),
    call. = FALSE
  )
}

# -----------------------------
# 2. Helper functions
# -----------------------------
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

candidate_dirs <- function(...) {
  unique(normalizePath(c(...), mustWork = FALSE))
}

locate_file <- function(filename, dirs, required = TRUE) {
  dirs <- unique(dirs)
  paths <- file.path(dirs, filename)
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0L) {
    if (isTRUE(required)) {
      stop(sprintf("Required file not found: %s\nSearched in:\n%s", filename, paste(dirs, collapse = "\n")), call. = FALSE)
    }
    return(NA_character_)
  }
  normalizePath(hit[[1]], mustWork = TRUE)
}

read_csv_utf8 <- function(path) {
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA", "NaN"), fileEncoding = "UTF-8-BOM")
}

write_csv_utf8 <- function(x, path) {
  write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

write_lines_utf8 <- function(lines, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con, useBytes = TRUE)
}

json_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub('"', '\\"', x)
  x <- gsub("\n", "\\n", x)
  x
}

json_array_chr <- function(x) {
  paste0("[", paste(sprintf('"%s"', json_escape(x)), collapse = ", "), "]")
}

json_array_num <- function(x) {
  paste0("[", paste(formatC(as.numeric(x), format = "f", digits = 2), collapse = ", "), "]")
}

sha_md5 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  unname(tools::md5sum(path))
}

as_bool_vec <- function(x) {
  if (is.logical(x)) return(x)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "y")
}

assert_columns <- function(df, cols, label) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0L) stop(sprintf("%s missing required columns: %s", label, paste(missing, collapse = ", ")), call. = FALSE)
}

safe_date <- function(x) {
  as.Date(x)
}

quarter_idx_rows <- function(panel, idx) {
  rows <- match(idx, panel$Quarter_Index)
  if (any(is.na(rows))) stop(sprintf("Quarter_Index not found in GT panel: %s", paste(idx[is.na(rows)], collapse = ", ")), call. = FALSE)
  rows
}

clip_label <- function(x, n = 60L) {
  x <- as.character(x)
  ifelse(nchar(x) > n, paste0(substr(x, 1L, n - 3L), "..."), x)
}

first_nonempty <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0L) return(NA_character_)
  x[[1L]]
}

make_unique_labels <- function(x) {
  x <- as.character(x)
  if (!anyDuplicated(x)) return(x)
  out <- x
  groups <- split(seq_along(x), x)
  for (idx in groups[vapply(groups, length, integer(1L)) > 1L]) {
    out[idx] <- paste0(out[idx], " [", seq_along(idx), "]")
  }
  out
}

make_model_id <- function(aggregation, threshold, horizon_label) {
  sprintf("BSTS_GT_%s_LL_Z%.2f_%s", aggregation, threshold, horizon_label)
}

horizon_file_name <- function(h) {
  if (h == "h0") return("rolling_origins_h0_nowcast.csv")
  if (h == "h1") return("rolling_origins_h1_forecast.csv")
  if (h == "h2") return("rolling_origins_h2_forecast_exploratory.csv")
  stop("Unknown horizon.", call. = FALSE)
}

horizon_output_name <- function(h) {
  if (h == "h0") return("bsts_gt_predictions_h0.csv")
  if (h == "h1") return("bsts_gt_predictions_h1.csv")
  if (h == "h2") return("bsts_gt_predictions_h2.csv")
  stop("Unknown horizon.", call. = FALSE)
}

get_candidate_columns <- function(panel, aggregation) {
  prefix <- paste0(aggregation, "__Keyword_")
  cols <- grep(paste0("^", prefix), names(panel), value = TRUE)
  cols <- cols[order(as.integer(sub(paste0("^", prefix), "", cols)))]
  if (length(cols) == 0L) stop(sprintf("No predictor columns found for aggregation %s", aggregation), call. = FALSE)
  cols
}

clean_predictor_names <- function(cols, aggregation) {
  sub(paste0("^", aggregation, "__"), "", cols)
}

fit_preprocess <- function(x_train_raw, x_new_raw, zero_threshold, max_predictors = Inf) {
  if (anyNA(x_train_raw)) stop("Missing values detected in training Google Trends predictors. Step 9 does not impute.", call. = FALSE)
  if (anyNA(x_new_raw)) stop("Missing values detected in prediction Google Trends newdata. Step 9 does not impute.", call. = FALSE)

  zero_prop <- colMeans(x_train_raw == 0)
  means <- colMeans(x_train_raw)
  sds <- apply(x_train_raw, 2L, stats::sd)
  zero_variance <- is.na(sds) | sds <= 0
  too_sparse <- is.na(zero_prop) | zero_prop > zero_threshold
  retain <- !zero_variance & !too_sparse

  retained <- names(retain)[retain]
  dropped_zero_variance <- names(retain)[zero_variance]
  dropped_sparsity <- names(retain)[!zero_variance & too_sparse]

  if (length(retained) > 0L && is.finite(max_predictors) && length(retained) > max_predictors) {
    # If a cap is requested, retain the densest and most variable predictors using training data only.
    score <- (1 - zero_prop[retained]) * sds[retained]
    retained <- retained[order(score, decreasing = TRUE)][seq_len(max_predictors)]
  }

  if (length(retained) == 0L) {
    return(list(
      x_train = NULL,
      x_new = NULL,
      retained = character(),
      dropped_zero_variance = dropped_zero_variance,
      dropped_sparsity = dropped_sparsity,
      zero_prop = zero_prop,
      means = means,
      sds = sds
    ))
  }

  mu <- means[retained]
  sig <- sds[retained]
  x_train_scaled <- sweep(sweep(x_train_raw[, retained, drop = FALSE], 2L, mu, "-"), 2L, sig, "/")
  x_new_scaled <- sweep(sweep(x_new_raw[, retained, drop = FALSE], 2L, mu, "-"), 2L, sig, "/")

  list(
    x_train = as.data.frame(x_train_scaled, check.names = FALSE),
    x_new = as.data.frame(x_new_scaled, check.names = FALSE),
    retained = retained,
    dropped_zero_variance = dropped_zero_variance,
    dropped_sparsity = dropped_sparsity,
    zero_prop = zero_prop,
    means = means,
    sds = sds
  )
}

extract_prediction <- function(pred, interval_level = 0.95) {
  alpha <- 1 - interval_level
  draws <- numeric()

  if (!is.null(pred$distribution)) {
    d <- pred$distribution
    if (is.matrix(d)) {
      draws <- as.numeric(d[, 1L])
    } else if (is.array(d)) {
      dim_d <- dim(d)
      draws <- as.numeric(d[, 1L])
    } else {
      draws <- as.numeric(d)
    }
    draws <- draws[is.finite(draws)]
  }

  mean_val <- NA_real_
  if (!is.null(pred$mean)) mean_val <- as.numeric(pred$mean[1L])
  if (!is.finite(mean_val) && length(draws) > 0L) mean_val <- mean(draws)
  median_val <- if (length(draws) > 0L) stats::median(draws) else mean_val

  lower <- NA_real_
  upper <- NA_real_
  if (!is.null(pred$interval)) {
    ints <- pred$interval
    if (is.matrix(ints)) {
      cn <- tolower(colnames(ints))
      low_idx <- grep("lower|lwr|2.5|0.025|low", cn)
      upp_idx <- grep("upper|upr|97.5|0.975|high", cn)
      if (length(low_idx) > 0L) lower <- as.numeric(ints[1L, low_idx[[1]]])
      if (length(upp_idx) > 0L) upper <- as.numeric(ints[1L, upp_idx[[1]]])
      if ((!is.finite(lower) || !is.finite(upper)) && ncol(ints) >= 2L) {
        row_vals <- as.numeric(ints[1L, ])
        lower <- min(row_vals, na.rm = TRUE)
        upper <- max(row_vals, na.rm = TRUE)
      }
    } else if (is.array(ints)) {
      vals <- as.numeric(ints[1L, ])
      lower <- min(vals, na.rm = TRUE)
      upper <- max(vals, na.rm = TRUE)
    }
  }

  if ((!is.finite(lower) || !is.finite(upper)) && length(draws) > 0L) {
    lower <- as.numeric(stats::quantile(draws, probs = alpha / 2, na.rm = TRUE, names = FALSE))
    upper <- as.numeric(stats::quantile(draws, probs = 1 - alpha / 2, na.rm = TRUE, names = FALSE))
  }

  data.frame(
    forecast_mean = mean_val,
    forecast_median = median_val,
    lower_95 = lower,
    upper_95 = upper,
    n_posterior_predictive_draws = length(draws),
    stringsAsFactors = FALSE
  )
}

extract_inclusion <- function(model, retained_predictors, burn) {
  if (length(retained_predictors) == 0L || is.null(model$coefficients)) {
    return(data.frame(
      predictor_id = character(),
      inclusion_probability = numeric(),
      coefficient_posterior_mean = numeric(),
      coefficient_posterior_median = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  coefs <- tryCatch(as.matrix(model$coefficients), error = function(e) NULL)
  if (is.null(coefs) || length(coefs) == 0L) {
    return(data.frame(
      predictor_id = retained_predictors,
      inclusion_probability = NA_real_,
      coefficient_posterior_mean = NA_real_,
      coefficient_posterior_median = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  if (nrow(coefs) == length(retained_predictors) && ncol(coefs) != length(retained_predictors)) {
    coefs <- t(coefs)
  }
  if (ncol(coefs) != length(retained_predictors)) {
    # Try to match by column names if available. Otherwise return NA rows.
    cn <- colnames(coefs)
    if (!is.null(cn) && all(retained_predictors %in% cn)) {
      coefs <- coefs[, retained_predictors, drop = FALSE]
    } else {
      return(data.frame(
        predictor_id = retained_predictors,
        inclusion_probability = NA_real_,
        coefficient_posterior_mean = NA_real_,
        coefficient_posterior_median = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
  }

  colnames(coefs) <- retained_predictors
  start_row <- min(nrow(coefs), burn + 1L)
  keep_rows <- seq.int(start_row, nrow(coefs))
  if (length(keep_rows) == 0L) keep_rows <- seq_len(nrow(coefs))
  coefs_burned <- coefs[keep_rows, , drop = FALSE]

  data.frame(
    predictor_id = retained_predictors,
    inclusion_probability = colMeans(abs(coefs_burned) > 1e-12, na.rm = TRUE),
    coefficient_posterior_mean = colMeans(coefs_burned, na.rm = TRUE),
    coefficient_posterior_median = apply(coefs_burned, 2L, stats::median, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

compute_metrics <- function(df, group_cols) {
  if (nrow(df) == 0L) return(data.frame())
  key <- interaction(df[, group_cols, drop = FALSE], drop = TRUE, sep = "___")
  pieces <- split(df, key)
  rows <- lapply(pieces, function(d) {
    abs_error <- abs(d$error)
    sq_error <- d$error^2
    ape <- ifelse(d$actual == 0, NA_real_, abs(d$error / d$actual) * 100)
    smape <- ifelse((abs(d$actual) + abs(d$forecast_mean)) == 0, NA_real_, 100 * abs(d$error) / ((abs(d$actual) + abs(d$forecast_mean)) / 2))
    covered <- ifelse(is.finite(d$lower_95) & is.finite(d$upper_95), d$actual >= d$lower_95 & d$actual <= d$upper_95, NA)
    width <- ifelse(is.finite(d$lower_95) & is.finite(d$upper_95), d$upper_95 - d$lower_95, NA_real_)
    alpha <- 1 - INTERVAL_LEVEL
    interval_score <- width + (2 / alpha) * (d$lower_95 - d$actual) * (d$actual < d$lower_95) +
      (2 / alpha) * (d$actual - d$upper_95) * (d$actual > d$upper_95)
    interval_score[!is.finite(interval_score)] <- NA_real_

    out <- d[1L, group_cols, drop = FALSE]
    data.frame(
      out,
      n_predictions = nrow(d),
      MAE = mean(abs_error, na.rm = TRUE),
      RMSE = sqrt(mean(sq_error, na.rm = TRUE)),
      MAPE = mean(ape, na.rm = TRUE),
      sMAPE = mean(smape, na.rm = TRUE),
      mean_error = mean(d$error, na.rm = TRUE),
      median_abs_error = stats::median(abs_error, na.rm = TRUE),
      coverage_95 = mean(covered, na.rm = TRUE),
      average_interval_width_95 = mean(width, na.rm = TRUE),
      mean_interval_score_95 = mean(interval_score, na.rm = TRUE),
      min_target_quarter = d$target_quarter[which.min(d$target_index)],
      max_target_quarter = d$target_quarter[which.max(d$target_index)],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

fit_one_bsts_gt <- function(y_train, x_train, x_new, niter, burn, expected_model_size, seed) {
  set.seed(seed)
  state_spec <- bsts::AddLocalLevel(list(), y_train)

  if (is.null(x_train) || ncol(x_train) == 0L) {
    model <- bsts::bsts(y_train, state.spec = state_spec, niter = niter, ping = 0)
    pred <- stats::predict(model, horizon = 1L, burn = burn, quantiles = c((1 - INTERVAL_LEVEL) / 2, 1 - (1 - INTERVAL_LEVEL) / 2))
    return(list(model = model, pred = pred))
  }

  train_df <- data.frame(y = y_train, x_train, check.names = FALSE)
  p <- ncol(x_train)
  expected_size <- min(max(1, expected_model_size), p)

  model <- bsts::bsts(
    y ~ . - 1,
    state.spec = state_spec,
    data = train_df,
    niter = niter,
    expected.model.size = expected_size,
    ping = 0
  )

  new_df <- as.data.frame(x_new, check.names = FALSE)
  pred <- stats::predict(
    model,
    horizon = 1L,
    newdata = new_df,
    burn = burn,
    quantiles = c((1 - INTERVAL_LEVEL) / 2, 1 - (1 - INTERVAL_LEVEL) / 2)
  )
  list(model = model, pred = pred)
}

save_model_object <- function(model, model_dir, model_id, origin_id) {
  ensure_dir(model_dir)
  path <- file.path(model_dir, paste0(model_id, "__", origin_id, ".rds"))
  saveRDS(model, path)
  path
}

# -----------------------------
# 3. Locate and load inputs
# -----------------------------
ensure_dir(OUT_DIR)
ensure_dir(FIG_DIR)
if (SAVE_MODELS) ensure_dir(file.path(OUT_DIR, "models"))

search_dirs_processed <- candidate_dirs(
  PROCESSED_DIR,
  file.path(PROJECT_ROOT, "data", "processed"),
  file.path(PROJECT_ROOT, "processed"),
  PROJECT_ROOT,
  getwd(),
  "/mnt/data"
)
search_dirs_validation <- candidate_dirs(
  VALIDATION_DIR,
  file.path(PROCESSED_DIR, "validation"),
  file.path(PROJECT_ROOT, "data", "processed", "validation"),
  file.path(PROJECT_ROOT, "step6_validation"),
  file.path(getwd(), "step6_validation"),
  "/mnt/data/step6_validation",
  "/mnt/data"
)
search_dirs_bench <- candidate_dirs(
  BENCHMARKS_DIR,
  file.path(PROCESSED_DIR, "benchmarks"),
  file.path(PROJECT_ROOT, "data", "processed", "benchmarks"),
  file.path(PROJECT_ROOT, "step7_benchmarks"),
  "/mnt/data/step7_benchmarks",
  "/mnt/data"
)
search_dirs_bsts_state <- candidate_dirs(
  BSTS_STATE_SPECS_DIR,
  file.path(PROCESSED_DIR, "bsts_state_specs"),
  file.path(PROJECT_ROOT, "data", "processed", "bsts_state_specs"),
  "/mnt/data"
)

GT_PANEL_PATH <- locate_file("modeling_panel_gt_overlap.csv", search_dirs_processed)
KEYWORD_DICTIONARY_PATH <- locate_file("keyword_dictionary_clean.csv", search_dirs_processed)
THRESHOLD_GRID_PATH <- locate_file("sparsity_threshold_grid.csv", search_dirs_validation, required = FALSE)

panel <- read_csv_utf8(GT_PANEL_PATH)
keyword_dictionary <- read_csv_utf8(KEYWORD_DICTIONARY_PATH)

required_panel_cols <- c("Quarter", "Year", "Q", "Quarter_Index", "quarter_start", "unemployment_rate_nationals")
assert_columns(panel, required_panel_cols, "modeling_panel_gt_overlap.csv")
panel <- panel[order(panel$Quarter_Index), , drop = FALSE]
panel$quarter_start <- safe_date(panel$quarter_start)

if (anyDuplicated(panel$Quarter_Index)) stop("Duplicate Quarter_Index values in GT overlap panel.", call. = FALSE)
if (any(is.na(panel$unemployment_rate_nationals))) stop("Missing unemployment values in GT overlap panel.", call. = FALSE)

# Thresholds: prefer explicit argument, then grid file, then hard-coded agreed grid.
if (nzchar(THRESHOLDS_ARG)) {
  THRESHOLDS <- parse_numeric_csv_arg(THRESHOLDS_ARG, numeric())
} else if (!is.na(THRESHOLD_GRID_PATH) && file.exists(THRESHOLD_GRID_PATH)) {
  th_grid <- read_csv_utf8(THRESHOLD_GRID_PATH)
  if ("zero_prop_threshold" %in% names(th_grid)) {
    THRESHOLDS <- as.numeric(th_grid$zero_prop_threshold)
  } else {
    THRESHOLDS <- c(0.95, 0.90, 0.80, 0.70, 0.50, 0.30)
  }
} else {
  THRESHOLDS <- c(0.95, 0.90, 0.80, 0.70, 0.50, 0.30)
}
THRESHOLDS <- sort(unique(round(THRESHOLDS, 2)), decreasing = TRUE)
if (!PRIMARY_THRESHOLD %in% THRESHOLDS) {
  THRESHOLDS <- sort(unique(c(THRESHOLDS, PRIMARY_THRESHOLD)), decreasing = TRUE)
}

# Validation files
validation_paths <- list()
for (h in HORIZONS) {
  validation_paths[[h]] <- locate_file(horizon_file_name(h), search_dirs_validation)
}
validation_plans <- lapply(validation_paths, read_csv_utf8)

# Optional comparison files
BENCHMARK_H0_PATH <- locate_file("benchmark_h0_model_ranking.csv", search_dirs_bench, required = FALSE)
BSTS_STATE_H0_PATH <- locate_file("bsts_state_spec_h0_ranking.csv", search_dirs_bsts_state, required = FALSE)

# -----------------------------
# 4. Main model loop
# -----------------------------
set.seed(SEED)
BURN <- max(1L, floor(NITER * BURN_FRACTION))

all_predictions <- list()
all_selection <- list()
all_inclusion <- list()
model_counter <- 0L

message("Step 9 BSTS + Google Trends run starting.")
message(sprintf("Aggregations: %s", paste(AGGREGATIONS, collapse = ", ")))
message(sprintf("Horizons: %s", paste(HORIZONS, collapse = ", ")))
message(sprintf("Thresholds: %s", paste(formatC(THRESHOLDS, format = "f", digits = 2), collapse = ", ")))
message(sprintf("niter: %d; burn: %d; state spec: %s", NITER, BURN, STATE_SPEC))

for (aggregation in AGGREGATIONS) {
  raw_cols <- get_candidate_columns(panel, aggregation)
  clean_cols <- clean_predictor_names(raw_cols, aggregation)

  predictor_panel <- panel[, raw_cols, drop = FALSE]
  names(predictor_panel) <- clean_cols

  for (threshold in THRESHOLDS) {
    for (h in HORIZONS) {
      plan <- validation_plans[[h]]
      assert_columns(plan, c(
        "fold_id", "origin_id", "design_label", "horizon_quarters", "target_index", "target_quarter",
        "model_response_start_index", "model_response_end_index", "model_predictor_start_index", "model_predictor_end_index",
        "prediction_predictor_index", "gt_preprocessing_fit_start_index", "gt_preprocessing_fit_end_index",
        "target_gt_used_for_preprocessing", "uses_future_gt"
      ), horizon_file_name(h))

      if (any(as_bool_vec(plan$uses_future_gt))) {
        stop(sprintf("Validation plan %s contains uses_future_gt = TRUE, which is not allowed.", h), call. = FALSE)
      }
      if (any(as_bool_vec(plan$target_gt_used_for_preprocessing))) {
        stop(sprintf("Validation plan %s contains target_gt_used_for_preprocessing = TRUE, which is not allowed.", h), call. = FALSE)
      }

      for (i in seq_len(nrow(plan))) {
        fold <- plan[i, , drop = FALSE]
        model_counter <- model_counter + 1L
        fold_seed <- SEED + model_counter
        model_id <- make_model_id(aggregation, threshold, h)
        start_time <- Sys.time()

        response_idx <- seq.int(as.integer(fold$model_response_start_index), as.integer(fold$model_response_end_index))
        predictor_idx <- seq.int(as.integer(fold$model_predictor_start_index), as.integer(fold$model_predictor_end_index))
        if (length(response_idx) != length(predictor_idx)) {
          stop(sprintf("Fold %s has mismatched response and predictor lengths.", fold$origin_id), call. = FALSE)
        }

        response_rows <- quarter_idx_rows(panel, response_idx)
        predictor_rows <- quarter_idx_rows(panel, predictor_idx)
        new_row <- quarter_idx_rows(panel, as.integer(fold$prediction_predictor_index))
        target_row <- quarter_idx_rows(panel, as.integer(fold$target_index))

        y_train <- as.numeric(panel$unemployment_rate_nationals[response_rows])
        x_train_raw <- predictor_panel[predictor_rows, , drop = FALSE]
        x_new_raw <- predictor_panel[new_row, , drop = FALSE]

        prep <- fit_preprocess(x_train_raw, x_new_raw, threshold, MAX_PREDICTORS)

        fit <- tryCatch(
          fit_one_bsts_gt(
            y_train = y_train,
            x_train = prep$x_train,
            x_new = prep$x_new,
            niter = NITER,
            burn = BURN,
            expected_model_size = EXPECTED_MODEL_SIZE,
            seed = fold_seed
          ),
          error = function(e) e
        )

        if (inherits(fit, "error")) {
          warning(sprintf("Model failed: %s / %s / %s / %s: %s", aggregation, threshold, h, fold$origin_id, fit$message))
          pred_values <- data.frame(
            forecast_mean = NA_real_,
            forecast_median = NA_real_,
            lower_95 = NA_real_,
            upper_95 = NA_real_,
            n_posterior_predictive_draws = NA_integer_,
            stringsAsFactors = FALSE
          )
          model_path <- NA_character_
          inc <- data.frame()
          status <- "failed"
          error_message <- fit$message
        } else {
          pred_values <- extract_prediction(fit$pred, INTERVAL_LEVEL)
          if (SAVE_MODELS) {
            model_path <- save_model_object(fit$model, file.path(OUT_DIR, "models"), model_id, fold$origin_id)
          } else {
            model_path <- NA_character_
          }
          inc <- extract_inclusion(fit$model, prep$retained, BURN)
          status <- "success"
          error_message <- ""
        }

        actual <- as.numeric(panel$unemployment_rate_nationals[target_row])
        forecast_mean <- as.numeric(pred_values$forecast_mean[[1]])
        error <- actual - forecast_mean

        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

        prediction_row <- data.frame(
          model_id = model_id,
          model_family = "BSTS_GT_LL",
          aggregation = aggregation,
          state_spec = STATE_SPEC,
          zero_prop_threshold = threshold,
          threshold_role = ifelse(abs(threshold - PRIMARY_THRESHOLD) < 1e-9, "primary", "sensitivity"),
          horizon = h,
          design_label = fold$design_label,
          horizon_quarters = as.integer(fold$horizon_quarters),
          fold_id = as.integer(fold$fold_id),
          origin_id = fold$origin_id,
          target_index = as.integer(fold$target_index),
          target_quarter = fold$target_quarter,
          target_quarter_start = as.character(panel$quarter_start[target_row]),
          prediction_predictor_index = as.integer(fold$prediction_predictor_index),
          prediction_predictor_quarter = fold$prediction_predictor_quarter,
          actual = actual,
          forecast_mean = forecast_mean,
          forecast_median = as.numeric(pred_values$forecast_median[[1]]),
          lower_95 = as.numeric(pred_values$lower_95[[1]]),
          upper_95 = as.numeric(pred_values$upper_95[[1]]),
          error = error,
          abs_error = abs(error),
          squared_error = error^2,
          ape = ifelse(actual == 0, NA_real_, abs(error / actual) * 100),
          smape = ifelse((abs(actual) + abs(forecast_mean)) == 0, NA_real_, 100 * abs(error) / ((abs(actual) + abs(forecast_mean)) / 2)),
          interval_covered_95 = ifelse(is.finite(pred_values$lower_95[[1]]) & is.finite(pred_values$upper_95[[1]]), actual >= pred_values$lower_95[[1]] & actual <= pred_values$upper_95[[1]], NA),
          interval_width_95 = ifelse(is.finite(pred_values$lower_95[[1]]) & is.finite(pred_values$upper_95[[1]]), pred_values$upper_95[[1]] - pred_values$lower_95[[1]], NA_real_),
          n_candidate_predictors = length(clean_cols),
          n_retained_predictors = length(prep$retained),
          n_dropped_zero_variance = length(prep$dropped_zero_variance),
          n_dropped_sparsity = length(prep$dropped_sparsity),
          expected_model_size = min(max(1, EXPECTED_MODEL_SIZE), max(1, length(prep$retained))),
          niter = NITER,
          burn = BURN,
          interval_level = INTERVAL_LEVEL,
          status = status,
          error_message = error_message,
          elapsed_seconds = elapsed,
          model_rds_path = model_path,
          stringsAsFactors = FALSE
        )
        all_predictions[[length(all_predictions) + 1L]] <- prediction_row

        selection_row <- data.frame(
          model_id = model_id,
          aggregation = aggregation,
          zero_prop_threshold = threshold,
          threshold_role = ifelse(abs(threshold - PRIMARY_THRESHOLD) < 1e-9, "primary", "sensitivity"),
          horizon = h,
          design_label = fold$design_label,
          fold_id = as.integer(fold$fold_id),
          origin_id = fold$origin_id,
          target_quarter = fold$target_quarter,
          training_predictor_start = fold$model_predictor_start_quarter,
          training_predictor_end = fold$model_predictor_end_quarter,
          n_candidate_predictors = length(clean_cols),
          n_retained_predictors = length(prep$retained),
          n_dropped_zero_variance = length(prep$dropped_zero_variance),
          n_dropped_sparsity = length(prep$dropped_sparsity),
          retained_predictors = paste(prep$retained, collapse = ";"),
          dropped_zero_variance_predictors = paste(prep$dropped_zero_variance, collapse = ";"),
          dropped_sparsity_predictors = paste(prep$dropped_sparsity, collapse = ";"),
          preprocessing_fit_scope = "training predictor rows only",
          target_gt_used_for_preprocessing = FALSE,
          uses_future_gt = FALSE,
          stringsAsFactors = FALSE
        )
        all_selection[[length(all_selection) + 1L]] <- selection_row

        if (nrow(inc) > 0L) {
          inc$model_id <- model_id
          inc$aggregation <- aggregation
          inc$zero_prop_threshold <- threshold
          inc$threshold_role <- ifelse(abs(threshold - PRIMARY_THRESHOLD) < 1e-9, "primary", "sensitivity")
          inc$horizon <- h
          inc$design_label <- fold$design_label
          inc$fold_id <- as.integer(fold$fold_id)
          inc$origin_id <- fold$origin_id
          inc$target_quarter <- fold$target_quarter
          inc$n_retained_predictors <- length(prep$retained)
          all_inclusion[[length(all_inclusion) + 1L]] <- inc
        }

        message(sprintf(
          "[%04d] %s | %s | z=%.2f | %s | fold %s | retained=%d | status=%s | %.1fs",
          model_counter, aggregation, h, threshold, model_id, fold$origin_id, length(prep$retained), status, elapsed
        ))
      }
    }
  }
}

predictions_all <- if (length(all_predictions) > 0L) do.call(rbind, all_predictions) else data.frame()
selection_all <- if (length(all_selection) > 0L) do.call(rbind, all_selection) else data.frame()
inclusion_by_fold <- if (length(all_inclusion) > 0L) do.call(rbind, all_inclusion) else data.frame()

# Add interval score after predictions are collected.
if (nrow(predictions_all) > 0L) {
  alpha <- 1 - INTERVAL_LEVEL
  predictions_all$interval_score_95 <- with(
    predictions_all,
    interval_width_95 +
      (2 / alpha) * (lower_95 - actual) * (actual < lower_95) +
      (2 / alpha) * (actual - upper_95) * (actual > upper_95)
  )
  predictions_all$interval_score_95[!is.finite(predictions_all$interval_score_95)] <- NA_real_
}

# -----------------------------
# 5. Metrics and summaries
# -----------------------------
metrics <- compute_metrics(
  predictions_all[predictions_all$status == "success", , drop = FALSE],
  group_cols = c("model_id", "model_family", "aggregation", "state_spec", "zero_prop_threshold", "threshold_role", "horizon", "design_label", "horizon_quarters")
)
if (nrow(metrics) > 0L) {
  metrics <- metrics[order(metrics$horizon, metrics$MAE, metrics$RMSE), , drop = FALSE]
  metrics$MAE_rank_within_horizon <- ave(metrics$MAE, metrics$horizon, FUN = function(x) rank(x, ties.method = "first"))
}

h0_ranking <- metrics[metrics$horizon == "h0", , drop = FALSE]
if (nrow(h0_ranking) > 0L) {
  h0_ranking <- h0_ranking[order(h0_ranking$MAE, h0_ranking$RMSE), , drop = FALSE]
  h0_ranking$MAE_rank <- seq_len(nrow(h0_ranking))
}

# Predictor inclusion summary.
if (nrow(inclusion_by_fold) > 0L) {
  # Join dictionary metadata.
  dict_cols <- intersect(c("Column_Name", "Clean_Column", "Keyword", "English_Translation"), names(keyword_dictionary))
  dict <- keyword_dictionary[, dict_cols, drop = FALSE]
  if ("Clean_Column" %in% names(dict)) {
    names(dict)[names(dict) == "Clean_Column"] <- "predictor_id"
  } else if ("Column_Name" %in% names(dict)) {
    names(dict)[names(dict) == "Column_Name"] <- "predictor_id"
  }
  inclusion_by_fold <- merge(inclusion_by_fold, dict, by = "predictor_id", all.x = TRUE, sort = FALSE)

  inc_key <- interaction(inclusion_by_fold$aggregation, inclusion_by_fold$zero_prop_threshold, inclusion_by_fold$horizon, inclusion_by_fold$predictor_id, drop = TRUE, sep = "___")
  inc_pieces <- split(inclusion_by_fold, inc_key)
  inc_summary_list <- lapply(inc_pieces, function(d) {
    data.frame(
      aggregation = d$aggregation[[1]],
      zero_prop_threshold = d$zero_prop_threshold[[1]],
      threshold_role = d$threshold_role[[1]],
      horizon = d$horizon[[1]],
      predictor_id = d$predictor_id[[1]],
      Keyword = if ("Keyword" %in% names(d)) d$Keyword[[1]] else NA_character_,
      English_Translation = if ("English_Translation" %in% names(d)) d$English_Translation[[1]] else NA_character_,
      n_folds_present = nrow(d),
      mean_inclusion_probability = mean(d$inclusion_probability, na.rm = TRUE),
      median_inclusion_probability = stats::median(d$inclusion_probability, na.rm = TRUE),
      max_inclusion_probability = max(d$inclusion_probability, na.rm = TRUE),
      mean_coefficient = mean(d$coefficient_posterior_mean, na.rm = TRUE),
      median_coefficient = stats::median(d$coefficient_posterior_median, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  inclusion_summary <- do.call(rbind, inc_summary_list)
  inclusion_summary <- inclusion_summary[order(inclusion_summary$horizon, -inclusion_summary$mean_inclusion_probability), , drop = FALSE]
  rownames(inclusion_summary) <- NULL
} else {
  inclusion_summary <- data.frame()
}

# Optional comparison with Step 7 and Step 8B.
comparison_rows <- list()
if (!is.na(BENCHMARK_H0_PATH) && file.exists(BENCHMARK_H0_PATH)) {
  bench <- read_csv_utf8(BENCHMARK_H0_PATH)
  if (all(c("model_label", "MAE") %in% names(bench))) {
    comparison_rows[[length(comparison_rows) + 1L]] <- data.frame(
      source = "Step7 benchmark",
      model = bench$model_label,
      MAE = bench$MAE,
      stringsAsFactors = FALSE
    )
  }
}
if (!is.na(BSTS_STATE_H0_PATH) && file.exists(BSTS_STATE_H0_PATH)) {
  st <- read_csv_utf8(BSTS_STATE_H0_PATH)
  model_col <- if ("model_label" %in% names(st)) "model_label" else if ("model_id" %in% names(st)) "model_id" else NA_character_
  if (!is.na(model_col) && "MAE" %in% names(st)) {
    comparison_rows[[length(comparison_rows) + 1L]] <- data.frame(
      source = "Step8B BSTS no-GT state spec",
      model = st[[model_col]],
      MAE = st$MAE,
      stringsAsFactors = FALSE
    )
  }
}
if (nrow(h0_ranking) > 0L) {
  comparison_rows[[length(comparison_rows) + 1L]] <- data.frame(
    source = "Step9 BSTS GT",
    model = h0_ranking$model_id,
    MAE = h0_ranking$MAE,
    stringsAsFactors = FALSE
  )
}
vs_benchmarks <- if (length(comparison_rows) > 0L) do.call(rbind, comparison_rows) else data.frame()
if (nrow(vs_benchmarks) > 0L) {
  vs_benchmarks <- vs_benchmarks[order(vs_benchmarks$MAE), , drop = FALSE]
  vs_benchmarks$MAE_rank <- seq_len(nrow(vs_benchmarks))
}

# -----------------------------
# 6. Write CSV outputs
# -----------------------------
OUTPUT_PRED_ALL <- file.path(OUT_DIR, "bsts_gt_predictions_all.csv")
OUTPUT_METRICS <- file.path(OUT_DIR, "bsts_gt_metrics_by_model_horizon_threshold.csv")
OUTPUT_H0_RANKING <- file.path(OUT_DIR, "bsts_gt_h0_ranking.csv")
OUTPUT_SELECTION <- file.path(OUT_DIR, "bsts_gt_selected_predictors_by_fold.csv")
OUTPUT_INCLUSION_BY_FOLD <- file.path(OUT_DIR, "bsts_gt_predictor_inclusion_by_fold.csv")
OUTPUT_INCLUSION_SUMMARY <- file.path(OUT_DIR, "bsts_gt_predictor_inclusion_summary.csv")
OUTPUT_VS_BENCHMARKS <- file.path(OUT_DIR, "bsts_gt_vs_benchmarks_h0.csv")

if (nrow(predictions_all) > 0L) {
  for (h in unique(predictions_all$horizon)) {
    write_csv_utf8(predictions_all[predictions_all$horizon == h, , drop = FALSE], file.path(OUT_DIR, horizon_output_name(h)))
  }
}
write_csv_utf8(predictions_all, OUTPUT_PRED_ALL)
write_csv_utf8(metrics, OUTPUT_METRICS)
write_csv_utf8(h0_ranking, OUTPUT_H0_RANKING)
write_csv_utf8(selection_all, OUTPUT_SELECTION)
write_csv_utf8(inclusion_by_fold, OUTPUT_INCLUSION_BY_FOLD)
write_csv_utf8(inclusion_summary, OUTPUT_INCLUSION_SUMMARY)
write_csv_utf8(vs_benchmarks, OUTPUT_VS_BENCHMARKS)

# -----------------------------
# 7. Figures with ggplot2, no embedded titles/subtitles
# -----------------------------
make_base_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank()
    )
}

save_plot <- function(plot, filename, width = 11, height = 7) {
  ggplot2::ggsave(file.path(FIG_DIR, filename), plot = plot, width = width, height = height, dpi = 300)
}

figure_files <- character()

if (nrow(h0_ranking) > 0L) {
  fig_data <- metrics[metrics$horizon == "h0", , drop = FALSE]
  fig_data$threshold_factor <- factor(formatC(fig_data$zero_prop_threshold, format = "f", digits = 2), levels = formatC(sort(unique(fig_data$zero_prop_threshold), decreasing = TRUE), format = "f", digits = 2))
  p <- ggplot2::ggplot(fig_data, ggplot2::aes(x = threshold_factor, y = MAE, group = aggregation, colour = aggregation, shape = aggregation)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::labs(x = "Zero-proportion threshold", y = "MAE (percentage points)", colour = "Aggregation", shape = "Aggregation") +
    make_base_theme()
  save_plot(p, "bsts_gt_h0_mae_by_aggregation_threshold.png", width = 9, height = 6)
  figure_files <- c(figure_files, file.path(FIG_DIR, "bsts_gt_h0_mae_by_aggregation_threshold.png"))

  best_model_id <- h0_ranking$model_id[[1]]
  best_pred <- predictions_all[predictions_all$model_id == best_model_id & predictions_all$horizon == "h0" & predictions_all$status == "success", , drop = FALSE]
  best_pred$target_quarter_start <- safe_date(best_pred$target_quarter_start)

  if (nrow(best_pred) > 0L) {
    actual_df <- data.frame(target_quarter_start = best_pred$target_quarter_start, target_quarter = best_pred$target_quarter, Series = "Actual", value = best_pred$actual, stringsAsFactors = FALSE)
    pred_df <- data.frame(target_quarter_start = best_pred$target_quarter_start, target_quarter = best_pred$target_quarter, Series = best_model_id, value = best_pred$forecast_mean, stringsAsFactors = FALSE)
    long_df <- rbind(actual_df, pred_df)
    p <- ggplot2::ggplot(long_df, ggplot2::aes(x = target_quarter_start, y = value, colour = Series, linetype = Series, shape = Series)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_point(size = 2.3) +
      ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      ggplot2::labs(x = "Target quarter", y = "Unemployment rate (%)", colour = "Series", linetype = "Series", shape = "Series") +
      make_base_theme()
    save_plot(p, "bsts_gt_h0_actual_vs_predicted_best.png", width = 11, height = 7)
    figure_files <- c(figure_files, file.path(FIG_DIR, "bsts_gt_h0_actual_vs_predicted_best.png"))

    p <- ggplot2::ggplot(best_pred, ggplot2::aes(x = target_quarter_start)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_95, ymax = upper_95), alpha = 0.20) +
      ggplot2::geom_line(ggplot2::aes(y = actual, linetype = "Actual"), linewidth = 0.9) +
      ggplot2::geom_point(ggplot2::aes(y = actual, shape = "Actual"), size = 2.2) +
      ggplot2::geom_line(ggplot2::aes(y = forecast_mean, linetype = best_model_id), linewidth = 0.9) +
      ggplot2::geom_point(ggplot2::aes(y = forecast_mean, shape = best_model_id), size = 2.2) +
      ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      ggplot2::labs(x = "Target quarter", y = "Unemployment rate (%)", linetype = "Series", shape = "Series") +
      make_base_theme()
    save_plot(p, "bsts_gt_h0_intervals_best.png", width = 11, height = 7)
    figure_files <- c(figure_files, file.path(FIG_DIR, "bsts_gt_h0_intervals_best.png"))
  }

  top_models <- h0_ranking$model_id[seq_len(min(4L, nrow(h0_ranking)))]
  err_df <- predictions_all[predictions_all$model_id %in% top_models & predictions_all$horizon == "h0" & predictions_all$status == "success", , drop = FALSE]
  err_df$target_quarter_start <- safe_date(err_df$target_quarter_start)
  if (nrow(err_df) > 0L) {
    p <- ggplot2::ggplot(err_df, ggplot2::aes(x = target_quarter_start, y = error, colour = model_id, linetype = model_id, shape = model_id)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 2.1) +
      ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      ggplot2::labs(x = "Target quarter", y = "Error: actual - forecast", colour = "Model", linetype = "Model", shape = "Model") +
      make_base_theme()
    save_plot(p, "bsts_gt_h0_rolling_errors_best_models.png", width = 11, height = 7)
    figure_files <- c(figure_files, file.path(FIG_DIR, "bsts_gt_h0_rolling_errors_best_models.png"))
  }
}

if (nrow(vs_benchmarks) > 0L) {
  comp <- vs_benchmarks[seq_len(min(20L, nrow(vs_benchmarks))), , drop = FALSE]
  comp_model_levels <- unique(comp$model[order(comp$MAE)])
  comp$model <- factor(comp$model, levels = comp_model_levels)
  p <- ggplot2::ggplot(comp, ggplot2::aes(x = model, y = MAE, fill = source)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", MAE)), vjust = -0.25, size = 3.2) +
    ggplot2::labs(x = "Model", y = "MAE (percentage points)", fill = "Source") +
    make_base_theme()
  save_plot(p, "bsts_gt_vs_benchmarks_h0.png", width = 12, height = 7)
  figure_files <- c(figure_files, file.path(FIG_DIR, "bsts_gt_vs_benchmarks_h0.png"))
}

if (nrow(inclusion_summary) > 0L) {
  inc_plot <- inclusion_summary[inclusion_summary$horizon == "h0", , drop = FALSE]
  if (nrow(inc_plot) > 0L) {
    # `inclusion_summary` has one row per aggregation x threshold x horizon x predictor.
    # For the top-keyword figure we collapse across aggregation and threshold so each
    # keyword appears once; otherwise repeated predictor labels create duplicated
    # factor levels in ggplot.
    inc_pieces <- split(inc_plot, inc_plot$predictor_id)
    inc_plot <- do.call(rbind, lapply(inc_pieces, function(d) {
      data.frame(
        predictor_id = d$predictor_id[[1L]],
        Keyword = if ("Keyword" %in% names(d)) first_nonempty(d$Keyword) else NA_character_,
        English_Translation = if ("English_Translation" %in% names(d)) first_nonempty(d$English_Translation) else NA_character_,
        n_model_specs_present = nrow(d),
        mean_inclusion_probability = mean(d$mean_inclusion_probability, na.rm = TRUE),
        median_inclusion_probability = stats::median(d$median_inclusion_probability, na.rm = TRUE),
        max_inclusion_probability = max(d$max_inclusion_probability, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
    inc_plot$mean_inclusion_probability[!is.finite(inc_plot$mean_inclusion_probability)] <- NA_real_
    inc_plot <- inc_plot[is.finite(inc_plot$mean_inclusion_probability), , drop = FALSE]
    inc_plot <- inc_plot[order(-inc_plot$mean_inclusion_probability), , drop = FALSE]
    inc_plot <- inc_plot[seq_len(min(25L, nrow(inc_plot))), , drop = FALSE]
    inc_plot$label_raw <- ifelse(
      !is.na(inc_plot$English_Translation) & nzchar(inc_plot$English_Translation),
      paste0(inc_plot$predictor_id, " — ", inc_plot$English_Translation),
      inc_plot$predictor_id
    )
    inc_plot$label_chr <- make_unique_labels(clip_label(inc_plot$label_raw, 55L))
    inc_plot$label <- factor(inc_plot$label_chr, levels = rev(inc_plot$label_chr))
    p <- ggplot2::ggplot(inc_plot, ggplot2::aes(x = label, y = mean_inclusion_probability)) +
      ggplot2::geom_col(width = 0.75) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "Keyword", y = "Mean posterior inclusion probability") +
      make_base_theme() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0))
    save_plot(p, "bsts_gt_predictor_inclusion_top_keywords.png", width = 10, height = 8)
    figure_files <- c(figure_files, file.path(FIG_DIR, "bsts_gt_predictor_inclusion_top_keywords.png"))
  }
}

if (nrow(selection_all) > 0L) {
  sel_plot <- selection_all[selection_all$horizon == "h0", , drop = FALSE]
  if (nrow(sel_plot) > 0L) {
    cnt <- aggregate(n_retained_predictors ~ aggregation + zero_prop_threshold, data = sel_plot, FUN = mean)
    cnt$threshold_factor <- factor(formatC(cnt$zero_prop_threshold, format = "f", digits = 2), levels = formatC(sort(unique(cnt$zero_prop_threshold), decreasing = TRUE), format = "f", digits = 2))
    p <- ggplot2::ggplot(cnt, ggplot2::aes(x = threshold_factor, y = n_retained_predictors, group = aggregation, colour = aggregation, shape = aggregation)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 2.6) +
      ggplot2::labs(x = "Zero-proportion threshold", y = "Mean retained predictors per fold", colour = "Aggregation", shape = "Aggregation") +
      make_base_theme()
    save_plot(p, "bsts_gt_retained_predictors_by_threshold.png", width = 9, height = 6)
    figure_files <- c(figure_files, file.path(FIG_DIR, "bsts_gt_retained_predictors_by_threshold.png"))
  }
}

# -----------------------------
# 8. Config, report, captions, manifest
# -----------------------------
OUTPUT_CONFIG <- file.path(OUT_DIR, "08_run_bsts_gt_MA_EMA_config.json")
OUTPUT_REPORT <- file.path(OUT_DIR, "08_run_bsts_gt_MA_EMA_report.md")
OUTPUT_MANIFEST <- file.path(OUT_DIR, "08_run_bsts_gt_MA_EMA_manifest.csv")
OUTPUT_CAPTIONS <- file.path(OUT_DIR, "figure_captions_step9.md")

best_line <- if (nrow(h0_ranking) > 0L) {
  sprintf("Best h = 0 model by MAE: `%s` with MAE %.3f percentage points.", h0_ranking$model_id[[1]], h0_ranking$MAE[[1]])
} else {
  "No successful h = 0 model run was available for ranking."
}

config_lines <- c(
  "{",
  '  "script": "08_run_bsts_gt_MA_EMA.R",',
  '  "step": "Step 9 — BSTS with Google Trends, MA and EMA",',
  sprintf('  "generated_at": "%s",', format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  sprintf('  "state_specification": "%s",', STATE_SPEC),
  sprintf('  "aggregations": %s,', json_array_chr(AGGREGATIONS)),
  sprintf('  "horizons": %s,', json_array_chr(HORIZONS)),
  sprintf('  "zero_proportion_thresholds": %s,', json_array_num(THRESHOLDS)),
  sprintf('  "primary_zero_proportion_threshold": %.2f,', PRIMARY_THRESHOLD),
  sprintf('  "niter": %d,', NITER),
  sprintf('  "burn": %d,', BURN),
  sprintf('  "burn_fraction": %.3f,', BURN_FRACTION),
  sprintf('  "interval_level": %.2f,', INTERVAL_LEVEL),
  sprintf('  "expected_model_size": %.1f,', EXPECTED_MODEL_SIZE),
  sprintf('  "seed": %d,', SEED),
  '  "leakage_prevention": {',
  '    "global_feature_selection": false,',
  '    "global_standardisation": false,',
  '    "preprocessing_fit_scope": "inside each rolling training window only",',
  '    "target_quarter_gt_used_for_preprocessing": false,',
  '    "future_gt_placeholders": false',
  '  },',
  '  "figures": {',
  '    "titles_or_subtitles_embedded": false,',
  '    "captions_file": "figure_captions_step9.md"',
  '  }',
  "}"
)
write_lines_utf8(config_lines, OUTPUT_CONFIG)

report_lines <- c(
  "# Step 9 — BSTS with Google Trends, MA and EMA",
  "",
  "## Status",
  "",
  "Step 9 fits Google-Trends-augmented BSTS models using the frozen local-level state specification selected in Step 8B.",
  "",
  "## Model contract",
  "",
  "```text",
  "State specification: local level only",
  sprintf("Aggregations: %s", paste(AGGREGATIONS, collapse = ", ")),
  sprintf("Horizons: %s", paste(HORIZONS, collapse = ", ")),
  sprintf("Sparsity thresholds: %s", paste(formatC(THRESHOLDS, format = "f", digits = 2), collapse = ", ")),
  sprintf("Primary threshold: %.2f", PRIMARY_THRESHOLD),
  sprintf("niter: %d", NITER),
  sprintf("burn: %d", BURN),
  "SAWA: deferred",
  "```",
  "",
  "## Leakage prevention",
  "",
  "For every rolling fold, the script estimates zero-variance filtering, zero-proportion filtering, and standardisation on training predictor rows only. The same retained columns and training-window scaling parameters are then applied to the validation newdata. Target-quarter Google Trends are not used for preprocessing in the h = 0 nowcast design, and future Google Trends are never used in h = 1 or h = 2.",
  "",
  "## Result summary",
  "",
  best_line,
  "",
  "## Files written",
  "",
  "- `bsts_gt_predictions_h0.csv`, if h0 was run",
  "- `bsts_gt_predictions_h1.csv`, if h1 was run",
  "- `bsts_gt_predictions_h2.csv`, if h2 was run",
  "- `bsts_gt_predictions_all.csv`",
  "- `bsts_gt_metrics_by_model_horizon_threshold.csv`",
  "- `bsts_gt_h0_ranking.csv`",
  "- `bsts_gt_selected_predictors_by_fold.csv`",
  "- `bsts_gt_predictor_inclusion_by_fold.csv`",
  "- `bsts_gt_predictor_inclusion_summary.csv`",
  "- `bsts_gt_vs_benchmarks_h0.csv`",
  "- `08_run_bsts_gt_MA_EMA_config.json`",
  "- `08_run_bsts_gt_MA_EMA_manifest.csv`",
  "- `figure_captions_step9.md`",
  "",
  "## Figure style",
  "",
  "Figures are exported without embedded titles or subtitles. Use the separate captions file in the manuscript."
)
write_lines_utf8(report_lines, OUTPUT_REPORT)

caption_lines <- c(
  "# Step 9 figure captions",
  "",
  "Use these captions in the manuscript or supplement. The PNG files do not include embedded titles or subtitles.",
  "",
  "**bsts_gt_h0_mae_by_aggregation_threshold.png.** Mean absolute error for h = 0 Google-Trends-augmented BSTS nowcasts by aggregation method and training-window zero-proportion threshold.",
  "",
  "**bsts_gt_h0_actual_vs_predicted_best.png.** Official unemployment rate and point nowcasts from the best h = 0 Google-Trends-augmented BSTS specification by rolling-origin MAE.",
  "",
  "**bsts_gt_h0_intervals_best.png.** Official unemployment rate, point nowcasts, and 95% predictive intervals for the best h = 0 Google-Trends-augmented BSTS specification.",
  "",
  "**bsts_gt_h0_rolling_errors_best_models.png.** Rolling-origin h = 0 errors for the best Google-Trends-augmented BSTS specifications. Error is defined as actual minus forecast.",
  "",
  "**bsts_gt_vs_benchmarks_h0.png.** h = 0 MAE comparison across available unemployment-only benchmarks, no-GT BSTS state specifications, and Google-Trends-augmented BSTS models.",
  "",
  "**bsts_gt_predictor_inclusion_top_keywords.png.** Highest mean posterior inclusion probabilities among Google Trends predictors in the h = 0 validation design.",
  "",
  "**bsts_gt_retained_predictors_by_threshold.png.** Mean number of retained Google Trends predictors per fold by aggregation method and zero-proportion threshold."
)
write_lines_utf8(caption_lines, OUTPUT_CAPTIONS)

# Manifest includes only files that actually exist.
output_files <- c(
  file.path(OUT_DIR, "bsts_gt_predictions_h0.csv"),
  file.path(OUT_DIR, "bsts_gt_predictions_h1.csv"),
  file.path(OUT_DIR, "bsts_gt_predictions_h2.csv"),
  OUTPUT_PRED_ALL,
  OUTPUT_METRICS,
  OUTPUT_H0_RANKING,
  OUTPUT_SELECTION,
  OUTPUT_INCLUSION_BY_FOLD,
  OUTPUT_INCLUSION_SUMMARY,
  OUTPUT_VS_BENCHMARKS,
  OUTPUT_CONFIG,
  OUTPUT_REPORT,
  OUTPUT_CAPTIONS,
  figure_files
)
output_files <- output_files[file.exists(output_files)]
manifest <- data.frame(
  file_name = basename(output_files),
  relative_path = sub(paste0("^", gsub("\\\\", "/", normalizePath(PROJECT_ROOT, mustWork = FALSE)), "/?"), "", gsub("\\\\", "/", normalizePath(output_files, mustWork = FALSE))),
  file_type = tools::file_ext(output_files),
  checksum_md5 = vapply(output_files, sha_md5, character(1L)),
  stringsAsFactors = FALSE
)
write_csv_utf8(manifest, OUTPUT_MANIFEST)

message("Step 9 BSTS + Google Trends run complete.")
message(sprintf("Outputs written to: %s", OUT_DIR))
if (nrow(h0_ranking) > 0L) message(best_line)
