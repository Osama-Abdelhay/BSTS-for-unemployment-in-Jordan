#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# 10_mcmc_stability_check.R
# Step 11: MCMC stability and robustness check for top BSTS models
# -----------------------------------------------------------------------------
# Purpose:
#   Re-run the top stochastic BSTS models using longer MCMC chains and multiple
#   seeds to check whether the final ranking is stable. This script does not
#   introduce new model families. It focuses on the h = 0 rolling nowcast design
#   and on the top models selected in Steps 8B-10.
#
# Default models:
#   1. BSTS_NO_GT_LL                  local-level BSTS without Google Trends
#   2. BSTS_GT_EMA_LL_Z0.80_h0        local-level BSTS + EMA GT, threshold 0.80
#   3. BSTS_GT_EMA_LL_Z0.70_h0        sensitivity GT model close to top variant
#   4. BSTS_GT_EMA_LL_Z0.90_h0        sensitivity GT model near primary threshold
#
# Inputs:
#   data/processed/modeling_panel_gt_overlap.csv
#   data/processed/keyword_dictionary_clean.csv
#   data/processed/validation/rolling_origins_h0_nowcast.csv
#   Optional for reference lines/tables:
#     data/processed/benchmarks/benchmark_h0_model_ranking.csv
#     data/processed/final_evaluation/final_model_comparison_h0.csv
#
# Outputs:
#   data/processed/mcmc_stability/mcmc_stability_predictions_h0.csv
#   data/processed/mcmc_stability/mcmc_stability_metrics_by_seed.csv
#   data/processed/mcmc_stability/mcmc_stability_summary_by_model.csv
#   data/processed/mcmc_stability/mcmc_stability_model_ranking_by_seed.csv
#   data/processed/mcmc_stability/mcmc_stability_pairwise_differences.csv
#   data/processed/mcmc_stability/mcmc_stability_predictor_inclusion_by_seed_fold.csv
#   data/processed/mcmc_stability/mcmc_stability_predictor_inclusion_summary.csv
#   data/processed/mcmc_stability/mcmc_stability_decision_summary.csv
#   data/processed/mcmc_stability/10_mcmc_stability_check_config.json
#   data/processed/mcmc_stability/10_mcmc_stability_check_manifest.csv
#   data/processed/mcmc_stability/10_mcmc_stability_check_report.md
#   data/processed/mcmc_stability/figure_captions_step11.md
#   data/processed/mcmc_stability/figures/*.png
#
# Pipeline rules:
#   - h = 0 only by default; target unemployment is withheld.
#   - Frozen state specification: local level only.
#   - GT preprocessing remains fold-specific: zero-variance filtering,
#     sparsity filtering, and standardisation are fitted on training predictor
#     rows only.
#   - Target-quarter GT is used only as newdata for h = 0 predictions.
#   - No SAWA, no future GT placeholders, no global feature selection.
#   - Figures are publication-style: no embedded titles or subtitles.
#
# Usage from project root:
#   Rscript scripts/10_mcmc_stability_check.R
#
# Full example:
#   Rscript scripts/10_mcmc_stability_check.R \
#     --processed-dir=data/processed \
#     --validation-dir=data/processed/validation \
#     --out-dir=data/processed/mcmc_stability \
#     --models=BSTS_NO_GT_LL,BSTS_GT_EMA_LL_Z0.80_h0,BSTS_GT_EMA_LL_Z0.70_h0,BSTS_GT_EMA_LL_Z0.90_h0 \
#     --seeds=20260616,20260617,20260618,20260619,20260620 \
#     --niter=10000 \
#     --burn-fraction=0.25 \
#     --expected-model-size=10
#
# Quick diagnostic run:
#   Rscript scripts/10_mcmc_stability_check.R \
#     --models=BSTS_NO_GT_LL,BSTS_GT_EMA_LL_Z0.80_h0 \
#     --seeds=20260616,20260617 \
#     --niter=2000
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
  if (length(hit_pos) > 0L && hit_pos[[1]] < length(args)) return(args[[hit_pos[[1]] + 1L]])
  default
}

parse_csv_arg <- function(x, default = character()) {
  if (is.null(x) || !nzchar(trimws(x))) return(default)
  trimws(unlist(strsplit(x, ",", fixed = TRUE)))
}

parse_int_csv_arg <- function(x, default = integer()) {
  out <- suppressWarnings(as.integer(parse_csv_arg(x, character())))
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
FINAL_EVAL_DIR <- normalizePath(get_arg("final-evaluation-dir", file.path(PROCESSED_DIR, "final_evaluation")), mustWork = FALSE)
OUT_DIR <- normalizePath(get_arg("out-dir", file.path(PROCESSED_DIR, "mcmc_stability")), mustWork = FALSE)
FIG_DIR <- file.path(OUT_DIR, "figures")

MODELS <- parse_csv_arg(
  get_arg("models", "BSTS_NO_GT_LL,BSTS_GT_EMA_LL_Z0.80_h0,BSTS_GT_EMA_LL_Z0.70_h0,BSTS_GT_EMA_LL_Z0.90_h0"),
  c("BSTS_NO_GT_LL", "BSTS_GT_EMA_LL_Z0.80_h0")
)
SEEDS <- parse_int_csv_arg(get_arg("seeds", "20260616,20260617,20260618,20260619,20260620"), c(20260616L, 20260617L))
NITER <- as.integer(get_arg("niter", "10000"))
BURN_FRACTION <- as.numeric(get_arg("burn-fraction", "0.25"))
INTERVAL_LEVEL <- as.numeric(get_arg("interval-level", "95")) / 100
EXPECTED_MODEL_SIZE <- as.numeric(get_arg("expected-model-size", "10"))
MAX_PREDICTORS <- suppressWarnings(as.numeric(get_arg("max-predictors", "Inf")))
if (is.na(MAX_PREDICTORS)) MAX_PREDICTORS <- Inf
SAVE_MODELS <- parse_bool(get_arg("save-models", "false"), FALSE)
STOP_ON_MODEL_FAILURE <- parse_bool(get_arg("stop-on-model-failure", "false"), FALSE)
PRIMARY_REFERENCE_MODEL <- get_arg("primary-reference-model", "RW")

if (!is.finite(NITER) || NITER < 500L) stop("niter must be at least 500 for Step 11.", call. = FALSE)
if (!is.finite(BURN_FRACTION) || BURN_FRACTION < 0 || BURN_FRACTION >= 0.8) stop("burn-fraction must be in [0, 0.8).", call. = FALSE)
if (!is.finite(INTERVAL_LEVEL) || INTERVAL_LEVEL <= 0 || INTERVAL_LEVEL >= 1) stop("interval-level must be between 0 and 100.", call. = FALSE)
if (length(SEEDS) < 2L) warning("Step 11 is most useful with at least two seeds.")

# -----------------------------
# 1. Package checks
# -----------------------------
required_packages <- c("bsts", "ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "Missing required package(s): ", paste(missing_packages, collapse = ", "),
      ". Install with install.packages(c(\"bsts\", \"ggplot2\")) before running Step 11."
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

candidate_dirs <- function(...) unique(normalizePath(c(...), mustWork = FALSE))

locate_file <- function(filename, dirs, required = TRUE) {
  dirs <- unique(dirs)
  paths <- file.path(dirs, filename)
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0L) {
    if (isTRUE(required)) stop(sprintf("Required file not found: %s\nSearched in:\n%s", filename, paste(dirs, collapse = "\n")), call. = FALSE)
    return(NA_character_)
  }
  normalizePath(hit[[1]], mustWork = TRUE)
}

read_csv_utf8 <- function(path) {
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA", "NaN"), fileEncoding = "UTF-8-BOM")
}

write_csv_utf8 <- function(x, path) write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")

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

json_array_chr <- function(x) paste0("[", paste(sprintf('"%s"', json_escape(x)), collapse = ", "), "]")
json_array_num <- function(x) paste0("[", paste(as.character(x), collapse = ", "), "]")

sha_md5 <- function(path) unname(tools::md5sum(path))

assert_columns <- function(df, cols, label) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0L) stop(sprintf("%s is missing required columns: %s", label, paste(missing, collapse = ", ")), call. = FALSE)
}

as_bool_vec <- function(x) {
  if (is.logical(x)) return(x)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "y")
}

quarter_idx_rows <- function(panel, idx) {
  rows <- match(idx, panel$Quarter_Index)
  if (any(is.na(rows))) stop(sprintf("Quarter_Index values not found: %s", paste(idx[is.na(rows)], collapse = ", ")), call. = FALSE)
  rows
}

safe_date_chr <- function(x) as.character(x)

clip_label <- function(x, max_chars = 60L) {
  x <- as.character(x)
  too_long <- nchar(x, type = "chars") > max_chars
  x[too_long] <- paste0(substr(x[too_long], 1L, max_chars - 1L), "…")
  x
}

make_unique_labels <- function(labels) {
  labels <- as.character(labels)
  dup <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  if (any(dup)) labels[dup] <- make.unique(labels[dup], sep = " #")
  labels
}

save_plot <- function(plot, filename, width = 10, height = 6, dpi = 300) {
  ggplot2::ggsave(filename = file.path(FIG_DIR, filename), plot = plot, width = width, height = height, dpi = dpi, bg = "white")
}

make_base_theme <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = base_size),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.minor = ggplot2::element_line(linewidth = 0.25),
      panel.grid.major = ggplot2::element_line(linewidth = 0.35)
    )
}

get_candidate_columns <- function(panel, aggregation) {
  prefix <- paste0(aggregation, "__Keyword_")
  cols <- grep(paste0("^", prefix), names(panel), value = TRUE)
  cols <- cols[order(as.integer(sub(paste0("^", prefix), "", cols)))]
  if (length(cols) == 0L) stop(sprintf("No predictor columns found for aggregation %s", aggregation), call. = FALSE)
  cols
}

clean_predictor_names <- function(cols, aggregation) sub(paste0("^", aggregation, "__"), "", cols)

parse_bsts_model_id <- function(model_id) {
  if (identical(model_id, "BSTS_NO_GT_LL")) {
    return(list(model_id = model_id, model_type = "NO_GT", aggregation = NA_character_, threshold = NA_real_))
  }
  # Expected form: BSTS_GT_EMA_LL_Z0.80_h0 or BSTS_GT_MA_LL_Z0.70_h0
  m <- regexec("^BSTS_GT_(MA|EMA)_LL_Z([0-9.]+)_h0$", model_id)
  hit <- regmatches(model_id, m)[[1]]
  if (length(hit) == 0L) stop(sprintf("Unsupported Step 11 model id: %s", model_id), call. = FALSE)
  list(model_id = model_id, model_type = "GT", aggregation = hit[[2]], threshold = as.numeric(hit[[3]]))
}

model_parsed <- lapply(MODELS, parse_bsts_model_id)
model_grid <- do.call(rbind, lapply(model_parsed, function(x) {
  data.frame(model_id = x$model_id, model_type = x$model_type, aggregation = x$aggregation, zero_prop_threshold = x$threshold, stringsAsFactors = FALSE)
}))
model_grid$model_order <- seq_len(nrow(model_grid))

fit_preprocess <- function(x_train_raw, x_new_raw, zero_threshold, max_predictors = Inf) {
  if (anyNA(x_train_raw)) stop("Missing values detected in training Google Trends predictors. Step 11 does not impute.", call. = FALSE)
  if (anyNA(x_new_raw)) stop("Missing values detected in prediction Google Trends newdata. Step 11 does not impute.", call. = FALSE)

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
    score <- (1 - zero_prop[retained]) * sds[retained]
    retained <- retained[order(score, decreasing = TRUE)][seq_len(max_predictors)]
  }

  if (length(retained) == 0L) {
    return(list(
      x_train = NULL, x_new = NULL, retained = character(), dropped_zero_variance = dropped_zero_variance,
      dropped_sparsity = dropped_sparsity, zero_prop = zero_prop, means = means, sds = sds
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
      draws <- as.numeric(d[, 1L])
    } else {
      draws <- as.numeric(d)
    }
    draws <- draws[is.finite(draws)]
  }

  point <- NA_real_
  if (!is.null(pred$mean)) point <- as.numeric(pred$mean[1L])
  if (!is.finite(point) && length(draws) > 0L) point <- mean(draws)
  median_val <- if (length(draws) > 0L) stats::median(draws) else point

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
    }
  }
  if ((!is.finite(lower) || !is.finite(upper)) && length(draws) > 0L) {
    lower <- as.numeric(stats::quantile(draws, probs = alpha / 2, na.rm = TRUE, names = FALSE))
    upper <- as.numeric(stats::quantile(draws, probs = 1 - alpha / 2, na.rm = TRUE, names = FALSE))
  }

  data.frame(
    point_forecast = point,
    median_forecast = median_val,
    lower_95 = lower,
    upper_95 = upper,
    n_posterior_predictive_draws = length(draws),
    stringsAsFactors = FALSE
  )
}

extract_inclusion <- function(model, retained_predictors, burn) {
  if (length(retained_predictors) == 0L || is.null(model$coefficients)) {
    return(data.frame(predictor_id = character(), inclusion_probability = numeric(), coefficient_posterior_mean = numeric(), coefficient_posterior_median = numeric(), stringsAsFactors = FALSE))
  }
  coefs <- tryCatch(as.matrix(model$coefficients), error = function(e) NULL)
  if (is.null(coefs) || length(coefs) == 0L) {
    return(data.frame(predictor_id = retained_predictors, inclusion_probability = NA_real_, coefficient_posterior_mean = NA_real_, coefficient_posterior_median = NA_real_, stringsAsFactors = FALSE))
  }
  if (nrow(coefs) == length(retained_predictors) && ncol(coefs) != length(retained_predictors)) coefs <- t(coefs)
  if (ncol(coefs) != length(retained_predictors)) {
    cn <- colnames(coefs)
    if (!is.null(cn) && all(retained_predictors %in% cn)) {
      coefs <- coefs[, retained_predictors, drop = FALSE]
    } else {
      return(data.frame(predictor_id = retained_predictors, inclusion_probability = NA_real_, coefficient_posterior_mean = NA_real_, coefficient_posterior_median = NA_real_, stringsAsFactors = FALSE))
    }
  }
  colnames(coefs) <- retained_predictors
  keep_rows <- seq.int(min(nrow(coefs), burn + 1L), nrow(coefs))
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

interval_score <- function(actual, lower, upper, interval_level = INTERVAL_LEVEL) {
  alpha <- 1 - interval_level
  width <- upper - lower
  width + (2 / alpha) * (lower - actual) * (actual < lower) + (2 / alpha) * (actual - upper) * (actual > upper)
}

compute_metrics <- function(df, group_cols) {
  if (nrow(df) == 0L) return(data.frame())
  key <- interaction(df[, group_cols, drop = FALSE], drop = TRUE, sep = "___")
  pieces <- split(df, key)
  out <- lapply(pieces, function(d) {
    abs_error <- abs(d$error)
    ape <- ifelse(d$actual == 0, NA_real_, abs(d$error / d$actual) * 100)
    smape <- ifelse((abs(d$actual) + abs(d$point_forecast)) == 0, NA_real_, 100 * abs(d$error) / ((abs(d$actual) + abs(d$point_forecast)) / 2))
    covered <- ifelse(is.finite(d$lower_95) & is.finite(d$upper_95), d$actual >= d$lower_95 & d$actual <= d$upper_95, NA)
    width <- ifelse(is.finite(d$lower_95) & is.finite(d$upper_95), d$upper_95 - d$lower_95, NA_real_)
    iscore <- interval_score(d$actual, d$lower_95, d$upper_95)
    iscore[!is.finite(iscore)] <- NA_real_
    first <- d[1L, group_cols, drop = FALSE]
    data.frame(
      first,
      n_predictions = nrow(d),
      n_successful_predictions = sum(d$status == "success", na.rm = TRUE),
      MAE = mean(abs_error, na.rm = TRUE),
      RMSE = sqrt(mean(d$error^2, na.rm = TRUE)),
      MAPE = mean(ape, na.rm = TRUE),
      sMAPE = mean(smape, na.rm = TRUE),
      mean_error = mean(d$error, na.rm = TRUE),
      median_abs_error = stats::median(abs_error, na.rm = TRUE),
      coverage_95 = mean(covered, na.rm = TRUE),
      average_interval_width_95 = mean(width, na.rm = TRUE),
      mean_interval_score_95 = mean(iscore, na.rm = TRUE),
      min_target_quarter = d$target_quarter[which.min(d$target_index)],
      max_target_quarter = d$target_quarter[which.max(d$target_index)],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

fit_bsts_no_gt <- function(y_train, niter, burn, seed) {
  set.seed(seed)
  state_spec <- bsts::AddLocalLevel(list(), y_train)
  model <- bsts::bsts(y_train, state.specification = state_spec, niter = niter, ping = 0)
  pred <- stats::predict(
    model,
    horizon = 1L,
    burn = burn,
    quantiles = c((1 - INTERVAL_LEVEL) / 2, 1 - (1 - INTERVAL_LEVEL) / 2)
  )
  list(model = model, pred = pred)
}

fit_bsts_gt <- function(y_train, x_train, x_new, niter, burn, expected_model_size, seed) {
  set.seed(seed)
  state_spec <- bsts::AddLocalLevel(list(), y_train)
  if (is.null(x_train) || ncol(x_train) == 0L) {
    model <- bsts::bsts(y_train, state.specification = state_spec, niter = niter, ping = 0)
    pred <- stats::predict(model, horizon = 1L, burn = burn, quantiles = c((1 - INTERVAL_LEVEL) / 2, 1 - (1 - INTERVAL_LEVEL) / 2))
    return(list(model = model, pred = pred))
  }
  train_df <- data.frame(y = y_train, x_train, check.names = FALSE)
  p <- ncol(x_train)
  expected_size <- min(max(1, expected_model_size), p)
  model <- bsts::bsts(
    y ~ . - 1,
    state.specification = state_spec,
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

save_model_object <- function(model, model_dir, model_id, seed_run, origin_id) {
  ensure_dir(model_dir)
  path <- file.path(model_dir, paste0(model_id, "__seed_", seed_run, "__", origin_id, ".rds"))
  saveRDS(model, path)
  path
}

# -----------------------------
# 3. Locate and load inputs
# -----------------------------
ensure_dir(OUT_DIR)
ensure_dir(FIG_DIR)
if (SAVE_MODELS) ensure_dir(file.path(OUT_DIR, "models"))

search_dirs_processed <- candidate_dirs(PROCESSED_DIR, file.path(PROJECT_ROOT, "data", "processed"), file.path(PROJECT_ROOT, "processed"), PROJECT_ROOT, getwd(), "/mnt/data")
search_dirs_validation <- candidate_dirs(VALIDATION_DIR, file.path(PROCESSED_DIR, "validation"), file.path(PROJECT_ROOT, "data", "processed", "validation"), file.path(PROJECT_ROOT, "step6_validation"), file.path(getwd(), "step6_validation"), "/mnt/data/step6_validation", "/mnt/data")
search_dirs_benchmarks <- candidate_dirs(BENCHMARKS_DIR, file.path(PROCESSED_DIR, "benchmarks"), file.path(PROJECT_ROOT, "data", "processed", "benchmarks"), file.path(PROJECT_ROOT, "step7_benchmarks"), "/mnt/data/step7_benchmarks", "/mnt/data")
search_dirs_final_eval <- candidate_dirs(FINAL_EVAL_DIR, file.path(PROCESSED_DIR, "final_evaluation"), file.path(PROJECT_ROOT, "data", "processed", "final_evaluation"), "/mnt/data")

PANEL_PATH <- locate_file("modeling_panel_gt_overlap.csv", search_dirs_processed)
KEYWORD_DICTIONARY_PATH <- locate_file("keyword_dictionary_clean.csv", search_dirs_processed)
H0_PLAN_PATH <- locate_file("rolling_origins_h0_nowcast.csv", search_dirs_validation)
BENCHMARK_RANKING_PATH <- locate_file("benchmark_h0_model_ranking.csv", search_dirs_benchmarks, required = FALSE)
FINAL_H0_PATH <- locate_file("final_model_comparison_h0.csv", search_dirs_final_eval, required = FALSE)

panel <- read_csv_utf8(PANEL_PATH)
keyword_dictionary <- read_csv_utf8(KEYWORD_DICTIONARY_PATH)
h0_plan <- read_csv_utf8(H0_PLAN_PATH)

assert_columns(panel, c("Quarter", "Year", "Q", "Quarter_Index", "quarter_start", "unemployment_rate_nationals"), "modeling_panel_gt_overlap.csv")
assert_columns(h0_plan, c(
  "fold_id", "origin_id", "target_index", "target_quarter", "target_quarter_start",
  "train_y_start_index", "train_y_end_index",
  "model_response_start_index", "model_response_end_index",
  "model_predictor_start_index", "model_predictor_end_index",
  "prediction_predictor_index", "target_gt_used_for_preprocessing", "uses_future_gt"
), "rolling_origins_h0_nowcast.csv")

if (any(as_bool_vec(h0_plan$uses_future_gt))) stop("h0 validation plan contains uses_future_gt = TRUE; stopping.", call. = FALSE)
if (any(as_bool_vec(h0_plan$target_gt_used_for_preprocessing))) stop("h0 validation plan contains target_gt_used_for_preprocessing = TRUE; stopping.", call. = FALSE)

panel <- panel[order(panel$Quarter_Index), , drop = FALSE]
if (anyDuplicated(panel$Quarter_Index)) stop("Duplicate Quarter_Index values in modelling panel.", call. = FALSE)
if (any(is.na(panel$unemployment_rate_nationals))) stop("Missing unemployment values in modelling panel.", call. = FALSE)

# Reference metrics for fixed non-stochastic comparators, when available.
reference_metrics <- data.frame(model = character(), MAE = numeric(), source_file = character(), stringsAsFactors = FALSE)
if (!is.na(FINAL_H0_PATH) && file.exists(FINAL_H0_PATH)) {
  final_h0 <- read_csv_utf8(FINAL_H0_PATH)
  model_col <- intersect(c("model", "model_id", "model_label"), names(final_h0))[1]
  mae_col <- intersect(c("MAE", "mae"), names(final_h0))[1]
  if (!is.na(model_col) && !is.na(mae_col)) {
    reference_metrics <- rbind(reference_metrics, data.frame(model = as.character(final_h0[[model_col]]), MAE = as.numeric(final_h0[[mae_col]]), source_file = basename(FINAL_H0_PATH), stringsAsFactors = FALSE))
  }
}
if (!is.na(BENCHMARK_RANKING_PATH) && file.exists(BENCHMARK_RANKING_PATH)) {
  bench <- read_csv_utf8(BENCHMARK_RANKING_PATH)
  model_col <- intersect(c("model", "model_id", "model_label"), names(bench))[1]
  mae_col <- intersect(c("MAE", "mae"), names(bench))[1]
  if (!is.na(model_col) && !is.na(mae_col)) {
    reference_metrics <- rbind(reference_metrics, data.frame(model = as.character(bench[[model_col]]), MAE = as.numeric(bench[[mae_col]]), source_file = basename(BENCHMARK_RANKING_PATH), stringsAsFactors = FALSE))
  }
}
reference_metrics <- reference_metrics[is.finite(reference_metrics$MAE), , drop = FALSE]
if (nrow(reference_metrics) > 0L) {
  reference_metrics <- reference_metrics[!duplicated(reference_metrics$model), , drop = FALSE]
}
RW_REFERENCE_MAE <- NA_real_
if (nrow(reference_metrics) > 0L && PRIMARY_REFERENCE_MODEL %in% reference_metrics$model) {
  RW_REFERENCE_MAE <- reference_metrics$MAE[match(PRIMARY_REFERENCE_MODEL, reference_metrics$model)]
}

# -----------------------------
# 4. Main stability loop
# -----------------------------
BURN <- max(1L, floor(NITER * BURN_FRACTION))
all_predictions <- list()
all_inclusion <- list()
all_selection <- list()
run_counter <- 0L

message("Step 11 MCMC stability run starting.")
message(sprintf("Models: %s", paste(MODELS, collapse = ", ")))
message(sprintf("Seeds: %s", paste(SEEDS, collapse = ", ")))
message(sprintf("niter: %d; burn: %d", NITER, BURN))

for (seed_run in SEEDS) {
  for (m_idx in seq_len(nrow(model_grid))) {
    model_info <- model_grid[m_idx, , drop = FALSE]
    model_id <- model_info$model_id[[1]]
    model_type <- model_info$model_type[[1]]
    aggregation <- as.character(model_info$aggregation[[1]])
    threshold <- as.numeric(model_info$zero_prop_threshold[[1]])

    predictor_panel <- NULL
    predictor_clean_cols <- character()
    if (identical(model_type, "GT")) {
      raw_cols <- get_candidate_columns(panel, aggregation)
      predictor_clean_cols <- clean_predictor_names(raw_cols, aggregation)
      predictor_panel <- panel[, raw_cols, drop = FALSE]
      names(predictor_panel) <- predictor_clean_cols
    }

    for (i in seq_len(nrow(h0_plan))) {
      fold <- h0_plan[i, , drop = FALSE]
      run_counter <- run_counter + 1L
      fold_seed <- as.integer(seed_run + 10000L * m_idx + as.integer(fold$fold_id[[1]]))
      start_time <- Sys.time()

      target_row <- quarter_idx_rows(panel, as.integer(fold$target_index[[1]]))
      actual <- as.numeric(panel$unemployment_rate_nationals[target_row])

      fit <- NULL
      prep <- list(retained = character(), dropped_zero_variance = character(), dropped_sparsity = character())
      status <- "success"
      message_text <- ""
      model_path <- NA_character_

      if (identical(model_type, "NO_GT")) {
        train_rows <- quarter_idx_rows(panel, seq.int(as.integer(fold$train_y_start_index[[1]]), as.integer(fold$train_y_end_index[[1]])))
        y_train <- as.numeric(panel$unemployment_rate_nationals[train_rows])
        fit <- tryCatch(
          fit_bsts_no_gt(y_train = y_train, niter = NITER, burn = BURN, seed = fold_seed),
          error = function(e) e
        )
      } else {
        response_idx <- seq.int(as.integer(fold$model_response_start_index[[1]]), as.integer(fold$model_response_end_index[[1]]))
        predictor_idx <- seq.int(as.integer(fold$model_predictor_start_index[[1]]), as.integer(fold$model_predictor_end_index[[1]]))
        if (length(response_idx) != length(predictor_idx)) stop(sprintf("Fold %s has mismatched response/predictor lengths.", fold$origin_id[[1]]), call. = FALSE)
        response_rows <- quarter_idx_rows(panel, response_idx)
        predictor_rows <- quarter_idx_rows(panel, predictor_idx)
        new_row <- quarter_idx_rows(panel, as.integer(fold$prediction_predictor_index[[1]]))
        y_train <- as.numeric(panel$unemployment_rate_nationals[response_rows])
        x_train_raw <- predictor_panel[predictor_rows, , drop = FALSE]
        x_new_raw <- predictor_panel[new_row, , drop = FALSE]
        prep <- fit_preprocess(x_train_raw, x_new_raw, threshold, MAX_PREDICTORS)
        fit <- tryCatch(
          fit_bsts_gt(y_train = y_train, x_train = prep$x_train, x_new = prep$x_new, niter = NITER, burn = BURN, expected_model_size = EXPECTED_MODEL_SIZE, seed = fold_seed),
          error = function(e) e
        )
      }

      if (inherits(fit, "error")) {
        status <- "failed"
        message_text <- fit$message
        if (STOP_ON_MODEL_FAILURE) stop(message_text, call. = FALSE)
        warning(sprintf("Model failed: %s / seed %s / %s: %s", model_id, seed_run, fold$origin_id[[1]], message_text))
        pred_values <- data.frame(point_forecast = NA_real_, median_forecast = NA_real_, lower_95 = NA_real_, upper_95 = NA_real_, n_posterior_predictive_draws = NA_integer_, stringsAsFactors = FALSE)
        inc <- data.frame()
      } else {
        pred_values <- extract_prediction(fit$pred, INTERVAL_LEVEL)
        if (SAVE_MODELS) model_path <- save_model_object(fit$model, file.path(OUT_DIR, "models"), model_id, seed_run, fold$origin_id[[1]])
        if (identical(model_type, "GT")) {
          inc <- extract_inclusion(fit$model, prep$retained, BURN)
        } else {
          inc <- data.frame()
        }
      }

      point <- as.numeric(pred_values$point_forecast[[1]])
      err <- actual - point
      lower <- as.numeric(pred_values$lower_95[[1]])
      upper <- as.numeric(pred_values$upper_95[[1]])
      width <- ifelse(is.finite(lower) && is.finite(upper), upper - lower, NA_real_)
      covered <- ifelse(is.finite(lower) && is.finite(upper), actual >= lower && actual <= upper, NA)
      iscore <- ifelse(is.finite(lower) && is.finite(upper), interval_score(actual, lower, upper), NA_real_)
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

      all_predictions[[length(all_predictions) + 1L]] <- data.frame(
        model_id = model_id,
        model_type = model_type,
        aggregation = ifelse(is.na(aggregation), "", aggregation),
        zero_prop_threshold = threshold,
        state_spec = "local_level",
        horizon = "h0",
        seed_run = seed_run,
        fold_seed = fold_seed,
        fold_id = as.integer(fold$fold_id[[1]]),
        origin_id = as.character(fold$origin_id[[1]]),
        target_index = as.integer(fold$target_index[[1]]),
        target_quarter = as.character(fold$target_quarter[[1]]),
        target_quarter_start = as.character(fold$target_quarter_start[[1]]),
        actual = actual,
        point_forecast = point,
        median_forecast = as.numeric(pred_values$median_forecast[[1]]),
        lower_95 = lower,
        upper_95 = upper,
        error = err,
        abs_error = abs(err),
        squared_error = err^2,
        ape = ifelse(actual == 0, NA_real_, abs(err / actual) * 100),
        smape = ifelse((abs(actual) + abs(point)) == 0, NA_real_, 100 * abs(err) / ((abs(actual) + abs(point)) / 2)),
        interval_covered_95 = covered,
        interval_width_95 = width,
        interval_score_95 = iscore,
        n_candidate_predictors = ifelse(identical(model_type, "GT"), length(predictor_clean_cols), 0L),
        n_retained_predictors = length(prep$retained),
        n_dropped_zero_variance = length(prep$dropped_zero_variance),
        n_dropped_sparsity = length(prep$dropped_sparsity),
        expected_model_size = ifelse(identical(model_type, "GT"), min(max(1, EXPECTED_MODEL_SIZE), max(1, length(prep$retained))), NA_real_),
        niter = NITER,
        burn = BURN,
        interval_level = INTERVAL_LEVEL,
        model_object_path = model_path,
        elapsed_seconds = elapsed,
        status = status,
        message = message_text,
        stringsAsFactors = FALSE
      )

      if (nrow(inc) > 0L) {
        inc$model_id <- model_id
        inc$aggregation <- aggregation
        inc$zero_prop_threshold <- threshold
        inc$seed_run <- seed_run
        inc$fold_id <- as.integer(fold$fold_id[[1]])
        inc$origin_id <- as.character(fold$origin_id[[1]])
        inc$target_quarter <- as.character(fold$target_quarter[[1]])
        all_inclusion[[length(all_inclusion) + 1L]] <- inc
      }

      if (identical(model_type, "GT")) {
        all_selection[[length(all_selection) + 1L]] <- data.frame(
          model_id = model_id,
          aggregation = aggregation,
          zero_prop_threshold = threshold,
          seed_run = seed_run,
          fold_id = as.integer(fold$fold_id[[1]]),
          origin_id = as.character(fold$origin_id[[1]]),
          target_quarter = as.character(fold$target_quarter[[1]]),
          n_candidate_predictors = length(predictor_clean_cols),
          n_retained_predictors = length(prep$retained),
          retained_predictors = paste(prep$retained, collapse = ";"),
          dropped_zero_variance = paste(prep$dropped_zero_variance, collapse = ";"),
          dropped_sparsity = paste(prep$dropped_sparsity, collapse = ";"),
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

predictions <- if (length(all_predictions) > 0L) do.call(rbind, all_predictions) else data.frame()
inclusion_by_seed_fold <- if (length(all_inclusion) > 0L) do.call(rbind, all_inclusion) else data.frame()
selection_by_seed_fold <- if (length(all_selection) > 0L) do.call(rbind, all_selection) else data.frame()

# -----------------------------
# 5. Summaries
# -----------------------------
metrics_by_seed <- compute_metrics(predictions[predictions$status == "success", , drop = FALSE], c("model_id", "model_type", "aggregation", "zero_prop_threshold", "seed_run", "horizon"))
metrics_by_seed <- metrics_by_seed[order(metrics_by_seed$seed_run, metrics_by_seed$MAE), , drop = FALSE]

ranking_by_seed <- metrics_by_seed
if (nrow(ranking_by_seed) > 0L) {
  ranking_by_seed <- do.call(rbind, lapply(split(ranking_by_seed, ranking_by_seed$seed_run), function(d) {
    d <- d[order(d$MAE), , drop = FALSE]
    d$rank_by_MAE_within_seed <- seq_len(nrow(d))
    d
  }))
  rownames(ranking_by_seed) <- NULL
}

summary_by_model <- data.frame()
if (nrow(metrics_by_seed) > 0L) {
  summary_by_model <- do.call(rbind, lapply(split(metrics_by_seed, metrics_by_seed$model_id), function(d) {
    ranks <- ranking_by_seed$rank_by_MAE_within_seed[match(paste(d$model_id, d$seed_run), paste(ranking_by_seed$model_id, ranking_by_seed$seed_run))]
    data.frame(
      model_id = d$model_id[[1]],
      model_type = d$model_type[[1]],
      aggregation = d$aggregation[[1]],
      zero_prop_threshold = d$zero_prop_threshold[[1]],
      n_seed_runs = nrow(d),
      mean_MAE = mean(d$MAE, na.rm = TRUE),
      sd_MAE = stats::sd(d$MAE, na.rm = TRUE),
      min_MAE = min(d$MAE, na.rm = TRUE),
      max_MAE = max(d$MAE, na.rm = TRUE),
      mean_RMSE = mean(d$RMSE, na.rm = TRUE),
      mean_MAPE = mean(d$MAPE, na.rm = TRUE),
      mean_coverage_95 = mean(d$coverage_95, na.rm = TRUE),
      mean_interval_width_95 = mean(d$average_interval_width_95, na.rm = TRUE),
      mean_interval_score_95 = mean(d$mean_interval_score_95, na.rm = TRUE),
      mean_rank_by_MAE = mean(ranks, na.rm = TRUE),
      pct_seed_runs_ranked_first = mean(ranks == 1, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  summary_by_model <- summary_by_model[order(summary_by_model$mean_MAE), , drop = FALSE]
  rownames(summary_by_model) <- NULL
}

pairwise_differences <- data.frame()
if (nrow(metrics_by_seed) > 0L) {
  no_gt_id <- "BSTS_NO_GT_LL"
  if (no_gt_id %in% metrics_by_seed$model_id) {
    rows <- list()
    for (seed_run in unique(metrics_by_seed$seed_run)) {
      d <- metrics_by_seed[metrics_by_seed$seed_run == seed_run, , drop = FALSE]
      ref_mae <- d$MAE[d$model_id == no_gt_id]
      if (length(ref_mae) == 1L) {
        for (m in setdiff(d$model_id, no_gt_id)) {
          mae_m <- d$MAE[d$model_id == m]
          if (length(mae_m) == 1L) {
            rows[[length(rows) + 1L]] <- data.frame(seed_run = seed_run, reference_model = no_gt_id, comparison_model = m, MAE_difference_comparison_minus_reference = mae_m - ref_mae, stringsAsFactors = FALSE)
          }
        }
      }
    }
    if (length(rows) > 0L) pairwise_differences <- do.call(rbind, rows)
  }
  if (is.finite(RW_REFERENCE_MAE)) {
    add_rows <- do.call(rbind, lapply(seq_len(nrow(metrics_by_seed)), function(i) {
      data.frame(seed_run = metrics_by_seed$seed_run[[i]], reference_model = PRIMARY_REFERENCE_MODEL, comparison_model = metrics_by_seed$model_id[[i]], MAE_difference_comparison_minus_reference = metrics_by_seed$MAE[[i]] - RW_REFERENCE_MAE, stringsAsFactors = FALSE)
    }))
    pairwise_differences <- rbind(pairwise_differences, add_rows)
  }
}

predictor_inclusion_summary <- data.frame()
if (nrow(inclusion_by_seed_fold) > 0L) {
  dict <- keyword_dictionary
  id_col <- if ("Clean_Column" %in% names(dict)) "Clean_Column" else if ("Column_Name" %in% names(dict)) "Column_Name" else NA_character_
  rows <- lapply(split(inclusion_by_seed_fold, inclusion_by_seed_fold$predictor_id), function(d) {
    data.frame(
      predictor_id = d$predictor_id[[1]],
      n_seed_fold_records = nrow(d),
      n_seed_runs = length(unique(d$seed_run)),
      n_folds = length(unique(d$fold_id)),
      mean_inclusion_probability = mean(d$inclusion_probability, na.rm = TRUE),
      median_inclusion_probability = stats::median(d$inclusion_probability, na.rm = TRUE),
      sd_inclusion_probability = stats::sd(d$inclusion_probability, na.rm = TRUE),
      mean_coefficient_posterior_mean = mean(d$coefficient_posterior_mean, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  predictor_inclusion_summary <- do.call(rbind, rows)
  rownames(predictor_inclusion_summary) <- NULL
  if (!is.na(id_col)) {
    dict_subset <- dict[, intersect(c(id_col, "Keyword", "English_Translation"), names(dict)), drop = FALSE]
    names(dict_subset)[names(dict_subset) == id_col] <- "predictor_id"
    predictor_inclusion_summary <- merge(predictor_inclusion_summary, dict_subset, by = "predictor_id", all.x = TRUE)
  }
  predictor_inclusion_summary <- predictor_inclusion_summary[order(-predictor_inclusion_summary$mean_inclusion_probability), , drop = FALSE]
}

decision_summary <- data.frame(
  criterion = character(),
  value = character(),
  interpretation = character(),
  stringsAsFactors = FALSE
)
if (nrow(summary_by_model) > 0L) {
  best_model <- summary_by_model$model_id[[1]]
  best_mean <- summary_by_model$mean_MAE[[1]]
  decision_summary <- rbind(decision_summary, data.frame(
    criterion = "best_mean_MAE_across_seed_runs",
    value = sprintf("%s; mean MAE = %.4f", best_model, best_mean),
    interpretation = "Lower mean MAE across seed runs indicates stronger MCMC-stable point accuracy.",
    stringsAsFactors = FALSE
  ))
  if ("BSTS_NO_GT_LL" %in% summary_by_model$model_id) {
    no_gt_mean <- summary_by_model$mean_MAE[summary_by_model$model_id == "BSTS_NO_GT_LL"][[1]]
    gt_rows <- summary_by_model[summary_by_model$model_type == "GT", , drop = FALSE]
    if (nrow(gt_rows) > 0L) {
      best_gt <- gt_rows[order(gt_rows$mean_MAE), , drop = FALSE][1L, ]
      diff <- best_gt$mean_MAE - no_gt_mean
      decision_summary <- rbind(decision_summary, data.frame(
        criterion = "best_GT_minus_BSTS_NO_GT_LL_mean_MAE",
        value = sprintf("%s minus BSTS_NO_GT_LL = %.4f", best_gt$model_id, diff),
        interpretation = ifelse(diff < 0, "The best GT model improves on the no-GT BSTS baseline across seed runs.", "The best GT model does not improve on the no-GT BSTS baseline across seed runs."),
        stringsAsFactors = FALSE
      ))
    }
  }
  if (is.finite(RW_REFERENCE_MAE)) {
    diff_best_rw <- best_mean - RW_REFERENCE_MAE
    decision_summary <- rbind(decision_summary, data.frame(
      criterion = "best_model_minus_RW_reference_MAE",
      value = sprintf("%.4f", diff_best_rw),
      interpretation = ifelse(diff_best_rw < 0, "The best rerun BSTS model beats the fixed RW benchmark reference.", "The best rerun BSTS model does not beat the fixed RW benchmark reference."),
      stringsAsFactors = FALSE
    ))
  }
  max_pct_first <- max(summary_by_model$pct_seed_runs_ranked_first, na.rm = TRUE)
  decision_summary <- rbind(decision_summary, data.frame(
    criterion = "ranking_stability",
    value = sprintf("maximum proportion ranked first across seeds = %.2f", max_pct_first),
    interpretation = ifelse(max_pct_first >= 0.80, "The top MCMC ranking is relatively stable across seeds.", "The top MCMC ranking changes across seeds; avoid overinterpreting small differences."),
    stringsAsFactors = FALSE
  ))
}

# -----------------------------
# 6. Write CSV outputs
# -----------------------------
OUT_PRED <- file.path(OUT_DIR, "mcmc_stability_predictions_h0.csv")
OUT_METRICS <- file.path(OUT_DIR, "mcmc_stability_metrics_by_seed.csv")
OUT_SUMMARY <- file.path(OUT_DIR, "mcmc_stability_summary_by_model.csv")
OUT_RANKING <- file.path(OUT_DIR, "mcmc_stability_model_ranking_by_seed.csv")
OUT_DIFF <- file.path(OUT_DIR, "mcmc_stability_pairwise_differences.csv")
OUT_SELECTION <- file.path(OUT_DIR, "mcmc_stability_selected_predictors_by_seed_fold.csv")
OUT_INCLUSION <- file.path(OUT_DIR, "mcmc_stability_predictor_inclusion_by_seed_fold.csv")
OUT_INCLUSION_SUMMARY <- file.path(OUT_DIR, "mcmc_stability_predictor_inclusion_summary.csv")
OUT_DECISION <- file.path(OUT_DIR, "mcmc_stability_decision_summary.csv")
OUT_REFERENCE <- file.path(OUT_DIR, "mcmc_stability_reference_metrics.csv")

write_csv_utf8(predictions, OUT_PRED)
write_csv_utf8(metrics_by_seed, OUT_METRICS)
write_csv_utf8(summary_by_model, OUT_SUMMARY)
write_csv_utf8(ranking_by_seed, OUT_RANKING)
write_csv_utf8(pairwise_differences, OUT_DIFF)
write_csv_utf8(selection_by_seed_fold, OUT_SELECTION)
write_csv_utf8(inclusion_by_seed_fold, OUT_INCLUSION)
write_csv_utf8(predictor_inclusion_summary, OUT_INCLUSION_SUMMARY)
write_csv_utf8(decision_summary, OUT_DECISION)
write_csv_utf8(reference_metrics, OUT_REFERENCE)

# -----------------------------
# 7. Figures without titles/subtitles
# -----------------------------
figure_files <- character()

if (nrow(metrics_by_seed) > 0L) {
  p <- ggplot2::ggplot(metrics_by_seed, ggplot2::aes(x = model_id, y = MAE, group = model_id)) +
    ggplot2::geom_boxplot(width = 0.65, outlier.shape = NA) +
    ggplot2::geom_point(ggplot2::aes(shape = factor(seed_run)), position = ggplot2::position_jitter(width = 0.12, height = 0), size = 2.2) +
    ggplot2::labs(x = "Model", y = "MAE (percentage points)", shape = "Seed") +
    make_base_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
  if (is.finite(RW_REFERENCE_MAE)) {
    p <- p + ggplot2::geom_hline(yintercept = RW_REFERENCE_MAE, linetype = "dashed")
  }
  save_plot(p, "mcmc_stability_mae_by_seed.png", width = 11, height = 6)
  figure_files <- c(figure_files, file.path(FIG_DIR, "mcmc_stability_mae_by_seed.png"))
}

if (nrow(ranking_by_seed) > 0L) {
  p <- ggplot2::ggplot(ranking_by_seed, ggplot2::aes(x = factor(seed_run), y = model_id, fill = rank_by_MAE_within_seed)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = rank_by_MAE_within_seed), size = 3.5) +
    ggplot2::scale_fill_gradient(low = "white", high = "grey50") +
    ggplot2::labs(x = "Seed", y = "Model", fill = "Rank") +
    make_base_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0))
  save_plot(p, "mcmc_stability_ranking_by_seed.png", width = 10, height = 6)
  figure_files <- c(figure_files, file.path(FIG_DIR, "mcmc_stability_ranking_by_seed.png"))
}

if (nrow(pairwise_differences) > 0L) {
  pd <- pairwise_differences[pairwise_differences$reference_model %in% c("BSTS_NO_GT_LL", PRIMARY_REFERENCE_MODEL), , drop = FALSE]
  if (nrow(pd) > 0L) {
    p <- ggplot2::ggplot(pd, ggplot2::aes(x = comparison_model, y = MAE_difference_comparison_minus_reference, group = factor(seed_run))) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::geom_point(ggplot2::aes(shape = factor(seed_run)), position = ggplot2::position_jitter(width = 0.1, height = 0), size = 2) +
      ggplot2::facet_wrap(~ reference_model, scales = "free_y") +
      ggplot2::labs(x = "Comparison model", y = "MAE difference relative to reference", shape = "Seed") +
      make_base_theme() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
    save_plot(p, "mcmc_stability_pairwise_differences.png", width = 12, height = 7)
    figure_files <- c(figure_files, file.path(FIG_DIR, "mcmc_stability_pairwise_differences.png"))
  }
}

if (nrow(predictions) > 0L) {
  success_pred <- predictions[predictions$status == "success", , drop = FALSE]
  if (nrow(success_pred) > 0L) {
    ensemble <- do.call(rbind, lapply(split(success_pred, paste(success_pred$model_id, success_pred$target_quarter, sep = "___")), function(d) {
      data.frame(
        model_id = d$model_id[[1]],
        target_index = d$target_index[[1]],
        target_quarter = d$target_quarter[[1]],
        target_quarter_start = as.Date(d$target_quarter_start[[1]]),
        actual = d$actual[[1]],
        mean_point_forecast = mean(d$point_forecast, na.rm = TRUE),
        lower_seed_range = min(d$point_forecast, na.rm = TRUE),
        upper_seed_range = max(d$point_forecast, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
    actual_df <- ensemble[!duplicated(ensemble$target_quarter), c("target_quarter_start", "target_quarter", "actual"), drop = FALSE]
    p <- ggplot2::ggplot() +
      ggplot2::geom_line(data = actual_df, ggplot2::aes(x = target_quarter_start, y = actual, linetype = "Actual"), linewidth = 0.9) +
      ggplot2::geom_point(data = actual_df, ggplot2::aes(x = target_quarter_start, y = actual), size = 1.8) +
      ggplot2::geom_line(data = ensemble, ggplot2::aes(x = target_quarter_start, y = mean_point_forecast, linetype = model_id), linewidth = 0.7) +
      ggplot2::labs(x = "Target quarter", y = "Unemployment rate (%)", linetype = "Series") +
      make_base_theme() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
    save_plot(p, "mcmc_stability_actual_vs_seed_mean_predictions.png", width = 12, height = 7)
    figure_files <- c(figure_files, file.path(FIG_DIR, "mcmc_stability_actual_vs_seed_mean_predictions.png"))
  }
}

if (nrow(predictor_inclusion_summary) > 0L) {
  inc_plot <- predictor_inclusion_summary[seq_len(min(25L, nrow(predictor_inclusion_summary))), , drop = FALSE]
  label <- ifelse(!is.na(inc_plot$English_Translation) & nzchar(inc_plot$English_Translation), paste0(inc_plot$predictor_id, " — ", inc_plot$English_Translation), inc_plot$predictor_id)
  label <- make_unique_labels(clip_label(label, 60L))
  inc_plot$label <- factor(label, levels = rev(label))
  p <- ggplot2::ggplot(inc_plot, ggplot2::aes(x = label, y = mean_inclusion_probability)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Keyword", y = "Mean posterior inclusion probability") +
    make_base_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0))
  save_plot(p, "mcmc_stability_predictor_inclusion_top_keywords.png", width = 10, height = 8)
  figure_files <- c(figure_files, file.path(FIG_DIR, "mcmc_stability_predictor_inclusion_top_keywords.png"))
}

# -----------------------------
# 8. Captions, config, report, manifest
# -----------------------------
OUT_CAPTIONS <- file.path(OUT_DIR, "figure_captions_step11.md")
caption_lines <- c(
  "# Figure captions — Step 11",
  "",
  "**mcmc_stability_mae_by_seed.png.** Distribution of h = 0 mean absolute error across repeated MCMC seed runs for the selected BSTS models. If available, the dashed horizontal line marks the fixed random-walk benchmark MAE from Step 7 or Step 10.",
  "",
  "**mcmc_stability_ranking_by_seed.png.** Model ranking by h = 0 MAE within each MCMC seed run. Lower ranks indicate better point accuracy.",
  "",
  "**mcmc_stability_pairwise_differences.png.** Pairwise differences in h = 0 MAE relative to reference models. Positive values indicate worse accuracy than the reference; negative values indicate improvement.",
  "",
  "**mcmc_stability_actual_vs_seed_mean_predictions.png.** Actual unemployment rates and seed-averaged h = 0 BSTS predictions for the selected models.",
  "",
  "**mcmc_stability_predictor_inclusion_top_keywords.png.** Highest mean posterior inclusion probabilities across repeated MCMC seed runs and rolling folds for the selected Google Trends models."
)
write_lines_utf8(caption_lines, OUT_CAPTIONS)

OUT_CONFIG <- file.path(OUT_DIR, "10_mcmc_stability_check_config.json")
config_lines <- c(
  "{",
  '  "script": "10_mcmc_stability_check.R",',
  '  "step": "Step 11 — MCMC stability and robustness check",',
  sprintf('  "generated_at": "%s",', format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  sprintf('  "models": %s,', json_array_chr(MODELS)),
  sprintf('  "seeds": %s,', json_array_num(SEEDS)),
  sprintf('  "niter": %d,', NITER),
  sprintf('  "burn_fraction": %.3f,', BURN_FRACTION),
  sprintf('  "burn": %d,', BURN),
  sprintf('  "interval_level": %.3f,', INTERVAL_LEVEL),
  sprintf('  "expected_model_size": %.3f,', EXPECTED_MODEL_SIZE),
  sprintf('  "max_predictors": "%s",', as.character(MAX_PREDICTORS)),
  sprintf('  "primary_reference_model": "%s",', json_escape(PRIMARY_REFERENCE_MODEL)),
  sprintf('  "rw_reference_mae": %s,', ifelse(is.finite(RW_REFERENCE_MAE), sprintf("%.8f", RW_REFERENCE_MAE), "null")),
  '  "state_specification": "local_level",',
  '  "horizon": "h0_same_quarter_nowcast",',
  '  "preprocessing": {',
  '    "fit_scope": "inside each rolling training window only",',
  '    "zero_variance_filter": true,',
  '    "sparsity_filter": true,',
  '    "standardise_predictors": true,',
  '    "target_quarter_gt_used_for_preprocessing": false,',
  '    "future_gt_placeholders": false',
  '  },',
  '  "figures": {',
  '    "embedded_titles": false,',
  '    "embedded_subtitles": false',
  '  }',
  "}"
)
write_lines_utf8(config_lines, OUT_CONFIG)

OUT_REPORT <- file.path(OUT_DIR, "10_mcmc_stability_check_report.md")
report_lines <- c(
  "# Step 11 — MCMC stability and robustness check",
  "",
  "## Purpose",
  "",
  "Step 11 reruns the selected stochastic BSTS models with multiple seeds and longer chains. It is designed to verify whether small differences between the best no-GT BSTS model and the best Google Trends BSTS model are stable enough to support manuscript claims.",
  "",
  "## Models evaluated",
  "",
  "```text",
  paste(MODELS, collapse = "\n"),
  "```",
  "",
  "## MCMC settings",
  "",
  sprintf("- `niter`: %d", NITER),
  sprintf("- `burn_fraction`: %.2f", BURN_FRACTION),
  sprintf("- `burn`: %d", BURN),
  sprintf("- `seeds`: %s", paste(SEEDS, collapse = ", ")),
  "- state specification: local level only",
  "- validation horizon: h = 0 same-quarter nowcast",
  "",
  "## Leakage safeguards",
  "",
  "For Google Trends models, zero-variance filtering, sparsity filtering, and standardisation are fitted inside each rolling training window only. Target-quarter Google Trends are used only as prediction newdata for h = 0; they are not used to estimate preprocessing transformations. No future Google Trends placeholders are created.",
  "",
  "## Summary by model",
  "",
  if (nrow(summary_by_model) > 0L) paste(capture.output(print(summary_by_model[, intersect(c("model_id", "n_seed_runs", "mean_MAE", "sd_MAE", "mean_rank_by_MAE", "pct_seed_runs_ranked_first"), names(summary_by_model)), drop = FALSE], row.names = FALSE)), collapse = "\n") else "No successful model summaries were produced.",
  "",
  "## Decision summary",
  "",
  if (nrow(decision_summary) > 0L) paste(capture.output(print(decision_summary, row.names = FALSE)), collapse = "\n") else "No decision summary was produced.",
  "",
  "## Files written",
  "",
  "- `mcmc_stability_predictions_h0.csv`",
  "- `mcmc_stability_metrics_by_seed.csv`",
  "- `mcmc_stability_summary_by_model.csv`",
  "- `mcmc_stability_model_ranking_by_seed.csv`",
  "- `mcmc_stability_pairwise_differences.csv`",
  "- `mcmc_stability_predictor_inclusion_by_seed_fold.csv`",
  "- `mcmc_stability_predictor_inclusion_summary.csv`",
  "- `mcmc_stability_decision_summary.csv`",
  "- `figure_captions_step11.md`",
  "",
  "## Next step",
  "",
  "Use the Step 11 summary to decide whether the final manuscript can treat the top BSTS model ranking as stable, or whether the conclusion should emphasise that the best no-GT and GT BSTS models are statistically and computationally indistinguishable in practical terms."
)
write_lines_utf8(report_lines, OUT_REPORT)

OUT_MANIFEST <- file.path(OUT_DIR, "10_mcmc_stability_check_manifest.csv")
manifest_files <- c(
  OUT_PRED, OUT_METRICS, OUT_SUMMARY, OUT_RANKING, OUT_DIFF, OUT_SELECTION, OUT_INCLUSION,
  OUT_INCLUSION_SUMMARY, OUT_DECISION, OUT_REFERENCE, OUT_CONFIG, OUT_REPORT, OUT_CAPTIONS,
  figure_files
)
manifest_files <- manifest_files[file.exists(manifest_files)]
manifest <- data.frame(
  file_name = basename(manifest_files),
  relative_path = file.path("data", "processed", "mcmc_stability", ifelse(dirname(manifest_files) == FIG_DIR, file.path("figures", basename(manifest_files)), basename(manifest_files))),
  file_size_bytes = file.info(manifest_files)$size,
  checksum_md5 = vapply(manifest_files, sha_md5, character(1L)),
  stringsAsFactors = FALSE
)
write_csv_utf8(manifest, OUT_MANIFEST)

message("Step 11 MCMC stability check complete.")
message(sprintf("Outputs written to: %s", OUT_DIR))
message(sprintf("Predictions: %d rows", nrow(predictions)))
message(sprintf("Metrics by seed: %d rows", nrow(metrics_by_seed)))
