#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# 07b_compare_bsts_state_specs.R
# Step 8B: Compare BSTS no-Google-Trends state specifications
# -----------------------------------------------------------------------------
# Purpose:
#   Fit and compare several unemployment-only Bayesian structural time-series
#   specifications under the rolling-origin validation design created in Step 6.
#   This diagnostic freezes the preferred no-GT state structure before adding
#   Google Trends predictors in the next step.
#
# Inputs:
#   data/processed/modeling_panel_unemployment_full.csv
#   data/processed/validation/rolling_origins_h0_nowcast.csv
#   data/processed/validation/rolling_origins_h1_forecast.csv
#   data/processed/validation/rolling_origins_h2_forecast_exploratory.csv
#   data/processed/validation/post_gt_forecast_validation_block.csv
#   optional: data/processed/benchmarks/benchmark_h0_model_ranking.csv
#
# Outputs:
#   data/processed/bsts_state_specs/bsts_state_spec_predictions_h0.csv
#   data/processed/bsts_state_specs/bsts_state_spec_predictions_h1.csv
#   data/processed/bsts_state_specs/bsts_state_spec_predictions_h2.csv
#   data/processed/bsts_state_specs/bsts_state_spec_predictions_all.csv
#   data/processed/bsts_state_specs/bsts_state_spec_metrics_by_horizon.csv
#   data/processed/bsts_state_specs/bsts_state_spec_h0_ranking.csv
#   data/processed/bsts_state_specs/bsts_state_spec_post_gt_forecasts.csv
#   data/processed/bsts_state_specs/bsts_state_spec_post_gt_metrics.csv
#   data/processed/bsts_state_specs/bsts_state_spec_vs_benchmark_h0.csv
#   data/processed/bsts_state_specs/07b_compare_bsts_state_specs_config.json
#   data/processed/bsts_state_specs/07b_compare_bsts_state_specs_report.md
#   data/processed/bsts_state_specs/07b_compare_bsts_state_specs_manifest.csv
#   data/processed/bsts_state_specs/figures/*.png
#
# Candidate state specifications:
#   BSTS_NO_GT_LL        = local level only
#   BSTS_NO_GT_LL_SEAS   = local level + quarterly seasonal
#   BSTS_NO_GT_LLT       = local linear trend only
#   BSTS_NO_GT_LLT_SEAS  = local linear trend + quarterly seasonal
#
# Agreed pipeline rules:
#   - No Google Trends predictors are used here.
#   - No Google Trends preprocessing, sparsity filtering, or standardisation is
#     performed here.
#   - No future unemployment values are used.
#   - Rolling predictions follow the Step 6 information-set design.
#   - h = 0 is interpreted as an unemployment-only comparator for same-quarter
#     nowcasting. Because no target-quarter Google Trends are used, it is a
#     one-step-ahead unemployment-only prediction using unemployment through t-1.
#   - Post-GT forecasts are trained through 2024 Q3 only.
#
# Usage from project root:
#   Rscript scripts/07b_compare_bsts_state_specs.R
#
# Optional arguments:
#   Rscript scripts/07b_compare_bsts_state_specs.R \
#     --processed-dir=data/processed \
#     --validation-dir=data/processed/validation \
#     --benchmarks-dir=data/processed/benchmarks \
#     --bsts-dir=data/processed/bsts_state_specs \
#     --state-specs=local_level,local_level_seasonal,local_linear_trend,local_linear_trend_seasonal \
#     --horizons=h0,h1,h2 \
#     --run-post-gt=true \
#     --niter=3000 \
#     --burn-fraction=0.25 \
#     --interval-level=95 \
#     --seed=20260616
#
# Dependencies:
#   bsts, ggplot2.
# -----------------------------------------------------------------------------

options(stringsAsFactors = FALSE, warn = 1)
suppressWarnings(try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE))

# -----------------------------
# 0. Configuration
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

to_bool <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  x_chr %in% c("true", "t", "1", "yes", "y")
}

parse_csv_arg <- function(x) {
  out <- trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
  out[nzchar(out)]
}

PROJECT_ROOT <- normalizePath(get_arg("project-root", Sys.getenv("BSTS_PROJECT_ROOT", unset = getwd())), mustWork = FALSE)
PROCESSED_DIR_DEFAULT <- file.path(PROJECT_ROOT, "data", "processed")
PROCESSED_DIR <- normalizePath(get_arg("processed-dir", Sys.getenv("BSTS_PROCESSED_DIR", unset = PROCESSED_DIR_DEFAULT)), mustWork = FALSE)
VALIDATION_DIR <- normalizePath(get_arg("validation-dir", file.path(PROCESSED_DIR, "validation")), mustWork = FALSE)
BENCHMARKS_DIR <- normalizePath(get_arg("benchmarks-dir", file.path(PROCESSED_DIR, "benchmarks")), mustWork = FALSE)
BSTS_DIR <- normalizePath(get_arg("bsts-dir", file.path(PROCESSED_DIR, "bsts_state_specs")), mustWork = FALSE)
FIGURE_DIR <- file.path(BSTS_DIR, "figures")
TABLE_DIR <- file.path(BSTS_DIR, "tables")
RDS_DIR <- file.path(BSTS_DIR, "rds")

NITER <- as.integer(get_arg("niter", "3000"))
BURN_FRACTION <- as.numeric(get_arg("burn-fraction", "0.25"))
INTERVAL_LEVEL <- as.numeric(get_arg("interval-level", "95"))
ALPHA <- 1 - INTERVAL_LEVEL / 100
SEASONAL_FREQUENCY <- as.integer(get_arg("seasonal-frequency", "4"))
SEED <- as.integer(get_arg("seed", "20260616"))
PING <- as.integer(get_arg("ping", "0"))
SAVE_RDS <- to_bool(get_arg("save-rds", "false"))
RUN_POST_GT <- to_bool(get_arg("run-post-gt", "true"))

VALID_STATE_SPECS <- c(
  "local_level",
  "local_level_seasonal",
  "local_linear_trend",
  "local_linear_trend_seasonal"
)
DEFAULT_STATE_SPECS <- paste(VALID_STATE_SPECS, collapse = ",")
STATE_SPECS <- parse_csv_arg(get_arg("state-specs", DEFAULT_STATE_SPECS))
if (length(STATE_SPECS) == 0L) stop("No state specifications supplied.", call. = FALSE)
invalid_specs <- setdiff(STATE_SPECS, VALID_STATE_SPECS)
if (length(invalid_specs) > 0L) {
  stop(sprintf("Unsupported --state-specs value(s): %s. Valid choices: %s", paste(invalid_specs, collapse = ", "), paste(VALID_STATE_SPECS, collapse = ", ")), call. = FALSE)
}
STATE_SPECS <- unique(STATE_SPECS)

VALID_HORIZONS <- c("h0", "h1", "h2")
HORIZONS <- parse_csv_arg(get_arg("horizons", "h0,h1,h2"))
invalid_horizons <- setdiff(HORIZONS, VALID_HORIZONS)
if (length(invalid_horizons) > 0L) {
  stop(sprintf("Unsupported --horizons value(s): %s. Valid choices: %s", paste(invalid_horizons, collapse = ", "), paste(VALID_HORIZONS, collapse = ", ")), call. = FALSE)
}
HORIZONS <- unique(HORIZONS)

if (!requireNamespace("bsts", quietly = TRUE)) {
  stop("Package 'bsts' is required for Step 8B. Install it with install.packages('bsts') and rerun this script.", call. = FALSE)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required for Step 8B figures. Install it with install.packages('ggplot2') and rerun this script.", call. = FALSE)
}

FULL_PANEL_FILE <- "modeling_panel_unemployment_full.csv"
H0_FILE <- "rolling_origins_h0_nowcast.csv"
H1_FILE <- "rolling_origins_h1_forecast.csv"
H2_FILE <- "rolling_origins_h2_forecast_exploratory.csv"
POST_GT_FILE <- "post_gt_forecast_validation_block.csv"
BENCHMARK_H0_FILE <- "benchmark_h0_model_ranking.csv"

OUTPUT_H0 <- file.path(BSTS_DIR, "bsts_state_spec_predictions_h0.csv")
OUTPUT_H1 <- file.path(BSTS_DIR, "bsts_state_spec_predictions_h1.csv")
OUTPUT_H2 <- file.path(BSTS_DIR, "bsts_state_spec_predictions_h2.csv")
OUTPUT_ALL <- file.path(BSTS_DIR, "bsts_state_spec_predictions_all.csv")
OUTPUT_METRICS <- file.path(BSTS_DIR, "bsts_state_spec_metrics_by_horizon.csv")
OUTPUT_H0_RANKING <- file.path(BSTS_DIR, "bsts_state_spec_h0_ranking.csv")
OUTPUT_POST <- file.path(BSTS_DIR, "bsts_state_spec_post_gt_forecasts.csv")
OUTPUT_POST_METRICS <- file.path(BSTS_DIR, "bsts_state_spec_post_gt_metrics.csv")
OUTPUT_COMPARISON_H0 <- file.path(BSTS_DIR, "bsts_state_spec_vs_benchmark_h0.csv")
OUTPUT_CONFIG <- file.path(BSTS_DIR, "07b_compare_bsts_state_specs_config.json")
OUTPUT_REPORT <- file.path(BSTS_DIR, "07b_compare_bsts_state_specs_report.md")
OUTPUT_MANIFEST <- file.path(BSTS_DIR, "07b_compare_bsts_state_specs_manifest.csv")

FIG_MAE_H0 <- file.path(FIGURE_DIR, "bsts_state_spec_mae_h0.png")
FIG_MAE_HORIZON <- file.path(FIGURE_DIR, "bsts_state_spec_mae_by_horizon.png")
FIG_H0_ACTUAL <- file.path(FIGURE_DIR, "bsts_state_spec_actual_vs_predicted_h0.png")
FIG_H0_ERRORS <- file.path(FIGURE_DIR, "bsts_state_spec_rolling_errors_h0.png")
FIG_BEST_INTERVALS <- file.path(FIGURE_DIR, "bsts_state_spec_best_intervals_h0.png")
FIG_POST <- file.path(FIGURE_DIR, "bsts_state_spec_post_gt_forecasts.png")
FIG_BENCHMARK_COMPARE <- file.path(FIGURE_DIR, "bsts_state_spec_vs_benchmark_h0.png")

CANDIDATE_PROCESSED_DIRS <- unique(c(
  PROCESSED_DIR,
  PROCESSED_DIR_DEFAULT,
  file.path(PROJECT_ROOT, "processed"),
  PROJECT_ROOT,
  getwd(),
  "/mnt/data"
))

CANDIDATE_VALIDATION_DIRS <- unique(c(
  VALIDATION_DIR,
  file.path(PROCESSED_DIR, "validation"),
  file.path(PROCESSED_DIR_DEFAULT, "validation"),
  file.path(PROJECT_ROOT, "validation"),
  PROJECT_ROOT,
  getwd(),
  "/mnt/data"
))

CANDIDATE_BENCHMARK_DIRS <- unique(c(
  BENCHMARKS_DIR,
  file.path(PROCESSED_DIR, "benchmarks"),
  file.path(PROCESSED_DIR_DEFAULT, "benchmarks"),
  file.path(PROJECT_ROOT, "benchmarks"),
  PROJECT_ROOT,
  getwd(),
  "/mnt/data"
))

# -----------------------------
# 1. Helper functions
# -----------------------------
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

locate_file <- function(filename, candidate_dirs, required = TRUE) {
  candidate_dirs <- candidate_dirs[dir.exists(candidate_dirs)]
  candidate_paths <- file.path(candidate_dirs, filename)
  hit <- candidate_paths[file.exists(candidate_paths)]
  if (length(hit) == 0L) {
    if (isTRUE(required)) {
      stop(sprintf("Required file not found: %s\nSearched in:\n%s", filename, paste(candidate_dirs, collapse = "\n")), call. = FALSE)
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

sha256_or_md5 <- function(path) unname(tools::md5sum(path))

json_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub('"', '\\"', x)
  x <- gsub("\n", "\\n", x)
  x
}

json_vec <- function(x) paste0("[", paste(sprintf('"%s"', json_escape(x)), collapse = ", "), "]")
json_bool <- function(x) ifelse(isTRUE(x), "true", "false")

assert_has_columns <- function(df, cols, file_label) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0L) {
    stop(sprintf("%s is missing required columns: %s", file_label, paste(missing, collapse = ", ")), call. = FALSE)
  }
}

state_spec_label <- function(state_spec) {
  switch(
    state_spec,
    local_level = "BSTS_NO_GT_LL",
    local_level_seasonal = "BSTS_NO_GT_LL_SEAS",
    local_linear_trend = "BSTS_NO_GT_LLT",
    local_linear_trend_seasonal = "BSTS_NO_GT_LLT_SEAS",
    stop(sprintf("Unknown state specification: %s", state_spec), call. = FALSE)
  )
}

state_spec_description <- function(state_spec) {
  switch(
    state_spec,
    local_level = "local level only",
    local_level_seasonal = "local level + quarterly seasonal",
    local_linear_trend = "local linear trend only",
    local_linear_trend_seasonal = "local linear trend + quarterly seasonal",
    state_spec
  )
}

lookup_by_index <- function(df, idx) {
  hit <- df[df$Quarter_Index == idx, , drop = FALSE]
  if (nrow(hit) != 1L) stop(sprintf("Quarter_Index not uniquely found: %s", idx), call. = FALSE)
  hit
}

q_label <- function(df, idx) lookup_by_index(df, idx)$Quarter[[1]]
q_start <- function(df, idx) lookup_by_index(df, idx)$quarter_start[[1]]
q_year <- function(df, idx) as.integer(lookup_by_index(df, idx)$Year[[1]])
q_num <- function(df, idx) as.integer(lookup_by_index(df, idx)$Q[[1]])
y_actual <- function(df, idx) as.numeric(lookup_by_index(df, idx)$unemployment_rate_nationals[[1]])

get_train_y <- function(full_panel, start_idx, end_idx) {
  train <- full_panel[full_panel$Quarter_Index >= start_idx & full_panel$Quarter_Index <= end_idx, , drop = FALSE]
  train <- train[order(train$Quarter_Index), , drop = FALSE]
  as.numeric(train$unemployment_rate_nationals)
}

make_ts <- function(y) {
  stats::ts(as.numeric(y), frequency = SEASONAL_FREQUENCY)
}

build_state_spec <- function(y_ts, state_spec) {
  ss <- list()
  if (state_spec %in% c("local_linear_trend", "local_linear_trend_seasonal")) {
    ss <- bsts::AddLocalLinearTrend(ss, y_ts)
  } else if (state_spec %in% c("local_level", "local_level_seasonal")) {
    ss <- bsts::AddLocalLevel(ss, y_ts)
  }

  if (state_spec %in% c("local_linear_trend_seasonal", "local_level_seasonal")) {
    ss <- bsts::AddSeasonal(ss, y_ts, nseasons = SEASONAL_FREQUENCY)
  }

  ss
}

extract_prediction_step <- function(pred, step, alpha = ALPHA) {
  point <- NA_real_
  lower <- NA_real_
  upper <- NA_real_

  if (!is.null(pred$mean) && length(pred$mean) >= step) {
    point <- as.numeric(pred$mean[[step]])
  }

  if (!is.null(pred$interval)) {
    int <- pred$interval
    if (is.matrix(int) || is.data.frame(int)) {
      int <- as.matrix(int)
      if (nrow(int) >= step && ncol(int) >= 2L) {
        lower <- as.numeric(int[step, 1L])
        upper <- as.numeric(int[step, 2L])
      } else if (ncol(int) >= step && nrow(int) >= 2L) {
        lower <- as.numeric(int[1L, step])
        upper <- as.numeric(int[2L, step])
      }
    }
  }

  if (!is.finite(point) || !is.finite(lower) || !is.finite(upper)) {
    dist <- pred$distribution
    if (!is.null(dist)) {
      if (is.vector(dist)) {
        d <- as.numeric(dist)
      } else if (is.matrix(dist)) {
        if (ncol(dist) >= step) {
          d <- as.numeric(dist[, step])
        } else if (nrow(dist) >= step) {
          d <- as.numeric(dist[step, ])
        } else {
          d <- numeric(0)
        }
      } else {
        d <- numeric(0)
      }
      d <- d[is.finite(d)]
      if (length(d) > 0L) {
        if (!is.finite(point)) point <- mean(d)
        if (!is.finite(lower)) lower <- as.numeric(stats::quantile(d, probs = alpha / 2, names = FALSE, na.rm = TRUE))
        if (!is.finite(upper)) upper <- as.numeric(stats::quantile(d, probs = 1 - alpha / 2, names = FALSE, na.rm = TRUE))
      }
    }
  }

  c(point_forecast = point, lower = lower, upper = upper)
}

interval_score <- function(actual, lower, upper, alpha = ALPHA) {
  width <- upper - lower
  penalty_low <- ifelse(actual < lower, (2 / alpha) * (lower - actual), 0)
  penalty_high <- ifelse(actual > upper, (2 / alpha) * (actual - upper), 0)
  width + penalty_low + penalty_high
}

error_row_metrics <- function(actual, point, lower, upper) {
  err <- actual - point
  abs_err <- abs(err)
  ape <- ifelse(is.finite(actual) && actual != 0, 100 * abs_err / abs(actual), NA_real_)
  smape <- ifelse((abs(actual) + abs(point)) > 0, 100 * abs_err / ((abs(actual) + abs(point)) / 2), NA_real_)
  width <- upper - lower
  covered <- is.finite(lower) && is.finite(upper) && actual >= lower && actual <= upper
  iscore <- interval_score(actual, lower, upper)
  c(
    error_actual_minus_forecast = err,
    abs_error = abs_err,
    mape_contribution = ape,
    smape_contribution = smape,
    interval_width = width,
    covered_95 = as.integer(covered),
    interval_score = iscore
  )
}

fit_bsts_forecast <- function(train_y, forecast_horizon, state_spec, seed_offset = 0L) {
  if (length(train_y) < 8L) stop("BSTS training series is too short.", call. = FALSE)
  y_ts <- make_ts(train_y)
  ss <- build_state_spec(y_ts, state_spec)
  set.seed(SEED + as.integer(seed_offset))
  model <- bsts::bsts(
    formula = y_ts,
    state.specification = ss,
    niter = NITER,
    ping = PING
  )
  burn <- max(1L, min(NITER - 1L, floor(BURN_FRACTION * NITER)))
  pred <- stats::predict(
    model,
    horizon = forecast_horizon,
    burn = burn,
    quantiles = c(ALPHA / 2, 1 - ALPHA / 2)
  )
  list(model = model, prediction = pred, burn = burn)
}

empty_prediction_row <- function(origin_row, full_panel, horizon_label, state_spec, status, message_text) {
  target_idx <- as.integer(origin_row$target_index[[1]])
  train_end_idx <- as.integer(origin_row$train_y_end_index[[1]])
  forecast_steps <- target_idx - train_end_idx
  actual <- y_actual(full_panel, target_idx)
  data.frame(
    model_family = "BSTS_STATE_SPEC_COMPARISON",
    model = state_spec_label(state_spec),
    model_label = state_spec_label(state_spec),
    state_specification = state_spec,
    state_description = state_spec_description(state_spec),
    horizon_label = horizon_label,
    horizon_quarters = as.integer(origin_row$horizon_quarters[[1]]),
    forecast_steps = forecast_steps,
    fold_id = as.integer(origin_row$fold_id[[1]]),
    origin_id = as.character(origin_row$origin_id[[1]]),
    target_index = target_idx,
    target_quarter = q_label(full_panel, target_idx),
    target_year = q_year(full_panel, target_idx),
    target_q = q_num(full_panel, target_idx),
    target_quarter_start = q_start(full_panel, target_idx),
    train_y_start_index = as.integer(origin_row$train_y_start_index[[1]]),
    train_y_start_quarter = as.character(origin_row$train_y_start_quarter[[1]]),
    train_y_end_index = train_end_idx,
    train_y_end_quarter = as.character(origin_row$train_y_end_quarter[[1]]),
    n_train = as.integer(origin_row$n_y_available_at_origin[[1]]),
    actual = actual,
    point_forecast = NA_real_,
    lower_95 = NA_real_,
    upper_95 = NA_real_,
    interval_width = NA_real_,
    error_actual_minus_forecast = NA_real_,
    abs_error = NA_real_,
    mape_contribution = NA_real_,
    smape_contribution = NA_real_,
    covered_95 = NA_integer_,
    interval_score = NA_real_,
    niter = NITER,
    burn = NA_integer_,
    interval_level = INTERVAL_LEVEL,
    seed = SEED,
    status = status,
    message = message_text,
    stringsAsFactors = FALSE
  )
}

make_prediction_row <- function(origin_row, full_panel, horizon_label, state_spec, pred_values, burn) {
  target_idx <- as.integer(origin_row$target_index[[1]])
  train_start_idx <- as.integer(origin_row$train_y_start_index[[1]])
  train_end_idx <- as.integer(origin_row$train_y_end_index[[1]])
  forecast_steps <- target_idx - train_end_idx
  actual <- y_actual(full_panel, target_idx)
  point <- as.numeric(pred_values[["point_forecast"]])
  lower <- as.numeric(pred_values[["lower"]])
  upper <- as.numeric(pred_values[["upper"]])
  mets <- error_row_metrics(actual, point, lower, upper)

  data.frame(
    model_family = "BSTS_STATE_SPEC_COMPARISON",
    model = state_spec_label(state_spec),
    model_label = state_spec_label(state_spec),
    state_specification = state_spec,
    state_description = state_spec_description(state_spec),
    horizon_label = horizon_label,
    horizon_quarters = as.integer(origin_row$horizon_quarters[[1]]),
    forecast_steps = forecast_steps,
    fold_id = as.integer(origin_row$fold_id[[1]]),
    origin_id = as.character(origin_row$origin_id[[1]]),
    target_index = target_idx,
    target_quarter = q_label(full_panel, target_idx),
    target_year = q_year(full_panel, target_idx),
    target_q = q_num(full_panel, target_idx),
    target_quarter_start = q_start(full_panel, target_idx),
    train_y_start_index = train_start_idx,
    train_y_start_quarter = q_label(full_panel, train_start_idx),
    train_y_end_index = train_end_idx,
    train_y_end_quarter = q_label(full_panel, train_end_idx),
    n_train = train_end_idx - train_start_idx + 1L,
    actual = actual,
    point_forecast = point,
    lower_95 = lower,
    upper_95 = upper,
    interval_width = as.numeric(mets[["interval_width"]]),
    error_actual_minus_forecast = as.numeric(mets[["error_actual_minus_forecast"]]),
    abs_error = as.numeric(mets[["abs_error"]]),
    mape_contribution = as.numeric(mets[["mape_contribution"]]),
    smape_contribution = as.numeric(mets[["smape_contribution"]]),
    covered_95 = as.integer(mets[["covered_95"]]),
    interval_score = as.numeric(mets[["interval_score"]]),
    niter = NITER,
    burn = burn,
    interval_level = INTERVAL_LEVEL,
    seed = SEED,
    status = "success",
    message = "",
    stringsAsFactors = FALSE
  )
}

summarise_metrics <- function(df, group_cols) {
  if (nrow(df) == 0L) return(data.frame())
  key <- interaction(df[, group_cols, drop = FALSE], drop = TRUE, sep = "||")
  pieces <- split(df, key)
  out <- lapply(pieces, function(d) {
    good <- d[d$status == "success" & is.finite(d$point_forecast), , drop = FALSE]
    if (nrow(good) == 0L) {
      base <- d[1L, group_cols, drop = FALSE]
      return(cbind(base, data.frame(
        n_predictions = nrow(d), n_success = 0L, MAE = NA_real_, RMSE = NA_real_,
        MAPE = NA_real_, sMAPE = NA_real_, mean_error = NA_real_, median_abs_error = NA_real_,
        coverage_95 = NA_real_, avg_interval_width = NA_real_, avg_interval_score = NA_real_,
        stringsAsFactors = FALSE
      )))
    }
    base <- good[1L, group_cols, drop = FALSE]
    cbind(base, data.frame(
      n_predictions = nrow(d),
      n_success = nrow(good),
      MAE = mean(good$abs_error, na.rm = TRUE),
      RMSE = sqrt(mean(good$error_actual_minus_forecast^2, na.rm = TRUE)),
      MAPE = mean(good$mape_contribution, na.rm = TRUE),
      sMAPE = mean(good$smape_contribution, na.rm = TRUE),
      mean_error = mean(good$error_actual_minus_forecast, na.rm = TRUE),
      median_abs_error = stats::median(good$abs_error, na.rm = TRUE),
      coverage_95 = mean(good$covered_95, na.rm = TRUE),
      avg_interval_width = mean(good$interval_width, na.rm = TRUE),
      avg_interval_score = mean(good$interval_score, na.rm = TRUE),
      min_target_quarter = good$target_quarter[which.min(good$target_index)],
      max_target_quarter = good$target_quarter[which.max(good$target_index)],
      stringsAsFactors = FALSE
    ))
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

# -----------------------------
# 2. Load and validate inputs
# -----------------------------
ensure_dir(BSTS_DIR)
ensure_dir(FIGURE_DIR)
ensure_dir(TABLE_DIR)
if (SAVE_RDS) ensure_dir(RDS_DIR)

FULL_PANEL_PATH <- locate_file(FULL_PANEL_FILE, CANDIDATE_PROCESSED_DIRS)
H0_PATH <- locate_file(H0_FILE, CANDIDATE_VALIDATION_DIRS, required = "h0" %in% HORIZONS)
H1_PATH <- locate_file(H1_FILE, CANDIDATE_VALIDATION_DIRS, required = "h1" %in% HORIZONS)
H2_PATH <- locate_file(H2_FILE, CANDIDATE_VALIDATION_DIRS, required = "h2" %in% HORIZONS)
POST_GT_PATH <- locate_file(POST_GT_FILE, CANDIDATE_VALIDATION_DIRS, required = RUN_POST_GT)
BENCHMARK_H0_PATH <- locate_file(BENCHMARK_H0_FILE, CANDIDATE_BENCHMARK_DIRS, required = FALSE)

full_panel <- read_csv_utf8(FULL_PANEL_PATH)
required_full_cols <- c("Quarter", "Year", "Q", "Quarter_Index", "quarter_start", "unemployment_rate_nationals")
assert_has_columns(full_panel, required_full_cols, FULL_PANEL_FILE)
full_panel <- full_panel[order(full_panel$Quarter_Index), , drop = FALSE]
full_panel$unemployment_rate_nationals <- as.numeric(full_panel$unemployment_rate_nationals)
if (any(is.na(full_panel$unemployment_rate_nationals))) stop("Missing unemployment values in full unemployment panel.", call. = FALSE)

read_origins <- function(path, file_label) {
  d <- read_csv_utf8(path)
  required_origin_cols <- c(
    "fold_id", "origin_id", "horizon_quarters", "target_index", "target_quarter",
    "train_y_start_index", "train_y_start_quarter", "train_y_end_index",
    "train_y_end_quarter", "n_y_available_at_origin"
  )
  assert_has_columns(d, required_origin_cols, file_label)
  d
}

rolling_h0 <- if ("h0" %in% HORIZONS) read_origins(H0_PATH, H0_FILE) else data.frame()
rolling_h1 <- if ("h1" %in% HORIZONS) read_origins(H1_PATH, H1_FILE) else data.frame()
rolling_h2 <- if ("h2" %in% HORIZONS) read_origins(H2_PATH, H2_FILE) else data.frame()

if (RUN_POST_GT) {
  post_gt <- read_csv_utf8(POST_GT_PATH)
  required_post_cols <- c(
    "block_id", "origin_index", "origin_quarter", "target_index", "target_quarter",
    "horizon_from_gt_endpoint", "train_y_start_index", "train_y_end_index"
  )
  assert_has_columns(post_gt, required_post_cols, POST_GT_FILE)
} else {
  post_gt <- data.frame()
}

# -----------------------------
# 3. Rolling-origin state-spec comparison
# -----------------------------
run_rolling_for_spec <- function(origins, horizon_label, state_spec, seed_base) {
  if (nrow(origins) == 0L) return(data.frame())
  rows <- vector("list", nrow(origins))
  model_label <- state_spec_label(state_spec)

  for (i in seq_len(nrow(origins))) {
    origin_row <- origins[i, , drop = FALSE]
    target_idx <- as.integer(origin_row$target_index[[1]])
    train_start_idx <- as.integer(origin_row$train_y_start_index[[1]])
    train_end_idx <- as.integer(origin_row$train_y_end_index[[1]])
    forecast_steps <- target_idx - train_end_idx

    message(sprintf("%s %s fold %d/%d: train through %s, target %s, forecast steps %d",
                    model_label, horizon_label, i, nrow(origins), q_label(full_panel, train_end_idx), q_label(full_panel, target_idx), forecast_steps))

    result <- tryCatch({
      train_y <- get_train_y(full_panel, train_start_idx, train_end_idx)
      fit <- fit_bsts_forecast(train_y, forecast_steps, state_spec, seed_offset = seed_base + i)
      vals <- extract_prediction_step(fit$prediction, forecast_steps)
      if (!all(is.finite(vals))) stop("Prediction extraction returned non-finite values.", call. = FALSE)

      if (SAVE_RDS) {
        rds_path <- file.path(RDS_DIR, sprintf("%s_%s_fold_%03d.rds", model_label, horizon_label, i))
        saveRDS(fit$model, rds_path)
      }

      make_prediction_row(origin_row, full_panel, horizon_label, state_spec, vals, fit$burn)
    }, error = function(e) {
      warning(sprintf("%s failed for %s fold %d: %s", model_label, horizon_label, i, conditionMessage(e)))
      empty_prediction_row(origin_row, full_panel, horizon_label, state_spec, "failed", conditionMessage(e))
    })

    rows[[i]] <- result
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

run_rolling_all_specs <- function(origins, horizon_label, seed_base) {
  if (nrow(origins) == 0L) return(data.frame())
  pieces <- vector("list", length(STATE_SPECS))
  for (j in seq_along(STATE_SPECS)) {
    spec <- STATE_SPECS[[j]]
    pieces[[j]] <- run_rolling_for_spec(origins, horizon_label, spec, seed_base = seed_base + j * 100000L)
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

pred_h0 <- if ("h0" %in% HORIZONS) run_rolling_all_specs(rolling_h0, "h0_same_quarter_nowcast", seed_base = 10000L) else data.frame()
pred_h1 <- if ("h1" %in% HORIZONS) run_rolling_all_specs(rolling_h1, "h1_one_quarter_ahead_forecast", seed_base = 20000L) else data.frame()
pred_h2 <- if ("h2" %in% HORIZONS) run_rolling_all_specs(rolling_h2, "h2_two_quarter_ahead_forecast_exploratory", seed_base = 30000L) else data.frame()
pred_all <- do.call(rbind, Filter(function(x) nrow(x) > 0L, list(pred_h0, pred_h1, pred_h2)))
if (is.null(pred_all)) pred_all <- data.frame()

# -----------------------------
# 4. Post-GT validation block
# -----------------------------
run_post_gt_for_spec <- function(post_gt, state_spec, seed_base) {
  if (nrow(post_gt) == 0L) return(data.frame())
  model_label <- state_spec_label(state_spec)
  origin_idx <- as.integer(post_gt$origin_index[[1]])
  train_start_idx <- as.integer(post_gt$train_y_start_index[[1]])
  train_end_idx <- as.integer(post_gt$train_y_end_index[[1]])
  max_horizon <- max(as.integer(post_gt$horizon_from_gt_endpoint), na.rm = TRUE)

  message(sprintf("%s post-GT forecast: train through %s, horizon %d quarters", model_label, q_label(full_panel, train_end_idx), max_horizon))

  train_y <- get_train_y(full_panel, train_start_idx, train_end_idx)
  fit <- tryCatch({
    fit_bsts_forecast(train_y, max_horizon, state_spec, seed_offset = seed_base)
  }, error = function(e) {
    warning(sprintf("%s post-GT fit failed: %s", model_label, conditionMessage(e)))
    NULL
  })

  rows <- vector("list", nrow(post_gt))
  for (i in seq_len(nrow(post_gt))) {
    target_idx <- as.integer(post_gt$target_index[[i]])
    h <- as.integer(post_gt$horizon_from_gt_endpoint[[i]])
    actual <- y_actual(full_panel, target_idx)

    base <- data.frame(
      model_family = "BSTS_STATE_SPEC_COMPARISON",
      model = model_label,
      model_label = model_label,
      state_specification = state_spec,
      state_description = state_spec_description(state_spec),
      validation_block = "post_gt_forecast_validation",
      block_id = as.character(post_gt$block_id[[i]]),
      origin_index = origin_idx,
      origin_quarter = q_label(full_panel, origin_idx),
      target_index = target_idx,
      target_quarter = q_label(full_panel, target_idx),
      target_year = q_year(full_panel, target_idx),
      target_q = q_num(full_panel, target_idx),
      target_quarter_start = q_start(full_panel, target_idx),
      forecast_steps = h,
      train_y_start_index = train_start_idx,
      train_y_start_quarter = q_label(full_panel, train_start_idx),
      train_y_end_index = train_end_idx,
      train_y_end_quarter = q_label(full_panel, train_end_idx),
      n_train = train_end_idx - train_start_idx + 1L,
      actual = actual,
      stringsAsFactors = FALSE
    )

    if (is.null(fit)) {
      rows[[i]] <- cbind(base, data.frame(
        point_forecast = NA_real_, lower_95 = NA_real_, upper_95 = NA_real_, interval_width = NA_real_,
        error_actual_minus_forecast = NA_real_, abs_error = NA_real_, mape_contribution = NA_real_,
        smape_contribution = NA_real_, covered_95 = NA_integer_, interval_score = NA_real_,
        niter = NITER, burn = NA_integer_, interval_level = INTERVAL_LEVEL, seed = SEED,
        status = "failed", message = "post-GT model fit failed", stringsAsFactors = FALSE
      ))
      next
    }

    vals <- extract_prediction_step(fit$prediction, h)
    point <- as.numeric(vals[["point_forecast"]])
    lower <- as.numeric(vals[["lower"]])
    upper <- as.numeric(vals[["upper"]])
    mets <- error_row_metrics(actual, point, lower, upper)

    rows[[i]] <- cbind(base, data.frame(
      point_forecast = point,
      lower_95 = lower,
      upper_95 = upper,
      interval_width = as.numeric(mets[["interval_width"]]),
      error_actual_minus_forecast = as.numeric(mets[["error_actual_minus_forecast"]]),
      abs_error = as.numeric(mets[["abs_error"]]),
      mape_contribution = as.numeric(mets[["mape_contribution"]]),
      smape_contribution = as.numeric(mets[["smape_contribution"]]),
      covered_95 = as.integer(mets[["covered_95"]]),
      interval_score = as.numeric(mets[["interval_score"]]),
      niter = NITER,
      burn = fit$burn,
      interval_level = INTERVAL_LEVEL,
      seed = SEED,
      status = "success",
      message = "",
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

if (RUN_POST_GT) {
  post_pieces <- vector("list", length(STATE_SPECS))
  for (j in seq_along(STATE_SPECS)) {
    post_pieces[[j]] <- run_post_gt_for_spec(post_gt, STATE_SPECS[[j]], seed_base = 40000L + j * 100000L)
  }
  post_pred <- do.call(rbind, post_pieces)
  rownames(post_pred) <- NULL
} else {
  post_pred <- data.frame()
}

# -----------------------------
# 5. Metrics and rankings
# -----------------------------
metrics_by_horizon <- if (nrow(pred_all) > 0L) {
  summarise_metrics(pred_all, c("model_family", "model", "model_label", "state_specification", "state_description", "horizon_label", "horizon_quarters"))
} else {
  data.frame()
}

post_metrics <- if (nrow(post_pred) > 0L) {
  summarise_metrics(post_pred, c("model_family", "model", "model_label", "state_specification", "state_description", "validation_block"))
} else {
  data.frame()
}

h0_ranking <- metrics_by_horizon[metrics_by_horizon$horizon_label == "h0_same_quarter_nowcast" & is.finite(metrics_by_horizon$MAE), , drop = FALSE]
if (nrow(h0_ranking) > 0L) {
  h0_ranking <- h0_ranking[order(h0_ranking$MAE, h0_ranking$RMSE, h0_ranking$avg_interval_score), , drop = FALSE]
  h0_ranking$h0_MAE_rank <- seq_len(nrow(h0_ranking))
}

benchmark_compare <- data.frame()
if (!is.na(BENCHMARK_H0_PATH) && file.exists(BENCHMARK_H0_PATH) && nrow(h0_ranking) > 0L) {
  benchmark_h0 <- read_csv_utf8(BENCHMARK_H0_PATH)
  if (all(c("model_label", "model_family", "MAE", "RMSE", "MAPE", "sMAPE") %in% names(benchmark_h0))) {
    b <- data.frame(
      source = "Step7 benchmark",
      model_label = as.character(benchmark_h0$model_label),
      model_family = as.character(benchmark_h0$model_family),
      MAE = as.numeric(benchmark_h0$MAE),
      RMSE = as.numeric(benchmark_h0$RMSE),
      MAPE = as.numeric(benchmark_h0$MAPE),
      sMAPE = as.numeric(benchmark_h0$sMAPE),
      stringsAsFactors = FALSE
    )
    s <- data.frame(
      source = "Step8B BSTS no-GT state spec",
      model_label = as.character(h0_ranking$model_label),
      model_family = as.character(h0_ranking$state_description),
      MAE = as.numeric(h0_ranking$MAE),
      RMSE = as.numeric(h0_ranking$RMSE),
      MAPE = as.numeric(h0_ranking$MAPE),
      sMAPE = as.numeric(h0_ranking$sMAPE),
      stringsAsFactors = FALSE
    )
    benchmark_compare <- rbind(b, s)
    benchmark_compare <- benchmark_compare[order(benchmark_compare$MAE, benchmark_compare$RMSE), , drop = FALSE]
    benchmark_compare$MAE_rank <- seq_len(nrow(benchmark_compare))
  }
}

# -----------------------------
# 6. Write tables
# -----------------------------
if (nrow(pred_h0) > 0L) write_csv_utf8(pred_h0, OUTPUT_H0)
if (nrow(pred_h1) > 0L) write_csv_utf8(pred_h1, OUTPUT_H1)
if (nrow(pred_h2) > 0L) write_csv_utf8(pred_h2, OUTPUT_H2)
if (nrow(pred_all) > 0L) write_csv_utf8(pred_all, OUTPUT_ALL)
write_csv_utf8(metrics_by_horizon, OUTPUT_METRICS)
write_csv_utf8(h0_ranking, OUTPUT_H0_RANKING)
if (nrow(post_pred) > 0L) write_csv_utf8(post_pred, OUTPUT_POST)
write_csv_utf8(post_metrics, OUTPUT_POST_METRICS)
if (nrow(benchmark_compare) > 0L) write_csv_utf8(benchmark_compare, OUTPUT_COMPARISON_H0)

# mirror main tables inside /tables for convenience
for (path in c(OUTPUT_H0, OUTPUT_H1, OUTPUT_H2, OUTPUT_ALL, OUTPUT_METRICS, OUTPUT_H0_RANKING, OUTPUT_POST, OUTPUT_POST_METRICS, OUTPUT_COMPARISON_H0)) {
  if (file.exists(path)) file.copy(path, file.path(TABLE_DIR, basename(path)), overwrite = TRUE)
}

# -----------------------------
# 7. Figures
# -----------------------------
plot_theme <- ggplot2::theme_minimal(base_size = 14) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

MODEL_COLORS <- c(
  "Actual" = "black",
  "BSTS_NO_GT_LL" = "#E69F00",
  "BSTS_NO_GT_LL_SEAS" = "#009E73",
  "BSTS_NO_GT_LLT" = "#56B4E9",
  "BSTS_NO_GT_LLT_SEAS" = "#0072B2"
)
MODEL_LINETYPES <- c(
  "Actual" = "solid",
  "BSTS_NO_GT_LL" = "solid",
  "BSTS_NO_GT_LL_SEAS" = "dashed",
  "BSTS_NO_GT_LLT" = "dotdash",
  "BSTS_NO_GT_LLT_SEAS" = "longdash"
)

get_h0_order <- function(h0_ranking) {
  if (nrow(h0_ranking) == 0L) return(vapply(STATE_SPECS, state_spec_label, character(1L)))
  as.character(h0_ranking$model_label)
}

make_mae_h0_plot <- function(h0_ranking, path) {
  d <- h0_ranking[is.finite(h0_ranking$MAE), , drop = FALSE]
  if (nrow(d) == 0L) return(invisible(FALSE))
  d$model_label <- factor(d$model_label, levels = d$model_label)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = model_label, y = MAE)) +
    ggplot2::geom_col(fill = "#0072B2") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", MAE)), vjust = -0.35, size = 4) +
    ggplot2::labs(
      x = "BSTS state specification",
      y = "MAE (percentage points)"
    ) +
    plot_theme +
    ggplot2::theme(legend.position = "none")
  ggplot2::ggsave(path, p, width = 12, height = 7, dpi = 300)
  invisible(TRUE)
}

make_mae_horizon_plot <- function(metrics, path) {
  d <- metrics[is.finite(metrics$MAE), , drop = FALSE]
  if (nrow(d) == 0L) return(invisible(FALSE))
  order <- get_h0_order(h0_ranking)
  d$model_label <- factor(d$model_label, levels = order)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = model_label, y = MAE, fill = horizon_label)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    ggplot2::labs(
      x = "BSTS state specification",
      y = "MAE (percentage points)",
      fill = "Validation design"
    ) +
    plot_theme
  ggplot2::ggsave(path, p, width = 13, height = 7, dpi = 300)
  invisible(TRUE)
}

make_h0_actual_predicted_plot <- function(pred_h0, h0_ranking, path) {
  d <- pred_h0[pred_h0$status == "success", , drop = FALSE]
  if (nrow(d) == 0L) return(invisible(FALSE))
  d$target_date <- as.Date(d$target_quarter_start)
  actual <- unique(d[, c("target_date", "target_quarter", "actual"), drop = FALSE])
  names(actual)[names(actual) == "actual"] <- "value"
  actual$Series <- "Actual"
  model <- data.frame(
    target_date = d$target_date,
    target_quarter = d$target_quarter,
    value = d$point_forecast,
    Series = d$model_label,
    stringsAsFactors = FALSE
  )
  long <- rbind(actual[, c("target_date", "target_quarter", "Series", "value")], model)
  series_order <- c("Actual", get_h0_order(h0_ranking))
  long$Series <- factor(long$Series, levels = series_order)
  p <- ggplot2::ggplot(long, ggplot2::aes(x = target_date, y = value, colour = Series, linetype = Series, group = Series)) +
    ggplot2::geom_line(linewidth = 1.0) +
    ggplot2::geom_point(size = 2.1) +
    ggplot2::scale_colour_manual(values = MODEL_COLORS[series_order], breaks = series_order) +
    ggplot2::scale_linetype_manual(values = MODEL_LINETYPES[series_order], breaks = series_order) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::labs(
      x = "Target quarter",
      y = "Unemployment rate (%)",
      colour = "Series",
      linetype = "Series"
    ) +
    plot_theme
  ggplot2::ggsave(path, p, width = 13, height = 7, dpi = 300)
  invisible(TRUE)
}

make_h0_error_plot <- function(pred_h0, h0_ranking, path) {
  d <- pred_h0[pred_h0$status == "success", , drop = FALSE]
  if (nrow(d) == 0L) return(invisible(FALSE))
  d$target_date <- as.Date(d$target_quarter_start)
  order <- get_h0_order(h0_ranking)
  d$model_label <- factor(d$model_label, levels = order)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = target_date, y = error_actual_minus_forecast, colour = model_label, linetype = model_label, group = model_label)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "black", linewidth = 0.8) +
    ggplot2::geom_line(linewidth = 1.0) +
    ggplot2::geom_point(size = 2.0) +
    ggplot2::scale_colour_manual(values = MODEL_COLORS[order], breaks = order) +
    ggplot2::scale_linetype_manual(values = MODEL_LINETYPES[order], breaks = order) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::labs(
      x = "Target quarter",
      y = "Error: actual - forecast",
      colour = "Model",
      linetype = "Model"
    ) +
    plot_theme
  ggplot2::ggsave(path, p, width = 13, height = 7, dpi = 300)
  invisible(TRUE)
}

make_best_interval_plot <- function(pred_h0, h0_ranking, path) {
  if (nrow(h0_ranking) == 0L) return(invisible(FALSE))
  best_model <- as.character(h0_ranking$model_label[[1]])
  d <- pred_h0[pred_h0$status == "success" & pred_h0$model_label == best_model, , drop = FALSE]
  if (nrow(d) == 0L) return(invisible(FALSE))
  d$target_date <- as.Date(d$target_quarter_start)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = target_date)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_95, ymax = upper_95), fill = "#56B4E9", alpha = 0.20) +
    ggplot2::geom_line(ggplot2::aes(y = point_forecast, colour = best_model), linewidth = 1.1) +
    ggplot2::geom_point(ggplot2::aes(y = point_forecast, colour = best_model), size = 2.1) +
    ggplot2::geom_line(ggplot2::aes(y = actual, colour = "Actual"), linewidth = 1.2) +
    ggplot2::geom_point(ggplot2::aes(y = actual, colour = "Actual"), size = 2.3) +
    ggplot2::scale_colour_manual(values = MODEL_COLORS[c("Actual", best_model)]) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::labs(
      x = "Target quarter",
      y = "Unemployment rate (%)",
      colour = "Series"
    ) +
    plot_theme
  ggplot2::ggsave(path, p, width = 12, height = 7, dpi = 300)
  invisible(TRUE)
}

make_post_gt_plot <- function(post_pred, post_metrics, path) {
  d <- post_pred[post_pred$status == "success", , drop = FALSE]
  if (nrow(d) == 0L) return(invisible(FALSE))
  order <- if (nrow(post_metrics) > 0L) {
    pm <- post_metrics[is.finite(post_metrics$MAE), , drop = FALSE]
    pm <- pm[order(pm$MAE, pm$RMSE), , drop = FALSE]
    as.character(pm$model_label)
  } else {
    unique(as.character(d$model_label))
  }
  d$target_quarter_factor <- factor(d$target_quarter, levels = unique(d$target_quarter[order(d$target_index)]))
  actual <- unique(d[, c("target_quarter_factor", "target_quarter", "actual"), drop = FALSE])
  names(actual)[names(actual) == "actual"] <- "value"
  actual$Series <- "Actual"
  model <- data.frame(target_quarter_factor = d$target_quarter_factor, Series = d$model_label, value = d$point_forecast, stringsAsFactors = FALSE)
  long <- rbind(actual[, c("target_quarter_factor", "Series", "value")], model)
  series_order <- c("Actual", order)
  long$Series <- factor(long$Series, levels = series_order)
  p <- ggplot2::ggplot(long, ggplot2::aes(x = target_quarter_factor, y = value, colour = Series, linetype = Series, group = Series)) +
    ggplot2::geom_line(linewidth = 1.05) +
    ggplot2::geom_point(size = 2.3) +
    ggplot2::scale_colour_manual(values = MODEL_COLORS[series_order], breaks = series_order) +
    ggplot2::scale_linetype_manual(values = MODEL_LINETYPES[series_order], breaks = series_order) +
    ggplot2::labs(
      x = "Target quarter",
      y = "Unemployment rate (%)",
      colour = "Series",
      linetype = "Series"
    ) +
    plot_theme
  ggplot2::ggsave(path, p, width = 13, height = 7, dpi = 300)
  invisible(TRUE)
}

make_benchmark_compare_plot <- function(comparison, path) {
  d <- comparison[is.finite(comparison$MAE), , drop = FALSE]
  if (nrow(d) == 0L) return(invisible(FALSE))
  d <- d[order(d$MAE, d$RMSE), , drop = FALSE]
  d$model_label <- factor(d$model_label, levels = d$model_label)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = model_label, y = MAE, fill = source)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", MAE)), vjust = -0.30, size = 3.7) +
    ggplot2::labs(
      x = "Model",
      y = "MAE (percentage points)",
      fill = "Source"
    ) +
    plot_theme
  ggplot2::ggsave(path, p, width = 13, height = 7, dpi = 300)
  invisible(TRUE)
}

if (nrow(h0_ranking) > 0L) make_mae_h0_plot(h0_ranking, FIG_MAE_H0)
if (nrow(metrics_by_horizon) > 0L) make_mae_horizon_plot(metrics_by_horizon, FIG_MAE_HORIZON)
if (nrow(pred_h0) > 0L) make_h0_actual_predicted_plot(pred_h0, h0_ranking, FIG_H0_ACTUAL)
if (nrow(pred_h0) > 0L) make_h0_error_plot(pred_h0, h0_ranking, FIG_H0_ERRORS)
if (nrow(pred_h0) > 0L) make_best_interval_plot(pred_h0, h0_ranking, FIG_BEST_INTERVALS)
if (nrow(post_pred) > 0L) make_post_gt_plot(post_pred, post_metrics, FIG_POST)
if (nrow(benchmark_compare) > 0L) make_benchmark_compare_plot(benchmark_compare, FIG_BENCHMARK_COMPARE)

# -----------------------------
# 8. Config, report, manifest
# -----------------------------
best_line <- if (nrow(h0_ranking) > 0L) {
  sprintf("Best h = 0 state specification by MAE: `%s` (`%s`), MAE = %.3f.",
          h0_ranking$model_label[[1]], h0_ranking$state_description[[1]], h0_ranking$MAE[[1]])
} else {
  "Best h = 0 state specification by MAE: unavailable because no h0 predictions were produced."
}

config_lines <- c(
  "{",
  '  "script": "07b_compare_bsts_state_specs.R",',
  '  "step": "Step 8B — Compare BSTS no-Google-Trends state specifications",',
  sprintf('  "generated_at": "%s",', format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  '  "inputs": {',
  sprintf('    "full_unemployment_panel": "%s",', json_escape(FULL_PANEL_PATH)),
  sprintf('    "h0_origins": "%s",', json_escape(ifelse(is.na(H0_PATH), "", H0_PATH))),
  sprintf('    "h1_origins": "%s",', json_escape(ifelse(is.na(H1_PATH), "", H1_PATH))),
  sprintf('    "h2_origins": "%s",', json_escape(ifelse(is.na(H2_PATH), "", H2_PATH))),
  sprintf('    "post_gt_block": "%s",', json_escape(ifelse(is.na(POST_GT_PATH), "", POST_GT_PATH))),
  sprintf('    "benchmark_h0_optional": "%s"', json_escape(ifelse(is.na(BENCHMARK_H0_PATH), "", BENCHMARK_H0_PATH))),
  '  },',
  '  "model_grid": {',
  sprintf('    "state_specifications": %s,', json_vec(STATE_SPECS)),
  sprintf('    "model_labels": %s,', json_vec(vapply(STATE_SPECS, state_spec_label, character(1L)))),
  sprintf('    "horizons": %s,', json_vec(HORIZONS)),
  sprintf('    "run_post_gt": %s,', json_bool(RUN_POST_GT)),
  sprintf('    "seasonal_frequency": %d,', SEASONAL_FREQUENCY),
  sprintf('    "niter": %d,', NITER),
  sprintf('    "burn_fraction": %.4f,', BURN_FRACTION),
  sprintf('    "interval_level": %.1f,', INTERVAL_LEVEL),
  sprintf('    "seed": %d,', SEED),
  sprintf('    "save_rds": %s', json_bool(SAVE_RDS)),
  '  },',
  '  "information_set": {',
  '    "uses_google_trends": false,',
  '    "uses_future_unemployment": false,',
  '    "post_gt_forecasts_train_through": "2024-3"',
  '  },',
  '  "outputs": {',
  sprintf('    "output_dir": "%s",', json_escape(BSTS_DIR)),
  sprintf('    "h0_predictions": "%s",', basename(OUTPUT_H0)),
  sprintf('    "h1_predictions": "%s",', basename(OUTPUT_H1)),
  sprintf('    "h2_predictions": "%s",', basename(OUTPUT_H2)),
  sprintf('    "all_predictions": "%s",', basename(OUTPUT_ALL)),
  sprintf('    "metrics": "%s",', basename(OUTPUT_METRICS)),
  sprintf('    "h0_ranking": "%s",', basename(OUTPUT_H0_RANKING)),
  sprintf('    "post_gt_forecasts": "%s",', basename(OUTPUT_POST)),
  sprintf('    "post_gt_metrics": "%s"', basename(OUTPUT_POST_METRICS)),
  '  }',
  "}"
)
write_lines_utf8(config_lines, OUTPUT_CONFIG)

metric_lines <- if (nrow(h0_ranking) > 0L) {
  apply(h0_ranking, 1, function(r) {
    sprintf("- `%s` (%s): h0 MAE = %.3f, RMSE = %.3f, MAPE = %.2f%%, 95%% coverage = %.2f.",
            r[["model_label"]], r[["state_description"]], as.numeric(r[["MAE"]]), as.numeric(r[["RMSE"]]), as.numeric(r[["MAPE"]]), as.numeric(r[["coverage_95"]]))
  })
} else {
  "- h0 ranking unavailable."
}

post_lines <- if (nrow(post_metrics) > 0L) {
  pm <- post_metrics[order(post_metrics$MAE, post_metrics$RMSE), , drop = FALSE]
  apply(pm, 1, function(r) {
    sprintf("- `%s`: post-GT MAE = %.3f, RMSE = %.3f, MAPE = %.2f%%, 95%% coverage = %.2f.",
            r[["model_label"]], as.numeric(r[["MAE"]]), as.numeric(r[["RMSE"]]), as.numeric(r[["MAPE"]]), as.numeric(r[["coverage_95"]]))
  })
} else {
  "- Post-GT comparison was not run or no successful post-GT predictions were produced."
}

report_lines <- c(
  "# Step 8B — BSTS no-Google-Trends state-specification comparison",
  "",
  "## Status",
  "",
  "This step compares unemployment-only BSTS state structures before adding Google Trends predictors. No Google Trends predictors, filtering, sparsity thresholds, or standardisation are used here.",
  "",
  "## Candidate state specifications",
  "",
  "| Model label | State specification | Description |",
  "|---|---|---|",
  sprintf("| `%s` | `%s` | %s |", vapply(STATE_SPECS, state_spec_label, character(1L)), STATE_SPECS, vapply(STATE_SPECS, state_spec_description, character(1L))),
  "",
  "## MCMC settings",
  "",
  sprintf("- MCMC iterations: `%d`.", NITER),
  sprintf("- Burn fraction: `%.2f`.", BURN_FRACTION),
  sprintf("- Predictive interval level: `%.1f%%`.", INTERVAL_LEVEL),
  sprintf("- Random seed base: `%d`.", SEED),
  "",
  "## h = 0 ranking",
  "",
  best_line,
  "",
  metric_lines,
  "",
  "## Post-GT validation block",
  "",
  post_lines,
  "",
  "## Interpretation rule",
  "",
  "The preferred no-GT state specification should be selected before Step 9. The choice should primarily consider h = 0 MAE, RMSE, interval score, and interval coverage, with post-GT behaviour used as a secondary diagnostic. The goal is not to tune the state structure after seeing Google Trends results.",
  "",
  "## Files written",
  "",
  "- `bsts_state_spec_predictions_h0.csv`",
  "- `bsts_state_spec_predictions_h1.csv`",
  "- `bsts_state_spec_predictions_h2.csv`",
  "- `bsts_state_spec_predictions_all.csv`",
  "- `bsts_state_spec_metrics_by_horizon.csv`",
  "- `bsts_state_spec_h0_ranking.csv`",
  "- `bsts_state_spec_post_gt_forecasts.csv`",
  "- `bsts_state_spec_post_gt_metrics.csv`",
  "- `bsts_state_spec_vs_benchmark_h0.csv` if benchmark outputs are available",
  "- `figures/bsts_state_spec_mae_h0.png`",
  "- `figures/bsts_state_spec_mae_by_horizon.png`",
  "- `figures/bsts_state_spec_actual_vs_predicted_h0.png`",
  "- `figures/bsts_state_spec_rolling_errors_h0.png`",
  "- `figures/bsts_state_spec_best_intervals_h0.png`",
  "- `figures/bsts_state_spec_post_gt_forecasts.png`",
  "- `figures/bsts_state_spec_vs_benchmark_h0.png` if benchmark outputs are available",
  "",
  "## Next step",
  "",
  "After reviewing this comparison, freeze one no-GT state specification and use it in Step 9 for BSTS + MA and BSTS + EMA Google Trends models."
)
write_lines_utf8(report_lines, OUTPUT_REPORT)

output_files <- c(
  OUTPUT_H0,
  OUTPUT_H1,
  OUTPUT_H2,
  OUTPUT_ALL,
  OUTPUT_METRICS,
  OUTPUT_H0_RANKING,
  OUTPUT_POST,
  OUTPUT_POST_METRICS,
  OUTPUT_COMPARISON_H0,
  OUTPUT_CONFIG,
  OUTPUT_REPORT,
  FIG_MAE_H0,
  FIG_MAE_HORIZON,
  FIG_H0_ACTUAL,
  FIG_H0_ERRORS,
  FIG_BEST_INTERVALS,
  FIG_POST,
  FIG_BENCHMARK_COMPARE
)
output_files <- output_files[file.exists(output_files)]

manifest <- data.frame(
  output_key = seq_along(output_files),
  file_name = basename(output_files),
  relative_path = file.path("data", "processed", "bsts_state_specs", basename(output_files)),
  file_size_bytes = file.info(output_files)$size,
  checksum_md5 = vapply(output_files, sha256_or_md5, character(1L)),
  stringsAsFactors = FALSE
)
write_csv_utf8(manifest, OUTPUT_MANIFEST)
write_csv_utf8(manifest, file.path(TABLE_DIR, basename(OUTPUT_MANIFEST)))

message("Step 8B BSTS state-specification comparison complete.")
message(sprintf("Outputs written to: %s", BSTS_DIR))
if (nrow(h0_ranking) > 0L) {
  message(best_line)
}
