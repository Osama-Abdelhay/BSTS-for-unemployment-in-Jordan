#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# 07_run_bsts_baseline.R
# Step 8: Run BSTS baseline without Google Trends
# -----------------------------------------------------------------------------
# Purpose:
#   Fit the Bayesian structural time-series baseline model without Google Trends
#   predictors under the rolling-origin validation design created in Step 6.
#   This is the structural unemployment-only baseline used before adding MA/EMA
#   Google Trends predictors in the next step.
#
# Inputs:
#   data/processed/modeling_panel_unemployment_full.csv
#   data/processed/validation/rolling_origins_h0_nowcast.csv
#   data/processed/validation/rolling_origins_h1_forecast.csv
#   data/processed/validation/rolling_origins_h2_forecast_exploratory.csv
#   data/processed/validation/post_gt_forecast_validation_block.csv
#
# Outputs:
#   data/processed/bsts_baseline/bsts_baseline_predictions_h0.csv
#   data/processed/bsts_baseline/bsts_baseline_predictions_h1.csv
#   data/processed/bsts_baseline/bsts_baseline_predictions_h2.csv
#   data/processed/bsts_baseline/bsts_baseline_predictions_all.csv
#   data/processed/bsts_baseline/bsts_baseline_metrics_by_horizon.csv
#   data/processed/bsts_baseline/bsts_baseline_post_gt_forecasts.csv
#   data/processed/bsts_baseline/bsts_baseline_post_gt_metrics.csv
#   data/processed/bsts_baseline/07_run_bsts_baseline_config.json
#   data/processed/bsts_baseline/07_run_bsts_baseline_report.md
#   data/processed/bsts_baseline/07_run_bsts_baseline_manifest.csv
#   data/processed/bsts_baseline/figures/*.png
#
# Primary model:
#   BSTS_NO_GT: local linear trend + quarterly seasonal component.
#
# Agreed pipeline rules:
#   - No Google Trends predictors are used here.
#   - No Google Trends preprocessing, sparsity filtering, or standardisation is
#     performed here.
#   - No future unemployment values are used.
#   - Rolling predictions follow the Step 6 information-set design.
#   - h = 0 is interpreted as an unemployment-only comparator for same-quarter
#     nowcasting; because no target-quarter Google Trends are used, it is a
#     one-step-ahead unemployment-only prediction using unemployment through t-1.
#   - Post-GT forecasts are trained through 2024 Q3 only.
#
# Usage from project root:
#   Rscript scripts/07_run_bsts_baseline.R
#
# Optional arguments:
#   Rscript scripts/07_run_bsts_baseline.R \
#     --processed-dir=data/processed \
#     --validation-dir=data/processed/validation \
#     --bsts-dir=data/processed/bsts_baseline \
#     --niter=3000 \
#     --burn-fraction=0.25 \
#     --state-spec=local_linear_trend_seasonal \
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

PROJECT_ROOT <- normalizePath(get_arg("project-root", Sys.getenv("BSTS_PROJECT_ROOT", unset = getwd())), mustWork = FALSE)
PROCESSED_DIR_DEFAULT <- file.path(PROJECT_ROOT, "data", "processed")
PROCESSED_DIR <- normalizePath(get_arg("processed-dir", Sys.getenv("BSTS_PROCESSED_DIR", unset = PROCESSED_DIR_DEFAULT)), mustWork = FALSE)
VALIDATION_DIR <- normalizePath(get_arg("validation-dir", file.path(PROCESSED_DIR, "validation")), mustWork = FALSE)
BSTS_DIR <- normalizePath(get_arg("bsts-dir", file.path(PROCESSED_DIR, "bsts_baseline")), mustWork = FALSE)
FIGURE_DIR <- file.path(BSTS_DIR, "figures")
TABLE_DIR <- file.path(BSTS_DIR, "tables")

NITER <- as.integer(get_arg("niter", "3000"))
BURN_FRACTION <- as.numeric(get_arg("burn-fraction", "0.25"))
INTERVAL_LEVEL <- as.numeric(get_arg("interval-level", "95"))
ALPHA <- 1 - INTERVAL_LEVEL / 100
SEASONAL_FREQUENCY <- as.integer(get_arg("seasonal-frequency", "4"))
STATE_SPEC <- get_arg("state-spec", "local_linear_trend_seasonal")
SEED <- as.integer(get_arg("seed", "20260616"))
PING <- as.integer(get_arg("ping", "0"))
SAVE_RDS <- to_bool(get_arg("save-rds", "false"))

VALID_STATE_SPECS <- c(
  "local_linear_trend_seasonal",
  "local_level_seasonal",
  "local_linear_trend",
  "local_level"
)
if (!(STATE_SPEC %in% VALID_STATE_SPECS)) {
  stop(sprintf("Unsupported --state-spec: %s. Valid choices: %s", STATE_SPEC, paste(VALID_STATE_SPECS, collapse = ", ")), call. = FALSE)
}

if (!requireNamespace("bsts", quietly = TRUE)) {
  stop("Package 'bsts' is required for Step 8. Install it with install.packages('bsts') and rerun this script.", call. = FALSE)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required for Step 8 figures. Install it with install.packages('ggplot2') and rerun this script.", call. = FALSE)
}

FULL_PANEL_FILE <- "modeling_panel_unemployment_full.csv"
H0_FILE <- "rolling_origins_h0_nowcast.csv"
H1_FILE <- "rolling_origins_h1_forecast.csv"
H2_FILE <- "rolling_origins_h2_forecast_exploratory.csv"
POST_GT_FILE <- "post_gt_forecast_validation_block.csv"

OUTPUT_H0 <- file.path(BSTS_DIR, "bsts_baseline_predictions_h0.csv")
OUTPUT_H1 <- file.path(BSTS_DIR, "bsts_baseline_predictions_h1.csv")
OUTPUT_H2 <- file.path(BSTS_DIR, "bsts_baseline_predictions_h2.csv")
OUTPUT_ALL <- file.path(BSTS_DIR, "bsts_baseline_predictions_all.csv")
OUTPUT_METRICS <- file.path(BSTS_DIR, "bsts_baseline_metrics_by_horizon.csv")
OUTPUT_POST <- file.path(BSTS_DIR, "bsts_baseline_post_gt_forecasts.csv")
OUTPUT_POST_METRICS <- file.path(BSTS_DIR, "bsts_baseline_post_gt_metrics.csv")
OUTPUT_CONFIG <- file.path(BSTS_DIR, "07_run_bsts_baseline_config.json")
OUTPUT_REPORT <- file.path(BSTS_DIR, "07_run_bsts_baseline_report.md")
OUTPUT_MANIFEST <- file.path(BSTS_DIR, "07_run_bsts_baseline_manifest.csv")

FIG_H0_ACTUAL <- file.path(FIGURE_DIR, "bsts_baseline_actual_vs_predicted_h0.png")
FIG_H0_ERRORS <- file.path(FIGURE_DIR, "bsts_baseline_rolling_errors_h0.png")
FIG_H0_INTERVALS <- file.path(FIGURE_DIR, "bsts_baseline_intervals_h0.png")
FIG_POST <- file.path(FIGURE_DIR, "bsts_baseline_post_gt_forecasts.png")
FIG_MAE_HORIZON <- file.path(FIGURE_DIR, "bsts_baseline_mae_by_horizon.png")

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

json_bool <- function(x) ifelse(isTRUE(x), "true", "false")

assert_has_columns <- function(df, cols, file_label) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0L) {
    stop(sprintf("%s is missing required columns: %s", file_label, paste(missing, collapse = ", ")), call. = FALSE)
  }
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

build_state_spec <- function(y_ts) {
  ss <- list()
  if (STATE_SPEC %in% c("local_linear_trend", "local_linear_trend_seasonal")) {
    ss <- bsts::AddLocalLinearTrend(ss, y_ts)
  } else if (STATE_SPEC %in% c("local_level", "local_level_seasonal")) {
    ss <- bsts::AddLocalLevel(ss, y_ts)
  }

  if (STATE_SPEC %in% c("local_linear_trend_seasonal", "local_level_seasonal")) {
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

fit_bsts_forecast <- function(train_y, forecast_horizon, seed_offset = 0L) {
  if (length(train_y) < 8L) stop("BSTS training series is too short.", call. = FALSE)
  y_ts <- make_ts(train_y)
  ss <- build_state_spec(y_ts)
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

empty_prediction_row <- function(origin_row, full_panel, horizon_label, status, message_text) {
  target_idx <- as.integer(origin_row$target_index[[1]])
  train_end_idx <- as.integer(origin_row$train_y_end_index[[1]])
  forecast_steps <- target_idx - train_end_idx
  actual <- y_actual(full_panel, target_idx)
  data.frame(
    model_family = "BSTS_BASELINE",
    model = "BSTS_NO_GT",
    state_specification = STATE_SPEC,
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

make_prediction_row <- function(origin_row, full_panel, horizon_label, pred_values, burn) {
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
    model_family = "BSTS_BASELINE",
    model = "BSTS_NO_GT",
    state_specification = STATE_SPEC,
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

FULL_PANEL_PATH <- locate_file(FULL_PANEL_FILE, CANDIDATE_PROCESSED_DIRS)
H0_PATH <- locate_file(H0_FILE, CANDIDATE_VALIDATION_DIRS)
H1_PATH <- locate_file(H1_FILE, CANDIDATE_VALIDATION_DIRS)
H2_PATH <- locate_file(H2_FILE, CANDIDATE_VALIDATION_DIRS)
POST_GT_PATH <- locate_file(POST_GT_FILE, CANDIDATE_VALIDATION_DIRS)

full_panel <- read_csv_utf8(FULL_PANEL_PATH)
rolling_h0 <- read_csv_utf8(H0_PATH)
rolling_h1 <- read_csv_utf8(H1_PATH)
rolling_h2 <- read_csv_utf8(H2_PATH)
post_gt <- read_csv_utf8(POST_GT_PATH)

required_full_cols <- c("Quarter", "Year", "Q", "Quarter_Index", "quarter_start", "unemployment_rate_nationals")
required_origin_cols <- c(
  "fold_id", "origin_id", "horizon_quarters", "target_index", "target_quarter",
  "train_y_start_index", "train_y_start_quarter", "train_y_end_index",
  "train_y_end_quarter", "n_y_available_at_origin"
)
required_post_cols <- c(
  "block_id", "origin_index", "origin_quarter", "target_index", "target_quarter",
  "horizon_from_gt_endpoint", "train_y_start_index", "train_y_end_index"
)
assert_has_columns(full_panel, required_full_cols, FULL_PANEL_FILE)
assert_has_columns(rolling_h0, required_origin_cols, H0_FILE)
assert_has_columns(rolling_h1, required_origin_cols, H1_FILE)
assert_has_columns(rolling_h2, required_origin_cols, H2_FILE)
assert_has_columns(post_gt, required_post_cols, POST_GT_FILE)

full_panel <- full_panel[order(full_panel$Quarter_Index), , drop = FALSE]
full_panel$unemployment_rate_nationals <- as.numeric(full_panel$unemployment_rate_nationals)
if (any(is.na(full_panel$unemployment_rate_nationals))) stop("Missing unemployment values in full unemployment panel.", call. = FALSE)

# -----------------------------
# 3. Rolling-origin BSTS baseline
# -----------------------------
run_rolling <- function(origins, horizon_label, seed_base) {
  rows <- vector("list", nrow(origins))

  for (i in seq_len(nrow(origins))) {
    origin_row <- origins[i, , drop = FALSE]
    target_idx <- as.integer(origin_row$target_index[[1]])
    train_start_idx <- as.integer(origin_row$train_y_start_index[[1]])
    train_end_idx <- as.integer(origin_row$train_y_end_index[[1]])
    forecast_steps <- target_idx - train_end_idx

    message(sprintf("BSTS baseline %s fold %d/%d: train through %s, target %s, forecast steps %d",
                    horizon_label, i, nrow(origins), q_label(full_panel, train_end_idx), q_label(full_panel, target_idx), forecast_steps))

    result <- tryCatch({
      train_y <- get_train_y(full_panel, train_start_idx, train_end_idx)
      fit <- fit_bsts_forecast(train_y, forecast_steps, seed_offset = seed_base + i)
      vals <- extract_prediction_step(fit$prediction, forecast_steps)
      if (!all(is.finite(vals))) stop("Prediction extraction returned non-finite values.", call. = FALSE)

      if (SAVE_RDS) {
        rds_path <- file.path(BSTS_DIR, sprintf("bsts_baseline_%s_fold_%03d.rds", horizon_label, i))
        saveRDS(fit$model, rds_path)
      }

      make_prediction_row(origin_row, full_panel, horizon_label, vals, fit$burn)
    }, error = function(e) {
      warning(sprintf("BSTS baseline failed for %s fold %d: %s", horizon_label, i, conditionMessage(e)))
      empty_prediction_row(origin_row, full_panel, horizon_label, "failed", conditionMessage(e))
    })

    rows[[i]] <- result
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

pred_h0 <- run_rolling(rolling_h0, "h0_same_quarter_nowcast", seed_base = 10000L)
pred_h1 <- run_rolling(rolling_h1, "h1_one_quarter_ahead_forecast", seed_base = 20000L)
pred_h2 <- run_rolling(rolling_h2, "h2_two_quarter_ahead_forecast_exploratory", seed_base = 30000L)
pred_all <- rbind(pred_h0, pred_h1, pred_h2)

# -----------------------------
# 4. Post-GT validation block
# -----------------------------
run_post_gt <- function(post_gt) {
  origin_idx <- as.integer(post_gt$origin_index[[1]])
  train_start_idx <- as.integer(post_gt$train_y_start_index[[1]])
  train_end_idx <- as.integer(post_gt$train_y_end_index[[1]])
  max_horizon <- max(as.integer(post_gt$horizon_from_gt_endpoint), na.rm = TRUE)

  message(sprintf("BSTS baseline post-GT forecast: train through %s, horizon %d quarters", q_label(full_panel, train_end_idx), max_horizon))

  train_y <- get_train_y(full_panel, train_start_idx, train_end_idx)
  fit <- tryCatch({
    fit_bsts_forecast(train_y, max_horizon, seed_offset = 40000L)
  }, error = function(e) {
    warning(sprintf("BSTS post-GT fit failed: %s", conditionMessage(e)))
    NULL
  })

  rows <- vector("list", nrow(post_gt))

  for (i in seq_len(nrow(post_gt))) {
    target_idx <- as.integer(post_gt$target_index[[i]])
    h <- as.integer(post_gt$horizon_from_gt_endpoint[[i]])
    actual <- y_actual(full_panel, target_idx)

    if (is.null(fit)) {
      rows[[i]] <- data.frame(
        model_family = "BSTS_BASELINE",
        model = "BSTS_NO_GT",
        state_specification = STATE_SPEC,
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
        point_forecast = NA_real_, lower_95 = NA_real_, upper_95 = NA_real_, interval_width = NA_real_,
        error_actual_minus_forecast = NA_real_, abs_error = NA_real_, mape_contribution = NA_real_,
        smape_contribution = NA_real_, covered_95 = NA_integer_, interval_score = NA_real_,
        niter = NITER, burn = NA_integer_, interval_level = INTERVAL_LEVEL, seed = SEED,
        status = "failed", message = "post-GT model fit failed", stringsAsFactors = FALSE
      )
      next
    }

    vals <- extract_prediction_step(fit$prediction, h)
    point <- as.numeric(vals[["point_forecast"]])
    lower <- as.numeric(vals[["lower"]])
    upper <- as.numeric(vals[["upper"]])
    mets <- error_row_metrics(actual, point, lower, upper)

    rows[[i]] <- data.frame(
      model_family = "BSTS_BASELINE",
      model = "BSTS_NO_GT",
      state_specification = STATE_SPEC,
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
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

post_pred <- run_post_gt(post_gt)

# -----------------------------
# 5. Metrics
# -----------------------------
metrics_by_horizon <- summarise_metrics(pred_all, c("model_family", "model", "state_specification", "horizon_label", "horizon_quarters"))
post_metrics <- summarise_metrics(post_pred, c("model_family", "model", "state_specification", "validation_block"))

# -----------------------------
# 6. Write tables
# -----------------------------
write_csv_utf8(pred_h0, OUTPUT_H0)
write_csv_utf8(pred_h1, OUTPUT_H1)
write_csv_utf8(pred_h2, OUTPUT_H2)
write_csv_utf8(pred_all, OUTPUT_ALL)
write_csv_utf8(metrics_by_horizon, OUTPUT_METRICS)
write_csv_utf8(post_pred, OUTPUT_POST)
write_csv_utf8(post_metrics, OUTPUT_POST_METRICS)

write_csv_utf8(pred_h0, file.path(TABLE_DIR, basename(OUTPUT_H0)))
write_csv_utf8(pred_h1, file.path(TABLE_DIR, basename(OUTPUT_H1)))
write_csv_utf8(pred_h2, file.path(TABLE_DIR, basename(OUTPUT_H2)))
write_csv_utf8(pred_all, file.path(TABLE_DIR, basename(OUTPUT_ALL)))
write_csv_utf8(metrics_by_horizon, file.path(TABLE_DIR, basename(OUTPUT_METRICS)))
write_csv_utf8(post_pred, file.path(TABLE_DIR, basename(OUTPUT_POST)))
write_csv_utf8(post_metrics, file.path(TABLE_DIR, basename(OUTPUT_POST_METRICS)))

# -----------------------------
# 7. Figures
# -----------------------------
plot_theme <- ggplot2::theme_minimal(base_size = 14) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

make_h0_actual_predicted_plot <- function(pred_h0, path) {
  d <- pred_h0[pred_h0$status == "success", , drop = FALSE]
  if (nrow(d) == 0L) return(invisible(FALSE))
  d$target_date <- as.Date(d$target_quarter_start)
  long <- rbind(
    data.frame(target_date = d$target_date, target_quarter = d$target_quarter, Series = "Actual", value = d$actual, stringsAsFactors = FALSE),
    data.frame(target_date = d$target_date, target_quarter = d$target_quarter, Series = "BSTS_NO_GT", value = d$point_forecast, stringsAsFactors = FALSE)
  )
  p <- ggplot2::ggplot(long, ggplot2::aes(x = target_date, y = value, colour = Series, linetype = Series, group = Series)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_colour_manual(values = c("Actual" = "black", "BSTS_NO_GT" = "#0072B2")) +
    ggplot2::scale_linetype_manual(values = c("Actual" = "solid", "BSTS_NO_GT" = "dashed")) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::labs(
      x = "Target quarter",
      y = "Unemployment rate (%)",
      colour = "Series",
      linetype = "Series"
    ) +
    plot_theme
  ggplot2::ggsave(path, p, width = 12, height = 7, dpi = 300)
  invisible(TRUE)
}

make_h0_error_plot <- function(pred_h0, path) {
  d <- pred_h0[pred_h0$status == "success", , drop = FALSE]
  if (nrow(d) == 0L) return(invisible(FALSE))
  d$target_date <- as.Date(d$target_quarter_start)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = target_date, y = error_actual_minus_forecast)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "black", linewidth = 0.8) +
    ggplot2::geom_line(colour = "#0072B2", linewidth = 1.1) +
    ggplot2::geom_point(colour = "#0072B2", size = 2.4) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::labs(
      x = "Target quarter",
      y = "Error: actual - forecast"
    ) +
    plot_theme +
    ggplot2::theme(legend.position = "none")
  ggplot2::ggsave(path, p, width = 12, height = 7, dpi = 300)
  invisible(TRUE)
}

make_h0_interval_plot <- function(pred_h0, path) {
  d <- pred_h0[pred_h0$status == "success", , drop = FALSE]
  if (nrow(d) == 0L) return(invisible(FALSE))
  d$target_date <- as.Date(d$target_quarter_start)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = target_date)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_95, ymax = upper_95), fill = "#56B4E9", alpha = 0.20) +
    ggplot2::geom_line(ggplot2::aes(y = point_forecast, colour = "BSTS_NO_GT"), linewidth = 1.1) +
    ggplot2::geom_point(ggplot2::aes(y = point_forecast, colour = "BSTS_NO_GT"), size = 2.1) +
    ggplot2::geom_line(ggplot2::aes(y = actual, colour = "Actual"), linewidth = 1.2) +
    ggplot2::geom_point(ggplot2::aes(y = actual, colour = "Actual"), size = 2.3) +
    ggplot2::scale_colour_manual(values = c("Actual" = "black", "BSTS_NO_GT" = "#0072B2")) +
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

make_post_gt_plot <- function(post_pred, path) {
  d <- post_pred[post_pred$status == "success", , drop = FALSE]
  if (nrow(d) == 0L) return(invisible(FALSE))
  d$target_quarter_factor <- factor(d$target_quarter, levels = d$target_quarter)
  long <- rbind(
    data.frame(target_quarter_factor = d$target_quarter_factor, Series = "Actual", value = d$actual, stringsAsFactors = FALSE),
    data.frame(target_quarter_factor = d$target_quarter_factor, Series = "BSTS_NO_GT", value = d$point_forecast, stringsAsFactors = FALSE)
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(x = target_quarter_factor)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_95, ymax = upper_95, group = 1), fill = "#56B4E9", alpha = 0.20) +
    ggplot2::geom_line(data = long, ggplot2::aes(y = value, colour = Series, linetype = Series, group = Series), linewidth = 1.1) +
    ggplot2::geom_point(data = long, ggplot2::aes(y = value, colour = Series, group = Series), size = 2.5) +
    ggplot2::scale_colour_manual(values = c("Actual" = "black", "BSTS_NO_GT" = "#0072B2")) +
    ggplot2::scale_linetype_manual(values = c("Actual" = "solid", "BSTS_NO_GT" = "dashed")) +
    ggplot2::labs(
      x = "Target quarter",
      y = "Unemployment rate (%)",
      colour = "Series",
      linetype = "Series"
    ) +
    plot_theme
  ggplot2::ggsave(path, p, width = 12, height = 7, dpi = 300)
  invisible(TRUE)
}

make_mae_horizon_plot <- function(metrics_by_horizon, path) {
  d <- metrics_by_horizon[is.finite(metrics_by_horizon$MAE), , drop = FALSE]
  if (nrow(d) == 0L) return(invisible(FALSE))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = horizon_label, y = MAE)) +
    ggplot2::geom_col(fill = "#0072B2") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", MAE)), vjust = -0.35, size = 4) +
    ggplot2::labs(
      x = "Validation design",
      y = "MAE (percentage points)"
    ) +
    plot_theme +
    ggplot2::theme(legend.position = "none")
  ggplot2::ggsave(path, p, width = 12, height = 7, dpi = 300)
  invisible(TRUE)
}

make_h0_actual_predicted_plot(pred_h0, FIG_H0_ACTUAL)
make_h0_error_plot(pred_h0, FIG_H0_ERRORS)
make_h0_interval_plot(pred_h0, FIG_H0_INTERVALS)
make_post_gt_plot(post_pred, FIG_POST)
make_mae_horizon_plot(metrics_by_horizon, FIG_MAE_HORIZON)

# -----------------------------
# 8. Config, report, manifest
# -----------------------------
config_lines <- c(
  "{",
  '  "script": "07_run_bsts_baseline.R",',
  '  "step": "Step 8 — BSTS baseline without Google Trends",',
  sprintf('  "generated_at": "%s",', format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  '  "inputs": {',
  sprintf('    "full_unemployment_panel": "%s",', json_escape(FULL_PANEL_PATH)),
  sprintf('    "h0_origins": "%s",', json_escape(H0_PATH)),
  sprintf('    "h1_origins": "%s",', json_escape(H1_PATH)),
  sprintf('    "h2_origins": "%s",', json_escape(H2_PATH)),
  sprintf('    "post_gt_block": "%s"', json_escape(POST_GT_PATH)),
  '  },',
  '  "model": {',
  '    "model_family": "BSTS_BASELINE",',
  '    "model": "BSTS_NO_GT",',
  sprintf('    "state_specification": "%s",', json_escape(STATE_SPEC)),
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
  sprintf('    "post_gt_forecasts": "%s",', basename(OUTPUT_POST)),
  sprintf('    "post_gt_metrics": "%s"', basename(OUTPUT_POST_METRICS)),
  '  }',
  "}"
)
write_lines_utf8(config_lines, OUTPUT_CONFIG)

metric_line <- function(metrics, horizon_label) {
  hit <- metrics[metrics$horizon_label == horizon_label, , drop = FALSE]
  if (nrow(hit) == 0L || !is.finite(hit$MAE[[1]])) return(sprintf("- `%s`: no successful predictions.", horizon_label))
  sprintf("- `%s`: MAE = %.3f, RMSE = %.3f, MAPE = %.2f%%, 95%% coverage = %.2f.",
          horizon_label, hit$MAE[[1]], hit$RMSE[[1]], hit$MAPE[[1]], hit$coverage_95[[1]])
}

post_metric_line <- if (nrow(post_metrics) > 0L && is.finite(post_metrics$MAE[[1]])) {
  sprintf("- Post-GT block: MAE = %.3f, RMSE = %.3f, MAPE = %.2f%%, 95%% coverage = %.2f.",
          post_metrics$MAE[[1]], post_metrics$RMSE[[1]], post_metrics$MAPE[[1]], post_metrics$coverage_95[[1]])
} else {
  "- Post-GT block: no successful predictions."
}

report_lines <- c(
  "# Step 8 — BSTS baseline without Google Trends",
  "",
  "## Status",
  "",
  "This step fits the unemployment-only Bayesian structural time-series baseline. No Google Trends predictors are used, and no Google Trends preprocessing is performed.",
  "",
  "## Model specification",
  "",
  sprintf("- Model label: `BSTS_NO_GT`."),
  sprintf("- State specification: `%s`.", STATE_SPEC),
  sprintf("- Quarterly seasonal frequency: `%d`.", SEASONAL_FREQUENCY),
  sprintf("- MCMC iterations: `%d`.", NITER),
  sprintf("- Burn fraction: `%.2f`.", BURN_FRACTION),
  sprintf("- Predictive interval level: `%.1f%%`.", INTERVAL_LEVEL),
  "",
  "## Validation designs",
  "",
  sprintf("- h = 0 rolling folds: `%d`.", nrow(pred_h0)),
  sprintf("- h = 1 rolling folds: `%d`.", nrow(pred_h1)),
  sprintf("- h = 2 rolling folds: `%d`.", nrow(pred_h2)),
  sprintf("- post-GT validation quarters: `%d`.", nrow(post_pred)),
  "",
  "## Main metrics",
  "",
  metric_line(metrics_by_horizon, "h0_same_quarter_nowcast"),
  metric_line(metrics_by_horizon, "h1_one_quarter_ahead_forecast"),
  metric_line(metrics_by_horizon, "h2_two_quarter_ahead_forecast_exploratory"),
  post_metric_line,
  "",
  "## Files written",
  "",
  "- `bsts_baseline_predictions_h0.csv`",
  "- `bsts_baseline_predictions_h1.csv`",
  "- `bsts_baseline_predictions_h2.csv`",
  "- `bsts_baseline_predictions_all.csv`",
  "- `bsts_baseline_metrics_by_horizon.csv`",
  "- `bsts_baseline_post_gt_forecasts.csv`",
  "- `bsts_baseline_post_gt_metrics.csv`",
  "- `07_run_bsts_baseline_config.json`",
  "- `07_run_bsts_baseline_manifest.csv`",
  "- `figures/bsts_baseline_actual_vs_predicted_h0.png`",
  "- `figures/bsts_baseline_rolling_errors_h0.png`",
  "- `figures/bsts_baseline_intervals_h0.png`",
  "- `figures/bsts_baseline_post_gt_forecasts.png`",
  "- `figures/bsts_baseline_mae_by_horizon.png`",
  "",
  "## Next step",
  "",
  "The next modelling step should add Google Trends predictors as separate MA and EMA BSTS variants, with all sparsity filtering and standardisation performed inside each rolling training window."
)
write_lines_utf8(report_lines, OUTPUT_REPORT)

output_files <- c(
  OUTPUT_H0,
  OUTPUT_H1,
  OUTPUT_H2,
  OUTPUT_ALL,
  OUTPUT_METRICS,
  OUTPUT_POST,
  OUTPUT_POST_METRICS,
  OUTPUT_CONFIG,
  OUTPUT_REPORT,
  FIG_H0_ACTUAL,
  FIG_H0_ERRORS,
  FIG_H0_INTERVALS,
  FIG_POST,
  FIG_MAE_HORIZON
)
output_files <- output_files[file.exists(output_files)]

manifest <- data.frame(
  output_key = seq_along(output_files),
  file_name = basename(output_files),
  relative_path = file.path("data", "processed", "bsts_baseline", basename(output_files)),
  file_size_bytes = file.info(output_files)$size,
  checksum_md5 = vapply(output_files, sha256_or_md5, character(1L)),
  stringsAsFactors = FALSE
)
write_csv_utf8(manifest, OUTPUT_MANIFEST)
write_csv_utf8(manifest, file.path(TABLE_DIR, basename(OUTPUT_MANIFEST)))

message("Step 8 BSTS baseline complete.")
message(sprintf("Outputs written to: %s", BSTS_DIR))
message(sprintf("h0 successful predictions: %d/%d", sum(pred_h0$status == "success"), nrow(pred_h0)))
message(sprintf("h1 successful predictions: %d/%d", sum(pred_h1$status == "success"), nrow(pred_h1)))
message(sprintf("h2 successful predictions: %d/%d", sum(pred_h2$status == "success"), nrow(pred_h2)))
message(sprintf("post-GT successful predictions: %d/%d", sum(post_pred$status == "success"), nrow(post_pred)))
