#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# 06_run_benchmarks.R
# Step 7: Run unemployment-only benchmark models
# -----------------------------------------------------------------------------
# Purpose:
#   Fit classical unemployment-only benchmark models under the rolling-origin
#   validation design defined in Step 6. These benchmarks provide the baseline
#   against which BSTS and Google Trends models will be compared later.
#
# Inputs:
#   data/processed/modeling_panel_unemployment_full.csv
#   data/processed/validation/rolling_origins_h0_nowcast.csv
#   data/processed/validation/rolling_origins_h1_forecast.csv
#   data/processed/validation/rolling_origins_h2_forecast_exploratory.csv
#   data/processed/validation/post_gt_forecast_validation_block.csv
#
# Outputs:
#   data/processed/benchmarks/benchmark_predictions_h0.csv
#   data/processed/benchmarks/benchmark_predictions_h1.csv
#   data/processed/benchmarks/benchmark_predictions_h2.csv
#   data/processed/benchmarks/benchmark_predictions_all.csv
#   data/processed/benchmarks/benchmark_metrics_by_model_horizon.csv
#   data/processed/benchmarks/benchmark_metrics_all_rolling.csv
#   data/processed/benchmarks/benchmark_post_gt_forecasts.csv
#   data/processed/benchmarks/benchmark_post_gt_metrics.csv
#   data/processed/benchmarks/benchmark_h0_model_ranking.csv
#   data/processed/benchmarks/06_run_benchmarks_config.json
#   data/processed/benchmarks/06_run_benchmarks_report.md
#   data/processed/benchmarks/06_run_benchmarks_manifest.csv
#   data/processed/benchmarks/figures/*.png
#
# Agreed pipeline rules:
#   - No Google Trends predictors are used in this script.
#   - No future unemployment values are used.
#   - No random train/test splitting is used.
#   - Each rolling-origin forecast is generated from unemployment observations
#     available at that origin only.
#   - The post-GT block is evaluated as unemployment-only forecasting from the
#     2024 Q3 endpoint.
#   - This script does not fit BSTS models; that comes in the next step.
#
# Usage from project root:
#   Rscript scripts/06_run_benchmarks.R
#
# Optional arguments:
#   Rscript scripts/06_run_benchmarks.R \
#     --processed-dir=data/processed \
#     --validation-dir=data/processed/validation \
#     --out-dir=data/processed/benchmarks
#
# Dependencies:
#   Base R only. No package installation required.
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

PROJECT_ROOT <- normalizePath(get_arg("project-root", Sys.getenv("BSTS_PROJECT_ROOT", unset = getwd())), mustWork = FALSE)
PROCESSED_DIR_DEFAULT <- file.path(PROJECT_ROOT, "data", "processed")
PROCESSED_DIR <- normalizePath(get_arg("processed-dir", Sys.getenv("BSTS_PROCESSED_DIR", unset = PROCESSED_DIR_DEFAULT)), mustWork = FALSE)
VALIDATION_DIR <- normalizePath(get_arg("validation-dir", file.path(PROCESSED_DIR, "validation")), mustWork = FALSE)
OUT_DIR <- normalizePath(get_arg("out-dir", file.path(PROCESSED_DIR, "benchmarks")), mustWork = FALSE)
FIGURE_DIR <- file.path(OUT_DIR, "figures")
TABLE_DIR <- file.path(OUT_DIR, "tables")

FULL_PANEL_FILE <- "modeling_panel_unemployment_full.csv"
H0_FILE <- "rolling_origins_h0_nowcast.csv"
H1_FILE <- "rolling_origins_h1_forecast.csv"
H2_FILE <- "rolling_origins_h2_forecast_exploratory.csv"
POST_GT_FILE <- "post_gt_forecast_validation_block.csv"

ALPHA <- 0.05
Z_95 <- qnorm(1 - ALPHA / 2)
SEASONAL_FREQUENCY <- 4L

CANDIDATE_DIRS <- unique(c(
  PROCESSED_DIR,
  VALIDATION_DIR,
  file.path(PROCESSED_DIR, "validation"),
  file.path(PROJECT_ROOT, "data", "processed"),
  file.path(PROJECT_ROOT, "data", "processed", "validation"),
  file.path(PROJECT_ROOT, "step6_validation"),
  PROJECT_ROOT,
  getwd(),
  "/mnt/data"
))

# -----------------------------
# 1. Helpers
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

json_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub('"', '\\"', x)
  x <- gsub("\n", "\\n", x)
  x
}

json_vec <- function(x) {
  paste0("[", paste(sprintf('"%s"', json_escape(x)), collapse = ", "), "]")
}

sha256_or_md5 <- function(path) {
  unname(tools::md5sum(path))
}

cell <- function(df, row, col, default = NA) {
  if (!col %in% names(df)) return(default)
  val <- df[[col]][[row]]
  if (length(val) == 0L || is.na(val)) return(default)
  val
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

safe_sigma <- function(resid) {
  resid <- safe_numeric(resid)
  resid <- resid[is.finite(resid)]
  if (length(resid) <= 1L) return(0.75)
  s <- stats::sd(resid)
  if (!is.finite(s) || s <= 0) return(0.75)
  as.numeric(s)
}

interval_95 <- function(point, sigma, horizon, scale = 1) {
  se <- sigma * sqrt(max(horizon, 1L)) * scale
  c(lower_95 = point - Z_95 * se, upper_95 = point + Z_95 * se)
}

interval_score <- function(actual, lower, upper, alpha = ALPHA) {
  score <- upper - lower
  if (actual < lower) score <- score + (2 / alpha) * (lower - actual)
  if (actual > upper) score <- score + (2 / alpha) * (actual - upper)
  score
}

# -----------------------------
# 2. Load inputs
# -----------------------------
ensure_dir(OUT_DIR)
ensure_dir(TABLE_DIR)
ensure_dir(FIGURE_DIR)

FULL_PANEL_PATH <- locate_file(FULL_PANEL_FILE, CANDIDATE_DIRS)
H0_PATH <- locate_file(H0_FILE, CANDIDATE_DIRS)
H1_PATH <- locate_file(H1_FILE, CANDIDATE_DIRS)
H2_PATH <- locate_file(H2_FILE, CANDIDATE_DIRS)
POST_GT_PATH <- locate_file(POST_GT_FILE, CANDIDATE_DIRS)

full_panel <- read_csv_utf8(FULL_PANEL_PATH)
rolling_h0 <- read_csv_utf8(H0_PATH)
rolling_h1 <- read_csv_utf8(H1_PATH)
rolling_h2 <- read_csv_utf8(H2_PATH)
post_gt <- read_csv_utf8(POST_GT_PATH)

required_full_cols <- c("Quarter", "Quarter_Index", "unemployment_rate_nationals")
missing_full <- setdiff(required_full_cols, names(full_panel))
if (length(missing_full) > 0L) stop(sprintf("Full panel missing required columns: %s", paste(missing_full, collapse = ", ")), call. = FALSE)

full_panel <- full_panel[order(full_panel$Quarter_Index), , drop = FALSE]
rownames(full_panel) <- NULL

if (anyDuplicated(full_panel$Quarter_Index)) stop("Duplicate Quarter_Index values in full unemployment panel.", call. = FALSE)
if (any(is.na(full_panel$unemployment_rate_nationals))) stop("Missing unemployment values in full unemployment panel.", call. = FALSE)

quarter_indices <- as.integer(full_panel$Quarter_Index)
y_values <- safe_numeric(full_panel$unemployment_rate_nationals)
names(y_values) <- as.character(quarter_indices)
quarter_labels <- as.character(full_panel$Quarter)
names(quarter_labels) <- as.character(quarter_indices)
quarter_starts <- if ("quarter_start" %in% names(full_panel)) as.character(full_panel$quarter_start) else rep("", nrow(full_panel))
names(quarter_starts) <- as.character(quarter_indices)

get_y <- function(start_idx, end_idx) {
  idx <- seq.int(as.integer(start_idx), as.integer(end_idx))
  y <- y_values[as.character(idx)]
  if (any(is.na(y))) stop(sprintf("Missing y value for requested training range %s-%s", start_idx, end_idx), call. = FALSE)
  as.numeric(y)
}

get_actual <- function(index) {
  val <- y_values[as.character(as.integer(index))]
  if (is.na(val)) stop(sprintf("Missing actual value for Quarter_Index %s", index), call. = FALSE)
  as.numeric(val)
}

q_label <- function(index) quarter_labels[as.character(as.integer(index))]
q_start <- function(index) quarter_starts[as.character(as.integer(index))]

# -----------------------------
# 3. Benchmark model functions
# -----------------------------
predict_rw <- function(y, horizon) {
  point <- tail(y, 1)
  sigma <- safe_sigma(diff(y))
  ints <- interval_95(point, sigma, horizon)
  data.frame(point = point, lower_95 = ints[["lower_95"]], upper_95 = ints[["upper_95"]], notes = "manual random walk; residual-based Gaussian interval")
}

predict_snaive <- function(y, horizon, season = SEASONAL_FREQUENCY) {
  n <- length(y)
  if (n < season) return(predict_rw(y, horizon))
  idx <- n - season + ((horizon - 1L) %% season) + 1L
  point <- y[[idx]]
  resid <- y[(season + 1L):n] - y[1L:(n - season)]
  sigma <- safe_sigma(resid)
  cycle <- floor((horizon - 1L) / season) + 1L
  lower <- point - Z_95 * sigma * sqrt(cycle)
  upper <- point + Z_95 * sigma * sqrt(cycle)
  data.frame(point = point, lower_95 = lower, upper_95 = upper, notes = "manual quarterly seasonal naive; seasonal residual interval")
}

predict_rw_drift <- function(y, horizon) {
  n <- length(y)
  if (n <= 1L) return(predict_rw(y, horizon))
  drift <- (tail(y, 1) - y[[1]]) / (n - 1L)
  point <- tail(y, 1) + horizon * drift
  fitted_one_step <- y[1L:(n - 1L)] + drift
  resid <- y[2L:n] - fitted_one_step
  sigma <- safe_sigma(resid)
  ints <- interval_95(point, sigma, horizon, scale = sqrt(1 + horizon / n))
  data.frame(point = point, lower_95 = ints[["lower_95"]], upper_95 = ints[["upper_95"]], notes = "manual random walk with drift; residual-based Gaussian interval")
}

ols_fit <- function(X, y) {
  fit <- tryCatch(stats::lm.fit(x = X, y = y), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  fitted <- as.numeric(X %*% fit$coefficients)
  resid <- y - fitted
  rss <- sum(resid^2, na.rm = TRUE)
  n <- length(y)
  k <- ncol(X)
  aic <- n * log(max(rss / max(n, 1L), 1e-12)) + 2 * k
  list(beta = fit$coefficients, fitted = fitted, resid = resid, rss = rss, aic = aic)
}

predict_arima_restricted <- function(y, horizon) {
  # Restricted ARIMA benchmark. The script first tries base R stats::arima
  # over a small candidate family. If a model fails, it falls back to OLS
  # approximations used for traceability in the generated verification outputs.
  candidates <- list()
  orders <- list(c(0, 1, 0), c(1, 1, 0), c(0, 1, 1), c(1, 1, 1), c(1, 0, 0), c(2, 0, 0), c(0, 0, 1), c(1, 0, 1))

  for (ord in orders) {
    fit <- tryCatch(
      stats::arima(y, order = ord, include.mean = ord[[2]] == 0, method = "ML"),
      error = function(e) NULL,
      warning = function(w) suppressWarnings(tryCatch(stats::arima(y, order = ord, include.mean = ord[[2]] == 0, method = "ML"), error = function(e) NULL))
    )
    if (!is.null(fit) && is.finite(fit$aic)) {
      pred <- tryCatch(stats::predict(fit, n.ahead = horizon), error = function(e) NULL)
      if (!is.null(pred)) {
        point <- as.numeric(tail(pred$pred, 1))
        se <- as.numeric(tail(pred$se, 1))
        if (is.finite(point) && is.finite(se) && se > 0) {
          candidates[[length(candidates) + 1L]] <- list(
            aic = as.numeric(fit$aic),
            point = point,
            lower = point - Z_95 * se,
            upper = point + Z_95 * se,
            name = sprintf("ARIMA(%d,%d,%d)", ord[[1]], ord[[2]], ord[[3]])
          )
        }
      }
    }
  }

  if (length(candidates) > 0L) {
    best <- candidates[[which.min(vapply(candidates, function(x) x$aic, numeric(1L)))]]
    return(data.frame(point = best$point, lower_95 = best$lower, upper_95 = best$upper, notes = sprintf("restricted base-R ARIMA selected %s by AIC", best$name)))
  }

  # Fallback if all stats::arima candidates fail.
  predict_rw_drift(y, horizon)
}

ses_forecast <- function(y, horizon, alpha) {
  level <- y[[1]]
  fitted <- numeric(0L)
  if (length(y) > 1L) {
    for (t in 2L:length(y)) {
      fitted <- c(fitted, level)
      level <- alpha * y[[t]] + (1 - alpha) * level
    }
  }
  list(point = as.numeric(level), resid = y[-1L] - fitted)
}

holt_forecast <- function(y, horizon, alpha, beta) {
  level <- y[[1]]
  trend <- if (length(y) > 1L) y[[2]] - y[[1]] else 0
  fitted <- numeric(0L)
  if (length(y) > 1L) {
    for (t in 2L:length(y)) {
      fitted <- c(fitted, level + trend)
      old_level <- level
      level <- alpha * y[[t]] + (1 - alpha) * (level + trend)
      trend <- beta * (level - old_level) + (1 - beta) * trend
    }
  }
  list(point = as.numeric(level + horizon * trend), resid = y[-1L] - fitted)
}

predict_ets_restricted <- function(y, horizon) {
  # Try base R HoltWinters first for standard ETS-style benchmarks.
  y_ts <- stats::ts(y, frequency = SEASONAL_FREQUENCY)
  candidates <- list()

  hw_specs <- list(
    list(name = "SES", beta = FALSE, gamma = FALSE),
    list(name = "Holt", beta = TRUE, gamma = FALSE),
    list(name = "Holt-Winters additive", beta = TRUE, gamma = TRUE)
  )

  for (sp in hw_specs) {
    fit <- tryCatch(stats::HoltWinters(y_ts, beta = sp$beta, gamma = sp$gamma, seasonal = "additive"), error = function(e) NULL)
    if (!is.null(fit) && is.finite(fit$SSE)) {
      pred <- tryCatch(stats::predict(fit, n.ahead = horizon, prediction.interval = TRUE, level = 0.95), error = function(e) NULL)
      if (!is.null(pred)) {
        pred_matrix <- as.matrix(pred)
        point <- as.numeric(tail(pred_matrix[, "fit"], 1))
        lower <- as.numeric(tail(pred_matrix[, "lwr"], 1))
        upper <- as.numeric(tail(pred_matrix[, "upr"], 1))
        if (all(is.finite(c(point, lower, upper)))) {
          candidates[[length(candidates) + 1L]] <- list(score = fit$SSE, point = point, lower = lower, upper = upper, name = sp$name)
        }
      }
    }
  }

  if (length(candidates) > 0L) {
    best <- candidates[[which.min(vapply(candidates, function(x) x$score, numeric(1L)))]]
    return(data.frame(point = best$point, lower_95 = best$lower, upper_95 = best$upper, notes = sprintf("base-R HoltWinters ETS-style benchmark selected %s by SSE", best$name)))
  }

  # Fallback deterministic ETS grid.
  best <- NULL
  for (alpha in seq(0.1, 0.9, by = 0.1)) {
    fc <- ses_forecast(y, horizon, alpha)
    rss <- sum(fc$resid^2, na.rm = TRUE)
    n <- max(length(fc$resid), 1L)
    aic <- n * log(max(rss / n, 1e-12)) + 2
    if (is.null(best) || aic < best$aic) best <- list(aic = aic, point = fc$point, resid = fc$resid, name = sprintf("SES alpha=%.1f", alpha))
  }
  for (alpha in c(0.2, 0.4, 0.6, 0.8)) {
    for (beta in c(0.2, 0.4, 0.6, 0.8)) {
      fc <- holt_forecast(y, horizon, alpha, beta)
      rss <- sum(fc$resid^2, na.rm = TRUE)
      n <- max(length(fc$resid), 1L)
      aic <- n * log(max(rss / n, 1e-12)) + 4
      if (aic < best$aic) best <- list(aic = aic, point = fc$point, resid = fc$resid, name = sprintf("Holt alpha=%.1f beta=%.1f", alpha, beta))
    }
  }

  sigma <- safe_sigma(best$resid)
  ints <- interval_95(best$point, sigma, horizon)
  data.frame(point = best$point, lower_95 = ints[["lower_95"]], upper_95 = ints[["upper_95"]], notes = sprintf("restricted ETS fallback selected %s by AIC", best$name))
}

model_labels <- c("RW", "SNAIVE", "RW_DRIFT", "ARIMA", "ETS")
model_families <- c("Random walk / naive", "Quarterly seasonal naive", "Random walk with drift", "Restricted ARIMA benchmark", "Restricted ETS benchmark")

run_model <- function(label, y, horizon) {
  if (label == "RW") return(predict_rw(y, horizon))
  if (label == "SNAIVE") return(predict_snaive(y, horizon))
  if (label == "RW_DRIFT") return(predict_rw_drift(y, horizon))
  if (label == "ARIMA") return(predict_arima_restricted(y, horizon))
  if (label == "ETS") return(predict_ets_restricted(y, horizon))
  stop(sprintf("Unsupported model: %s", label), call. = FALSE)
}

# -----------------------------
# 4. Rolling predictions
# -----------------------------
make_predictions <- function(plan_df, evaluation_set) {
  rows <- list()
  counter <- 0L

  for (i in seq_len(nrow(plan_df))) {
    start_idx <- as.integer(cell(plan_df, i, "train_y_start_index"))
    end_idx <- as.integer(cell(plan_df, i, "train_y_end_index"))
    target_idx <- as.integer(cell(plan_df, i, "target_index"))
    forecast_horizon <- target_idx - end_idx
    y <- get_y(start_idx, end_idx)
    actual <- get_actual(target_idx)

    for (m in seq_along(model_labels)) {
      label <- model_labels[[m]]
      pred <- run_model(label, y, forecast_horizon)
      point <- as.numeric(pred$point[[1]])
      lower <- as.numeric(pred$lower_95[[1]])
      upper <- as.numeric(pred$upper_95[[1]])
      err <- actual - point
      abs_err <- abs(err)
      ape <- ifelse(actual == 0, NA_real_, 100 * abs_err / abs(actual))
      smape <- ifelse(abs(actual) + abs(point) == 0, NA_real_, 100 * abs_err / ((abs(actual) + abs(point)) / 2))
      counter <- counter + 1L
      rows[[counter]] <- data.frame(
        evaluation_set = evaluation_set,
        experiment_role = as.character(cell(plan_df, i, "experiment_role", "")),
        design_label = as.character(cell(plan_df, i, "design_label", "")),
        horizon_quarters_design = as.integer(cell(plan_df, i, "horizon_quarters", NA)),
        forecast_horizon_from_origin = forecast_horizon,
        fold_id = as.integer(cell(plan_df, i, "fold_id", NA)),
        origin_id = as.character(cell(plan_df, i, "origin_id", "")),
        model_label = label,
        model_family = model_families[[m]],
        target_index = target_idx,
        target_quarter = as.character(cell(plan_df, i, "target_quarter", q_label(target_idx))),
        target_quarter_start = as.character(cell(plan_df, i, "target_quarter_start", q_start(target_idx))),
        official_origin_index = as.integer(cell(plan_df, i, "official_origin_index", end_idx)),
        official_origin_quarter = as.character(cell(plan_df, i, "official_origin_quarter", q_label(end_idx))),
        train_y_start_index = start_idx,
        train_y_start_quarter = as.character(cell(plan_df, i, "train_y_start_quarter", q_label(start_idx))),
        train_y_end_index = end_idx,
        train_y_end_quarter = as.character(cell(plan_df, i, "train_y_end_quarter", q_label(end_idx))),
        n_train = length(y),
        actual = actual,
        point_forecast = point,
        lower_95 = lower,
        upper_95 = upper,
        error_actual_minus_forecast = err,
        abs_error = abs_err,
        squared_error = err^2,
        ape = ape,
        smape = smape,
        in_95_interval = actual >= lower && actual <= upper,
        interval_width_95 = upper - lower,
        interval_score_95 = interval_score(actual, lower, upper),
        uses_google_trends = FALSE,
        notes = as.character(pred$notes[[1]])
      )
    }
  }

  do.call(rbind, rows)
}

benchmark_h0 <- make_predictions(rolling_h0, "rolling_h0_nowcast_benchmark")
benchmark_h1 <- make_predictions(rolling_h1, "rolling_h1_forecast_benchmark")
benchmark_h2 <- make_predictions(rolling_h2, "rolling_h2_forecast_exploratory_benchmark")
benchmark_all <- rbind(benchmark_h0, benchmark_h1, benchmark_h2)

# -----------------------------
# 5. Post-GT predictions
# -----------------------------
post_rows <- list()
counter <- 0L
for (i in seq_len(nrow(post_gt))) {
  start_idx <- as.integer(cell(post_gt, i, "train_y_start_index"))
  end_idx <- as.integer(cell(post_gt, i, "train_y_end_index"))
  target_idx <- as.integer(cell(post_gt, i, "target_index"))
  forecast_horizon <- target_idx - end_idx
  y <- get_y(start_idx, end_idx)
  actual <- get_actual(target_idx)

  for (m in seq_along(model_labels)) {
    label <- model_labels[[m]]
    pred <- run_model(label, y, forecast_horizon)
    point <- as.numeric(pred$point[[1]])
    lower <- as.numeric(pred$lower_95[[1]])
    upper <- as.numeric(pred$upper_95[[1]])
    err <- actual - point
    abs_err <- abs(err)
    ape <- ifelse(actual == 0, NA_real_, 100 * abs_err / abs(actual))
    smape <- ifelse(abs(actual) + abs(point) == 0, NA_real_, 100 * abs_err / ((abs(actual) + abs(point)) / 2))
    counter <- counter + 1L
    post_rows[[counter]] <- data.frame(
      evaluation_set = "post_gt_forecast_validation",
      experiment_role = as.character(cell(post_gt, i, "experiment_role", "post_gt_forecast_validation")),
      block_id = as.character(cell(post_gt, i, "block_id", "")),
      model_label = label,
      model_family = model_families[[m]],
      origin_index = as.integer(cell(post_gt, i, "origin_index", end_idx)),
      origin_quarter = as.character(cell(post_gt, i, "origin_quarter", q_label(end_idx))),
      target_index = target_idx,
      target_quarter = as.character(cell(post_gt, i, "target_quarter", q_label(target_idx))),
      target_quarter_start = as.character(cell(post_gt, i, "target_quarter_start", q_start(target_idx))),
      forecast_horizon_from_origin = forecast_horizon,
      horizon_from_gt_endpoint = as.integer(cell(post_gt, i, "horizon_from_gt_endpoint", forecast_horizon)),
      train_y_start_index = start_idx,
      train_y_start_quarter = as.character(cell(post_gt, i, "train_y_start_quarter", q_label(start_idx))),
      train_y_end_index = end_idx,
      train_y_end_quarter = as.character(cell(post_gt, i, "train_y_end_quarter", q_label(end_idx))),
      n_train = length(y),
      actual = actual,
      point_forecast = point,
      lower_95 = lower,
      upper_95 = upper,
      error_actual_minus_forecast = err,
      abs_error = abs_err,
      squared_error = err^2,
      ape = ape,
      smape = smape,
      in_95_interval = actual >= lower && actual <= upper,
      interval_width_95 = upper - lower,
      interval_score_95 = interval_score(actual, lower, upper),
      uses_google_trends = FALSE,
      same_quarter_gt_nowcast_allowed = FALSE,
      future_gt_placeholders_allowed = FALSE,
      notes = as.character(pred$notes[[1]])
    )
  }
}
benchmark_post_gt <- do.call(rbind, post_rows)

# -----------------------------
# 6. Metrics
# -----------------------------
make_metrics <- function(df, group_cols) {
  key <- do.call(paste, c(df[group_cols], sep = "\r"))
  splits <- split(seq_len(nrow(df)), key)
  rows <- lapply(splits, function(idx) {
    g <- df[idx, , drop = FALSE]
    base <- g[1L, group_cols, drop = FALSE]
    data.frame(
      base,
      n_predictions = nrow(g),
      MAE = mean(g$abs_error, na.rm = TRUE),
      RMSE = sqrt(mean(g$squared_error, na.rm = TRUE)),
      MAPE = mean(g$ape, na.rm = TRUE),
      sMAPE = mean(g$smape, na.rm = TRUE),
      mean_error = mean(g$error_actual_minus_forecast, na.rm = TRUE),
      median_abs_error = stats::median(g$abs_error, na.rm = TRUE),
      coverage_95 = mean(as.numeric(g$in_95_interval), na.rm = TRUE),
      average_interval_width_95 = mean(g$interval_width_95, na.rm = TRUE),
      mean_interval_score_95 = mean(g$interval_score_95, na.rm = TRUE),
      min_target_quarter = g$target_quarter[which.min(g$target_index)],
      max_target_quarter = g$target_quarter[which.max(g$target_index)],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out <- out[order(out[[group_cols[[1]]]], out$MAE), , drop = FALSE]
  out
}

metrics_by_model_horizon <- make_metrics(
  benchmark_all,
  c("evaluation_set", "design_label", "horizon_quarters_design", "forecast_horizon_from_origin", "model_label", "model_family")
)
metrics_all_rolling <- make_metrics(benchmark_all, c("model_label", "model_family"))
post_gt_metrics <- make_metrics(benchmark_post_gt, c("evaluation_set", "model_label", "model_family"))
h0_ranking <- metrics_by_model_horizon[metrics_by_model_horizon$evaluation_set == "rolling_h0_nowcast_benchmark", , drop = FALSE]
h0_ranking <- h0_ranking[order(h0_ranking$MAE), , drop = FALSE]
h0_ranking$MAE_rank <- seq_len(nrow(h0_ranking))

# -----------------------------
# 7. Write CSV outputs
# -----------------------------
write_csv_utf8(benchmark_h0, file.path(OUT_DIR, "benchmark_predictions_h0.csv"))
write_csv_utf8(benchmark_h1, file.path(OUT_DIR, "benchmark_predictions_h1.csv"))
write_csv_utf8(benchmark_h2, file.path(OUT_DIR, "benchmark_predictions_h2.csv"))
write_csv_utf8(benchmark_all, file.path(OUT_DIR, "benchmark_predictions_all.csv"))
write_csv_utf8(metrics_by_model_horizon, file.path(OUT_DIR, "benchmark_metrics_by_model_horizon.csv"))
write_csv_utf8(metrics_all_rolling, file.path(OUT_DIR, "benchmark_metrics_all_rolling.csv"))
write_csv_utf8(benchmark_post_gt, file.path(OUT_DIR, "benchmark_post_gt_forecasts.csv"))
write_csv_utf8(post_gt_metrics, file.path(OUT_DIR, "benchmark_post_gt_metrics.csv"))
write_csv_utf8(h0_ranking, file.path(OUT_DIR, "benchmark_h0_model_ranking.csv"))

# Optional duplicate tables subfolder for convenience.
for (f in c(
  "benchmark_predictions_h0.csv", "benchmark_predictions_h1.csv", "benchmark_predictions_h2.csv", "benchmark_predictions_all.csv",
  "benchmark_metrics_by_model_horizon.csv", "benchmark_metrics_all_rolling.csv", "benchmark_post_gt_forecasts.csv", "benchmark_post_gt_metrics.csv", "benchmark_h0_model_ranking.csv"
)) {
  file.copy(file.path(OUT_DIR, f), file.path(TABLE_DIR, f), overwrite = TRUE)
}

# -----------------------------
# 8. Figures
# -----------------------------
library(ggplot2)

# Colorblind-safe palette (Okabe-Ito)
okabe_ito_palette <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00", # vermillion
  "#CC79A7", # reddish purple
  "#999999"  # grey
)

plot_actual_vs_predicted <- function(pred_df, path, title) {
  pred_df <- pred_df[order(pred_df$target_index), , drop = FALSE]
  
  quarters <- unique(pred_df$target_quarter)
  
  actual_df <- pred_df[!duplicated(pred_df$target_index), , drop = FALSE]
  actual_df <- actual_df[order(actual_df$target_index), , drop = FALSE]
  
  model_labs <- unique(as.character(pred_df$model_label))
  
  pred_df$target_quarter_plot <- factor(pred_df$target_quarter, levels = quarters)
  actual_df$target_quarter_plot <- factor(actual_df$target_quarter, levels = quarters)
  
  pred_df$model_label <- factor(as.character(pred_df$model_label), levels = model_labs)
  
  # Build named palette: Actual black, models from Okabe-Ito
  model_cols <- rep(okabe_ito_palette, length.out = length(model_labs))
  names(model_cols) <- model_labs
  colour_values <- c("Actual" = "black", model_cols)
  
  y_range <- range(c(pred_df$point_forecast, actual_df$actual), na.rm = TRUE)
  
  p <- ggplot() +
    geom_line(
      data = pred_df,
      aes(
        x = target_quarter_plot,
        y = point_forecast,
        colour = model_label,
        group = model_label
      ),
      linewidth = 0.8
    ) +
    geom_point(
      data = pred_df,
      aes(
        x = target_quarter_plot,
        y = point_forecast,
        colour = model_label
      ),
      size = 1.8
    ) +
    geom_line(
      data = actual_df,
      aes(
        x = target_quarter_plot,
        y = actual,
        colour = "Actual",
        group = 1
      ),
      linewidth = 1.5
    ) +
    geom_point(
      data = actual_df,
      aes(
        x = target_quarter_plot,
        y = actual,
        colour = "Actual"
      ),
      size = 2.5
    ) +
    scale_color_manual(
      values = colour_values,
      breaks = c("Actual", model_labs),
      name = "Series"
    ) +
    coord_cartesian(ylim = y_range) +
    labs(
      x = "Target quarter",
      y = "Unemployment rate (%)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "bottom"
    ) +
    guides(colour = guide_legend(ncol = 6))
  
  ggsave(
    filename = path,
    plot = p,
    device = "png",
    width = 1800 / 300,
    height = 900 / 300,
    dpi = 300,
    bg = "white"
  )
  
  invisible(p)
}


plot_errors <- function(pred_df, path, title) {
  pred_df <- pred_df[order(pred_df$target_index), , drop = FALSE]
  
  quarters <- unique(pred_df$target_quarter)
  model_labs <- unique(as.character(pred_df$model_label))
  
  pred_df$target_quarter_plot <- factor(pred_df$target_quarter, levels = quarters)
  pred_df$model_label <- factor(as.character(pred_df$model_label), levels = model_labs)
  
  model_cols <- rep(okabe_ito_palette, length.out = length(model_labs))
  names(model_cols) <- model_labs
  
  y_range <- range(c(0, pred_df$error_actual_minus_forecast), na.rm = TRUE)
  
  p <- ggplot(pred_df) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.6,
      colour = "black"
    ) +
    geom_line(
      aes(
        x = target_quarter_plot,
        y = error_actual_minus_forecast,
        colour = model_label,
        group = model_label
      ),
      linewidth = 0.8
    ) +
    geom_point(
      aes(
        x = target_quarter_plot,
        y = error_actual_minus_forecast,
        colour = model_label
      ),
      size = 1.8
    ) +
    scale_color_manual(
      values = model_cols,
      name = "Model"
    ) +
    coord_cartesian(ylim = y_range) +
    labs(
      x = "Target quarter",
      y = "Error: actual - forecast"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "bottom"
    ) +
    guides(colour = guide_legend(ncol = 6))
  
  ggsave(
    filename = path,
    plot = p,
    device = "png",
    width = 1800 / 300,
    height = 900 / 300,
    dpi = 300,
    bg = "white"
  )
  
  invisible(p)
}


plot_mae <- function(ranking_df, path) {
  ranking_df <- ranking_df[order(ranking_df$MAE), , drop = FALSE]
  
  ranking_df$model_label_plot <- factor(
    ranking_df$model_label,
    levels = ranking_df$model_label
  )
  
  ymax <- max(ranking_df$MAE, na.rm = TRUE)
  
  p <- ggplot(ranking_df, aes(x = model_label_plot, y = MAE)) +
    geom_col(fill = "#0072B2") +
    geom_text(
      aes(label = sprintf("%.3f", MAE)),
      vjust = -0.4,
      size = 3.5
    ) +
    coord_cartesian(ylim = c(0, ymax * 1.12)) +
    labs(
      x = "Model",
      y = "MAE (percentage points)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  
  ggsave(
    filename = path,
    plot = p,
    device = "png",
    width = 1800 / 300,
    height = 900 / 300,
    dpi = 300,
    bg = "white"
  )
  
  invisible(p)
}

plot_actual_vs_predicted(benchmark_h0, file.path(FIGURE_DIR, "benchmark_actual_vs_predicted_h0.png"), "Benchmark h = 0 rolling nowcasts: actual vs predicted")
plot_errors(benchmark_h0, file.path(FIGURE_DIR, "benchmark_rolling_errors_h0.png"), "Benchmark h = 0 rolling forecast errors")
plot_actual_vs_predicted(benchmark_post_gt, file.path(FIGURE_DIR, "benchmark_post_gt_forecasts.png"), "Post-GT unemployment-only benchmark forecasts")
plot_mae(h0_ranking, file.path(FIGURE_DIR, "benchmark_mae_h0_by_model.png"))

# -----------------------------
# 9. Config, report, manifest
# -----------------------------
config_lines <- c(
  "{",
  '  "script": "06_run_benchmarks.R",',
  '  "step": "Step 7 — Run unemployment-only benchmark models",',
  sprintf('  "generated_at": "%s",', format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  '  "input_files": {',
  sprintf('    "modeling_panel_unemployment_full": "%s",', json_escape(FULL_PANEL_PATH)),
  sprintf('    "rolling_origins_h0": "%s",', json_escape(H0_PATH)),
  sprintf('    "rolling_origins_h1": "%s",', json_escape(H1_PATH)),
  sprintf('    "rolling_origins_h2": "%s",', json_escape(H2_PATH)),
  sprintf('    "post_gt_validation_block": "%s"', json_escape(POST_GT_PATH)),
  '  },',
  '  "models": [',
  '    {"model_label": "RW", "model_family": "Random walk / naive", "uses_google_trends": false},',
  '    {"model_label": "SNAIVE", "model_family": "Quarterly seasonal naive", "uses_google_trends": false},',
  '    {"model_label": "RW_DRIFT", "model_family": "Random walk with drift", "uses_google_trends": false},',
  '    {"model_label": "ARIMA", "model_family": "Restricted ARIMA benchmark", "uses_google_trends": false},',
  '    {"model_label": "ETS", "model_family": "Restricted ETS benchmark", "uses_google_trends": false}',
  '  ],',
  '  "validation_sets": {',
  sprintf('    "h0_folds": %d,', nrow(rolling_h0)),
  sprintf('    "h1_folds": %d,', nrow(rolling_h1)),
  sprintf('    "h2_folds": %d,', nrow(rolling_h2)),
  sprintf('    "post_gt_quarters": %d', nrow(post_gt)),
  '  },',
  '  "metrics": ["MAE", "RMSE", "MAPE", "sMAPE", "mean_error", "median_abs_error", "95% interval coverage", "average interval width", "interval score"],',
  '  "notes": "Benchmark models use only historical unemployment rates. No Google Trends predictors or future Google Trends placeholders are used."',
  "}"
)
write_lines_utf8(config_lines, file.path(OUT_DIR, "06_run_benchmarks_config.json"))

best_h0 <- h0_ranking[order(h0_ranking$MAE), , drop = FALSE][1L, ]
best_post <- post_gt_metrics[order(post_gt_metrics$MAE), , drop = FALSE][1L, ]

report_lines <- c(
  "# Step 7 — Benchmark model report",
  "",
  "## Status",
  "",
  "Step 7 fits unemployment-only benchmark models under the rolling-origin validation design from Step 6. These are baseline models; they do not use Google Trends predictors.",
  "",
  "## Inputs",
  "",
  "| Input | Rows | Role |",
  "|---|---:|---|",
  sprintf("| `modeling_panel_unemployment_full.csv` | %d | Full unemployment outcome series. |", nrow(full_panel)),
  sprintf("| `rolling_origins_h0_nowcast.csv` | %d | Main same-quarter nowcast benchmark design. |", nrow(rolling_h0)),
  sprintf("| `rolling_origins_h1_forecast.csv` | %d | Secondary one-quarter-ahead benchmark design. |", nrow(rolling_h1)),
  sprintf("| `rolling_origins_h2_forecast_exploratory.csv` | %d | Exploratory two-quarter-ahead benchmark design. |", nrow(rolling_h2)),
  sprintf("| `post_gt_forecast_validation_block.csv` | %d | Post-GT unemployment-only validation block. |", nrow(post_gt)),
  "",
  "## Models",
  "",
  "| Model label | Description | Uses Google Trends? |",
  "|---|---|---:|",
  "| `RW` | Random walk / naive | No |",
  "| `SNAIVE` | Quarterly seasonal naive | No |",
  "| `RW_DRIFT` | Random walk with drift | No |",
  "| `ARIMA` | Restricted ARIMA benchmark | No |",
  "| `ETS` | Restricted ETS benchmark | No |",
  "",
  "## h = 0 benchmark ranking",
  "",
  "| Rank | Model | MAE | RMSE | MAPE | 95% coverage |",
  "|---:|---|---:|---:|---:|---:|"
)
for (i in seq_len(nrow(h0_ranking))) {
  row <- h0_ranking[i, ]
  report_lines <- c(report_lines, sprintf("| %d | `%s` | %.3f | %.3f | %.3f | %.3f |", i, row$model_label, row$MAE, row$RMSE, row$MAPE, row$coverage_95))
}
report_lines <- c(
  report_lines,
  "",
  sprintf("Best h = 0 benchmark by MAE: `%s` with MAE %.3f percentage points.", best_h0$model_label, best_h0$MAE),
  "",
  "## Post-GT validation",
  "",
  "| Model | MAE | RMSE | MAPE | 95% coverage |",
  "|---|---:|---:|---:|---:|"
)
post_sorted <- post_gt_metrics[order(post_gt_metrics$MAE), , drop = FALSE]
for (i in seq_len(nrow(post_sorted))) {
  row <- post_sorted[i, ]
  report_lines <- c(report_lines, sprintf("| `%s` | %.3f | %.3f | %.3f | %.3f |", row$model_label, row$MAE, row$RMSE, row$MAPE, row$coverage_95))
}
report_lines <- c(
  report_lines,
  "",
  sprintf("Best post-GT unemployment-only benchmark by MAE: `%s` with MAE %.3f percentage points.", best_post$model_label, best_post$MAE),
  "",
  "## Safeguards",
  "",
  "- No Google Trends predictors are used in Step 7.",
  "- Forecasts use only unemployment observations available at each origin.",
  "- The post-GT block is evaluated as unemployment-only forecasting from the 2024 Q3 endpoint.",
  "- These results are benchmarks for later BSTS and Google Trends comparisons.",
  "",
  "## Next step",
  "",
  "The next pipeline step should fit the BSTS baseline without Google Trends:",
  "",
  "```text",
  "07_run_bsts_baseline.R",
  "```"
)
write_lines_utf8(report_lines, file.path(OUT_DIR, "06_run_benchmarks_report.md"))

# Manifest.
output_files <- c(
  file.path(OUT_DIR, "benchmark_predictions_h0.csv"),
  file.path(OUT_DIR, "benchmark_predictions_h1.csv"),
  file.path(OUT_DIR, "benchmark_predictions_h2.csv"),
  file.path(OUT_DIR, "benchmark_predictions_all.csv"),
  file.path(OUT_DIR, "benchmark_metrics_by_model_horizon.csv"),
  file.path(OUT_DIR, "benchmark_metrics_all_rolling.csv"),
  file.path(OUT_DIR, "benchmark_post_gt_forecasts.csv"),
  file.path(OUT_DIR, "benchmark_post_gt_metrics.csv"),
  file.path(OUT_DIR, "benchmark_h0_model_ranking.csv"),
  file.path(OUT_DIR, "06_run_benchmarks_config.json"),
  file.path(OUT_DIR, "06_run_benchmarks_report.md"),
  file.path(FIGURE_DIR, "benchmark_actual_vs_predicted_h0.png"),
  file.path(FIGURE_DIR, "benchmark_rolling_errors_h0.png"),
  file.path(FIGURE_DIR, "benchmark_post_gt_forecasts.png"),
  file.path(FIGURE_DIR, "benchmark_mae_h0_by_model.png")
)
manifest <- data.frame(
  file_name = basename(output_files),
  relative_path = c(
    basename(output_files[1:11]),
    file.path("figures", basename(output_files[12:15]))
  ),
  checksum_md5 = vapply(output_files, sha256_or_md5, character(1L)),
  stringsAsFactors = FALSE
)
write_csv_utf8(manifest, file.path(OUT_DIR, "06_run_benchmarks_manifest.csv"))

message("Step 7 benchmark modelling complete.")
message(sprintf("Benchmark outputs written to: %s", OUT_DIR))
message(sprintf("h0 rows: %d", nrow(benchmark_h0)))
message(sprintf("h1 rows: %d", nrow(benchmark_h1)))
message(sprintf("h2 rows: %d", nrow(benchmark_h2)))
message(sprintf("post-GT rows: %d", nrow(benchmark_post_gt)))
