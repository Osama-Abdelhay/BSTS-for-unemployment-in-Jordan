#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# 09_evaluate_models.R
# Step 10: Consolidated model evaluation and final comparison tables
# -----------------------------------------------------------------------------
# Purpose:
#   Merge the results from the benchmark, no-GT BSTS, and Google-Trends BSTS
#   stages into a single evidence layer. This script does not refit any model.
#   It reads saved prediction/metric CSV files, computes final rankings,
#   relative performance, interval diagnostics, period-specific errors, and
#   publication-style figures.
#
# Inputs expected from previous steps:
#   Step 7:
#     data/processed/benchmarks/benchmark_metrics_by_model_horizon.csv
#     data/processed/benchmarks/benchmark_h0_model_ranking.csv
#     data/processed/benchmarks/benchmark_predictions_all.csv
#     data/processed/benchmarks/benchmark_post_gt_metrics.csv       optional
#
#   Step 8B:
#     data/processed/bsts_state_specs/bsts_state_spec_metrics_by_horizon.csv
#     data/processed/bsts_state_specs/bsts_state_spec_h0_ranking.csv
#     data/processed/bsts_state_specs/bsts_state_spec_predictions_all.csv
#     data/processed/bsts_state_specs/bsts_state_spec_post_gt_metrics.csv optional
#
#   Step 9:
#     data/processed/bsts_gt/bsts_gt_metrics_by_model_horizon_threshold.csv
#     data/processed/bsts_gt/bsts_gt_h0_ranking.csv
#     data/processed/bsts_gt/bsts_gt_predictions_all.csv
#     data/processed/bsts_gt/bsts_gt_predictor_inclusion_summary.csv optional
#     data/processed/bsts_gt/bsts_gt_selected_predictors_by_fold.csv optional
#
# Outputs:
#   data/processed/final_evaluation/final_model_comparison_h0.csv
#   data/processed/final_evaluation/final_model_comparison_all_horizons.csv
#   data/processed/final_evaluation/relative_performance_vs_RW.csv
#   data/processed/final_evaluation/relative_performance_vs_BSTS_NO_GT_LL.csv
#   data/processed/final_evaluation/interval_quality_comparison.csv
#   data/processed/final_evaluation/period_specific_errors.csv
#   data/processed/final_evaluation/final_selected_models_h0_predictions.csv
#   data/processed/final_evaluation/final_gt_threshold_sensitivity.csv
#   data/processed/final_evaluation/final_predictor_inclusion_top_keywords.csv
#   data/processed/final_evaluation/post_gt_unemployment_only_comparison.csv
#   data/processed/final_evaluation/mcmc_stability_recommendation.csv
#   data/processed/final_evaluation/09_evaluate_models_config.json
#   data/processed/final_evaluation/09_evaluate_models_manifest.csv
#   data/processed/final_evaluation/09_final_model_ranking_report.md
#   data/processed/final_evaluation/figure_captions_step10.md
#   data/processed/final_evaluation/figures/*.png
#
# Figure rule:
#   Publication figures produced by this script have no embedded titles or
#   subtitles. Captions are written separately to figure_captions_step10.md.
#
# Usage from project root:
#   Rscript scripts/09_evaluate_models.R
#
# Optional arguments:
#   Rscript scripts/09_evaluate_models.R \
#     --processed-dir=data/processed \
#     --benchmarks-dir=data/processed/benchmarks \
#     --bsts-state-dir=data/processed/bsts_state_specs \
#     --bsts-gt-dir=data/processed/bsts_gt \
#     --out-dir=data/processed/final_evaluation
#
# Dependencies:
#   ggplot2 is required for figures. Data manipulation uses base R only.
# -----------------------------------------------------------------------------

options(stringsAsFactors = FALSE, warn = 1)
suppressWarnings(try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE))

# -----------------------------
# 0. Configuration and packages
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

PROJECT_ROOT <- normalizePath(get_arg("project-root", Sys.getenv("BSTS_PROJECT_ROOT", unset = getwd())), mustWork = FALSE)
PROCESSED_DIR <- normalizePath(get_arg("processed-dir", file.path(PROJECT_ROOT, "data", "processed")), mustWork = FALSE)
BENCHMARKS_DIR <- normalizePath(get_arg("benchmarks-dir", file.path(PROCESSED_DIR, "benchmarks")), mustWork = FALSE)
BSTS_STATE_DIR <- normalizePath(get_arg("bsts-state-dir", file.path(PROCESSED_DIR, "bsts_state_specs")), mustWork = FALSE)
BSTS_GT_DIR <- normalizePath(get_arg("bsts-gt-dir", file.path(PROCESSED_DIR, "bsts_gt")), mustWork = FALSE)
OUT_DIR <- normalizePath(get_arg("out-dir", file.path(PROCESSED_DIR, "final_evaluation")), mustWork = FALSE)
FIG_DIR <- file.path(OUT_DIR, "figures")
TABLE_DIR <- file.path(OUT_DIR, "tables")

TOP_N_MODELS_FOR_PLOTS <- as.integer(get_arg("top-n-models", "12"))
TOP_N_PREDICTORS <- as.integer(get_arg("top-n-predictors", "25"))
STABILITY_MAE_TOLERANCE <- as.numeric(get_arg("stability-mae-tolerance", "0.02"))

required_packages <- c("ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
  stop(
    paste0("Missing required packages: ", paste(missing_packages, collapse = ", "),
           ". Please install them before running Step 10."),
    call. = FALSE
  )
}

# -----------------------------
# 1. Helper functions
# -----------------------------
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

read_csv_utf8 <- function(path) {
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
           na.strings = c("", "NA", "NaN"), fileEncoding = "UTF-8-BOM")
}

write_csv_utf8 <- function(x, path) {
  ensure_dir(dirname(path))
  write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

write_lines_utf8 <- function(lines, path) {
  ensure_dir(dirname(path))
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con, useBytes = TRUE)
}

sha_or_md5 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  unname(tools::md5sum(path))
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

candidate_dirs <- function(...) {
  unique(normalizePath(c(...), mustWork = FALSE))
}

locate_file <- function(filename, dirs, required = TRUE) {
  dirs <- unique(dirs)
  direct <- file.path(dirs, filename)
  hit <- direct[file.exists(direct)]
  if (length(hit) > 0L) return(normalizePath(hit[[1]], mustWork = TRUE))

  recursive_hits <- character(0)
  for (d in dirs) {
    if (dir.exists(d)) {
      x <- list.files(d, pattern = paste0("^", gsub("([.])", "\\\\.", filename), "$"),
                      recursive = TRUE, full.names = TRUE)
      recursive_hits <- c(recursive_hits, x)
    }
  }
  recursive_hits <- unique(recursive_hits[file.exists(recursive_hits)])
  if (length(recursive_hits) > 0L) return(normalizePath(recursive_hits[[1]], mustWork = TRUE))

  if (required) {
    stop(sprintf("Required file not found: %s\nSearched in:\n%s", filename, paste(dirs, collapse = "\n")), call. = FALSE)
  }
  NA_character_
}

col_value <- function(df, candidates, default = NA_character_) {
  for (nm in candidates) {
    if (nm %in% names(df)) return(df[[nm]])
  }
  rep(default, nrow(df))
}

num_value <- function(df, candidates, default = NA_real_) {
  suppressWarnings(as.numeric(col_value(df, candidates, default)))
}

char_value <- function(df, candidates, default = NA_character_) {
  as.character(col_value(df, candidates, default))
}

bool_value <- function(df, candidates, default = NA) {
  x <- col_value(df, candidates, default)
  if (is.logical(x)) return(x)
  xl <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(xl))
  out[xl %in% c("true", "t", "1", "yes", "y")] <- TRUE
  out[xl %in% c("false", "f", "0", "no", "n")] <- FALSE
  out
}

derive_horizon <- function(df) {
  if ("horizon" %in% names(df)) {
    h <- as.character(df$horizon)
    h[grepl("^h[0-9]+$", h)] <- h[grepl("^h[0-9]+$", h)]
    if (all(grepl("^h[0-9]+$", h[!is.na(h)]))) return(h)
  }
  lab <- rep("", nrow(df))
  for (nm in c("design_label", "horizon_label", "evaluation_set")) {
    if (nm %in% names(df)) lab <- paste(lab, as.character(df[[nm]]))
  }
  out <- rep(NA_character_, nrow(df))
  out[grepl("h0|same[_ -]?quarter|nowcast", lab, ignore.case = TRUE)] <- "h0"
  out[grepl("h1|one[_ -]?quarter", lab, ignore.case = TRUE)] <- "h1"
  out[grepl("h2|two[_ -]?quarter", lab, ignore.case = TRUE)] <- "h2"
  if (any(is.na(out))) {
    hv <- num_value(df, c("horizon_quarters", "horizon_quarters_design"), NA_real_)
    out[is.na(out) & is.finite(hv)] <- paste0("h", as.integer(hv[is.na(out) & is.finite(hv)]))
  }
  out
}

parse_quarter_index <- function(q) {
  z <- as.character(q)
  z <- gsub("[Qq]", "", z)
  z <- gsub("[^0-9]", " ", z)
  parts <- strsplit(trimws(z), "\\s+")
  out <- vapply(parts, function(p) {
    p <- p[nzchar(p)]
    if (length(p) < 2L) return(NA_real_)
    yr <- suppressWarnings(as.integer(p[[1]]))
    qq <- suppressWarnings(as.integer(p[[2]]))
    if (!is.finite(yr) || !is.finite(qq)) return(NA_real_)
    yr * 4 + qq
  }, numeric(1))
  out
}

period_group <- function(q) {
  idx <- parse_quarter_index(q)
  p2020q1 <- parse_quarter_index("2020-1")
  p2021q4 <- parse_quarter_index("2021-4")
  out <- rep(NA_character_, length(idx))
  out[is.finite(idx) & idx <= p2020q1] <- "pre_COVID_validation"
  out[is.finite(idx) & idx > p2020q1 & idx <= p2021q4] <- "COVID_shock_adjustment"
  out[is.finite(idx) & idx > p2021q4] <- "post_COVID_validation"
  out
}

metric_col <- function(df, candidates) {
  x <- num_value(df, candidates, NA_real_)
  x
}

normalise_metrics <- function(df, source, uses_gt_default) {
  if (nrow(df) == 0L) return(data.frame())
  out <- data.frame(
    source = source,
    model_label = char_value(df, c("model_label", "model", "model_id")),
    model_family = char_value(df, c("model_family"), source),
    horizon = derive_horizon(df),
    design_label = char_value(df, c("design_label", "horizon_label", "evaluation_set")),
    horizon_quarters = num_value(df, c("horizon_quarters", "horizon_quarters_design"), NA_real_),
    aggregation = char_value(df, c("aggregation")),
    state_spec = char_value(df, c("state_spec", "state_specification")),
    zero_prop_threshold = num_value(df, c("zero_prop_threshold"), NA_real_),
    threshold_role = char_value(df, c("threshold_role")),
    uses_google_trends = ifelse(is.na(bool_value(df, c("uses_google_trends"), NA)), uses_gt_default, bool_value(df, c("uses_google_trends"), NA)),
    n_predictions = num_value(df, c("n_predictions", "n_success"), NA_real_),
    MAE = metric_col(df, c("MAE")),
    RMSE = metric_col(df, c("RMSE")),
    MAPE = metric_col(df, c("MAPE")),
    sMAPE = metric_col(df, c("sMAPE")),
    mean_error = metric_col(df, c("mean_error")),
    median_abs_error = metric_col(df, c("median_abs_error")),
    coverage_95 = metric_col(df, c("coverage_95")),
    average_interval_width_95 = metric_col(df, c("average_interval_width_95", "avg_interval_width", "mean_interval_width_95")),
    mean_interval_score_95 = metric_col(df, c("mean_interval_score_95", "avg_interval_score", "average_interval_score_95")),
    min_target_quarter = char_value(df, c("min_target_quarter")),
    max_target_quarter = char_value(df, c("max_target_quarter")),
    stringsAsFactors = FALSE
  )
  out$model_label <- ifelse(is.na(out$model_label) | out$model_label == "", out$model_family, out$model_label)
  out$model_display <- out$model_label
  out$evaluation_scope <- ifelse(out$horizon == "h0", "main_h0_nowcast", "secondary_or_exploratory")
  out
}

normalise_predictions <- function(df, source, uses_gt_default) {
  if (nrow(df) == 0L) return(data.frame())
  point <- num_value(df, c("forecast_mean", "point_forecast"), NA_real_)
  actual <- num_value(df, c("actual"), NA_real_)
  err <- num_value(df, c("error", "error_actual_minus_forecast"), NA_real_)
  err[!is.finite(err) & is.finite(actual) & is.finite(point)] <- actual[!is.finite(err) & is.finite(actual) & is.finite(point)] - point[!is.finite(err) & is.finite(actual) & is.finite(point)]
  out <- data.frame(
    source = source,
    model_label = char_value(df, c("model_label", "model", "model_id")),
    model_family = char_value(df, c("model_family"), source),
    horizon = derive_horizon(df),
    design_label = char_value(df, c("design_label", "horizon_label", "evaluation_set")),
    aggregation = char_value(df, c("aggregation")),
    zero_prop_threshold = num_value(df, c("zero_prop_threshold"), NA_real_),
    threshold_role = char_value(df, c("threshold_role")),
    fold_id = num_value(df, c("fold_id"), NA_real_),
    origin_id = char_value(df, c("origin_id")),
    target_index = num_value(df, c("target_index"), NA_real_),
    target_quarter = char_value(df, c("target_quarter")),
    target_quarter_start = char_value(df, c("target_quarter_start")),
    actual = actual,
    point_forecast = point,
    lower_95 = num_value(df, c("lower_95"), NA_real_),
    upper_95 = num_value(df, c("upper_95"), NA_real_),
    error_actual_minus_forecast = err,
    abs_error = num_value(df, c("abs_error"), NA_real_),
    ape = num_value(df, c("ape", "mape_contribution"), NA_real_),
    smape = num_value(df, c("smape", "smape_contribution"), NA_real_),
    in_95_interval = bool_value(df, c("in_95_interval", "interval_covered_95", "covered_95"), NA),
    interval_width_95 = num_value(df, c("interval_width_95", "interval_width"), NA_real_),
    interval_score_95 = num_value(df, c("interval_score_95", "interval_score"), NA_real_),
    uses_google_trends = ifelse(is.na(bool_value(df, c("uses_google_trends"), NA)), uses_gt_default, bool_value(df, c("uses_google_trends"), NA)),
    stringsAsFactors = FALSE
  )
  out$model_label <- ifelse(is.na(out$model_label) | out$model_label == "", out$model_family, out$model_label)
  out$abs_error[!is.finite(out$abs_error) & is.finite(out$error_actual_minus_forecast)] <- abs(out$error_actual_minus_forecast[!is.finite(out$abs_error) & is.finite(out$error_actual_minus_forecast)])
  out$period <- period_group(out$target_quarter)
  out
}

relative_to_reference <- function(h0, reference_label, reference_source = NULL, output_label) {
  ref_rows <- h0[h0$model_label == reference_label, , drop = FALSE]
  if (!is.null(reference_source)) ref_rows <- ref_rows[ref_rows$source == reference_source, , drop = FALSE]
  if (nrow(ref_rows) == 0L) return(data.frame())
  ref_mae <- ref_rows$MAE[[1]]
  if (!is.finite(ref_mae)) return(data.frame())
  out <- h0
  out$reference_model <- reference_label
  out$reference_source <- ifelse(is.null(reference_source), "", reference_source)
  out$reference_MAE <- ref_mae
  out$MAE_difference_vs_reference <- out$MAE - ref_mae
  out$MAE_relative_change_percent <- 100 * (out$MAE - ref_mae) / ref_mae
  out$MAE_improvement_percent <- 100 * (ref_mae - out$MAE) / ref_mae
  out$comparison_note <- ifelse(out$MAE_difference_vs_reference < 0, "lower MAE than reference", ifelse(out$MAE_difference_vs_reference > 0, "higher MAE than reference", "same MAE as reference"))
  out$reference_name <- output_label
  out
}

summarise_period_errors <- function(pred) {
  good <- pred[pred$horizon == "h0" & is.finite(pred$point_forecast) & is.finite(pred$actual) & !is.na(pred$period), , drop = FALSE]
  if (nrow(good) == 0L) return(data.frame())
  key <- interaction(good$source, good$model_label, good$period, drop = TRUE, sep = "___")
  pieces <- split(good, key)
  rows <- lapply(pieces, function(d) {
    data.frame(
      source = d$source[[1]],
      model_label = d$model_label[[1]],
      period = d$period[[1]],
      n_predictions = nrow(d),
      MAE = mean(abs(d$error_actual_minus_forecast), na.rm = TRUE),
      RMSE = sqrt(mean(d$error_actual_minus_forecast^2, na.rm = TRUE)),
      mean_error = mean(d$error_actual_minus_forecast, na.rm = TRUE),
      median_abs_error = stats::median(abs(d$error_actual_minus_forecast), na.rm = TRUE),
      min_target_quarter = d$target_quarter[which.min(parse_quarter_index(d$target_quarter))],
      max_target_quarter = d$target_quarter[which.max(parse_quarter_index(d$target_quarter))],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(out$model_label, out$period), , drop = FALSE]
  rownames(out) <- NULL
  out
}

save_plot <- function(p, filename, width = 10, height = 6, dpi = 300) {
  ensure_dir(FIG_DIR)
  path <- file.path(FIG_DIR, filename)
  ggplot2::ggsave(path, plot = p, width = width, height = height, dpi = dpi, bg = "white")
  path
}

base_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_line(linewidth = 0.25),
      panel.grid.major = ggplot2::element_line(linewidth = 0.35),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
}

clip_label <- function(x, n = 70L) {
  x <- as.character(x)
  ifelse(nchar(x) > n, paste0(substr(x, 1L, n - 1L), "…"), x)
}

make_unique_clipped <- function(x, n = 70L) {
  make.unique(clip_label(x, n), sep = " ")
}

# -----------------------------
# 2. Locate and read inputs
# -----------------------------
ensure_dir(OUT_DIR)
ensure_dir(FIG_DIR)
ensure_dir(TABLE_DIR)

benchmark_dirs <- candidate_dirs(BENCHMARKS_DIR, file.path(PROCESSED_DIR, "benchmarks"), file.path(PROJECT_ROOT, "step7_benchmarks"), "/mnt/data/step7_benchmarks", "/mnt/data")
bsts_state_dirs <- candidate_dirs(BSTS_STATE_DIR, file.path(PROCESSED_DIR, "bsts_state_specs"), "/mnt/data")
bsts_gt_dirs <- candidate_dirs(BSTS_GT_DIR, file.path(PROCESSED_DIR, "bsts_gt"), "/mnt/data")

paths <- list(
  benchmark_metrics = locate_file("benchmark_metrics_by_model_horizon.csv", benchmark_dirs),
  benchmark_h0 = locate_file("benchmark_h0_model_ranking.csv", benchmark_dirs),
  benchmark_predictions = locate_file("benchmark_predictions_all.csv", benchmark_dirs),
  benchmark_post = locate_file("benchmark_post_gt_metrics.csv", benchmark_dirs, required = FALSE),

  bsts_state_metrics = locate_file("bsts_state_spec_metrics_by_horizon.csv", bsts_state_dirs),
  bsts_state_h0 = locate_file("bsts_state_spec_h0_ranking.csv", bsts_state_dirs),
  bsts_state_predictions = locate_file("bsts_state_spec_predictions_all.csv", bsts_state_dirs),
  bsts_state_post = locate_file("bsts_state_spec_post_gt_metrics.csv", bsts_state_dirs, required = FALSE),

  bsts_gt_metrics = locate_file("bsts_gt_metrics_by_model_horizon_threshold.csv", bsts_gt_dirs),
  bsts_gt_h0 = locate_file("bsts_gt_h0_ranking.csv", bsts_gt_dirs),
  bsts_gt_predictions = locate_file("bsts_gt_predictions_all.csv", bsts_gt_dirs),
  bsts_gt_inclusion = locate_file("bsts_gt_predictor_inclusion_summary.csv", bsts_gt_dirs, required = FALSE),
  bsts_gt_selection = locate_file("bsts_gt_selected_predictors_by_fold.csv", bsts_gt_dirs, required = FALSE)
)

benchmark_metrics_raw <- read_csv_utf8(paths$benchmark_metrics)
benchmark_h0_raw <- read_csv_utf8(paths$benchmark_h0)
benchmark_predictions_raw <- read_csv_utf8(paths$benchmark_predictions)
bsts_state_metrics_raw <- read_csv_utf8(paths$bsts_state_metrics)
bsts_state_h0_raw <- read_csv_utf8(paths$bsts_state_h0)
bsts_state_predictions_raw <- read_csv_utf8(paths$bsts_state_predictions)
bsts_gt_metrics_raw <- read_csv_utf8(paths$bsts_gt_metrics)
bsts_gt_h0_raw <- read_csv_utf8(paths$bsts_gt_h0)
bsts_gt_predictions_raw <- read_csv_utf8(paths$bsts_gt_predictions)

benchmark_post_raw <- if (!is.na(paths$benchmark_post) && file.exists(paths$benchmark_post)) read_csv_utf8(paths$benchmark_post) else data.frame()
bsts_state_post_raw <- if (!is.na(paths$bsts_state_post) && file.exists(paths$bsts_state_post)) read_csv_utf8(paths$bsts_state_post) else data.frame()
inclusion_raw <- if (!is.na(paths$bsts_gt_inclusion) && file.exists(paths$bsts_gt_inclusion)) read_csv_utf8(paths$bsts_gt_inclusion) else data.frame()
selection_raw <- if (!is.na(paths$bsts_gt_selection) && file.exists(paths$bsts_gt_selection)) read_csv_utf8(paths$bsts_gt_selection) else data.frame()

# -----------------------------
# 3. Normalise and combine outputs
# -----------------------------
metrics_all <- rbind(
  normalise_metrics(benchmark_metrics_raw, "Step7 benchmark", FALSE),
  normalise_metrics(bsts_state_metrics_raw, "Step8B BSTS no-GT state spec", FALSE),
  normalise_metrics(bsts_gt_metrics_raw, "Step9 BSTS GT", TRUE)
)
metrics_all <- metrics_all[is.finite(metrics_all$MAE), , drop = FALSE]
metrics_all <- metrics_all[order(metrics_all$horizon, metrics_all$MAE, metrics_all$RMSE), , drop = FALSE]
metrics_all$rank_within_horizon <- ave(metrics_all$MAE, metrics_all$horizon, FUN = function(x) rank(x, ties.method = "first"))

h0_comparison <- metrics_all[metrics_all$horizon == "h0", , drop = FALSE]
h0_comparison <- h0_comparison[order(h0_comparison$MAE, h0_comparison$RMSE), , drop = FALSE]
h0_comparison$h0_MAE_rank <- seq_len(nrow(h0_comparison))

predictions_all <- rbind(
  normalise_predictions(benchmark_predictions_raw, "Step7 benchmark", FALSE),
  normalise_predictions(bsts_state_predictions_raw, "Step8B BSTS no-GT state spec", FALSE),
  normalise_predictions(bsts_gt_predictions_raw, "Step9 BSTS GT", TRUE)
)
predictions_all <- predictions_all[is.finite(predictions_all$actual) & is.finite(predictions_all$point_forecast), , drop = FALSE]

# Reference-relative comparisons.
rel_vs_rw <- relative_to_reference(h0_comparison, "RW", "Step7 benchmark", "RW benchmark")
rel_vs_bsts_ll <- relative_to_reference(h0_comparison, "BSTS_NO_GT_LL", "Step8B BSTS no-GT state spec", "BSTS no-GT local level")

# Interval quality comparison.
interval_quality <- h0_comparison[, c("source", "model_label", "model_family", "aggregation", "zero_prop_threshold", "threshold_role", "n_predictions", "MAE", "RMSE", "coverage_95", "average_interval_width_95", "mean_interval_score_95"), drop = FALSE]
interval_quality <- interval_quality[order(interval_quality$mean_interval_score_95, interval_quality$MAE), , drop = FALSE]

# Period-specific errors.
period_errors <- summarise_period_errors(predictions_all)

# Best models by source/family.
best_by_source <- h0_comparison[order(h0_comparison$source, h0_comparison$MAE), , drop = FALSE]
best_by_source <- best_by_source[!duplicated(best_by_source$source), , drop = FALSE]

best_gt <- h0_comparison[h0_comparison$source == "Step9 BSTS GT", , drop = FALSE]
best_gt <- best_gt[order(best_gt$MAE, best_gt$RMSE), , drop = FALSE]

best_overall <- h0_comparison[1L, , drop = FALSE]
best_benchmark <- h0_comparison[h0_comparison$source == "Step7 benchmark", , drop = FALSE]
best_benchmark <- best_benchmark[order(best_benchmark$MAE, best_benchmark$RMSE), , drop = FALSE]
best_bsts_no_gt <- h0_comparison[h0_comparison$source == "Step8B BSTS no-GT state spec", , drop = FALSE]
best_bsts_no_gt <- best_bsts_no_gt[order(best_bsts_no_gt$MAE, best_bsts_no_gt$RMSE), , drop = FALSE]

# Step 9 threshold sensitivity.
gt_threshold_sensitivity <- h0_comparison[h0_comparison$source == "Step9 BSTS GT", , drop = FALSE]
if (nrow(gt_threshold_sensitivity) > 0L) {
  gt_threshold_sensitivity <- gt_threshold_sensitivity[order(gt_threshold_sensitivity$aggregation, -gt_threshold_sensitivity$zero_prop_threshold), , drop = FALSE]
}

# Predictor inclusion summary: collapse h0 to one row per predictor for final report.
if (nrow(inclusion_raw) > 0L) {
  inc <- inclusion_raw[derive_horizon(inclusion_raw) == "h0", , drop = FALSE]
  if (nrow(inc) > 0L && "predictor_id" %in% names(inc) && "mean_inclusion_probability" %in% names(inc)) {
    key <- inc$predictor_id
    pieces <- split(inc, key)
    inc_top <- lapply(pieces, function(d) {
      data.frame(
        predictor_id = d$predictor_id[[1]],
        Keyword = if ("Keyword" %in% names(d)) d$Keyword[[1]] else NA_character_,
        English_Translation = if ("English_Translation" %in% names(d)) d$English_Translation[[1]] else NA_character_,
        n_rows = nrow(d),
        mean_inclusion_probability = mean(as.numeric(d$mean_inclusion_probability), na.rm = TRUE),
        median_inclusion_probability = if ("median_inclusion_probability" %in% names(d)) stats::median(as.numeric(d$median_inclusion_probability), na.rm = TRUE) else NA_real_,
        max_inclusion_probability = if ("max_inclusion_probability" %in% names(d)) max(as.numeric(d$max_inclusion_probability), na.rm = TRUE) else max(as.numeric(d$mean_inclusion_probability), na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })
    predictor_inclusion_top <- do.call(rbind, inc_top)
    predictor_inclusion_top <- predictor_inclusion_top[order(-predictor_inclusion_top$mean_inclusion_probability), , drop = FALSE]
    predictor_inclusion_top <- predictor_inclusion_top[seq_len(min(TOP_N_PREDICTORS, nrow(predictor_inclusion_top))), , drop = FALSE]
  } else {
    predictor_inclusion_top <- data.frame()
  }
} else {
  predictor_inclusion_top <- data.frame()
}

# Retained predictors summary from Step 9, if available.
if (nrow(selection_raw) > 0L && all(c("aggregation", "zero_prop_threshold", "horizon", "n_retained_predictors") %in% names(selection_raw))) {
  sel_h0 <- selection_raw[as.character(selection_raw$horizon) == "h0", , drop = FALSE]
  if (nrow(sel_h0) > 0L) {
    key <- interaction(sel_h0$aggregation, sel_h0$zero_prop_threshold, drop = TRUE, sep = "___")
    retained_summary <- do.call(rbind, lapply(split(sel_h0, key), function(d) {
      data.frame(
        aggregation = d$aggregation[[1]],
        zero_prop_threshold = as.numeric(d$zero_prop_threshold[[1]]),
        n_folds = nrow(d),
        mean_retained_predictors = mean(as.numeric(d$n_retained_predictors), na.rm = TRUE),
        min_retained_predictors = min(as.numeric(d$n_retained_predictors), na.rm = TRUE),
        max_retained_predictors = max(as.numeric(d$n_retained_predictors), na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
    retained_summary <- retained_summary[order(retained_summary$aggregation, -retained_summary$zero_prop_threshold), , drop = FALSE]
  } else {
    retained_summary <- data.frame()
  }
} else {
  retained_summary <- data.frame()
}

# Post-GT unemployment-only comparison, excluding Step 9 because GT predictors end at 2024 Q3.
post_parts <- list()
if (nrow(benchmark_post_raw) > 0L) post_parts[[length(post_parts) + 1L]] <- normalise_metrics(benchmark_post_raw, "Step7 benchmark", FALSE)
if (nrow(bsts_state_post_raw) > 0L) post_parts[[length(post_parts) + 1L]] <- normalise_metrics(bsts_state_post_raw, "Step8B BSTS no-GT state spec", FALSE)
post_gt_comparison <- if (length(post_parts) > 0L) do.call(rbind, post_parts) else data.frame()
if (nrow(post_gt_comparison) > 0L) {
  post_gt_comparison <- post_gt_comparison[order(post_gt_comparison$MAE, post_gt_comparison$RMSE), , drop = FALSE]
  post_gt_comparison$post_gt_MAE_rank <- seq_len(nrow(post_gt_comparison))
}

# MCMC stability recommendation for close stochastic comparisons.
stability_rows <- data.frame()
if (nrow(best_gt) > 0L && nrow(best_bsts_no_gt) > 0L) {
  diff_gt_no_gt <- best_gt$MAE[[1]] - best_bsts_no_gt$MAE[[1]]
  stability_rows <- rbind(stability_rows, data.frame(
    comparison = "best_GT_vs_best_BSTS_no_GT",
    model_A = best_gt$model_label[[1]],
    model_B = best_bsts_no_gt$model_label[[1]],
    MAE_A = best_gt$MAE[[1]],
    MAE_B = best_bsts_no_gt$MAE[[1]],
    MAE_difference_A_minus_B = diff_gt_no_gt,
    tolerance = STABILITY_MAE_TOLERANCE,
    recommend_multi_seed_check = abs(diff_gt_no_gt) <= STABILITY_MAE_TOLERANCE,
    recommendation = ifelse(abs(diff_gt_no_gt) <= STABILITY_MAE_TOLERANCE,
                            "Difference is small; rerun top stochastic models with multiple seeds before final manuscript claims.",
                            "Difference exceeds tolerance; multi-seed check still useful but less critical."),
    stringsAsFactors = FALSE
  ))
}
if (nrow(best_gt) > 0L && nrow(best_benchmark) > 0L) {
  diff_gt_bench <- best_gt$MAE[[1]] - best_benchmark$MAE[[1]]
  stability_rows <- rbind(stability_rows, data.frame(
    comparison = "best_GT_vs_best_benchmark",
    model_A = best_gt$model_label[[1]],
    model_B = best_benchmark$model_label[[1]],
    MAE_A = best_gt$MAE[[1]],
    MAE_B = best_benchmark$MAE[[1]],
    MAE_difference_A_minus_B = diff_gt_bench,
    tolerance = STABILITY_MAE_TOLERANCE,
    recommend_multi_seed_check = abs(diff_gt_bench) <= STABILITY_MAE_TOLERANCE,
    recommendation = ifelse(abs(diff_gt_bench) <= STABILITY_MAE_TOLERANCE,
                            "Difference is small; rerun top stochastic models with multiple seeds before final manuscript claims.",
                            "Difference exceeds tolerance; interpretation is less likely to depend on MCMC seed."),
    stringsAsFactors = FALSE
  ))
}

# Predictions for selected h0 models.
selected_labels <- unique(c(
  if (nrow(best_benchmark) > 0L) best_benchmark$model_label[[1]] else NA_character_,
  "RW", "ETS", "ARIMA",
  if (nrow(best_bsts_no_gt) > 0L) best_bsts_no_gt$model_label[[1]] else NA_character_,
  if (nrow(best_gt) > 0L) best_gt$model_label[[1]] else NA_character_
))
selected_labels <- selected_labels[!is.na(selected_labels) & nzchar(selected_labels)]
selected_predictions_h0 <- predictions_all[predictions_all$horizon == "h0" & predictions_all$model_label %in% selected_labels, , drop = FALSE]
selected_predictions_h0 <- selected_predictions_h0[order(selected_predictions_h0$target_index, selected_predictions_h0$model_label), , drop = FALSE]

# -----------------------------
# 4. Write tables
# -----------------------------
files_written <- character(0)
write_out <- function(x, filename) {
  path <- file.path(OUT_DIR, filename)
  write_csv_utf8(x, path)
  write_csv_utf8(x, file.path(TABLE_DIR, filename))
  files_written <<- c(files_written, path, file.path(TABLE_DIR, filename))
  invisible(path)
}

write_out(h0_comparison, "final_model_comparison_h0.csv")
write_out(metrics_all, "final_model_comparison_all_horizons.csv")
write_out(rel_vs_rw, "relative_performance_vs_RW.csv")
write_out(rel_vs_bsts_ll, "relative_performance_vs_BSTS_NO_GT_LL.csv")
write_out(interval_quality, "interval_quality_comparison.csv")
write_out(period_errors, "period_specific_errors.csv")
write_out(best_by_source, "final_best_model_by_source.csv")
write_out(best_gt, "final_best_gt_specification.csv")
write_out(gt_threshold_sensitivity, "final_gt_threshold_sensitivity.csv")
write_out(selected_predictions_h0, "final_selected_models_h0_predictions.csv")
write_out(predictor_inclusion_top, "final_predictor_inclusion_top_keywords.csv")
write_out(retained_summary, "final_retained_predictors_by_threshold.csv")
write_out(post_gt_comparison, "post_gt_unemployment_only_comparison.csv")
write_out(stability_rows, "mcmc_stability_recommendation.csv")

# -----------------------------
# 5. Figures, no embedded titles/subtitles
# -----------------------------
figure_files <- character(0)

if (nrow(h0_comparison) > 0L) {
  plot_d <- h0_comparison[seq_len(min(TOP_N_MODELS_FOR_PLOTS, nrow(h0_comparison))), , drop = FALSE]
  plot_d$model_label_plot_raw <- make_unique_clipped(plot_d$model_label, 45L)
  plot_d$model_label_plot <- factor(plot_d$model_label_plot_raw, levels = plot_d$model_label_plot_raw)
  p <- ggplot2::ggplot(plot_d, ggplot2::aes(x = model_label_plot, y = MAE, fill = source)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", MAE)), vjust = -0.25, size = 3.6) +
    ggplot2::labs(x = "Model", y = "MAE (percentage points)", fill = "Source") +
    base_theme()
  figure_files <- c(figure_files, save_plot(p, "final_h0_model_comparison.png", width = 11, height = 6))
}

if (nrow(rel_vs_rw) > 0L) {
  plot_d <- rel_vs_rw[order(rel_vs_rw$MAE), , drop = FALSE]
  plot_d <- plot_d[seq_len(min(TOP_N_MODELS_FOR_PLOTS, nrow(plot_d))), , drop = FALSE]
  plot_d$model_label_plot_raw <- make_unique_clipped(plot_d$model_label, 45L)
  plot_d$model_label_plot <- factor(plot_d$model_label_plot_raw, levels = plot_d$model_label_plot_raw)
  p <- ggplot2::ggplot(plot_d, ggplot2::aes(x = model_label_plot, y = MAE_difference_vs_reference, fill = source)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::labs(x = "Model", y = "MAE difference relative to RW", fill = "Source") +
    base_theme()
  figure_files <- c(figure_files, save_plot(p, "final_relative_mae_vs_rw.png", width = 11, height = 6))
}

if (nrow(selected_predictions_h0) > 0L) {
  actual_df <- selected_predictions_h0[!duplicated(selected_predictions_h0$target_quarter), c("target_index", "target_quarter", "actual"), drop = FALSE]
  actual_df$series <- "Actual"
  actual_df$value <- actual_df$actual
  model_df <- selected_predictions_h0[, c("target_index", "target_quarter", "model_label", "point_forecast"), drop = FALSE]
  model_df$series <- model_df$model_label
  model_df$value <- model_df$point_forecast
  plot_d <- rbind(
    actual_df[, c("target_index", "target_quarter", "series", "value"), drop = FALSE],
    model_df[, c("target_index", "target_quarter", "series", "value"), drop = FALSE]
  )
  plot_d <- plot_d[order(plot_d$target_index), , drop = FALSE]
  plot_d$target_quarter_factor <- factor(plot_d$target_quarter, levels = unique(plot_d$target_quarter[order(plot_d$target_index)]))
  p <- ggplot2::ggplot(plot_d, ggplot2::aes(x = target_quarter_factor, y = value, group = series, colour = series, linetype = series, shape = series)) +
    ggplot2::geom_line(linewidth = 0.75) +
    ggplot2::geom_point(size = 2.0) +
    ggplot2::labs(x = "Target quarter", y = "Unemployment rate (%)", colour = "Series", linetype = "Series", shape = "Series") +
    base_theme()
  figure_files <- c(figure_files, save_plot(p, "final_h0_actual_vs_predicted_key_models.png", width = 12, height = 7))

  err_d <- selected_predictions_h0
  err_d$target_quarter_factor <- factor(err_d$target_quarter, levels = unique(err_d$target_quarter[order(err_d$target_index)]))
  p <- ggplot2::ggplot(err_d, ggplot2::aes(x = target_quarter_factor, y = error_actual_minus_forecast, group = model_label, colour = model_label, linetype = model_label, shape = model_label)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_line(linewidth = 0.75) +
    ggplot2::geom_point(size = 2.0) +
    ggplot2::labs(x = "Target quarter", y = "Error: actual - forecast", colour = "Model", linetype = "Model", shape = "Model") +
    base_theme()
  figure_files <- c(figure_files, save_plot(p, "final_h0_rolling_errors_key_models.png", width = 12, height = 7))
}

if (nrow(interval_quality) > 0L && any(is.finite(interval_quality$average_interval_width_95))) {
  plot_d <- interval_quality[is.finite(interval_quality$coverage_95) & is.finite(interval_quality$average_interval_width_95), , drop = FALSE]
  plot_d <- plot_d[order(plot_d$MAE), , drop = FALSE]
  plot_d <- plot_d[seq_len(min(TOP_N_MODELS_FOR_PLOTS, nrow(plot_d))), , drop = FALSE]
  p <- ggplot2::ggplot(plot_d, ggplot2::aes(x = average_interval_width_95, y = coverage_95, colour = source, label = model_label)) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_hline(yintercept = 0.95, linetype = "dashed") +
    ggplot2::labs(x = "Average 95% interval width", y = "95% interval coverage", colour = "Source") +
    base_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0))
  figure_files <- c(figure_files, save_plot(p, "final_interval_quality_h0.png", width = 8, height = 6))
}

if (nrow(period_errors) > 0L) {
  top_models <- unique(h0_comparison$model_label[seq_len(min(TOP_N_MODELS_FOR_PLOTS, nrow(h0_comparison)))])
  plot_d <- period_errors[period_errors$model_label %in% top_models, , drop = FALSE]
  label_map <- stats::setNames(make_unique_clipped(top_models, 45L), top_models)
  plot_d$model_label_plot <- factor(unname(label_map[plot_d$model_label]), levels = unname(label_map))
  p <- ggplot2::ggplot(plot_d, ggplot2::aes(x = model_label_plot, y = MAE, fill = period)) +
    ggplot2::geom_col(position = "dodge", width = 0.75) +
    ggplot2::labs(x = "Model", y = "MAE (percentage points)", fill = "Validation period") +
    base_theme()
  figure_files <- c(figure_files, save_plot(p, "final_period_specific_mae.png", width = 12, height = 6))
}

if (nrow(gt_threshold_sensitivity) > 0L) {
  plot_d <- gt_threshold_sensitivity[is.finite(gt_threshold_sensitivity$zero_prop_threshold), , drop = FALSE]
  if (nrow(plot_d) > 0L) {
    p <- ggplot2::ggplot(plot_d, ggplot2::aes(x = zero_prop_threshold, y = MAE, colour = aggregation, shape = aggregation, group = aggregation)) +
      ggplot2::geom_line(linewidth = 0.75) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::scale_x_reverse(breaks = sort(unique(plot_d$zero_prop_threshold), decreasing = TRUE)) +
      ggplot2::labs(x = "Zero-proportion threshold", y = "MAE (percentage points)", colour = "Aggregation", shape = "Aggregation") +
      base_theme() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
    figure_files <- c(figure_files, save_plot(p, "final_gt_threshold_sensitivity.png", width = 9, height = 6))
  }
}

if (nrow(predictor_inclusion_top) > 0L) {
  plot_d <- predictor_inclusion_top
  plot_d$label <- ifelse(!is.na(plot_d$English_Translation) & nzchar(plot_d$English_Translation),
                         paste0(plot_d$predictor_id, " — ", plot_d$English_Translation),
                         plot_d$predictor_id)
  plot_d$label <- clip_label(plot_d$label, 60L)
  plot_d$label <- make.unique(plot_d$label, sep = " ")
  plot_d$label <- factor(plot_d$label, levels = rev(plot_d$label))
  p <- ggplot2::ggplot(plot_d, ggplot2::aes(x = label, y = mean_inclusion_probability)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Keyword", y = "Mean posterior inclusion probability") +
    base_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0))
  figure_files <- c(figure_files, save_plot(p, "final_predictor_inclusion_top_keywords.png", width = 10, height = 8))
}

if (nrow(post_gt_comparison) > 0L) {
  plot_d <- post_gt_comparison[seq_len(min(TOP_N_MODELS_FOR_PLOTS, nrow(post_gt_comparison))), , drop = FALSE]
  plot_d$model_label_plot_raw <- make_unique_clipped(plot_d$model_label, 45L)
  plot_d$model_label_plot <- factor(plot_d$model_label_plot_raw, levels = plot_d$model_label_plot_raw)
  p <- ggplot2::ggplot(plot_d, ggplot2::aes(x = model_label_plot, y = MAE, fill = source)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", MAE)), vjust = -0.25, size = 3.6) +
    ggplot2::labs(x = "Model", y = "MAE (percentage points)", fill = "Source") +
    base_theme()
  figure_files <- c(figure_files, save_plot(p, "final_post_gt_unemployment_only_mae.png", width = 10, height = 6))
}

# -----------------------------
# 6. Report, captions, config, manifest
# -----------------------------
fmt <- function(x, digits = 3) ifelse(is.finite(as.numeric(x)), sprintf(paste0("%.", digits, "f"), as.numeric(x)), "NA")

best_lines <- c()
if (nrow(best_benchmark) > 0L) best_lines <- c(best_lines, sprintf("- Best Step 7 benchmark: `%s`, h0 MAE = %s.", best_benchmark$model_label[[1]], fmt(best_benchmark$MAE[[1]])))
if (nrow(best_bsts_no_gt) > 0L) best_lines <- c(best_lines, sprintf("- Best no-GT BSTS state specification: `%s`, h0 MAE = %s.", best_bsts_no_gt$model_label[[1]], fmt(best_bsts_no_gt$MAE[[1]])))
if (nrow(best_gt) > 0L) best_lines <- c(best_lines, sprintf("- Best Google Trends BSTS specification: `%s`, h0 MAE = %s.", best_gt$model_label[[1]], fmt(best_gt$MAE[[1]])))
if (nrow(best_overall) > 0L) best_lines <- c(best_lines, sprintf("- Best overall h0 model in the consolidated comparison: `%s` from %s, h0 MAE = %s.", best_overall$model_label[[1]], best_overall$source[[1]], fmt(best_overall$MAE[[1]])))

interpretation_lines <- c()
if (nrow(best_gt) > 0L && nrow(best_benchmark) > 0L) {
  d <- best_gt$MAE[[1]] - best_benchmark$MAE[[1]]
  interpretation_lines <- c(interpretation_lines, sprintf("The best Google Trends model has an h0 MAE difference of %s percentage points relative to the best benchmark. Positive values mean the GT model has higher MAE.", fmt(d)))
}
if (nrow(best_gt) > 0L && nrow(best_bsts_no_gt) > 0L) {
  d <- best_gt$MAE[[1]] - best_bsts_no_gt$MAE[[1]]
  interpretation_lines <- c(interpretation_lines, sprintf("The best Google Trends model has an h0 MAE difference of %s percentage points relative to the best no-GT BSTS specification.", fmt(d)))
}

report_lines <- c(
  "# Step 10 — Consolidated model evaluation report",
  "",
  "## Status",
  "",
  "Step 10 merged the saved outputs from Step 7, Step 8B, and Step 9. It did not refit any models.",
  "",
  "## Main h = 0 ranking summary",
  "",
  if (length(best_lines) > 0L) best_lines else "No h0 model ranking was available.",
  "",
  "## Relative interpretation",
  "",
  if (length(interpretation_lines) > 0L) interpretation_lines else "Relative performance could not be computed because one or more reference models were missing.",
  "",
  "## Output tables",
  "",
  "- `final_model_comparison_h0.csv`",
  "- `final_model_comparison_all_horizons.csv`",
  "- `relative_performance_vs_RW.csv`",
  "- `relative_performance_vs_BSTS_NO_GT_LL.csv`",
  "- `interval_quality_comparison.csv`",
  "- `period_specific_errors.csv`",
  "- `final_selected_models_h0_predictions.csv`",
  "- `final_gt_threshold_sensitivity.csv`",
  "- `final_predictor_inclusion_top_keywords.csv`",
  "- `post_gt_unemployment_only_comparison.csv`",
  "- `mcmc_stability_recommendation.csv`",
  "",
  "## Figure rule",
  "",
  "All generated figures intentionally omit embedded titles and subtitles. Captions are written separately in `figure_captions_step10.md`.",
  "",
  "## Recommended manuscript use",
  "",
  "Use `final_model_comparison_h0.csv` as the primary accuracy table for the main nowcasting result. Use `interval_quality_comparison.csv` to discuss uncertainty calibration. Use `period_specific_errors.csv` as a diagnostic table, not as the primary ranking, because the subperiod sample sizes are small.",
  ""
)
REPORT_PATH <- file.path(OUT_DIR, "09_final_model_ranking_report.md")
write_lines_utf8(report_lines, REPORT_PATH)
files_written <- c(files_written, REPORT_PATH)

caption_lines <- c(
  "# Step 10 figure captions",
  "",
  "The PNG files generated by Step 10 contain no embedded titles or subtitles. Use or adapt the captions below in the manuscript or supplementary material.",
  "",
  "**final_h0_model_comparison.png.** Rolling-origin h = 0 mean absolute error by model. Lower values indicate more accurate same-quarter nowcasts.",
  "",
  "**final_relative_mae_vs_rw.png.** Difference in h = 0 mean absolute error relative to the random-walk benchmark. Negative values indicate lower MAE than the random walk; positive values indicate higher MAE.",
  "",
  "**final_h0_actual_vs_predicted_key_models.png.** Official unemployment rates and selected h = 0 model nowcasts across rolling validation quarters.",
  "",
  "**final_h0_rolling_errors_key_models.png.** Rolling-origin h = 0 errors for selected models, defined as actual unemployment minus forecast unemployment.",
  "",
  "**final_interval_quality_h0.png.** Relationship between average 95% predictive interval width and empirical 95% coverage for h = 0 models.",
  "",
  "**final_period_specific_mae.png.** Period-specific h = 0 mean absolute error by model across pre-COVID, COVID shock/adjustment, and post-COVID validation periods.",
  "",
  "**final_gt_threshold_sensitivity.png.** h = 0 mean absolute error of Google-Trends BSTS models by aggregation method and zero-proportion threshold.",
  "",
  "**final_predictor_inclusion_top_keywords.png.** Top Google Trends predictors ranked by mean posterior inclusion probability across h = 0 Google-Trends BSTS specifications.",
  "",
  "**final_post_gt_unemployment_only_mae.png.** Unemployment-only forecast performance in the post-Google-Trends validation block. Google Trends models are excluded because contemporaneous search predictors are unavailable after 2024 Q3."
)
CAPTION_PATH <- file.path(OUT_DIR, "figure_captions_step10.md")
write_lines_utf8(caption_lines, CAPTION_PATH)
files_written <- c(files_written, CAPTION_PATH, figure_files)

CONFIG_PATH <- file.path(OUT_DIR, "09_evaluate_models_config.json")
config_lines <- c(
  "{",
  '  "script": "09_evaluate_models.R",',
  '  "step": "Step 10 — Consolidated model evaluation",',
  sprintf('  "generated_at": "%s",', format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  sprintf('  "top_n_models_for_plots": %d,', TOP_N_MODELS_FOR_PLOTS),
  sprintf('  "top_n_predictors": %d,', TOP_N_PREDICTORS),
  sprintf('  "stability_mae_tolerance": %.4f,', STABILITY_MAE_TOLERANCE),
  '  "input_files": {',
  paste0('    "', names(paths), '": "', json_escape(unlist(paths)), '"', collapse = ",\n"),
  "  },",
  '  "figure_policy": "no embedded figure titles or subtitles",',
  sprintf('  "figures_written": %s', json_vec(basename(figure_files))),
  "}"
)
write_lines_utf8(config_lines, CONFIG_PATH)
files_written <- c(files_written, CONFIG_PATH)

MANIFEST_PATH <- file.path(OUT_DIR, "09_evaluate_models_manifest.csv")
manifest <- data.frame(
  file_name = basename(files_written),
  path = files_written,
  exists = file.exists(files_written),
  checksum_md5 = vapply(files_written, sha_or_md5, character(1L)),
  stringsAsFactors = FALSE
)
write_csv_utf8(manifest, MANIFEST_PATH)
files_written <- c(files_written, MANIFEST_PATH)

message("Step 10 consolidated evaluation complete.")
message(sprintf("Outputs written to: %s", OUT_DIR))
message(sprintf("h0 models compared: %d", nrow(h0_comparison)))
if (nrow(best_overall) > 0L) message(sprintf("Best h0 model: %s (%s), MAE = %s", best_overall$model_label[[1]], best_overall$source[[1]], fmt(best_overall$MAE[[1]])))
