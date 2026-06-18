#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# 11_build_manuscript_assets_v2.R
# Step 12: Build final manuscript tables, figure captions, and results text
# -----------------------------------------------------------------------------
# Purpose:
#   Convert the validated pipeline outputs into publication-facing manuscript
#   assets: final comparison tables, caption files, results narrative, policy
#   significance text, and figure inventory. This script does not refit models.
#
# Inputs, when available:
#   data/processed/final_evaluation/*.csv
#   data/processed/mcmc_stability/*.csv
#   data/processed/benchmarks/*.csv
#   data/processed/bsts_state_specs/*.csv
#   data/processed/bsts_gt/*.csv
#   data/processed/validation/*.csv
#   data/processed/modeling_panel_*.csv
#   data/processed/keyword_dictionary_clean.csv
#
# Outputs:
#   manuscript_assets/tables/*.csv
#   manuscript_assets/text/*.md
#   manuscript_assets/figures/*.{png}
#   manuscript_assets/11_build_manuscript_assets_manifest.csv
#   manuscript_assets/11_build_manuscript_assets_report.md
#
# Figure style rule:
#   No embedded figure titles or subtitles are created by this script.
#   Figure titles are handled as manuscript captions outside the image files.
#
# Usage:
#   Rscript scripts/11_build_manuscript_assets_v2.R
#
# Optional arguments:
#   Rscript scripts/11_build_manuscript_assets_v2.R \
#     --processed-dir=data/processed \
#     --out-dir=manuscript_assets \
#     --figure-source-dir=data/processed/final_evaluation/figures
#
# Dependencies:
#   Base R only.
# -----------------------------------------------------------------------------

options(stringsAsFactors = FALSE, warn = 1)
suppressWarnings(try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE))

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
OUT_DIR <- normalizePath(get_arg("out-dir", file.path(PROJECT_ROOT, "manuscript_assets")), mustWork = FALSE)
FIGURE_SOURCE_DIR_ARG <- get_arg("figure-source-dir", file.path(PROCESSED_DIR, "final_evaluation", "figures"))

TABLE_DIR <- file.path(OUT_DIR, "tables")
TEXT_DIR <- file.path(OUT_DIR, "text")
FIG_DIR <- file.path(OUT_DIR, "figures")

BENCH_DIR <- file.path(PROCESSED_DIR, "benchmarks")
BSTS_STATE_DIR <- file.path(PROCESSED_DIR, "bsts_state_specs")
BSTS_GT_DIR <- file.path(PROCESSED_DIR, "bsts_gt")
FINAL_EVAL_DIR <- file.path(PROCESSED_DIR, "final_evaluation")
MCMC_DIR <- file.path(PROCESSED_DIR, "mcmc_stability")
VALIDATION_DIR <- file.path(PROCESSED_DIR, "validation")

# Candidate directories allow use from either a project folder or the flat /mnt/data
# artifact directory used during development.
CANDIDATE_DIRS <- unique(c(
  PROCESSED_DIR,
  FINAL_EVAL_DIR,
  MCMC_DIR,
  BENCH_DIR,
  BSTS_STATE_DIR,
  BSTS_GT_DIR,
  VALIDATION_DIR,
  file.path(PROJECT_ROOT, "data", "processed"),
  file.path(PROJECT_ROOT, "data", "processed", "final_evaluation"),
  file.path(PROJECT_ROOT, "data", "processed", "mcmc_stability"),
  file.path(PROJECT_ROOT, "data", "processed", "validation"),
  PROJECT_ROOT,
  getwd(),
  "/mnt/data"
))

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
ensure_dir(OUT_DIR); ensure_dir(TABLE_DIR); ensure_dir(TEXT_DIR); ensure_dir(FIG_DIR)

locate_file <- function(filename, candidate_dirs = CANDIDATE_DIRS, required = FALSE) {
  candidate_dirs <- candidate_dirs[dir.exists(candidate_dirs)]
  candidate_paths <- file.path(candidate_dirs, filename)
  hit <- candidate_paths[file.exists(candidate_paths)]
  if (length(hit) > 0L) return(normalizePath(hit[[1]], mustWork = TRUE))
  if (isTRUE(required)) stop(sprintf("Required file not found: %s", filename), call. = FALSE)
  NA_character_
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

fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(as.numeric(x), format = "f", digits = digits))
}

normalise_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  gsub("[^a-z0-9]+", "_", x)
}

find_col <- function(df, candidates) {
  nms <- names(df)
  if (length(nms) == 0L) return(NA_character_)
  nms_norm <- normalise_name(nms)
  cand_norm <- normalise_name(candidates)
  idx <- match(cand_norm, nms_norm)
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0L) return(NA_character_)
  nms[[idx[[1]]]]
}

normalise_metric_table <- function(df) {
  if (is.null(df) || nrow(df) == 0L) {
    return(data.frame(model = character(0), source = character(0), horizon = character(0), MAE = numeric(0), RMSE = numeric(0), MAPE = numeric(0), sMAPE = numeric(0), stringsAsFactors = FALSE))
  }

  model_col <- find_col(df, c("model", "model_name", "model_label", "Model", "Model label", "series", "Series"))
  source_col <- find_col(df, c("source", "Source", "model_source", "Source label"))
  horizon_col <- find_col(df, c("horizon", "validation_design", "design_label", "horizon_label"))
  mae_col <- find_col(df, c("MAE", "mae", "mean_absolute_error", "h0_MAE"))
  rmse_col <- find_col(df, c("RMSE", "rmse", "root_mean_squared_error"))
  mape_col <- find_col(df, c("MAPE", "mape"))
  smape_col <- find_col(df, c("sMAPE", "SMAPE", "smape"))

  n <- nrow(df)
  out <- data.frame(
    model = if (!is.na(model_col)) as.character(df[[model_col]]) else rep(NA_character_, n),
    source = if (!is.na(source_col)) as.character(df[[source_col]]) else rep(NA_character_, n),
    horizon = if (!is.na(horizon_col)) as.character(df[[horizon_col]]) else rep(NA_character_, n),
    MAE = if (!is.na(mae_col)) suppressWarnings(as.numeric(df[[mae_col]])) else rep(NA_real_, n),
    RMSE = if (!is.na(rmse_col)) suppressWarnings(as.numeric(df[[rmse_col]])) else rep(NA_real_, n),
    MAPE = if (!is.na(mape_col)) suppressWarnings(as.numeric(df[[mape_col]])) else rep(NA_real_, n),
    sMAPE = if (!is.na(smape_col)) suppressWarnings(as.numeric(df[[smape_col]])) else rep(NA_real_, n),
    stringsAsFactors = FALSE
  )

  # Keep only rows that can serve as a publication metric row.
  out <- out[!is.na(out$model) & nzchar(out$model) & is.finite(out$MAE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

fallback_final_h0 <- function() {
  data.frame(
    rank = seq_len(12L),
    model = c(
      "RW", "RW_DRIFT", "ETS", "BSTS_NO_GT_LL", "BSTS_GT_EMA_LL_Z0.80_h0", "ARIMA",
      "BSTS_GT_EMA_LL_Z0.70_h0", "BSTS_GT_EMA_LL_Z0.95_h0", "BSTS_GT_EMA_LL_Z0.90_h0",
      "BSTS_GT_MA_LL_Z0.70_h0", "BSTS_GT_MA_LL_Z0.50_h0", "BSTS_GT_EMA_LL_Z0.50_h0"
    ),
    source = c(rep("Step7 benchmark", 3L), "Step8B BSTS no-GT state spec", rep("Step9 BSTS GT", 1L), "Step7 benchmark", rep("Step9 BSTS GT", 6L)),
    horizon = "h0_same_quarter_nowcast",
    MAE = c(0.459, 0.501, 0.515, 0.519, 0.525, 0.526, 0.532, 0.550, 0.557, 0.594, 0.618, 0.618),
    RMSE = NA_real_, MAPE = NA_real_, sMAPE = NA_real_,
    value_source = "fallback_values_from_step10_rendered_results",
    stringsAsFactors = FALSE
  )
}

fallback_gt_sensitivity <- function() {
  data.frame(
    aggregation = rep(c("EMA", "MA"), each = 6L),
    zero_proportion_threshold = rep(c(0.95, 0.90, 0.80, 0.70, 0.50, 0.30), times = 2L),
    MAE = c(0.550, 0.557, 0.525, 0.532, 0.618, 0.646, 0.630, 0.630, 0.639, 0.594, 0.618, 0.625),
    horizon = "h0_same_quarter_nowcast",
    value_source = "fallback_values_from_step10_rendered_results",
    stringsAsFactors = FALSE
  )
}

fallback_post_gt <- function() {
  data.frame(
    rank = seq_len(9L),
    model = c("SNAIVE", "BSTS_NO_GT_LL_SEAS", "BSTS_NO_GT_LLT_SEAS", "BSTS_NO_GT_LL", "ARIMA", "ETS", "RW", "BSTS_NO_GT_LLT", "RW_DRIFT"),
    source = c("Step7 benchmark", "Step8B BSTS no-GT state spec", "Step8B BSTS no-GT state spec", "Step8B BSTS no-GT state spec", "Step7 benchmark", "Step7 benchmark", "Step7 benchmark", "Step8B BSTS no-GT state spec", "Step7 benchmark"),
    validation_block = "post_GT_unemployment_only_2024Q4_2026Q1",
    MAE = c(0.133, 0.135, 0.141, 0.194, 0.197, 0.205, 0.217, 0.270, 0.766),
    value_source = "fallback_values_from_step10_rendered_results",
    stringsAsFactors = FALSE
  )
}

fallback_predictors <- function() {
  data.frame(
    rank = seq_len(15L),
    predictor_id = c("Keyword_41", "Keyword_12", "Keyword_15", "Keyword_11", "Keyword_34", "Keyword_46", "Keyword_27", "Keyword_37", "Keyword_42", "Keyword_32", "Keyword_56", "Keyword_30", "Keyword_5", "Keyword_40", "Keyword_59"),
    English_Translation = c("Spending", "Money", "Worker", "Work", "Payment", "CV", "Salary", "Bonus", "HR", "Employer", "Akhtaboot", "Employee", "My Work", "Taxes", "Salary"),
    keyword_theme = c("economic_pressure", "economic_pressure", "work_and_employment", "work_and_employment", "income_and_payments", "job_search", "income_and_payments", "income_and_payments", "job_search", "employment_relationship", "job_portal", "employment_relationship", "work_and_employment", "economic_pressure", "income_and_payments"),
    mean_inclusion_probability = c(0.84, 0.21, 0.13, 0.125, 0.124, 0.118, 0.113, 0.092, 0.084, 0.083, 0.079, 0.077, 0.075, 0.071, 0.060),
    value_source = "fallback_values_from_step11_rendered_results_approximate",
    stringsAsFactors = FALSE
  )
}

read_or_fallback <- function(filename, fallback_fun, dirs = CANDIDATE_DIRS) {
  path <- locate_file(filename, dirs, required = FALSE)
  if (!is.na(path)) {
    df <- read_csv_utf8(path)
    attr(df, "source_file") <- path
    return(df)
  }
  df <- fallback_fun()
  attr(df, "source_file") <- "fallback_embedded_in_step12_script"
  df
}

# -----------------------------
# 1. Build publication tables
# -----------------------------

# Table 1: data overview
mp_gt_path <- locate_file("modeling_panel_gt_overlap.csv", required = FALSE)
mp_full_path <- locate_file("modeling_panel_unemployment_full.csv", required = FALSE)
kw_path <- locate_file("keyword_dictionary_clean.csv", required = FALSE)
ma_path <- locate_file("google_trends_quarterly_MA.csv", required = FALSE)
ema_path <- locate_file("google_trends_quarterly_EMA.csv", required = FALSE)

n_gt <- n_full <- n_kw <- n_ma <- n_ema <- NA_integer_
period_gt <- period_full <- ""
if (!is.na(mp_gt_path)) {
  tmp <- read_csv_utf8(mp_gt_path); n_gt <- nrow(tmp); period_gt <- paste(tmp$Quarter[[1]], tail(tmp$Quarter, 1), sep = "–")
}
if (!is.na(mp_full_path)) {
  tmp <- read_csv_utf8(mp_full_path); n_full <- nrow(tmp); period_full <- paste(tmp$Quarter[[1]], tail(tmp$Quarter, 1), sep = "–")
}
if (!is.na(kw_path)) { tmp <- read_csv_utf8(kw_path); n_kw <- nrow(tmp) }
if (!is.na(ma_path)) { tmp <- read_csv_utf8(ma_path); n_ma <- nrow(tmp) }
if (!is.na(ema_path)) { tmp <- read_csv_utf8(ema_path); n_ema <- nrow(tmp) }

table1 <- data.frame(
  component = c("Official unemployment outcome", "Google Trends overlap panel", "Google Trends MA quarterly predictors", "Google Trends EMA quarterly predictors", "Retained keyword dictionary"),
  frequency = c("Quarterly", "Quarterly", "Quarterly", "Quarterly", "Keyword-level metadata"),
  period_or_scope = c(period_full, period_gt, period_gt, period_gt, "retained modelling keywords"),
  rows_or_count = c(n_full, n_gt, n_ma, n_ema, n_kw),
  role_in_manuscript = c(
    "Authoritative official target series and post-GT validation block",
    "Primary same-quarter nowcasting sample",
    "Mean-aggregated search predictors",
    "Recency-weighted search predictors",
    "Interpretation of keyword IDs, translations, and themes"
  ),
  stringsAsFactors = FALSE
)
write_csv_utf8(table1, file.path(TABLE_DIR, "Table_1_data_components.csv"))

# Table 2: validation design
val_summary_path <- locate_file("validation_design_summary.csv", candidate_dirs = c(VALIDATION_DIR, CANDIDATE_DIRS), required = FALSE)
if (!is.na(val_summary_path)) {
  table2 <- read_csv_utf8(val_summary_path)
} else {
  table2 <- data.frame(
    design_component = c("h0 same-quarter nowcast", "h1 one-quarter-ahead forecast", "h2 two-quarter-ahead exploratory forecast", "Post-GT validation block"),
    period_start = c("2018-1", "2018-1", "2018-2", "2024-4"),
    period_end = c("2024-3", "2024-3", "2024-3", "2026-1"),
    n_rows = c(27L, 27L, 26L, 6L),
    primary_use = c("Primary GT nowcasting experiment", "Secondary lagged-GT forecast", "Exploratory longer-horizon forecast", "Unemployment-only forecast validation"),
    stringsAsFactors = FALSE
  )
}
write_csv_utf8(table2, file.path(TABLE_DIR, "Table_2_validation_design.csv"))

# Table 3: main h0 model comparison
final_h0_path <- locate_file("final_model_comparison_h0.csv", candidate_dirs = c(FINAL_EVAL_DIR, CANDIDATE_DIRS), required = FALSE)
if (!is.na(final_h0_path)) {
  table3_raw <- normalise_metric_table(read_csv_utf8(final_h0_path))
  if (!all(c("model", "MAE") %in% names(table3_raw)) || nrow(table3_raw) == 0L) {
    table3 <- fallback_final_h0()
  } else {
    table3 <- table3_raw[order(table3_raw$MAE), , drop = FALSE]
    table3$rank <- seq_len(nrow(table3))
    table3$value_source <- final_h0_path
  }
} else {
  table3 <- fallback_final_h0()
}
# Keep the core publication rows but preserve a complete sorted output.
write_csv_utf8(table3, file.path(TABLE_DIR, "Table_3_main_h0_model_comparison.csv"))

# Table 4: Google Trends threshold sensitivity
gt_sens_path <- locate_file("final_gt_threshold_sensitivity.csv", candidate_dirs = c(FINAL_EVAL_DIR, CANDIDATE_DIRS), required = FALSE)
if (!is.na(gt_sens_path)) {
  table4 <- read_csv_utf8(gt_sens_path)
  if (!"MAE" %in% names(table4) && "mae" %in% names(table4)) table4$MAE <- table4$mae
  if (!"zero_proportion_threshold" %in% names(table4)) {
    th_col <- find_col(table4, c("threshold", "zero_prop_threshold", "zero_proportion_threshold"))
    if (!is.na(th_col)) table4$zero_proportion_threshold <- table4[[th_col]]
  }
  if (!"aggregation" %in% names(table4)) {
    ag_col <- find_col(table4, c("aggregation", "aggregation_method"))
    if (!is.na(ag_col)) table4$aggregation <- table4[[ag_col]]
  }
  table4$value_source <- gt_sens_path
} else {
  table4 <- fallback_gt_sensitivity()
}
write_csv_utf8(table4, file.path(TABLE_DIR, "Table_4_google_trends_threshold_sensitivity.csv"))

# Table 5: period-specific MAE, if available.
period_path <- locate_file("period_specific_errors.csv", candidate_dirs = c(FINAL_EVAL_DIR, CANDIDATE_DIRS), required = FALSE)
if (!is.na(period_path)) {
  period_raw <- read_csv_utf8(period_path)
  table5 <- period_raw
  table5$value_source <- period_path
} else {
  table5 <- data.frame(
    note = "Exact period-specific MAE table was not found. Use data/processed/final_evaluation/period_specific_errors.csv when available; the figure indicates COVID shock-adjustment dominates average error.",
    value_source = "narrative_from_step10_rendered_results",
    stringsAsFactors = FALSE
  )
}
write_csv_utf8(table5, file.path(TABLE_DIR, "Table_5_period_specific_errors.csv"))

# Table 6: post-GT validation comparison
post_path <- locate_file("post_gt_unemployment_only_comparison.csv", candidate_dirs = c(FINAL_EVAL_DIR, CANDIDATE_DIRS), required = FALSE)
if (!is.na(post_path)) {
  table6 <- read_csv_utf8(post_path)
  if (!"MAE" %in% names(table6) && "mae" %in% names(table6)) table6$MAE <- table6$mae
  table6$value_source <- post_path
} else {
  table6 <- fallback_post_gt()
}
write_csv_utf8(table6, file.path(TABLE_DIR, "Table_6_post_gt_validation.csv"))

# Table 7: predictor inclusion summary
pred_path <- locate_file("mcmc_stability_predictor_inclusion_summary.csv", candidate_dirs = c(MCMC_DIR, FINAL_EVAL_DIR, CANDIDATE_DIRS), required = FALSE)
if (is.na(pred_path)) pred_path <- locate_file("final_predictor_inclusion_top_keywords.csv", candidate_dirs = c(FINAL_EVAL_DIR, CANDIDATE_DIRS), required = FALSE)
if (!is.na(pred_path)) {
  table7 <- read_csv_utf8(pred_path)
  pip_col <- find_col(table7, c("mean_inclusion_probability", "mean_pip", "posterior_inclusion_probability"))
  if (!is.na(pip_col)) table7$mean_inclusion_probability <- as.numeric(table7[[pip_col]])
  pred_col <- find_col(table7, c("predictor_id", "keyword_id", "Keyword_ID"))
  if (!is.na(pred_col)) table7$predictor_id <- table7[[pred_col]]
  trans_col <- find_col(table7, c("English_Translation", "translation", "English", "english_translation"))
  if (!is.na(trans_col)) table7$English_Translation <- table7[[trans_col]]
  table7 <- table7[order(-table7$mean_inclusion_probability), , drop = FALSE]
  table7$rank <- seq_len(nrow(table7))
  table7$value_source <- pred_path
} else {
  table7 <- fallback_predictors()
}
write_csv_utf8(table7, file.path(TABLE_DIR, "Table_7_top_predictor_inclusion.csv"))

# Table 8: MCMC stability summary
rank_path <- locate_file("mcmc_stability_model_ranking_by_seed.csv", candidate_dirs = c(MCMC_DIR, CANDIDATE_DIRS), required = FALSE)
summary_path <- locate_file("mcmc_stability_summary_by_model.csv", candidate_dirs = c(MCMC_DIR, CANDIDATE_DIRS), required = FALSE)
if (!is.na(rank_path)) {
  rank_raw <- read_csv_utf8(rank_path)
  model_col <- find_col(rank_raw, c("model", "model_name"))
  rank_col <- find_col(rank_raw, c("rank", "model_rank"))
  if (!is.na(model_col) && !is.na(rank_col)) {
    models <- unique(rank_raw[[model_col]])
    table8 <- do.call(rbind, lapply(models, function(m) {
      r <- as.numeric(rank_raw[rank_raw[[model_col]] == m, rank_col])
      data.frame(model = m, mean_rank = mean(r, na.rm = TRUE), min_rank = min(r, na.rm = TRUE), max_rank = max(r, na.rm = TRUE), n_seeds = sum(!is.na(r)), stringsAsFactors = FALSE)
    }))
    table8 <- table8[order(table8$mean_rank), , drop = FALSE]
    table8$stability_interpretation <- ifelse(table8$min_rank == table8$max_rank, "rank stable across seeds", "rank varies across seeds")
    table8$value_source <- rank_path
  } else {
    table8 <- data.frame(model = character(0), mean_rank = numeric(0))
  }
} else if (!is.na(summary_path)) {
  table8 <- read_csv_utf8(summary_path); table8$value_source <- summary_path
} else {
  table8 <- data.frame(
    model = c("BSTS_GT_EMA_LL_Z0.80_h0", "BSTS_GT_EMA_LL_Z0.70_h0", "BSTS_GT_EMA_LL_Z0.90_h0"),
    mean_rank = c(1, 2, 3), min_rank = c(1, 2, 3), max_rank = c(1, 2, 3), n_seeds = 5L,
    stability_interpretation = "rank stable across seeds",
    value_source = "fallback_values_from_step11_rendered_results",
    stringsAsFactors = FALSE
  )
}
write_csv_utf8(table8, file.path(TABLE_DIR, "Table_8_mcmc_stability_summary.csv"))

# -----------------------------
# 2. Results summaries
# -----------------------------

best_overall <- table3[order(table3$MAE), , drop = FALSE][1L, ]
best_gt <- table3[grepl("GT_", table3$model) | grepl("BSTS_GT", table3$model), , drop = FALSE]
best_gt <- best_gt[order(best_gt$MAE), , drop = FALSE][1L, ]
bsts_no_gt <- table3[table3$model == "BSTS_NO_GT_LL", , drop = FALSE]
rw <- table3[table3$model == "RW", , drop = FALSE]

rw_mae <- if (nrow(rw) > 0L) rw$MAE[[1]] else NA_real_
best_gt_mae <- if (nrow(best_gt) > 0L) best_gt$MAE[[1]] else NA_real_
bsts_ll_mae <- if (nrow(bsts_no_gt) > 0L) bsts_no_gt$MAE[[1]] else NA_real_

gt_minus_rw <- best_gt_mae - rw_mae
gt_minus_bsts <- best_gt_mae - bsts_ll_mae

results_lines <- c(
  "# Step 12 results narrative",
  "",
  "## Primary h = 0 finding",
  "",
  sprintf("The best overall h = 0 same-quarter nowcasting model is `%s`, with MAE = %s percentage points.", best_overall$model[[1]], fmt(best_overall$MAE[[1]])),
  sprintf("The best Google-Trends-augmented BSTS model is `%s`, with MAE = %s percentage points.", best_gt$model[[1]], fmt(best_gt_mae)),
  sprintf("Relative to the random-walk benchmark, the best Google Trends model has an MAE difference of %s percentage points.", fmt(gt_minus_rw)),
  sprintf("Relative to the best no-GT BSTS model (`BSTS_NO_GT_LL`), the best Google Trends model has an MAE difference of %s percentage points.", fmt(gt_minus_bsts)),
  "",
  "The manuscript should therefore avoid claiming that Google Trends improves point nowcasting accuracy over the best unemployment-only benchmark. The defensible result is that the Google Trends model is competitive and interpretable, but not accuracy-superior under the clean rolling-origin design.",
  "",
  "## Aggregation and sparsity result",
  "",
  "Among the Google Trends variants, EMA is the preferred aggregation strategy. The best observed sparsity threshold is 0.80, which balances retention of useful search signals against removal of sparse and noisy predictors.",
  "",
  "## Predictor interpretation",
  "",
  "Posterior inclusion results indicate that the most stable search signals are concentrated in economic-pressure, work, income, job-search, and social-protection terms. The strongest stable keyword signal is `Keyword_41 — Spending`. These predictors should be interpreted as behavioural signals of labour-market concern, not as causal drivers of unemployment.",
  "",
  "## Policy interpretation",
  "",
  "The final policy message is that multilingual Google Trends indicators can support labour-market situation awareness, especially by providing interpretable digital-trace signals before official releases. However, they should complement rather than replace official unemployment statistics, and their output should be reviewed with uncertainty intervals and contextual information.",
  "",
  "## Recommended final claim",
  "",
  "> Under expanding-window rolling-origin validation, multilingual Google Trends indicators provide interpretable labour-market signals and perform competitively in a Bayesian nowcasting framework, but they do not outperform simple unemployment-only persistence benchmarks for h = 0 unemployment nowcasting in Jordan."
)
write_lines_utf8(results_lines, file.path(TEXT_DIR, "results_narrative.md"))

abstract_lines <- c(
  "# Draft abstract and policy significance statement",
  "",
  "## Draft abstract",
  "",
  "Official unemployment statistics are essential for labour-market policy, but they are released after the period they measure. This study evaluates whether multilingual Google Trends indicators can provide timely supplementary signals for monitoring unemployment in Jordan. Using a cleaned modelling panel that combines official quarterly unemployment rates with Arabic and English Google Trends search-interest indicators, we compare unemployment-only benchmarks with Bayesian structural time-series models that include recency-weighted search predictors. Models are evaluated using expanding-window rolling-origin validation, fold-specific preprocessing, sparsity sensitivity checks, uncertainty intervals, and multi-seed MCMC stability checks. The best overall h = 0 model is a random-walk unemployment-only benchmark. The best Google Trends specification uses EMA aggregation, a local-level BSTS structure, and a 0.80 zero-proportion threshold; it performs competitively but does not outperform the strongest unemployment-only benchmark. Posterior inclusion probabilities show that the most stable search signals are concentrated in spending, money, work, salary, job-search, and social-protection terms. The results suggest that multilingual search data can support labour-market situation awareness and keyword-level interpretation, but should be used as a transparent supplementary signal rather than a replacement for official labour-force statistics.",
  "",
  "## Draft policy significance statement, 120 words",
  "",
  "Official unemployment statistics are essential for labour-market policy, but they are released after the period they measure. This study evaluates whether multilingual Google search behaviour can provide an earlier, responsible signal of unemployment dynamics in Jordan. Using official quarterly unemployment rates and Arabic and English Google Trends indicators, we compare Bayesian nowcasting models with and without search predictors under time-series validation. The results show that search indicators provide interpretable labour-market signals but do not outperform the strongest unemployment-only benchmark. The study helps policymakers understand when digital trace data can complement, rather than replace, official statistics. It also highlights safeguards: transparent validation, uncertainty intervals, culturally informed keyword interpretation, and careful treatment of sparse search data. These lessons support responsible labour-market monitoring."
)
write_lines_utf8(abstract_lines, file.path(TEXT_DIR, "abstract_and_policy_significance.md"))

rq_lines <- c(
  "# Results mapped to research questions",
  "",
  "## RQ1. Does Google Trends improve nowcasting?",
  "No for h = 0 point accuracy under the final rolling-origin design. The best Google Trends model is competitive but does not beat the random-walk benchmark.",
  "",
  "## RQ2. Which aggregation strategy performs best?",
  "EMA is the preferred Google Trends aggregation strategy. The best GT specification uses EMA with a 0.80 zero-proportion threshold.",
  "",
  "## RQ3. Which search behaviours are most interpretable?",
  "Economic-pressure, spending, money, work, salary, job-search, and social-protection terms are the most interpretable and stable signals. `Keyword_41 — Spending` is the strongest stable inclusion signal in the MCMC stability check.",
  "",
  "## RQ4. How should the framework be used in policy?",
  "The framework should be used for supplementary labour-market situation awareness, not as a replacement for official unemployment statistics. Outputs should be reviewed with uncertainty intervals, contextual information, and periodic performance audits."
)
write_lines_utf8(rq_lines, file.path(TEXT_DIR, "research_questions_answers.md"))

claim_lines <- c(
  "# Claim guardrails for the manuscript",
  "",
  "## Claims supported by the final pipeline",
  "",
  "- The best h = 0 model is the random-walk unemployment-only benchmark.",
  "- The best no-GT BSTS state specification is the local-level model.",
  "- The best GT BSTS model uses EMA aggregation, local level, and a 0.80 zero-proportion threshold.",
  "- The best GT model is competitive with the best no-GT BSTS model but does not beat the random-walk benchmark.",
  "- Google Trends predictors are interpretable and concentrated in economic-pressure and labour-market terms.",
  "- MCMC stability checks confirm that the top GT ranking is stable across tested seeds.",
  "",
  "## Claims to avoid",
  "",
  "- Do not claim that Google Trends improves overall nowcasting accuracy.",
  "- Do not claim that Google Trends replaces official unemployment statistics.",
  "- Do not interpret posterior inclusion probabilities as causal effects.",
  "- Do not present post-2024 Q3 validation as Google Trends evidence, because target-quarter GT predictors are unavailable after 2024 Q3.",
  "- Do not generalise the Jordan-specific keyword results to other countries without validation.",
  "",
  "## Preferred manuscript wording",
  "",
  "> Search indicators provide a timely, interpretable, and policy-relevant supplementary signal, but their accuracy gains are not sufficient to outperform simple unemployment-only persistence benchmarks under rigorous validation."
)
write_lines_utf8(claim_lines, file.path(TEXT_DIR, "claim_guardrails.md"))

# -----------------------------
# 3. Captions and figure inventory
# -----------------------------
figure_inventory <- data.frame(
  figure_id = c("Figure 1", "Figure 2", "Figure 3", "Figure 4", "Figure 5", "Figure 6", "Figure 7", "Figure 8", "Supplementary Figure S1", "Supplementary Figure S2"),
  file_name = c(
    "final_h0_model_comparison.png",
    "final_h0_actual_vs_predicted_key_models.png",
    "final_h0_rolling_errors_key_models.png",
    "final_gt_threshold_sensitivity.png",
    "final_predictor_inclusion_top_keywords.png",
    "final_period_specific_mae.png",
    "final_interval_quality_h0.png",
    "final_post_gt_unemployment_only_mae.png",
    "mcmc_stability_mae_by_seed.png",
    "mcmc_stability_predictor_inclusion_top_keywords.png"
  ),
  suggested_caption = c(
    "Rolling-origin h = 0 mean absolute error by model. Lower values indicate better same-quarter nowcasting accuracy. Colours distinguish unemployment-only benchmarks, the selected no-GT BSTS state specification, and Google-Trends-augmented BSTS models.",
    "Observed unemployment rates and selected h = 0 nowcasts over the rolling-validation period. The selected models include the best unemployment-only benchmark, the best no-GT BSTS model, and the best Google Trends BSTS model.",
    "Rolling h = 0 nowcast errors for selected models. Errors are calculated as official unemployment minus model nowcast; positive values indicate underprediction.",
    "Sensitivity of Google Trends h = 0 nowcasting accuracy to quarterly aggregation method and training-window zero-proportion threshold. EMA performs best at the 0.80 threshold.",
    "Top Google Trends predictors by posterior inclusion probability. Keywords are interpreted as behavioural signals of economic pressure, job search, work, income, and social-protection concern rather than causal effects.",
    "Period-specific mean absolute error for selected models. The COVID shock-adjustment period dominates average forecast error relative to the pre-COVID and post-COVID validation periods.",
    "Predictive-interval quality for interval-producing models. The horizontal reference line indicates nominal 95% coverage; average interval width is shown on the x-axis.",
    "Post-Google-Trends unemployment-only forecast validation for 2024 Q4–2026 Q1. This block is used only for unemployment-only forecasts, not same-quarter Google Trends nowcasts.",
    "Multi-seed MCMC stability of h = 0 mean absolute error for the top Google Trends BSTS models. The random-walk benchmark is shown as a reference when available.",
    "Multi-seed posterior inclusion probabilities for the most stable Google Trends predictors among the top GT models."
  ),
  manuscript_section = c("Results", "Results", "Results", "Results", "Results", "Results", "Results", "Supplementary or robustness", "Supplementary robustness", "Supplementary robustness"),
  stringsAsFactors = FALSE
)
write_csv_utf8(figure_inventory, file.path(OUT_DIR, "figure_inventory.csv"))

caption_lines <- c("# Figure captions", "")
for (i in seq_len(nrow(figure_inventory))) {
  caption_lines <- c(caption_lines, sprintf("**%s.** %s", figure_inventory$figure_id[[i]], figure_inventory$suggested_caption[[i]]), "")
}
write_lines_utf8(caption_lines, file.path(TEXT_DIR, "figure_captions.md"))

# Table captions
caption_table_lines <- c(
  "# Table captions and notes",
  "",
  "**Table 1. Data components and modelling variables.** Summarises the official unemployment outcome, Google Trends overlap panel, quarterly predictor files, and retained keyword dictionary used in the modelling pipeline.",
  "",
  "**Table 2. Rolling-origin validation design.** Defines the primary h = 0 same-quarter nowcast, secondary h = 1 forecast, exploratory h = 2 forecast, and post-Google-Trends unemployment-only validation block.",
  "",
  "**Table 3. Main h = 0 model comparison.** Reports the final same-quarter nowcasting accuracy ranking. Mean absolute error is measured in unemployment-rate percentage points.",
  "",
  "**Table 4. Google Trends aggregation and sparsity-threshold sensitivity.** Compares EMA and MA Google Trends models across training-window zero-proportion thresholds.",
  "",
  "**Table 5. Period-specific errors.** Reports model errors by validation period when the underlying CSV is available. Use this table to support discussion of COVID-period instability.",
  "",
  "**Table 6. Post-Google-Trends unemployment-only validation.** Compares unemployment-only models for the 2024 Q4–2026 Q1 block. This block is not a same-quarter Google Trends nowcasting exercise.",
  "",
  "**Table 7. Predictor inclusion summary.** Lists the most stable Google Trends predictors by posterior inclusion probability. Inclusion probabilities support interpretation and should not be treated as causal evidence.",
  "",
  "**Table 8. MCMC stability summary.** Summarises model ranking stability across seeds for the top stochastic BSTS specifications."
)
write_lines_utf8(caption_table_lines, file.path(TEXT_DIR, "table_captions_and_notes.md"))

# Copy selected figures if found.
fig_candidates <- unique(c(
  FIGURE_SOURCE_DIR_ARG,
  file.path(FINAL_EVAL_DIR, "figures"),
  file.path(MCMC_DIR, "figures"),
  FINAL_EVAL_DIR,
  MCMC_DIR,
  PROCESSED_DIR,
  PROJECT_ROOT,
  getwd(),
  "/mnt/data"
))
fig_candidates <- fig_candidates[dir.exists(fig_candidates)]
copy_status <- data.frame(file_name = figure_inventory$file_name, copied = FALSE, source_path = NA_character_, destination_path = file.path(FIG_DIR, figure_inventory$file_name), stringsAsFactors = FALSE)
for (i in seq_len(nrow(copy_status))) {
  f <- figure_inventory$file_name[[i]]
  paths <- file.path(fig_candidates, f)
  hit <- paths[file.exists(paths)]
  if (length(hit) > 0L) {
    file.copy(hit[[1]], file.path(FIG_DIR, f), overwrite = TRUE)
    copy_status$copied[[i]] <- TRUE
    copy_status$source_path[[i]] <- normalizePath(hit[[1]], mustWork = TRUE)
  }
}
write_csv_utf8(copy_status, file.path(OUT_DIR, "figure_copy_status.csv"))

# -----------------------------
# 4. Report and manifest
# -----------------------------
output_files <- list.files(OUT_DIR, recursive = TRUE, full.names = TRUE)
output_files <- output_files[file.info(output_files)$isdir == FALSE]
manifest <- data.frame(
  file_name = basename(output_files),
  relative_path = sub(paste0(normalizePath(OUT_DIR, mustWork = FALSE), "/?"), "", normalizePath(output_files, mustWork = FALSE)),
  size_bytes = file.info(output_files)$size,
  stringsAsFactors = FALSE
)
write_csv_utf8(manifest, file.path(OUT_DIR, "11_build_manuscript_assets_manifest.csv"))

report_lines <- c(
  "# Step 12 — Manuscript assets report",
  "",
  sprintf("Generated at: %s UTC", format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")),
  "",
  "## Status",
  "",
  "Step 12 assembled publication-facing tables, captions, and narrative text. It did not refit any model.",
  "",
  "## Main evidence summary",
  "",
  sprintf("- Best overall h = 0 model: `%s`, MAE = %s percentage points.", best_overall$model[[1]], fmt(best_overall$MAE[[1]])),
  sprintf("- Best Google Trends h = 0 model: `%s`, MAE = %s percentage points.", best_gt$model[[1]], fmt(best_gt_mae)),
  sprintf("- Best no-GT BSTS h = 0 model: `BSTS_NO_GT_LL`, MAE = %s percentage points.", fmt(bsts_ll_mae)),
  "- Main manuscript claim: Google Trends is interpretable and competitive, but does not outperform the strongest unemployment-only persistence benchmark.",
  "",
  "## Figure rule",
  "",
  "No embedded titles or subtitles are added to figures. Captions are stored in `text/figure_captions.md`.",
  "",
  "## Output folders",
  "",
  "- `tables/`: publication and supplementary tables as CSV files.",
  "- `text/`: narrative text, captions, policy significance statement, and claim guardrails.",
  "- `figures/`: copied selected figures if available in the project outputs.",
  "",
  "## Notes",
  "",
  "If exact Step 10 or Step 11 CSV outputs are present, the script uses them. If not, it writes fallback values from the rendered results already reviewed in the pipeline; those rows are marked in the `value_source` column."
)
write_lines_utf8(report_lines, file.path(OUT_DIR, "11_build_manuscript_assets_report.md"))

message("Step 12 manuscript assets complete.")
message(sprintf("Outputs written to: %s", OUT_DIR))
