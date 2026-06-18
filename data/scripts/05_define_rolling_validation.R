#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# 05_define_rolling_validation.R
# Step 6: Define rolling-validation design
# Revision: sensitivity defaults corrected to 0.95, 0.80, 0.70, 0.50, 0.30 plus primary 0.90.
# -----------------------------------------------------------------------------
# Purpose:
#   Define expanding-window rolling-origin validation files for the clean
#   unemployment / Google Trends modelling panels. This script defines the
#   validation design only; it does not fit models.
#
# Inputs:
#   data/processed/modeling_panel_gt_overlap.csv
#   data/processed/modeling_panel_unemployment_full.csv
#   data/processed/keyword_dictionary_clean.csv
#
# Outputs:
#   data/processed/validation/validation_config.json
#   data/processed/validation/rolling_origins_h0_nowcast.csv
#   data/processed/validation/rolling_origins_h1_forecast.csv
#   data/processed/validation/rolling_origins_h2_forecast_exploratory.csv
#   data/processed/validation/rolling_origins_all.csv
#   data/processed/validation/post_gt_forecast_validation_block.csv
#   data/processed/validation/sparsity_threshold_grid.csv
#   data/processed/validation/preprocessing_rules.csv
#   data/processed/validation/validation_design_summary.csv
#   data/processed/validation/05_define_rolling_validation_manifest.csv
#   data/processed/validation/05_rolling_validation_design_report.md
#
# Agreed pipeline rules:
#   - No model fitting is performed here.
#   - No imputation, interpolation, or missing-value patching is performed here.
#   - No global feature selection or global standardisation is performed here.
#   - All filtering and standardisation must be estimated inside each rolling
#     training window by later modelling scripts.
#   - h = 0 is the primary same-quarter nowcasting design.
#   - h = 1 is the secondary one-quarter-ahead design using lagged GT.
#   - h = 2 is exploratory.
#   - The post-GT block after 2024 Q3 is unemployment-only validation.
#   - MA and EMA are active. SAWA remains deferred until fold-specific seasonal
#     adjustment is implemented.
#
# Usage from project root:
#   Rscript scripts/05_define_rolling_validation.R
#
# Optional arguments:
#   Rscript scripts/05_define_rolling_validation.R \
#     --processed-dir=data/processed \
#     --validation-dir=data/processed/validation \
#     --initial-train-end=2017-4 \
#     --zero-prop-threshold=0.90 \
#     --sensitivity-thresholds=0.95,0.80,0.70,0.50,0.30
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

parse_numeric_csv_arg <- function(value) {
  pieces <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  pieces <- pieces[nzchar(pieces)]
  out <- as.numeric(pieces)
  if (any(is.na(out))) stop(sprintf("Invalid numeric threshold list: %s", value), call. = FALSE)
  out
}

format_thresholds <- function(x) paste(sprintf("%.2f", x), collapse = ", ")

json_num_vec <- function(x) paste0("[", paste(sprintf("%.2f", x), collapse = ", "), "]")

parse_numeric_vector <- function(x) {
  if (length(x) == 0L || is.na(x) || trimws(x) == "") return(numeric(0L))
  pieces <- unlist(strsplit(as.character(x), ",", fixed = TRUE), use.names = FALSE)
  pieces <- trimws(pieces)
  pieces <- pieces[nzchar(pieces)]
  vals <- suppressWarnings(as.numeric(pieces))
  if (any(is.na(vals))) {
    stop(sprintf("Could not parse numeric vector from: %s", x), call. = FALSE)
  }
  vals
}

PROJECT_ROOT <- normalizePath(get_arg("project-root", Sys.getenv("BSTS_PROJECT_ROOT", unset = getwd())), mustWork = FALSE)
PROCESSED_DIR_DEFAULT <- file.path(PROJECT_ROOT, "data", "processed")
PROCESSED_DIR_ARG <- get_arg("processed-dir", Sys.getenv("BSTS_PROCESSED_DIR", unset = PROCESSED_DIR_DEFAULT))
VALIDATION_DIR <- normalizePath(get_arg("validation-dir", file.path(PROCESSED_DIR_DEFAULT, "validation")), mustWork = FALSE)

INITIAL_TRAIN_END <- get_arg("initial-train-end", "2017-4")

# Default zero-proportion thresholds.
# Primary specification: 0.90.
# Sensitivity specifications: 0.95, 0.80, 0.70, 0.50, 0.30.
# The complete grid is ordered from most permissive to strictest:
# 0.95, 0.90, 0.80, 0.70, 0.50, 0.30.
DEFAULT_ZERO_PROP_THRESHOLD <- 0.90
DEFAULT_SENSITIVITY_THRESHOLDS <- c(0.95, 0.80, 0.70, 0.50, 0.30)

ZERO_PROP_THRESHOLD <- as.numeric(get_arg("zero-prop-threshold", sprintf("%.2f", DEFAULT_ZERO_PROP_THRESHOLD)))
SENSITIVITY_THRESHOLDS <- parse_numeric_vector(
  get_arg("sensitivity-thresholds", paste(sprintf("%.2f", DEFAULT_SENSITIVITY_THRESHOLDS), collapse = ","))
)
SENSITIVITY_THRESHOLDS <- unique(SENSITIVITY_THRESHOLDS)
SPARSITY_THRESHOLD_GRID <- sort(unique(c(ZERO_PROP_THRESHOLD, SENSITIVITY_THRESHOLDS)), decreasing = TRUE)
if (is.na(ZERO_PROP_THRESHOLD) || ZERO_PROP_THRESHOLD < 0 || ZERO_PROP_THRESHOLD > 1) {
  stop("zero-prop-threshold must be between 0 and 1.", call. = FALSE)
}
if (any(SPARSITY_THRESHOLD_GRID < 0 | SPARSITY_THRESHOLD_GRID > 1)) {
  stop("All sparsity threshold values must be between 0 and 1.", call. = FALSE)
}
ACTIVE_AGGREGATIONS <- c("MA", "EMA")
DEFERRED_AGGREGATIONS <- c("SAWA")

GT_PANEL_FILE <- "modeling_panel_gt_overlap.csv"
FULL_PANEL_FILE <- "modeling_panel_unemployment_full.csv"
KEYWORD_DICTIONARY_FILE <- "keyword_dictionary_clean.csv"

EXPECTED_GT_START <- "2010-1"
EXPECTED_GT_END <- "2024-3"
EXPECTED_GT_ROWS <- 59L
EXPECTED_FULL_END <- "2026-1"
EXPECTED_FULL_ROWS <- 65L
EXPECTED_KEYWORDS <- 71L

OUTPUT_H0 <- file.path(VALIDATION_DIR, "rolling_origins_h0_nowcast.csv")
OUTPUT_H1 <- file.path(VALIDATION_DIR, "rolling_origins_h1_forecast.csv")
OUTPUT_H2 <- file.path(VALIDATION_DIR, "rolling_origins_h2_forecast_exploratory.csv")
OUTPUT_ALL <- file.path(VALIDATION_DIR, "rolling_origins_all.csv")
OUTPUT_POST_GT <- file.path(VALIDATION_DIR, "post_gt_forecast_validation_block.csv")
OUTPUT_THRESHOLD_GRID <- file.path(VALIDATION_DIR, "sparsity_threshold_grid.csv")
OUTPUT_RULES <- file.path(VALIDATION_DIR, "preprocessing_rules.csv")
OUTPUT_SUMMARY <- file.path(VALIDATION_DIR, "validation_design_summary.csv")
OUTPUT_CONFIG <- file.path(VALIDATION_DIR, "validation_config.json")
OUTPUT_MANIFEST <- file.path(VALIDATION_DIR, "05_define_rolling_validation_manifest.csv")
OUTPUT_REPORT <- file.path(VALIDATION_DIR, "05_rolling_validation_design_report.md")

CANDIDATE_PROCESSED_DIRS <- unique(c(
  PROCESSED_DIR_ARG,
  PROCESSED_DIR_DEFAULT,
  file.path(PROJECT_ROOT, "processed"),
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
      stop(
        sprintf(
          "Required file not found: %s\nSearched in:\n%s",
          filename,
          paste(candidate_dirs, collapse = "\n")
        ),
        call. = FALSE
      )
    }
    return(NA_character_)
  }
  normalizePath(hit[[1]], mustWork = TRUE)
}

read_csv_utf8 <- function(path) {
  read.csv(
    path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "NaN"),
    fileEncoding = "UTF-8-BOM"
  )
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

json_num_vec <- function(x, digits = 2L) {
  if (length(x) == 0L) return("[]")
  fmt <- paste0("%.", digits, "f")
  paste0("[", paste(sprintf(fmt, x), collapse = ", "), "]")
}

sha256_or_md5 <- function(path) {
  # Base R does not provide SHA-256 everywhere; md5sum is sufficient for manifest
  # traceability in this pipeline script.
  unname(tools::md5sum(path))
}

assert_has_columns <- function(df, cols, file_label) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0L) {
    stop(sprintf("%s is missing required columns: %s", file_label, paste(missing, collapse = ", ")), call. = FALSE)
  }
}

as_logical_vector <- function(x) {
  if (is.logical(x)) return(x)
  x_chr <- tolower(trimws(as.character(x)))
  x_chr %in% c("true", "t", "1", "yes", "y")
}

lookup_row <- function(df, idx) {
  row <- df[df$Quarter_Index == idx, , drop = FALSE]
  if (nrow(row) != 1L) stop(sprintf("Quarter_Index not uniquely found: %s", idx), call. = FALSE)
  row
}

q_label <- function(df, idx) lookup_row(df, idx)$Quarter[[1]]
q_start <- function(df, idx) lookup_row(df, idx)$quarter_start[[1]]
q_year <- function(df, idx) as.integer(lookup_row(df, idx)$Year[[1]])
q_num <- function(df, idx) as.integer(lookup_row(df, idx)$Q[[1]])

index_for_quarter <- function(df, quarter) {
  hit <- df$Quarter_Index[as.character(df$Quarter) == quarter]
  if (length(hit) != 1L) stop(sprintf("Quarter not uniquely found: %s", quarter), call. = FALSE)
  as.integer(hit[[1]])
}

# -----------------------------
# 2. Load and validate inputs
# -----------------------------
ensure_dir(VALIDATION_DIR)

GT_PANEL_PATH <- locate_file(GT_PANEL_FILE, CANDIDATE_PROCESSED_DIRS)
FULL_PANEL_PATH <- locate_file(FULL_PANEL_FILE, CANDIDATE_PROCESSED_DIRS)
KEYWORD_DICTIONARY_PATH <- locate_file(KEYWORD_DICTIONARY_FILE, CANDIDATE_PROCESSED_DIRS)

gt_panel <- read_csv_utf8(GT_PANEL_PATH)
full_panel <- read_csv_utf8(FULL_PANEL_PATH)
keyword_dictionary <- read_csv_utf8(KEYWORD_DICTIONARY_PATH)

required_cols <- c(
  "Quarter", "Year", "Q", "Quarter_Index", "quarter_start",
  "unemployment_rate_nationals", "gt_available", "post_gt_endpoint", "sample_role"
)
assert_has_columns(gt_panel, required_cols, GT_PANEL_FILE)
assert_has_columns(full_panel, required_cols, FULL_PANEL_FILE)

gt_panel <- gt_panel[order(gt_panel$Quarter_Index), , drop = FALSE]
full_panel <- full_panel[order(full_panel$Quarter_Index), , drop = FALSE]
rownames(gt_panel) <- NULL
rownames(full_panel) <- NULL

if (anyDuplicated(gt_panel$Quarter_Index)) stop("Duplicate Quarter_Index values in GT panel.", call. = FALSE)
if (anyDuplicated(full_panel$Quarter_Index)) stop("Duplicate Quarter_Index values in full unemployment panel.", call. = FALSE)
if (any(is.na(gt_panel$unemployment_rate_nationals))) stop("Missing unemployment values in GT overlap panel.", call. = FALSE)
if (any(is.na(full_panel$unemployment_rate_nationals))) stop("Missing unemployment values in full unemployment panel.", call. = FALSE)

if (nrow(gt_panel) != EXPECTED_GT_ROWS || gt_panel$Quarter[[1]] != EXPECTED_GT_START || tail(gt_panel$Quarter, 1) != EXPECTED_GT_END) {
  stop(sprintf("Unexpected GT overlap panel period. Expected %s-%s with %d rows.", EXPECTED_GT_START, EXPECTED_GT_END, EXPECTED_GT_ROWS), call. = FALSE)
}
if (nrow(full_panel) != EXPECTED_FULL_ROWS || tail(full_panel$Quarter, 1) != EXPECTED_FULL_END) {
  stop(sprintf("Unexpected full unemployment panel period. Expected end %s with %d rows.", EXPECTED_FULL_END, EXPECTED_FULL_ROWS), call. = FALSE)
}
if (nrow(keyword_dictionary) != EXPECTED_KEYWORDS) {
  stop(sprintf("Unexpected retained keyword count. Expected %d, found %d.", EXPECTED_KEYWORDS, nrow(keyword_dictionary)), call. = FALSE)
}

initial_train_end_idx <- index_for_quarter(full_panel, INITIAL_TRAIN_END)
gt_start_idx <- min(gt_panel$Quarter_Index)
gt_end_idx <- max(gt_panel$Quarter_Index)
full_end_idx <- max(full_panel$Quarter_Index)

if (initial_train_end_idx <= gt_start_idx || initial_train_end_idx >= gt_end_idx) {
  stop("initial-train-end must lie inside the GT overlap panel before the final GT quarter.", call. = FALSE)
}

# -----------------------------
# 3. Rolling-origin builders
# -----------------------------
make_horizon_plan <- function(horizon) {
  if (horizon == 0L) {
    target_start_idx <- initial_train_end_idx + 1L
    target_end_idx <- gt_end_idx
    role <- "primary_gt_nowcasting"
    label <- "h0_same_quarter_nowcast"
    interpretation <- "Same-quarter nowcast using target-quarter Google Trends only as prediction newdata; target unemployment is withheld."
    gt_alignment <- "same_quarter_x_t"
  } else if (horizon == 1L) {
    target_start_idx <- initial_train_end_idx + 1L
    target_end_idx <- gt_end_idx
    role <- "secondary_one_quarter_ahead_forecast"
    label <- "h1_one_quarter_ahead_forecast"
    interpretation <- "One-quarter-ahead forecast using lagged Google Trends available at the forecast origin; target-quarter Google Trends is not used."
    gt_alignment <- "lag_1_x_t_minus_1"
  } else if (horizon == 2L) {
    target_start_idx <- initial_train_end_idx + 2L
    target_end_idx <- gt_end_idx
    role <- "exploratory_two_quarter_ahead_forecast"
    label <- "h2_two_quarter_ahead_forecast_exploratory"
    interpretation <- "Two-quarter-ahead exploratory forecast using lagged Google Trends available at the forecast origin; future Google Trends is not used."
    gt_alignment <- "lag_2_x_t_minus_2"
  } else {
    stop("Unsupported horizon.", call. = FALSE)
  }

  rows <- list()
  fold_id <- 0L

  for (target_idx in seq.int(target_start_idx, target_end_idx)) {
    fold_id <- fold_id + 1L

    if (horizon == 0L) {
      official_origin_idx <- target_idx - 1L
      prediction_predictor_idx <- target_idx
    } else {
      official_origin_idx <- target_idx - horizon
      prediction_predictor_idx <- official_origin_idx
    }

    train_y_start_idx <- gt_start_idx
    train_y_end_idx <- official_origin_idx
    model_response_start_idx <- gt_start_idx + horizon
    model_response_end_idx <- official_origin_idx
    model_predictor_start_idx <- gt_start_idx
    model_predictor_end_idx <- official_origin_idx - horizon
    n_y_available <- train_y_end_idx - train_y_start_idx + 1L
    n_model_rows <- model_response_end_idx - model_response_start_idx + 1L

    if (n_model_rows <= 0L) next

    rows[[length(rows) + 1L]] <- data.frame(
      fold_id = fold_id,
      origin_id = sprintf("h%d_fold_%03d", horizon, fold_id),
      experiment_role = role,
      design_label = label,
      horizon_quarters = horizon,
      target_index = target_idx,
      target_quarter = q_label(full_panel, target_idx),
      target_year = q_year(full_panel, target_idx),
      target_q = q_num(full_panel, target_idx),
      target_quarter_start = q_start(full_panel, target_idx),
      official_origin_index = official_origin_idx,
      official_origin_quarter = q_label(full_panel, official_origin_idx),
      official_origin_quarter_start = q_start(full_panel, official_origin_idx),
      train_y_start_index = train_y_start_idx,
      train_y_start_quarter = q_label(full_panel, train_y_start_idx),
      train_y_end_index = train_y_end_idx,
      train_y_end_quarter = q_label(full_panel, train_y_end_idx),
      n_y_available_at_origin = n_y_available,
      model_response_start_index = model_response_start_idx,
      model_response_start_quarter = q_label(full_panel, model_response_start_idx),
      model_response_end_index = model_response_end_idx,
      model_response_end_quarter = q_label(full_panel, model_response_end_idx),
      model_predictor_start_index = model_predictor_start_idx,
      model_predictor_start_quarter = q_label(full_panel, model_predictor_start_idx),
      model_predictor_end_index = model_predictor_end_idx,
      model_predictor_end_quarter = q_label(full_panel, model_predictor_end_idx),
      n_model_rows_after_lag_alignment = n_model_rows,
      prediction_predictor_index = prediction_predictor_idx,
      prediction_predictor_quarter = q_label(full_panel, prediction_predictor_idx),
      gt_preprocessing_fit_start_index = model_predictor_start_idx,
      gt_preprocessing_fit_start_quarter = q_label(full_panel, model_predictor_start_idx),
      gt_preprocessing_fit_end_index = model_predictor_end_idx,
      gt_preprocessing_fit_end_quarter = q_label(full_panel, model_predictor_end_idx),
      gt_newdata_index = prediction_predictor_idx,
      gt_newdata_quarter = q_label(full_panel, prediction_predictor_idx),
      uses_target_quarter_gt_as_newdata = horizon == 0L,
      uses_future_gt = FALSE,
      target_y_withheld_from_training = TRUE,
      target_gt_used_for_preprocessing = FALSE,
      gt_alignment = gt_alignment,
      allowed_google_trends_aggregations = paste(ACTIVE_AGGREGATIONS, collapse = ";"),
      allowed_model_families = "seasonal_naive;random_walk;ARIMA_ETS;BSTS_no_GT;BSTS_GT_MA;BSTS_GT_EMA",
      preprocessing_scope = "fit_within_training_predictor_rows_only",
      interpretation = interpretation,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

rolling_h0 <- make_horizon_plan(0L)
rolling_h1 <- make_horizon_plan(1L)
rolling_h2 <- make_horizon_plan(2L)
rolling_all <- rbind(rolling_h0, rolling_h1, rolling_h2)

# -----------------------------
# 4. Post-GT validation block
# -----------------------------
post_rows <- list()
for (target_idx in seq.int(gt_end_idx + 1L, full_end_idx)) {
  post_rows[[length(post_rows) + 1L]] <- data.frame(
    block_id = sprintf("post_gt_%02d", target_idx - gt_end_idx),
    experiment_role = "post_gt_forecast_validation",
    origin_index = gt_end_idx,
    origin_quarter = q_label(full_panel, gt_end_idx),
    origin_quarter_start = q_start(full_panel, gt_end_idx),
    target_index = target_idx,
    target_quarter = q_label(full_panel, target_idx),
    target_year = q_year(full_panel, target_idx),
    target_q = q_num(full_panel, target_idx),
    target_quarter_start = q_start(full_panel, target_idx),
    horizon_from_gt_endpoint = target_idx - gt_end_idx,
    train_y_start_index = gt_start_idx,
    train_y_start_quarter = q_label(full_panel, gt_start_idx),
    train_y_end_index = gt_end_idx,
    train_y_end_quarter = q_label(full_panel, gt_end_idx),
    n_y_available_at_origin = gt_end_idx - gt_start_idx + 1L,
    gt_available_for_target_quarter = FALSE,
    same_quarter_gt_nowcast_allowed = FALSE,
    future_gt_placeholders_allowed = FALSE,
    allowed_gt_end_index = gt_end_idx,
    allowed_gt_end_quarter = q_label(full_panel, gt_end_idx),
    primary_allowed_model_scope = "unemployment_only_forecasts",
    notes = "Use for unemployment-only forecast validation after the Google Trends endpoint; do not create same-quarter GT nowcasts or GT placeholders.",
    stringsAsFactors = FALSE
  )
}
post_gt <- do.call(rbind, post_rows)

# -----------------------------
# 5. Preprocessing rules and summary
# -----------------------------
threshold_interpretation <- function(x) {
  if (x <= 0.30) return("Very strict sparsity screen; keeps only relatively continuous search series.")
  if (x <= 0.50) return("Strict sparsity screen; useful for testing whether results depend on low-zero predictors only.")
  if (x <= 0.70) return("Moderate sparsity screen; balances signal retention and sparse-keyword exclusion.")
  if (x <= 0.80) return("Permissive sensitivity screen; comparable to common sparse-predictor filtering.")
  if (x <= 0.90) return("Primary permissive screen; drops only highly sparse or all-zero training-window predictors.")
  "Very permissive screen; tests whether retaining very sparse predictors affects model performance."
}

threshold_selectivity <- function(x) {
  if (x <= 0.30) return("very strict / smallest predictor set")
  if (x <= 0.50) return("strict / smaller predictor set")
  if (x <= 0.70) return("moderate / medium predictor set")
  if (x <= 0.80) return("permissive / larger predictor set")
  if (x <= 0.90) return("primary permissive / large predictor set")
  "very permissive / largest predictor set"
}

sparsity_threshold_grid <- data.frame(
  threshold_id = sprintf("Z%02d", seq_along(SPARSITY_THRESHOLD_GRID)),
  zero_prop_threshold = SPARSITY_THRESHOLD_GRID,
  threshold_role = ifelse(abs(SPARSITY_THRESHOLD_GRID - ZERO_PROP_THRESHOLD) < 1e-12, "primary", "sensitivity"),
  drop_rule = sprintf("drop predictors with training-window zero proportion > %.2f", SPARSITY_THRESHOLD_GRID),
  expected_selectivity = vapply(SPARSITY_THRESHOLD_GRID, threshold_selectivity, character(1L)),
  interpretation = vapply(SPARSITY_THRESHOLD_GRID, threshold_interpretation, character(1L)),
  fit_scope = "inside each rolling training window only",
  leakage_guardrail = "Do not compute zero proportions on the full sample or validation target.",
  stringsAsFactors = FALSE
)

preprocessing_rules <- data.frame(
  rule_id = c("P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10"),
  rule_name = c(
    "Temporal ordering",
    "Zero-variance filter",
    "Sparsity filter",
    "Standardisation",
    "Missing values",
    "Aggregation isolation",
    "Same-quarter nowcast information set",
    "Lagged-GT forecast information set",
    "Post-GT validation block",
    "SAWA handling"
  ),
  applies_to = c(
    "all validation designs",
    "Google Trends predictors",
    "Google Trends predictors",
    "retained Google Trends predictors",
    "all modelling inputs",
    "MA and EMA Google Trends variants",
    "h = 0",
    "h = 1 and h = 2",
    "2024 Q4 to 2026 Q1",
    "seasonally adjusted GT variant"
  ),
  fit_scope = c(
    "training window only",
    "training predictor rows only",
    "training predictor rows only",
    "training predictor rows only",
    "input audit before model fitting",
    "each model run",
    "unemployment through t-1; GT preprocessing through t-1",
    "lag-aligned training rows only",
    "train through 2024 Q3",
    "deferred"
  ),
  validation_application = c(
    "target quarter held out according to horizon information set",
    "drop same columns from validation newdata",
    "drop same columns from validation newdata",
    "apply training means and standard deviations to validation newdata",
    "stop if missing values are detected in required modelling fields",
    "evaluate MA and EMA in separate model families",
    "use target-quarter GT x_t only as prediction newdata",
    "use only GT observed at the forecast origin; no target/future GT",
    "evaluate unemployment-only forecasts",
    "not included in Step 6 active model design"
  ),
  default_setting = c(
    "required",
    "drop predictors with training sd == 0",
    sprintf("drop predictors with training zero proportion > %.2f", ZERO_PROP_THRESHOLD),
    "z-score retained predictors; leave outcome in percentage points",
    "no imputation, interpolation, or blank-cell filling in validation scripts",
    "do not mix MA and EMA predictors in the same primary GT model",
    "allowed for h0 nowcasting",
    "h1 uses x_T for y_{T+1}; h2 uses x_T for y_{T+2}",
    "no same-quarter GT nowcasts after 2024 Q3",
    "SAWA postponed until MA and EMA rolling validation are stable"
  ),
  sensitivity_settings = c(
    "",
    "",
    sprintf("primary threshold %.2f; sensitivity thresholds %s; complete grid %s", ZERO_PROP_THRESHOLD, paste(sprintf("%.2f", SENSITIVITY_THRESHOLDS), collapse = ", "), paste(sprintf("%.2f", SPARSITY_THRESHOLD_GRID), collapse = ", ")),
    "",
    "",
    "",
    "",
    "",
    "",
    "if added later, estimate seasonal adjustment inside each training window"
  ),
  leakage_guardrail = c(
    "No outcome or transformation from the target or future quarters may enter model fitting.",
    "Do not compute variance on the full sample.",
    "Do not decide sparse predictors using validation or future quarters.",
    "Do not compute means or standard deviations on the full sample.",
    "Do not patch data during model validation.",
    "Avoid model variants that use future-selected aggregation choices.",
    "Target unemployment y_t is withheld; target GT x_t is not used for preprocessing or feature filtering.",
    "Do not use Google Trends from target or future quarters for forecasting horizons.",
    "Do not carry forward, zero-fill, or forecast Google Trends unless a separate explicit GT-forecasting model is introduced.",
    "Do not compute full-sample SAWA for rolling validation."
  ),
  status = c("active", "active", "active", "active", "active", "active", "active", "active", "active", "deferred"),
  stringsAsFactors = FALSE
)

summary_table <- data.frame(
  design_component = c(
    "GT overlap panel",
    "Full unemployment panel",
    "h0 same-quarter nowcast",
    "h1 one-quarter-ahead forecast",
    "h2 two-quarter-ahead forecast",
    "Post-GT validation block",
    "Retained keywords",
    "Sparsity threshold grid"
  ),
  period_start = c(
    q_label(full_panel, gt_start_idx),
    q_label(full_panel, gt_start_idx),
    rolling_h0$target_quarter[[1]],
    rolling_h1$target_quarter[[1]],
    rolling_h2$target_quarter[[1]],
    post_gt$target_quarter[[1]],
    "",
    ""
  ),
  period_end = c(
    q_label(full_panel, gt_end_idx),
    q_label(full_panel, full_end_idx),
    tail(rolling_h0$target_quarter, 1),
    tail(rolling_h1$target_quarter, 1),
    tail(rolling_h2$target_quarter, 1),
    tail(post_gt$target_quarter, 1),
    "",
    ""
  ),
  n_rows = c(
    nrow(gt_panel),
    nrow(full_panel),
    nrow(rolling_h0),
    nrow(rolling_h1),
    nrow(rolling_h2),
    nrow(post_gt),
    nrow(keyword_dictionary),
    nrow(sparsity_threshold_grid)
  ),
  primary_use = c(
    "Google Trends nowcasting/forecast validation where GT exists",
    "Official unemployment outcome series including post-GT validation block",
    "Primary GT nowcasting experiment",
    "Secondary short-horizon forecast using lagged GT",
    "Exploratory short-horizon forecast using lagged GT",
    "Unemployment-only forecasts after GT endpoint",
    "Candidate Google Trends predictors per aggregation method",
    sprintf("Primary %.2f plus sensitivity thresholds %s", ZERO_PROP_THRESHOLD, format_thresholds(SENSITIVITY_THRESHOLDS))
  ),
  stringsAsFactors = FALSE
)

# -----------------------------
# 6. Write outputs
# -----------------------------
write_csv_utf8(rolling_h0, OUTPUT_H0)
write_csv_utf8(rolling_h1, OUTPUT_H1)
write_csv_utf8(rolling_h2, OUTPUT_H2)
write_csv_utf8(rolling_all, OUTPUT_ALL)
write_csv_utf8(post_gt, OUTPUT_POST_GT)
write_csv_utf8(sparsity_threshold_grid, OUTPUT_THRESHOLD_GRID)
write_csv_utf8(preprocessing_rules, OUTPUT_RULES)
write_csv_utf8(summary_table, OUTPUT_SUMMARY)

config_lines <- c(
  "{",
  sprintf('  "script": "05_define_rolling_validation.R",'),
  sprintf('  "step": "Step 6 — Define rolling-validation design",'),
  sprintf('  "generated_at": "%s",', format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  '  "input_files": {',
  '    "gt_overlap_panel": "data/processed/modeling_panel_gt_overlap.csv",',
  '    "full_unemployment_panel": "data/processed/modeling_panel_unemployment_full.csv",',
  '    "keyword_dictionary": "data/processed/keyword_dictionary_clean.csv"',
  '  },',
  '  "sample_contract": {',
  sprintf('    "gt_overlap_start": "%s",', q_label(full_panel, gt_start_idx)),
  sprintf('    "gt_overlap_end": "%s",', q_label(full_panel, gt_end_idx)),
  sprintf('    "gt_overlap_quarters": %d,', nrow(gt_panel)),
  sprintf('    "full_unemployment_start": "%s",', q_label(full_panel, gt_start_idx)),
  sprintf('    "full_unemployment_end": "%s",', q_label(full_panel, full_end_idx)),
  sprintf('    "full_unemployment_quarters": %d,', nrow(full_panel)),
  sprintf('    "post_gt_start": "%s",', post_gt$target_quarter[[1]]),
  sprintf('    "post_gt_end": "%s",', tail(post_gt$target_quarter, 1)),
  sprintf('    "post_gt_quarters": %d,', nrow(post_gt)),
  sprintf('    "retained_keywords": %d,', nrow(keyword_dictionary)),
  sprintf('    "active_aggregation_methods": %s,', json_vec(ACTIVE_AGGREGATIONS)),
  sprintf('    "deferred_aggregation_methods": %s', json_vec(DEFERRED_AGGREGATIONS)),
  '  },',
  '  "rolling_origin_design": {',
  sprintf('    "initial_training_end_quarter": "%s",', INITIAL_TRAIN_END),
  sprintf('    "initial_training_end_index": %d,', initial_train_end_idx),
  sprintf('    "initial_training_y_quarters": %d,', initial_train_end_idx - gt_start_idx + 1L),
  '    "horizons": [',
  sprintf('      {"horizon_quarters": 0, "label": "same-quarter nowcast", "folds": %d, "target_start": "%s", "target_end": "%s", "uses_target_quarter_gt_as_newdata": true, "target_gt_used_for_preprocessing": false},', nrow(rolling_h0), rolling_h0$target_quarter[[1]], tail(rolling_h0$target_quarter, 1)),
  sprintf('      {"horizon_quarters": 1, "label": "one-quarter-ahead forecast", "folds": %d, "target_start": "%s", "target_end": "%s", "uses_target_quarter_gt_as_newdata": false, "gt_design": "lagged predictors only"},', nrow(rolling_h1), rolling_h1$target_quarter[[1]], tail(rolling_h1$target_quarter, 1)),
  sprintf('      {"horizon_quarters": 2, "label": "two-quarter-ahead forecast exploratory", "folds": %d, "target_start": "%s", "target_end": "%s", "uses_target_quarter_gt_as_newdata": false, "gt_design": "lagged predictors only"}', nrow(rolling_h2), rolling_h2$target_quarter[[1]], tail(rolling_h2$target_quarter, 1)),
  '    ]',
  '  },',
  '  "post_gt_forecast_validation": {',
  sprintf('    "origin_quarter": "%s",', q_label(full_panel, gt_end_idx)),
  sprintf('    "target_start": "%s",', post_gt$target_quarter[[1]]),
  sprintf('    "target_end": "%s",', tail(post_gt$target_quarter, 1)),
  sprintf('    "folds": %d,', nrow(post_gt)),
  '    "primary_scope": "unemployment_only_forecasts",',
  '    "same_quarter_gt_nowcasts_allowed": false,',
  '    "future_gt_placeholders_allowed": false',
  '  },',
  '  "preprocessing": {',
  '    "fit_scope": "inside each rolling training window only",',
  '    "zero_variance_filter": true,',
  sprintf('    "zero_proportion_threshold_primary": %.2f,', ZERO_PROP_THRESHOLD),
  sprintf('    "zero_proportion_threshold_sensitivity": %s,', json_num_vec(SENSITIVITY_THRESHOLDS)),
  sprintf('    "zero_proportion_threshold_grid": %s,', json_num_vec(SPARSITY_THRESHOLD_GRID)),
  '    "standardise_predictors": true,',
  '    "standardisation_fit_scope": "training predictor rows only",',
  '    "imputation_or_interpolation": false,',
  '    "global_feature_selection": false,',
  '    "global_scaling": false',
  '  },',
  '  "metrics_to_compute_later": ["MAE", "RMSE", "MAPE", "sMAPE", "95% interval coverage", "average interval width", "interval score"],',
  '  "notes": "This file defines validation design only. It does not fit models or choose predictors globally."',
  "}"
)
write_lines_utf8(config_lines, OUTPUT_CONFIG)

report_lines <- c(
  "# Step 6 — Rolling-validation design report",
  "",
  "## Status",
  "",
  "Step 6 defines the validation design only. It does **not** fit models, select predictors globally, standardise the full sample, impute data, or create future Google Trends placeholders.",
  "",
  "## Input panels",
  "",
  "| Input | Rows | Period | Role |",
  "|---|---:|---|---|",
  sprintf("| `modeling_panel_gt_overlap.csv` | %d | %s–%s | Main Google Trends overlap sample. |", nrow(gt_panel), q_label(full_panel, gt_start_idx), q_label(full_panel, gt_end_idx)),
  sprintf("| `modeling_panel_unemployment_full.csv` | %d | %s–%s | Full official unemployment series. |", nrow(full_panel), q_label(full_panel, gt_start_idx), q_label(full_panel, full_end_idx)),
  sprintf("| `keyword_dictionary_clean.csv` | %d | — | Retained keyword dictionary. |", nrow(keyword_dictionary)),
  "",
  "## Main rolling-origin design",
  "",
  sprintf("The initial training endpoint is fixed at **%s**, giving **%d official unemployment quarters** in the first training window.", INITIAL_TRAIN_END, initial_train_end_idx - gt_start_idx + 1L),
  "",
  "| Design | Target period | Folds | Information-set rule |",
  "|---|---|---:|---|",
  sprintf("| h = 0 same-quarter nowcast | %s–%s | %d | Uses unemployment through `t-1`; uses target-quarter GT `x_t` only as prediction newdata. |", rolling_h0$target_quarter[[1]], tail(rolling_h0$target_quarter, 1), nrow(rolling_h0)),
  sprintf("| h = 1 forecast | %s–%s | %d | Uses unemployment and GT available at the forecast origin; target-quarter GT is not used. |", rolling_h1$target_quarter[[1]], tail(rolling_h1$target_quarter, 1), nrow(rolling_h1)),
  sprintf("| h = 2 forecast | %s–%s | %d | Exploratory two-quarter-ahead design using only GT available at the forecast origin. |", rolling_h2$target_quarter[[1]], tail(rolling_h2$target_quarter, 1), nrow(rolling_h2)),
  "",
  "## Post-GT validation block",
  "",
  sprintf("The post-Google-Trends block is **%s–%s**. These %d quarters are reserved for unemployment-only forecast validation trained through **%s**. Same-quarter Google Trends nowcasts are not allowed in this block because GT predictors are unavailable beyond **%s**.", post_gt$target_quarter[[1]], tail(post_gt$target_quarter, 1), nrow(post_gt), q_label(full_panel, gt_end_idx), q_label(full_panel, gt_end_idx)),
  "",
  "## Preprocessing contract",
  "",
  "All preprocessing must be estimated inside each rolling training window only:",
  "",
  "- zero-variance filtering;",
  "- sparsity filtering;",
  "- standardisation;",
  "- any optional correlation diagnostics or feature screening.",
  "",
  sprintf("The primary sparsity rule is to drop Google Trends predictors with training-window zero proportion above **%.2f**. Sensitivity checks should repeat the analysis at **%s**. The complete modelling grid is therefore **%s**. These thresholds are design settings, not global full-sample feature selection.", ZERO_PROP_THRESHOLD, paste(sprintf("%.2f", SENSITIVITY_THRESHOLDS), collapse = ", "), paste(sprintf("%.2f", SPARSITY_THRESHOLD_GRID), collapse = ", ")),
  "",
  "## Active and deferred aggregation methods",
  "",
  "Active in the Step 6 design:",
  "",
  "```text",
  paste(ACTIVE_AGGREGATIONS, collapse = "\n"),
  "```",
  "",
  "Deferred:",
  "",
  "```text",
  paste(DEFERRED_AGGREGATIONS, collapse = "\n"),
  "```",
  "",
  "SAWA remains deferred because seasonal adjustment must be handled inside each rolling training window if used for validation.",
  "",
  "## Files written",
  "",
  "- `validation_config.json`",
  "- `rolling_origins_h0_nowcast.csv`",
  "- `rolling_origins_h1_forecast.csv`",
  "- `rolling_origins_h2_forecast_exploratory.csv`",
  "- `rolling_origins_all.csv`",
  "- `post_gt_forecast_validation_block.csv`",
  "- `sparsity_threshold_grid.csv`",
  "- `preprocessing_rules.csv`",
  "- `validation_design_summary.csv`",
  "- `05_define_rolling_validation_manifest.csv`",
  "",
  "## Next step",
  "",
  "The next pipeline step should fit benchmark models first, before BSTS with Google Trends:",
  "",
  "```text",
  "06_run_benchmarks.R",
  "```"
)
write_lines_utf8(report_lines, OUTPUT_REPORT)

# -----------------------------
# 7. Manifest
# -----------------------------
output_files <- c(
  OUTPUT_H0,
  OUTPUT_H1,
  OUTPUT_H2,
  OUTPUT_ALL,
  OUTPUT_POST_GT,
  OUTPUT_RULES,
  OUTPUT_THRESHOLD_GRID,
  OUTPUT_SUMMARY,
  OUTPUT_CONFIG,
  OUTPUT_REPORT
)

manifest <- data.frame(
  output_key = c("h0", "h1", "h2", "all", "post", "rules", "threshold_grid", "summary", "config", "report"),
  file_name = basename(output_files),
  relative_path = file.path("data", "processed", "validation", basename(output_files)),
  description = c(
    sprintf("%d rows x %d columns", nrow(rolling_h0), ncol(rolling_h0)),
    sprintf("%d rows x %d columns", nrow(rolling_h1), ncol(rolling_h1)),
    sprintf("%d rows x %d columns", nrow(rolling_h2), ncol(rolling_h2)),
    sprintf("%d rows x %d columns", nrow(rolling_all), ncol(rolling_all)),
    sprintf("%d rows x %d columns", nrow(post_gt), ncol(post_gt)),
    sprintf("%d rows x %d columns", nrow(preprocessing_rules), ncol(preprocessing_rules)),
    sprintf("%d rows x %d columns", nrow(sparsity_threshold_grid), ncol(sparsity_threshold_grid)),
    sprintf("%d rows x %d columns", nrow(summary_table), ncol(summary_table)),
    "JSON configuration",
    "Markdown report"
  ),
  checksum_md5 = vapply(output_files, sha256_or_md5, character(1L)),
  stringsAsFactors = FALSE
)
write_csv_utf8(manifest, OUTPUT_MANIFEST)

message("Step 6 rolling-validation design complete.")
message(sprintf("Validation outputs written to: %s", VALIDATION_DIR))
message(sprintf("h0 folds: %d", nrow(rolling_h0)))
message(sprintf("h1 folds: %d", nrow(rolling_h1)))
message(sprintf("h2 folds: %d", nrow(rolling_h2)))
message(sprintf("post-GT validation quarters: %d", nrow(post_gt)))
message(sprintf("sparsity threshold grid: %s", paste(sprintf("%.2f", SPARSITY_THRESHOLD_GRID), collapse = ", ")))
