#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# 02_build_quarterly_google_trends.R
# Step 3: Build quarterly Google Trends aggregates from the clean monthly file
# -----------------------------------------------------------------------------
# Purpose:
#   Generate quarterly Google Trends predictors from google_trends_monthly_clean.csv.
#
# Inputs:
#   data/processed/google_trends_monthly_clean.csv
#   data/processed/keyword_dictionary_clean.csv       # optional validation input
#
# Outputs:
#   google_trends_quarterly_MA.csv
#   google_trends_quarterly_EMA.csv
#   02_build_quarterly_google_trends_manifest.csv
#   02_build_quarterly_google_trends_report.md
#
# Agreed pipeline rules:
#   - Raw inputs remain frozen and are not modified by this script.
#   - This script starts from the Step 2 clean monthly file.
#   - The two excluded raw columns, "شغل حلال" and "فلسنا", should already be
#     absent because they were removed in Step 2.
#   - No missing-value filling, imputation, interpolation, or source correction
#     is performed here.
#   - Quarterly aggregates are regenerated from monthly values, not reused from
#     old quarterly files.
#   - Phase 1 generates MA and EMA only. SAWA is intentionally deferred until the
#     rolling-validation pipeline is stable, because seasonal adjustment must be
#     handled carefully to avoid leakage.
#
# Aggregation definitions:
#   MA for keyword k in quarter q:
#       MA_{k,q} = mean(x_{k,m1}, x_{k,m2}, x_{k,m3})
#
#   EMA for keyword k in quarter q with alpha = 0.5 by default:
#       EMA_{k,q} = sum(w_m * x_{k,m}) / sum(w_m)
#       w_m = alpha * (1 - alpha)^(3 - m), for m = 1,2,3 in chronological order.
#   Thus, for alpha = 0.5, the three months receive normalised weights:
#       month 1 = 1/7, month 2 = 2/7, month 3 = 4/7.
#   The most recent month in the quarter receives the largest weight.
#
# Usage from project root:
#   Rscript scripts/02_build_quarterly_google_trends.R
#
# Optional arguments:
#   Rscript scripts/02_build_quarterly_google_trends.R --processed-dir=data/processed
#   Rscript scripts/02_build_quarterly_google_trends.R --out-dir=data/processed
#   Rscript scripts/02_build_quarterly_google_trends.R --alpha=0.5
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
  if (length(hit_eq) > 0L) {
    return(sub(key_eq, "", hit_eq[[1]], fixed = TRUE))
  }

  hit_pos <- which(args == key_dash)
  if (length(hit_pos) > 0L && hit_pos[[1]] < length(args)) {
    return(args[[hit_pos[[1]] + 1L]])
  }

  default
}

PROJECT_ROOT <- normalizePath(get_arg("project-root", Sys.getenv("BSTS_PROJECT_ROOT", unset = getwd())), mustWork = FALSE)
PROCESSED_DIR_DEFAULT <- file.path(PROJECT_ROOT, "data", "processed")
PROCESSED_DIR_ARG <- get_arg("processed-dir", Sys.getenv("BSTS_PROCESSED_DIR", unset = PROCESSED_DIR_DEFAULT))
OUT_DIR <- normalizePath(get_arg("out-dir", Sys.getenv("BSTS_PROCESSED_DIR", unset = PROCESSED_DIR_DEFAULT)), mustWork = FALSE)

MONTHLY_CLEAN_FILE <- "google_trends_monthly_clean.csv"
KEYWORD_DICT_CLEAN_FILE <- "keyword_dictionary_clean.csv"

EXPECTED_KEYWORD_COLS <- 71L
EXPECTED_QUARTERS <- 59L
EXPECTED_MONTH_START <- as.Date("2010-01-01")
EXPECTED_MONTH_END <- as.Date("2024-09-01")
EXPECTED_MONTHS <- seq.Date(EXPECTED_MONTH_START, EXPECTED_MONTH_END, by = "month")
EXPECTED_QUARTER_START <- "2010-1"
EXPECTED_QUARTER_END <- "2024-3"

ALPHA <- suppressWarnings(as.numeric(get_arg("alpha", "0.5")))
if (length(ALPHA) != 1L || is.na(ALPHA) || ALPHA <= 0 || ALPHA > 1) {
  stop("--alpha must be numeric and satisfy 0 < alpha <= 1.", call. = FALSE)
}

ROUND_DIGITS <- as.integer(get_arg("round-digits", "6"))
if (is.na(ROUND_DIGITS) || ROUND_DIGITS < 0L) {
  stop("--round-digits must be a non-negative integer.", call. = FALSE)
}

OUTPUT_MA_FILE <- file.path(OUT_DIR, "google_trends_quarterly_MA.csv")
OUTPUT_EMA_FILE <- file.path(OUT_DIR, "google_trends_quarterly_EMA.csv")
OUTPUT_MANIFEST_FILE <- file.path(OUT_DIR, "02_build_quarterly_google_trends_manifest.csv")
OUTPUT_REPORT_FILE <- file.path(OUT_DIR, "02_build_quarterly_google_trends_report.md")

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
locate_file <- function(filename, candidate_dirs = CANDIDATE_PROCESSED_DIRS, required = TRUE) {
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

is_blank <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

format_values <- function(x, max_items = 12L) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  if (length(x) == 0L) return("none")
  if (length(x) > max_items) {
    paste0(paste(x[seq_len(max_items)], collapse = "; "), sprintf("; ... (+%d more)", length(x) - max_items))
  } else {
    paste(x, collapse = "; ")
  }
}

parse_month_date <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr <- ifelse(grepl("^[0-9]{4}-[0-9]{1,2}$", x_chr), paste0(x_chr, "-01"), x_chr)
  formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%m-%d-%Y")
  out <- rep(as.Date(NA), length(x_chr))

  for (fmt in formats) {
    idx <- which(is.na(out) & !is_blank(x_chr))
    if (length(idx) > 0L) {
      parsed <- as.Date(x_chr[idx], format = fmt)
      good <- !is.na(parsed)
      if (any(good)) out[idx[good]] <- parsed[good]
    }
  }

  as.Date(ifelse(is.na(out), NA_character_, paste0(format(out, "%Y-%m"), "-01")))
}

month_to_quarter <- function(dates) {
  year <- as.integer(format(dates, "%Y"))
  month <- as.integer(format(dates, "%m"))
  quarter <- ((month - 1L) %/% 3L) + 1L
  paste0(year, "-", quarter)
}

quarter_year <- function(quarter_label) {
  as.integer(sub("-.*$", "", quarter_label))
}

quarter_number <- function(quarter_label) {
  as.integer(sub("^.*-", "", quarter_label))
}

as_numeric_vector <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  suppressWarnings(as.numeric(x_chr))
}

count_nonnumeric_cells <- function(raw_df, numeric_df) {
  total <- 0L
  for (nm in names(raw_df)) {
    total <- total + sum(!is_blank(raw_df[[nm]]) & is.na(numeric_df[[nm]]))
  }
  as.integer(total)
}

add_manifest_row <- function(manifest, item, value, details = "") {
  rbind(
    manifest,
    data.frame(
      item = as.character(item),
      value = as.character(value),
      details = as.character(details),
      stringsAsFactors = FALSE
    )
  )
}

stop_with_report <- function(message_text, manifest, report_extra = character()) {
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  manifest <- add_manifest_row(manifest, "status", "FAIL", message_text)
  write_csv_utf8(manifest, OUTPUT_MANIFEST_FILE)
  report_lines <- c(
    "# Step 3 quarterly Google Trends aggregation",
    "",
    paste0("Generated on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "Overall status: **FAIL**",
    "",
    "## Reason",
    "",
    message_text,
    "",
    report_extra,
    "",
    "## Outputs written",
    "",
    "- `02_build_quarterly_google_trends_manifest.csv`",
    "- `02_build_quarterly_google_trends_report.md`"
  )
  write_lines_utf8(report_lines, OUTPUT_REPORT_FILE)
  stop(message_text, call. = FALSE)
}

weighted_ema_quarter <- function(values, alpha = 0.5) {
  n <- length(values)
  weights <- alpha * (1 - alpha)^((n:1) - 1L)
  sum(weights * values) / sum(weights)
}

# -----------------------------
# 2. Locate and read inputs
# -----------------------------
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
manifest <- data.frame(item = character(), value = character(), details = character(), stringsAsFactors = FALSE)

path_monthly <- locate_file(MONTHLY_CLEAN_FILE, required = TRUE)
path_dict <- locate_file(KEYWORD_DICT_CLEAN_FILE, required = FALSE)
PROCESSED_DIR_USED <- dirname(path_monthly)

manifest <- add_manifest_row(manifest, "project_root", PROJECT_ROOT)
manifest <- add_manifest_row(manifest, "processed_dir_used", PROCESSED_DIR_USED)
manifest <- add_manifest_row(manifest, "out_dir", OUT_DIR)
manifest <- add_manifest_row(manifest, "monthly_clean_file", path_monthly)
manifest <- add_manifest_row(manifest, "keyword_dictionary_clean_file", ifelse(is.na(path_dict), "not found; skipped optional dictionary validation", path_dict))
manifest <- add_manifest_row(manifest, "aggregation_methods", "MA; EMA", "SAWA deferred intentionally.")
manifest <- add_manifest_row(manifest, "ema_alpha", ALPHA, "Default alpha = 0.5.")
manifest <- add_manifest_row(manifest, "round_digits", ROUND_DIGITS)
manifest <- add_manifest_row(manifest, "missing_value_treatment", "none", "Script fails if retained monthly keyword values are missing.")

monthly <- read_csv_utf8(path_monthly)
keyword_dict <- NULL
if (!is.na(path_dict)) {
  keyword_dict <- read_csv_utf8(path_dict)
}

# -----------------------------
# 3. Validate clean monthly file
# -----------------------------
required_metadata <- c("Date", "Year", "Month", "Quarter")
missing_metadata <- setdiff(required_metadata, names(monthly))
if (length(missing_metadata) > 0L) {
  stop_with_report(
    paste0("Clean monthly file is missing metadata columns: ", format_values(missing_metadata)),
    manifest
  )
}

keyword_cols <- setdiff(names(monthly), required_metadata)
if (length(keyword_cols) != EXPECTED_KEYWORD_COLS) {
  stop_with_report(
    sprintf("Clean monthly file has %d keyword columns; expected %d.", length(keyword_cols), EXPECTED_KEYWORD_COLS),
    manifest
  )
}

monthly_dates <- parse_month_date(monthly$Date)
if (any(is.na(monthly_dates))) {
  bad <- which(is.na(monthly_dates))
  stop_with_report(
    paste0("Clean monthly file has unparsable Date values at rows: ", format_values(bad)),
    manifest
  )
}

missing_months <- setdiff(as.character(EXPECTED_MONTHS), as.character(monthly_dates))
extra_months <- setdiff(as.character(monthly_dates), as.character(EXPECTED_MONTHS))
duplicate_months <- unique(as.character(monthly_dates[duplicated(monthly_dates)]))

if (nrow(monthly) != length(EXPECTED_MONTHS) || length(missing_months) > 0L || length(extra_months) > 0L || length(duplicate_months) > 0L) {
  stop_with_report(
    "Clean monthly file does not match the expected complete monthly period 2010-01 to 2024-09.",
    manifest,
    c(
      paste0("Observed rows: ", nrow(monthly)),
      paste0("Expected rows: ", length(EXPECTED_MONTHS)),
      paste0("Missing months: ", format_values(missing_months)),
      paste0("Extra months: ", format_values(extra_months)),
      paste0("Duplicate months: ", format_values(duplicate_months))
    )
  )
}

if (!all(order(monthly_dates) == seq_along(monthly_dates))) {
  stop_with_report("Clean monthly rows are not sorted chronologically.", manifest)
}

computed_quarter <- month_to_quarter(monthly_dates)
if (!all(as.character(monthly$Quarter) == computed_quarter)) {
  bad <- which(as.character(monthly$Quarter) != computed_quarter)
  stop_with_report(
    paste0("Clean monthly Quarter values do not match Date-derived quarters at rows: ", format_values(bad)),
    manifest
  )
}

keyword_raw <- monthly[keyword_cols]
keyword_numeric <- as.data.frame(lapply(keyword_raw, as_numeric_vector), check.names = FALSE, stringsAsFactors = FALSE)

nonnumeric_count <- count_nonnumeric_cells(keyword_raw, keyword_numeric)
missing_count <- sum(is.na(as.matrix(keyword_numeric)))
out_of_range_count <- sum(as.matrix(keyword_numeric) < 0 | as.matrix(keyword_numeric) > 100, na.rm = TRUE)

if (nonnumeric_count > 0L) {
  stop_with_report(sprintf("Clean monthly keyword columns contain %d nonnumeric nonblank cells.", nonnumeric_count), manifest)
}

if (missing_count > 0L) {
  missing_locations <- character()
  for (nm in names(keyword_numeric)) {
    idx <- which(is.na(keyword_numeric[[nm]]))
    if (length(idx) > 0L) {
      missing_locations <- c(missing_locations, paste0(nm, " @ ", format(monthly_dates[idx], "%Y-%m-%d")))
    }
  }
  stop_with_report(
    sprintf("Clean monthly keyword columns contain %d missing value(s). No filling or imputation is performed by Step 3.", missing_count),
    manifest,
    c("Missing retained keyword values found:", paste0("- ", format_values(missing_locations, max_items = 25L)))
  )
}

if (out_of_range_count > 0L) {
  stop_with_report(sprintf("Clean monthly keyword columns contain %d values outside the expected 0-100 range.", out_of_range_count), manifest)
}

if (!is.null(keyword_dict) && all(c("Clean_Column", "Retained_For_Analysis") %in% names(keyword_dict))) {
  expected_from_dict <- as.character(keyword_dict$Clean_Column[keyword_dict$Retained_For_Analysis %in% c(TRUE, "TRUE", "True", "true", 1, "1")])
  missing_from_monthly <- setdiff(expected_from_dict, keyword_cols)
  extra_in_monthly <- setdiff(keyword_cols, expected_from_dict)
  if (length(missing_from_monthly) > 0L || length(extra_in_monthly) > 0L) {
    stop_with_report(
      "Clean monthly keyword columns do not align with keyword_dictionary_clean.csv.",
      manifest,
      c(
        paste0("Dictionary clean columns missing from monthly file: ", format_values(missing_from_monthly)),
        paste0("Monthly keyword columns not in dictionary clean file: ", format_values(extra_in_monthly))
      )
    )
  }
}

# -----------------------------
# 4. Build quarterly aggregates
# -----------------------------
monthly_for_aggregation <- monthly
monthly_for_aggregation$Date <- monthly_dates
monthly_for_aggregation$Year <- as.integer(format(monthly_dates, "%Y"))
monthly_for_aggregation$Month <- as.integer(format(monthly_dates, "%m"))
monthly_for_aggregation$Quarter <- computed_quarter
monthly_for_aggregation[keyword_cols] <- keyword_numeric

quarter_labels <- unique(monthly_for_aggregation$Quarter)
if (length(quarter_labels) != EXPECTED_QUARTERS) {
  stop_with_report(
    sprintf("Clean monthly file yields %d quarters; expected %d.", length(quarter_labels), EXPECTED_QUARTERS),
    manifest
  )
}

if (head(quarter_labels, 1L) != EXPECTED_QUARTER_START || tail(quarter_labels, 1L) != EXPECTED_QUARTER_END) {
  stop_with_report(
    sprintf(
      "Quarter coverage is %s to %s; expected %s to %s.",
      head(quarter_labels, 1L), tail(quarter_labels, 1L), EXPECTED_QUARTER_START, EXPECTED_QUARTER_END
    ),
    manifest
  )
}

ma_rows <- list()
ema_rows <- list()
quarter_month_counts <- integer(length(quarter_labels))

for (i in seq_along(quarter_labels)) {
  q_label <- quarter_labels[[i]]
  q_data <- monthly_for_aggregation[monthly_for_aggregation$Quarter == q_label, ]
  q_data <- q_data[order(q_data$Date), ]
  quarter_month_counts[[i]] <- nrow(q_data)

  if (nrow(q_data) != 3L) {
    stop_with_report(sprintf("Quarter %s has %d monthly rows; expected 3.", q_label, nrow(q_data)), manifest)
  }

  expected_q <- paste0(q_data$Year[[1]], "-", ((q_data$Month[[1]] - 1L) %/% 3L) + 1L)
  if (!all(q_data$Quarter == expected_q)) {
    stop_with_report(sprintf("Quarter %s contains months that do not belong to a single calendar quarter.", q_label), manifest)
  }

  numeric_mat <- as.matrix(q_data[keyword_cols])
  ma_values <- colMeans(numeric_mat)
  ema_values <- apply(numeric_mat, 2L, weighted_ema_quarter, alpha = ALPHA)

  ma_rows[[i]] <- c(
    Quarter = q_label,
    Year = as.character(quarter_year(q_label)),
    Q = as.character(quarter_number(q_label)),
    as.character(round(ma_values, ROUND_DIGITS))
  )
  ema_rows[[i]] <- c(
    Quarter = q_label,
    Year = as.character(quarter_year(q_label)),
    Q = as.character(quarter_number(q_label)),
    as.character(round(ema_values, ROUND_DIGITS))
  )
}

output_cols <- c("Quarter", "Year", "Q", keyword_cols)
ma_df <- as.data.frame(do.call(rbind, ma_rows), stringsAsFactors = FALSE, check.names = FALSE)
ema_df <- as.data.frame(do.call(rbind, ema_rows), stringsAsFactors = FALSE, check.names = FALSE)
names(ma_df) <- output_cols
names(ema_df) <- output_cols

ma_df$Year <- as.integer(ma_df$Year)
ma_df$Q <- as.integer(ma_df$Q)
ema_df$Year <- as.integer(ema_df$Year)
ema_df$Q <- as.integer(ema_df$Q)
for (nm in keyword_cols) {
  ma_df[[nm]] <- as.numeric(ma_df[[nm]])
  ema_df[[nm]] <- as.numeric(ema_df[[nm]])
}

ma_missing <- sum(is.na(as.matrix(ma_df[keyword_cols])))
ema_missing <- sum(is.na(as.matrix(ema_df[keyword_cols])))
ma_out_of_range <- sum(as.matrix(ma_df[keyword_cols]) < 0 | as.matrix(ma_df[keyword_cols]) > 100, na.rm = TRUE)
ema_out_of_range <- sum(as.matrix(ema_df[keyword_cols]) < 0 | as.matrix(ema_df[keyword_cols]) > 100, na.rm = TRUE)

if (ma_missing > 0L || ema_missing > 0L || ma_out_of_range > 0L || ema_out_of_range > 0L) {
  stop_with_report(
    "Quarterly aggregation produced missing or out-of-range values.",
    manifest,
    c(
      paste0("MA missing: ", ma_missing),
      paste0("EMA missing: ", ema_missing),
      paste0("MA out-of-range: ", ma_out_of_range),
      paste0("EMA out-of-range: ", ema_out_of_range)
    )
  )
}

if (!all(quarter_month_counts == 3L)) {
  stop_with_report("Not all quarters contain exactly three monthly observations.", manifest)
}

# -----------------------------
# 5. Write outputs
# -----------------------------
manifest <- add_manifest_row(manifest, "status", "PASS", "Quarterly MA and EMA Google Trends aggregates generated successfully.")
manifest <- add_manifest_row(manifest, "monthly_rows_input", nrow(monthly), "Expected 177 monthly rows.")
manifest <- add_manifest_row(manifest, "quarterly_rows_output", nrow(ma_df), "Expected 59 quarterly rows.")
manifest <- add_manifest_row(manifest, "quarter_range", paste0(head(ma_df$Quarter, 1L), " to ", tail(ma_df$Quarter, 1L)))
manifest <- add_manifest_row(manifest, "metadata_columns_per_output", 3L, "Quarter, Year, Q.")
manifest <- add_manifest_row(manifest, "keyword_columns_per_output", length(keyword_cols))
manifest <- add_manifest_row(manifest, "months_per_quarter", "3 for every quarter")
manifest <- add_manifest_row(manifest, "ma_missing_count", ma_missing)
manifest <- add_manifest_row(manifest, "ema_missing_count", ema_missing)
manifest <- add_manifest_row(manifest, "ma_out_of_range_count", ma_out_of_range)
manifest <- add_manifest_row(manifest, "ema_out_of_range_count", ema_out_of_range)
manifest <- add_manifest_row(manifest, "sawa_status", "deferred", "SAWA is not generated in Step 3 Phase 1.")

write_csv_utf8(ma_df, OUTPUT_MA_FILE)
write_csv_utf8(ema_df, OUTPUT_EMA_FILE)
write_csv_utf8(manifest, OUTPUT_MANIFEST_FILE)

report_lines <- c(
  "# Step 3 quarterly Google Trends aggregation",
  "",
  paste0("Generated on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "Overall status: **PASS**",
  "",
  "## Inputs",
  "",
  paste0("- Monthly clean file: `", basename(path_monthly), "`."),
  ifelse(is.na(path_dict), "- Keyword dictionary clean file: not found; optional validation skipped.", paste0("- Keyword dictionary clean file: `", basename(path_dict), "`.")),
  "",
  "## Aggregation outputs",
  "",
  paste0("- `", basename(OUTPUT_MA_FILE), "`: quarterly mean aggregation."),
  paste0("- `", basename(OUTPUT_EMA_FILE), "`: quarterly exponentially weighted aggregation with alpha = ", ALPHA, "."),
  "",
  "## Output structure",
  "",
  paste0("- Rows per output: ", nrow(ma_df), " quarters."),
  paste0("- Period: ", head(ma_df$Quarter, 1L), " to ", tail(ma_df$Quarter, 1L), "."),
  "- Metadata columns: `Quarter`, `Year`, `Q`.",
  paste0("- Keyword columns: ", length(keyword_cols), "."),
  "- Every quarter is based on exactly three monthly observations.",
  "- No missing-value filling, imputation, interpolation, or source correction was performed.",
  "",
  "## EMA weighting rule",
  "",
  paste0("- Alpha: ", ALPHA, "."),
  "- Monthly order inside a quarter is chronological.",
  "- For alpha = 0.5, normalised weights are 1/7, 2/7, and 4/7 for months 1, 2, and 3 respectively.",
  "- The most recent month receives the largest weight.",
  "",
  "## SAWA",
  "",
  "- SAWA is intentionally deferred until the rolling-validation pipeline is stable.",
  "- This avoids introducing a seasonal-adjustment step that could leak future information if estimated outside the validation folds.",
  "",
  "## Output files",
  "",
  "- `google_trends_quarterly_MA.csv`",
  "- `google_trends_quarterly_EMA.csv`",
  "- `02_build_quarterly_google_trends_manifest.csv`",
  "- `02_build_quarterly_google_trends_report.md`"
)
write_lines_utf8(report_lines, OUTPUT_REPORT_FILE)

message("Step 3 quarterly Google Trends aggregation complete.")
message("Input directory used: ", PROCESSED_DIR_USED)
message("Output directory: ", OUT_DIR)
message("MA output: ", OUTPUT_MA_FILE)
message("EMA output: ", OUTPUT_EMA_FILE)
message("Rows: ", nrow(ma_df), " | Keyword columns: ", length(keyword_cols), " | EMA alpha: ", ALPHA)

invisible(list(MA = ma_df, EMA = ema_df))
