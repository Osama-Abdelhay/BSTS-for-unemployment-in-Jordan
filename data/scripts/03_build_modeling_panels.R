#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# 03_build_modeling_panels.R
# Step 4: Build two modeling panels
# -----------------------------------------------------------------------------
# Purpose:
#   Construct the two canonical quarterly modeling panels used by the clean
#   unemployment nowcasting/forecasting pipeline.
#
# Inputs:
#   data/raw/Jordanian Unemployment Rate 2010- Q1 2026.csv
#   data/processed/google_trends_quarterly_MA.csv
#   data/processed/google_trends_quarterly_EMA.csv
#
# Outputs:
#   unemployment_quarterly_clean.csv
#   modeling_panel_gt_overlap.csv
#   modeling_panel_unemployment_full.csv
#   03_build_modeling_panels_manifest.csv
#   03_build_modeling_panels_report.md
#
# Agreed pipeline rules:
#   - Raw files remain frozen and are not modified by this script.
#   - Google Trends predictors are available only through 2024 Q3.
#   - Official unemployment values after 2024 Q3 are retained only for
#     unemployment-only forecast validation, not for same-quarter GT nowcasting.
#   - Step 3 quarterly MA and EMA files are used as the only GT aggregate inputs.
#   - SAWA is intentionally deferred until the rolling-validation pipeline is
#     stable.
#   - This script performs no missing-value filling, imputation, interpolation,
#     or source correction.
#
# Panel definitions:
#   1. modeling_panel_gt_overlap.csv
#      Period: 2010 Q1 to 2024 Q3.
#      Content: unemployment target + MA__Keyword_* + EMA__Keyword_* predictors.
#      Use: main retrospective same-quarter Google Trends nowcasting analysis.
#
#   2. modeling_panel_unemployment_full.csv
#      Period: 2010 Q1 to 2026 Q1.
#      Content: unemployment target + sample flags only.
#      Use: full official outcome series and post-GT unemployment-only forecast
#           validation from 2024 Q4 to 2026 Q1.
#
# Usage from project root:
#   Rscript scripts/03_build_modeling_panels.R
#
# Optional arguments:
#   Rscript scripts/03_build_modeling_panels.R --raw-dir=data/raw --processed-dir=data/processed --out-dir=data/processed
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
RAW_DIR_DEFAULT <- file.path(PROJECT_ROOT, "data", "raw")
PROCESSED_DIR_DEFAULT <- file.path(PROJECT_ROOT, "data", "processed")

RAW_DIR_ARG <- get_arg("raw-dir", Sys.getenv("BSTS_RAW_DIR", unset = RAW_DIR_DEFAULT))
PROCESSED_DIR_ARG <- get_arg("processed-dir", Sys.getenv("BSTS_PROCESSED_DIR", unset = PROCESSED_DIR_DEFAULT))
OUT_DIR <- normalizePath(get_arg("out-dir", Sys.getenv("BSTS_PROCESSED_DIR", unset = PROCESSED_DIR_DEFAULT)), mustWork = FALSE)

UNEMPLOYMENT_FILE <- "Jordanian Unemployment Rate 2010- Q1 2026.csv"
GT_MA_FILE <- "google_trends_quarterly_MA.csv"
GT_EMA_FILE <- "google_trends_quarterly_EMA.csv"

EXPECTED_UNEMP_START <- "2010-1"
EXPECTED_UNEMP_END <- "2026-1"
EXPECTED_UNEMP_QUARTERS <- 65L
EXPECTED_GT_START <- "2010-1"
EXPECTED_GT_END <- "2024-3"
EXPECTED_GT_QUARTERS <- 59L
EXPECTED_KEYWORD_COLS <- 71L
GT_ENDPOINT <- "2024-3"
POST_GT_START <- "2024-4"
POST_GT_END <- "2026-1"

OUTPUT_UNEMP_CLEAN <- file.path(OUT_DIR, "unemployment_quarterly_clean.csv")
OUTPUT_GT_PANEL <- file.path(OUT_DIR, "modeling_panel_gt_overlap.csv")
OUTPUT_UNEMP_PANEL <- file.path(OUT_DIR, "modeling_panel_unemployment_full.csv")
OUTPUT_MANIFEST <- file.path(OUT_DIR, "03_build_modeling_panels_manifest.csv")
OUTPUT_REPORT <- file.path(OUT_DIR, "03_build_modeling_panels_report.md")

CANDIDATE_RAW_DIRS <- unique(c(
  RAW_DIR_ARG,
  RAW_DIR_DEFAULT,
  file.path(PROJECT_ROOT, "raw"),
  PROJECT_ROOT,
  getwd(),
  "/mnt/data"
))

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

is_blank <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

as_numeric_vector <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  suppressWarnings(as.numeric(x_chr))
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

parse_quarter <- function(q) {
  q_chr <- trimws(as.character(q))
  q_chr <- gsub("Q", "", q_chr, fixed = TRUE)
  q_chr <- gsub("q", "", q_chr, fixed = TRUE)
  q_chr <- gsub(" ", "", q_chr, fixed = TRUE)
  parts <- strsplit(q_chr, "-", fixed = TRUE)
  valid <- vapply(parts, length, integer(1L)) == 2L
  year <- rep(NA_integer_, length(q_chr))
  quarter <- rep(NA_integer_, length(q_chr))
  year[valid] <- suppressWarnings(as.integer(vapply(parts[valid], `[`, character(1L), 1L)))
  quarter[valid] <- suppressWarnings(as.integer(vapply(parts[valid], `[`, character(1L), 2L)))
  data.frame(
    Quarter = ifelse(!is.na(year) & !is.na(quarter), paste0(year, "-", quarter), NA_character_),
    Year = year,
    Q = quarter,
    stringsAsFactors = FALSE
  )
}

quarter_order_value <- function(year, q) {
  as.integer((year - min(year, na.rm = TRUE)) * 4L + q)
}

quarter_sequence <- function(start_label, end_label) {
  s <- parse_quarter(start_label)
  e <- parse_quarter(end_label)
  if (any(is.na(s$Year), is.na(s$Q), is.na(e$Year), is.na(e$Q))) {
    stop("Invalid start/end quarter label.", call. = FALSE)
  }
  out <- character(0L)
  y <- s$Year[[1]]
  q <- s$Q[[1]]
  repeat {
    out <- c(out, paste0(y, "-", q))
    if (y == e$Year[[1]] && q == e$Q[[1]]) break
    q <- q + 1L
    if (q == 5L) {
      q <- 1L
      y <- y + 1L
    }
  }
  out
}

quarter_start_date <- function(year, q) {
  month <- (q - 1L) * 3L + 1L
  as.Date(sprintf("%04d-%02d-01", year, month))
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
  write_csv_utf8(manifest, OUTPUT_MANIFEST)
  report_lines <- c(
    "# Step 4 Modeling Panel Build Report",
    "",
    "## Status",
    "",
    "FAIL",
    "",
    "## Reason",
    "",
    message_text,
    "",
    report_extra
  )
  write_lines_utf8(report_lines, OUTPUT_REPORT)
  stop(message_text, call. = FALSE)
}

check_no_duplicates <- function(labels, object_name, manifest) {
  dup <- unique(labels[duplicated(labels)])
  if (length(dup) > 0L) {
    stop_with_report(
      sprintf("Duplicate quarter labels found in %s: %s", object_name, format_values(dup)),
      manifest
    )
  }
  invisible(TRUE)
}

# -----------------------------
# 2. Locate inputs
# -----------------------------
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
manifest <- data.frame(item = character(), value = character(), details = character(), stringsAsFactors = FALSE)
manifest <- add_manifest_row(manifest, "script", "03_build_modeling_panels.R")
manifest <- add_manifest_row(manifest, "raw_rule", "frozen", "Raw files are read only and never modified.")
manifest <- add_manifest_row(manifest, "gt_endpoint", GT_ENDPOINT, "Google Trends predictors are available through this quarter only.")
manifest <- add_manifest_row(manifest, "post_gt_validation_block", paste0(POST_GT_START, " to ", POST_GT_END), "Unemployment-only forecast validation period.")
manifest <- add_manifest_row(manifest, "sawa_status", "deferred", "SAWA is not included in Step 4 Phase 1 panels.")

unemp_path <- locate_file(UNEMPLOYMENT_FILE, CANDIDATE_RAW_DIRS)
ma_path <- locate_file(GT_MA_FILE, CANDIDATE_PROCESSED_DIRS)
ema_path <- locate_file(GT_EMA_FILE, CANDIDATE_PROCESSED_DIRS)

manifest <- add_manifest_row(manifest, "input_unemployment", unemp_path)
manifest <- add_manifest_row(manifest, "input_google_trends_MA", ma_path)
manifest <- add_manifest_row(manifest, "input_google_trends_EMA", ema_path)

# -----------------------------
# 3. Read and validate unemployment data
# -----------------------------
unemp_raw <- read_csv_utf8(unemp_path)
names(unemp_raw) <- trimws(names(unemp_raw))

if (!("Quarter" %in% names(unemp_raw))) {
  stop_with_report("Unemployment file must contain a Quarter column.", manifest)
}

candidate_value_cols <- setdiff(names(unemp_raw), "Quarter")
if (length(candidate_value_cols) < 1L) {
  stop_with_report("Unemployment file must contain one numeric unemployment-rate column in addition to Quarter.", manifest)
}

numeric_candidates <- lapply(candidate_value_cols, function(nm) as_numeric_vector(unemp_raw[[nm]]))
complete_numeric <- vapply(numeric_candidates, function(x) all(!is.na(x)), logical(1L))
if (!any(complete_numeric)) {
  stop_with_report("No complete numeric unemployment-rate column was found.", manifest)
}
unemp_value_col <- candidate_value_cols[which(complete_numeric)[[1L]]]

unemp_q <- parse_quarter(unemp_raw$Quarter)
if (any(is.na(unemp_q$Quarter)) || any(!(unemp_q$Q %in% 1:4))) {
  bad <- unemp_raw$Quarter[is.na(unemp_q$Quarter) | !(unemp_q$Q %in% 1:4)]
  stop_with_report(sprintf("Invalid unemployment quarter labels: %s", format_values(bad)), manifest)
}

unemp_clean <- data.frame(
  Quarter = unemp_q$Quarter,
  Year = unemp_q$Year,
  Q = unemp_q$Q,
  unemployment_rate_nationals = as_numeric_vector(unemp_raw[[unemp_value_col]]),
  stringsAsFactors = FALSE
)
unemp_clean <- unemp_clean[order(unemp_clean$Year, unemp_clean$Q), ]
row.names(unemp_clean) <- NULL
check_no_duplicates(unemp_clean$Quarter, "unemployment file", manifest)

expected_unemp_quarters <- quarter_sequence(EXPECTED_UNEMP_START, EXPECTED_UNEMP_END)
missing_unemp <- setdiff(expected_unemp_quarters, unemp_clean$Quarter)
extra_unemp <- setdiff(unemp_clean$Quarter, expected_unemp_quarters)
if (length(missing_unemp) > 0L || length(extra_unemp) > 0L) {
  stop_with_report(
    sprintf(
      "Unemployment quarter coverage mismatch. Missing: %s. Extra: %s.",
      format_values(missing_unemp),
      format_values(extra_unemp)
    ),
    manifest
  )
}
if (nrow(unemp_clean) != EXPECTED_UNEMP_QUARTERS) {
  stop_with_report(sprintf("Expected %d unemployment quarters, found %d.", EXPECTED_UNEMP_QUARTERS, nrow(unemp_clean)), manifest)
}
if (any(is.na(unemp_clean$unemployment_rate_nationals))) {
  stop_with_report("Missing unemployment values found after numeric conversion.", manifest)
}

unemp_clean$Quarter_Index <- seq_len(nrow(unemp_clean))
unemp_clean$quarter_start <- quarter_start_date(unemp_clean$Year, unemp_clean$Q)
unemp_clean <- unemp_clean[, c("Quarter", "Year", "Q", "Quarter_Index", "quarter_start", "unemployment_rate_nationals")]

manifest <- add_manifest_row(manifest, "unemployment_value_source_column", unemp_value_col)
manifest <- add_manifest_row(manifest, "unemployment_rows", nrow(unemp_clean), paste0(min(unemp_clean$Quarter_Index), " to ", max(unemp_clean$Quarter_Index)))
manifest <- add_manifest_row(manifest, "unemployment_period", paste0(unemp_clean$Quarter[[1]], " to ", unemp_clean$Quarter[[nrow(unemp_clean)]]))
manifest <- add_manifest_row(manifest, "unemployment_missing_values", sum(is.na(unemp_clean$unemployment_rate_nationals)))

# -----------------------------
# 4. Read and validate quarterly Google Trends files
# -----------------------------
read_gt_quarterly <- function(path, label, manifest) {
  x <- read_csv_utf8(path)
  names(x) <- trimws(names(x))
  required_meta <- c("Quarter", "Year", "Q")
  missing_meta <- setdiff(required_meta, names(x))
  if (length(missing_meta) > 0L) {
    stop_with_report(sprintf("%s file is missing required metadata columns: %s", label, format_values(missing_meta)), manifest)
  }

  q <- parse_quarter(x$Quarter)
  if (any(is.na(q$Quarter)) || any(!(q$Q %in% 1:4))) {
    bad <- x$Quarter[is.na(q$Quarter) | !(q$Q %in% 1:4)]
    stop_with_report(sprintf("Invalid quarter labels in %s file: %s", label, format_values(bad)), manifest)
  }
  x$Quarter <- q$Quarter
  x$Year <- as.integer(q$Year)
  x$Q <- as.integer(q$Q)
  x <- x[order(x$Year, x$Q), ]
  row.names(x) <- NULL
  check_no_duplicates(x$Quarter, paste0(label, " file"), manifest)

  expected_gt_quarters <- quarter_sequence(EXPECTED_GT_START, EXPECTED_GT_END)
  missing_gt <- setdiff(expected_gt_quarters, x$Quarter)
  extra_gt <- setdiff(x$Quarter, expected_gt_quarters)
  if (length(missing_gt) > 0L || length(extra_gt) > 0L) {
    stop_with_report(
      sprintf(
        "%s quarter coverage mismatch. Missing: %s. Extra: %s.",
        label,
        format_values(missing_gt),
        format_values(extra_gt)
      ),
      manifest
    )
  }
  if (nrow(x) != EXPECTED_GT_QUARTERS) {
    stop_with_report(sprintf("Expected %d %s quarters, found %d.", EXPECTED_GT_QUARTERS, label, nrow(x)), manifest)
  }

  keyword_cols <- grep("^Keyword_[0-9]+$", names(x), value = TRUE)
  keyword_order <- suppressWarnings(as.integer(sub("Keyword_", "", keyword_cols)))
  keyword_cols <- keyword_cols[order(keyword_order)]
  expected_keyword_cols <- paste0("Keyword_", seq_len(EXPECTED_KEYWORD_COLS))
  if (!identical(keyword_cols, expected_keyword_cols)) {
    stop_with_report(
      sprintf(
        "%s keyword columns do not match expected Keyword_1 ... Keyword_%d. Found %d keyword columns.",
        label,
        EXPECTED_KEYWORD_COLS,
        length(keyword_cols)
      ),
      manifest
    )
  }

  for (nm in keyword_cols) {
    x[[nm]] <- as_numeric_vector(x[[nm]])
  }
  values <- as.matrix(x[, keyword_cols, drop = FALSE])
  if (any(is.na(values))) {
    stop_with_report(sprintf("Missing or nonnumeric keyword values found in %s file.", label), manifest)
  }
  if (any(values < 0 | values > 100)) {
    stop_with_report(sprintf("Values outside 0-100 found in %s file.", label), manifest)
  }

  x[, c("Quarter", "Year", "Q", keyword_cols), drop = FALSE]
}

ma <- read_gt_quarterly(ma_path, "MA", manifest)
ema <- read_gt_quarterly(ema_path, "EMA", manifest)

if (!identical(ma$Quarter, ema$Quarter)) {
  stop_with_report("MA and EMA quarterly files do not have identical quarter sequences.", manifest)
}

keyword_cols <- paste0("Keyword_", seq_len(EXPECTED_KEYWORD_COLS))
manifest <- add_manifest_row(manifest, "gt_rows", nrow(ma), paste0(ma$Quarter[[1]], " to ", ma$Quarter[[nrow(ma)]]))
manifest <- add_manifest_row(manifest, "gt_keyword_columns", length(keyword_cols), "Keyword_1 to Keyword_71 retained from Step 2/3.")
manifest <- add_manifest_row(manifest, "gt_value_range_MA", paste0(min(as.matrix(ma[, keyword_cols])), " to ", max(as.matrix(ma[, keyword_cols]))))
manifest <- add_manifest_row(manifest, "gt_value_range_EMA", paste0(min(as.matrix(ema[, keyword_cols])), " to ", max(as.matrix(ema[, keyword_cols]))))

# -----------------------------
# 5. Build modeling panels
# -----------------------------
# Unemployment full panel: 2010 Q1 to 2026 Q1, no GT predictors.
unemp_panel <- unemp_clean
unemp_panel$gt_available <- unemp_panel$Quarter %in% ma$Quarter
unemp_panel$post_gt_endpoint <- !unemp_panel$gt_available
unemp_panel$sample_role <- ifelse(
  unemp_panel$gt_available,
  "gt_overlap_nowcasting",
  "post_gt_forecast_validation"
)
unemp_panel <- unemp_panel[, c(
  "Quarter", "Year", "Q", "Quarter_Index", "quarter_start",
  "unemployment_rate_nationals", "gt_available", "post_gt_endpoint", "sample_role"
)]

post_gt_rows <- unemp_panel[unemp_panel$post_gt_endpoint, , drop = FALSE]
if (nrow(post_gt_rows) != 6L || !identical(post_gt_rows$Quarter, quarter_sequence(POST_GT_START, POST_GT_END))) {
  stop_with_report("Post-GT validation block should contain exactly 2024-4 to 2026-1.", manifest)
}

# GT overlap panel: 2010 Q1 to 2024 Q3, target + MA and EMA predictors.
overlap_idx <- match(ma$Quarter, unemp_clean$Quarter)
if (any(is.na(overlap_idx))) {
  stop_with_report("At least one GT quarter is missing from the unemployment series.", manifest)
}

gt_meta <- unemp_clean[overlap_idx, , drop = FALSE]
if (!identical(gt_meta$Quarter, ma$Quarter)) {
  stop_with_report("Quarter sequence mismatch after aligning GT files with unemployment data.", manifest)
}

gt_panel <- gt_meta
gt_panel$gt_available <- TRUE
gt_panel$post_gt_endpoint <- FALSE
gt_panel$sample_role <- "gt_overlap_nowcasting"

ma_prefixed <- ma[, keyword_cols, drop = FALSE]
names(ma_prefixed) <- paste0("MA__", keyword_cols)
ema_prefixed <- ema[, keyword_cols, drop = FALSE]
names(ema_prefixed) <- paste0("EMA__", keyword_cols)

gt_panel <- cbind(
  gt_panel[, c(
    "Quarter", "Year", "Q", "Quarter_Index", "quarter_start",
    "unemployment_rate_nationals", "gt_available", "post_gt_endpoint", "sample_role"
  ), drop = FALSE],
  ma_prefixed,
  ema_prefixed
)

# Final consistency checks.
if (nrow(gt_panel) != EXPECTED_GT_QUARTERS) {
  stop_with_report(sprintf("GT overlap panel should have %d rows, found %d.", EXPECTED_GT_QUARTERS, nrow(gt_panel)), manifest)
}
if (nrow(unemp_panel) != EXPECTED_UNEMP_QUARTERS) {
  stop_with_report(sprintf("Unemployment full panel should have %d rows, found %d.", EXPECTED_UNEMP_QUARTERS, nrow(unemp_panel)), manifest)
}
if (!identical(gt_panel$Quarter[[1]], EXPECTED_GT_START) || !identical(gt_panel$Quarter[[nrow(gt_panel)]], EXPECTED_GT_END)) {
  stop_with_report("GT overlap panel period does not match 2010-1 to 2024-3.", manifest)
}
if (!identical(unemp_panel$Quarter[[1]], EXPECTED_UNEMP_START) || !identical(unemp_panel$Quarter[[nrow(unemp_panel)]], EXPECTED_UNEMP_END)) {
  stop_with_report("Unemployment full panel period does not match 2010-1 to 2026-1.", manifest)
}

# -----------------------------
# 6. Write outputs
# -----------------------------
write_csv_utf8(unemp_clean, OUTPUT_UNEMP_CLEAN)
write_csv_utf8(gt_panel, OUTPUT_GT_PANEL)
write_csv_utf8(unemp_panel, OUTPUT_UNEMP_PANEL)

manifest <- add_manifest_row(manifest, "output_unemployment_clean", OUTPUT_UNEMP_CLEAN)
manifest <- add_manifest_row(manifest, "output_modeling_panel_gt_overlap", OUTPUT_GT_PANEL, sprintf("%d rows, %d columns", nrow(gt_panel), ncol(gt_panel)))
manifest <- add_manifest_row(manifest, "output_modeling_panel_unemployment_full", OUTPUT_UNEMP_PANEL, sprintf("%d rows, %d columns", nrow(unemp_panel), ncol(unemp_panel)))
manifest <- add_manifest_row(manifest, "gt_overlap_rows", nrow(gt_panel), paste0(gt_panel$Quarter[[1]], " to ", gt_panel$Quarter[[nrow(gt_panel)]]))
manifest <- add_manifest_row(manifest, "gt_overlap_columns", ncol(gt_panel), "9 metadata/target/flags columns + 142 GT predictor columns.")
manifest <- add_manifest_row(manifest, "unemployment_full_rows", nrow(unemp_panel), paste0(unemp_panel$Quarter[[1]], " to ", unemp_panel$Quarter[[nrow(unemp_panel)]]))
manifest <- add_manifest_row(manifest, "unemployment_full_columns", ncol(unemp_panel), "Target and sample flags only; no future GT placeholders.")
manifest <- add_manifest_row(manifest, "post_gt_validation_rows", nrow(post_gt_rows), paste(post_gt_rows$Quarter, collapse = "; "))
manifest <- add_manifest_row(manifest, "status", "PASS", "Step 4 modeling panels were built successfully.")
write_csv_utf8(manifest, OUTPUT_MANIFEST)

report_lines <- c(
  "# Step 4 Modeling Panel Build Report",
  "",
  "## Status",
  "",
  "PASS",
  "",
  "## Inputs",
  "",
  paste0("- Unemployment: `", basename(unemp_path), "`"),
  paste0("- Google Trends MA: `", basename(ma_path), "`"),
  paste0("- Google Trends EMA: `", basename(ema_path), "`"),
  "",
  "## Outputs",
  "",
  paste0("- `", basename(OUTPUT_UNEMP_CLEAN), "`: ", nrow(unemp_clean), " rows, ", ncol(unemp_clean), " columns"),
  paste0("- `", basename(OUTPUT_GT_PANEL), "`: ", nrow(gt_panel), " rows, ", ncol(gt_panel), " columns"),
  paste0("- `", basename(OUTPUT_UNEMP_PANEL), "`: ", nrow(unemp_panel), " rows, ", ncol(unemp_panel), " columns"),
  "",
  "## Panel definitions",
  "",
  paste0("- GT overlap panel period: ", gt_panel$Quarter[[1]], " to ", gt_panel$Quarter[[nrow(gt_panel)]], "."),
  "- GT overlap panel contains the unemployment target plus both MA and EMA Google Trends predictors.",
  "- Google Trends predictor columns are prefixed with `MA__` and `EMA__`.",
  paste0("- Full unemployment panel period: ", unemp_panel$Quarter[[1]], " to ", unemp_panel$Quarter[[nrow(unemp_panel)]], "."),
  paste0("- Post-GT unemployment-only validation block: ", paste(post_gt_rows$Quarter, collapse = ", "), "."),
  "",
  "## Safeguards",
  "",
  "- No raw file is modified.",
  "- No missing-value filling, imputation, interpolation, or source correction is performed.",
  "- No placeholder Google Trends values are created for 2024 Q4 to 2026 Q1.",
  "- SAWA remains deferred until the rolling-validation pipeline is stable.",
  "- The post-2024 Q3 unemployment observations are clearly flagged as `post_gt_forecast_validation`."
)
write_lines_utf8(report_lines, OUTPUT_REPORT)

message("Step 4 modeling panels built successfully.")
message(sprintf("Wrote: %s", OUTPUT_GT_PANEL))
message(sprintf("Wrote: %s", OUTPUT_UNEMP_PANEL))
