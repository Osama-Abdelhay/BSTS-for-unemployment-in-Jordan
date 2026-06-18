#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# 01_prepare_clean_data.R
# Step 2: Build clean monthly Google Trends file for the Jordan unemployment-
#         Google Trends pipeline
# -----------------------------------------------------------------------------
# Purpose:
#   Create google_trends_monthly_clean.csv from the frozen raw KWMonthly.csv and
#   Arabic_Keywords_Translation.csv files.
#
# Agreed cleaning rules:
#   - Raw files are never modified by this script.
#   - KWMonthly.csv is treated as complete raw data, except for the two agreed
#     columns excluded from analysis: "شغل حلال" and "فلسنا".
#   - No blank-cell correction, filling, interpolation, or imputation is
#     performed.
#   - The two agreed dropped columns are removed from the clean modelling file.
#   - Retained keyword columns are matched to the dictionary by exact keyword
#     text, not by column position.
#   - Retained keyword values must be numeric, complete, and within the Google
#     Trends 0-100 scale. Any missing retained value fails the script.
#   - The clean file is ordered by the keyword dictionary and, by default,
#     keyword columns are renamed to canonical Column_Name values
#     (Keyword_1 ... Keyword_71). Set --keyword-names=keyword to retain original
#     keyword text as column names.
#
# Usage from project root:
#   Rscript scripts/01_prepare_clean_data.R
#
# Optional arguments:
#   Rscript scripts/01_prepare_clean_data.R --raw-dir=data/raw --out-dir=data/processed
#   Rscript scripts/01_prepare_clean_data.R --keyword-names=column_name
#   Rscript scripts/01_prepare_clean_data.R --keyword-names=keyword
#
# Outputs:
#   google_trends_monthly_clean.csv
#   keyword_dictionary_clean.csv
#   01_prepare_clean_data_manifest.csv
#   01_prepare_clean_data_report.md
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
OUT_DIR_DEFAULT <- file.path(PROJECT_ROOT, "data", "processed")
RAW_DIR_ARG <- get_arg("raw-dir", Sys.getenv("BSTS_RAW_DIR", unset = RAW_DIR_DEFAULT))
OUT_DIR <- normalizePath(get_arg("out-dir", Sys.getenv("BSTS_PROCESSED_DIR", unset = OUT_DIR_DEFAULT)), mustWork = FALSE)

DICT_FILE <- "Arabic_Keywords_Translation.csv"
GT_FILE <- "KWMonthly.csv"

DROP_GT_COLUMNS <- c("شغل حلال", "فلسنا")
EXPECTED_DICT_ROWS <- 71L
EXPECTED_RAW_GT_KEYWORD_COLS <- 73L
EXPECTED_RETAINED_GT_KEYWORD_COLS <- 71L
EXPECTED_GT_MONTH_START <- as.Date("2010-01-01")
EXPECTED_GT_MONTH_END <- as.Date("2024-09-01")
EXPECTED_GT_MONTHS <- seq.Date(EXPECTED_GT_MONTH_START, EXPECTED_GT_MONTH_END, by = "month")

KEYWORD_NAMES <- tolower(get_arg("keyword-names", "column_name"))
if (!KEYWORD_NAMES %in% c("column_name", "keyword")) {
  stop("--keyword-names must be either 'column_name' or 'keyword'.", call. = FALSE)
}

OUTPUT_GT_FILE <- file.path(OUT_DIR, "google_trends_monthly_clean.csv")
OUTPUT_DICT_FILE <- file.path(OUT_DIR, "keyword_dictionary_clean.csv")
OUTPUT_MANIFEST_FILE <- file.path(OUT_DIR, "01_prepare_clean_data_manifest.csv")
OUTPUT_REPORT_FILE <- file.path(OUT_DIR, "01_prepare_clean_data_report.md")

CANDIDATE_RAW_DIRS <- unique(c(
  RAW_DIR_ARG,
  RAW_DIR_DEFAULT,
  file.path(PROJECT_ROOT, "raw"),
  PROJECT_ROOT,
  getwd(),
  "/mnt/data"
))

# -----------------------------
# 1. Helper functions
# -----------------------------
locate_file <- function(filename, candidate_dirs = CANDIDATE_RAW_DIRS) {
  candidate_dirs <- candidate_dirs[dir.exists(candidate_dirs)]
  candidate_paths <- file.path(candidate_dirs, filename)
  hit <- candidate_paths[file.exists(candidate_paths)]
  if (length(hit) == 0L) {
    stop(
      sprintf(
        "Required file not found: %s\nSearched in:\n%s",
        filename,
        paste(candidate_dirs, collapse = "\n")
      ),
      call. = FALSE
    )
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
  formats <- c("%m/%d/%Y", "%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d", "%m-%d-%Y")
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
    "# Step 2 clean monthly Google Trends preparation",
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
    "- `01_prepare_clean_data_manifest.csv`",
    "- `01_prepare_clean_data_report.md`"
  )
  write_lines_utf8(report_lines, OUTPUT_REPORT_FILE)
  stop(message_text, call. = FALSE)
}

# -----------------------------
# 2. Locate and read raw inputs
# -----------------------------
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
manifest <- data.frame(item = character(), value = character(), details = character(), stringsAsFactors = FALSE)

path_dict <- locate_file(DICT_FILE)
path_gt <- locate_file(GT_FILE)
RAW_DIR_USED <- dirname(path_gt)

manifest <- add_manifest_row(manifest, "project_root", PROJECT_ROOT)
manifest <- add_manifest_row(manifest, "raw_dir_used", RAW_DIR_USED)
manifest <- add_manifest_row(manifest, "out_dir", OUT_DIR)
manifest <- add_manifest_row(manifest, "keyword_dictionary_file", path_dict)
manifest <- add_manifest_row(manifest, "google_trends_monthly_file", path_gt)
manifest <- add_manifest_row(manifest, "keyword_column_naming", KEYWORD_NAMES, "column_name = Keyword_1...Keyword_71; keyword = original keyword text")
manifest <- add_manifest_row(manifest, "dropped_google_trends_columns", paste(DROP_GT_COLUMNS, collapse = "; "), "Excluded from clean modelling file; raw file remains unchanged.")
manifest <- add_manifest_row(manifest, "blank_cell_filling_or_imputation_by_script", FALSE, "No missing retained values are filled or imputed.")

keyword_dict <- read_csv_utf8(path_dict)
gt_raw <- read_csv_utf8(path_gt)

# -----------------------------
# 3. Dictionary validation and cleaning
# -----------------------------
required_dict_cols <- c("Column_Name", "Keyword", "English_Translation")
missing_dict_cols <- setdiff(required_dict_cols, names(keyword_dict))
if (length(missing_dict_cols) > 0L) {
  stop_with_report(
    paste0("Keyword dictionary is missing required columns: ", format_values(missing_dict_cols)),
    manifest
  )
}

keyword_dict <- keyword_dict[required_dict_cols]
keyword_dict[] <- lapply(keyword_dict, function(z) trimws(as.character(z)))

if (nrow(keyword_dict) != EXPECTED_DICT_ROWS) {
  stop_with_report(
    sprintf("Keyword dictionary has %d rows; expected %d.", nrow(keyword_dict), EXPECTED_DICT_ROWS),
    manifest
  )
}

missing_dict_cells <- sum(is_blank(keyword_dict[required_dict_cols]))
if (missing_dict_cells > 0L) {
  stop_with_report(
    sprintf("Keyword dictionary has %d missing cells in required columns.", missing_dict_cells),
    manifest
  )
}

if (any(duplicated(keyword_dict$Keyword))) {
  dup <- unique(keyword_dict$Keyword[duplicated(keyword_dict$Keyword)])
  stop_with_report(paste0("Keyword dictionary has duplicate keyword text: ", format_values(dup)), manifest)
}

if (any(duplicated(keyword_dict$Column_Name))) {
  dup <- unique(keyword_dict$Column_Name[duplicated(keyword_dict$Column_Name)])
  stop_with_report(paste0("Keyword dictionary has duplicate Column_Name values: ", format_values(dup)), manifest)
}

# -----------------------------
# 4. Google Trends monthly validation
# -----------------------------
if (!"Date" %in% names(gt_raw)) {
  stop_with_report("KWMonthly.csv must contain a Date column.", manifest)
}

gt_dates <- parse_month_date(gt_raw$Date)
if (any(is.na(gt_dates))) {
  bad <- which(is.na(gt_dates))
  stop_with_report(
    paste0("KWMonthly.csv has unparsable Date values at rows: ", format_values(bad)),
    manifest
  )
}

missing_months <- setdiff(as.character(EXPECTED_GT_MONTHS), as.character(gt_dates))
extra_months <- setdiff(as.character(gt_dates), as.character(EXPECTED_GT_MONTHS))
duplicate_months <- unique(as.character(gt_dates[duplicated(gt_dates)]))

if (nrow(gt_raw) != length(EXPECTED_GT_MONTHS) || length(missing_months) > 0L || length(extra_months) > 0L || length(duplicate_months) > 0L) {
  stop_with_report(
    "KWMonthly.csv does not match the expected complete monthly period 2010-01 to 2024-09.",
    manifest,
    c(
      paste0("Observed rows: ", nrow(gt_raw)),
      paste0("Expected rows: ", length(EXPECTED_GT_MONTHS)),
      paste0("Missing months: ", format_values(missing_months)),
      paste0("Extra months: ", format_values(extra_months)),
      paste0("Duplicate months: ", format_values(duplicate_months))
    )
  )
}

if (!all(order(gt_dates) == seq_along(gt_dates))) {
  stop_with_report("KWMonthly.csv monthly rows are not sorted chronologically.", manifest)
}

raw_gt_cols <- setdiff(names(gt_raw), "Date")
if (length(raw_gt_cols) != EXPECTED_RAW_GT_KEYWORD_COLS) {
  stop_with_report(
    sprintf("KWMonthly.csv has %d raw keyword columns before exclusions; expected %d.", length(raw_gt_cols), EXPECTED_RAW_GT_KEYWORD_COLS),
    manifest
  )
}

missing_drop_cols <- setdiff(DROP_GT_COLUMNS, names(gt_raw))
if (length(missing_drop_cols) > 0L) {
  stop_with_report(paste0("Agreed dropped columns are missing from KWMonthly.csv: ", format_values(missing_drop_cols)), manifest)
}

retained_keywords <- keyword_dict$Keyword
missing_from_gt <- setdiff(retained_keywords, raw_gt_cols)
extra_retained_gt <- setdiff(setdiff(raw_gt_cols, DROP_GT_COLUMNS), retained_keywords)

if (length(missing_from_gt) > 0L || length(extra_retained_gt) > 0L) {
  stop_with_report(
    "Retained KWMonthly keyword columns do not align with the dictionary by exact keyword text.",
    manifest,
    c(
      paste0("Dictionary keywords missing from KWMonthly: ", format_values(missing_from_gt)),
      paste0("Retained KWMonthly columns not in dictionary: ", format_values(extra_retained_gt))
    )
  )
}

retained_raw <- gt_raw[retained_keywords]
retained_numeric <- as.data.frame(lapply(retained_raw, as_numeric_vector), check.names = FALSE, stringsAsFactors = FALSE)

nonnumeric_count <- count_nonnumeric_cells(retained_raw, retained_numeric)
missing_count <- sum(is.na(as.matrix(retained_numeric)))
out_of_range_count <- sum(as.matrix(retained_numeric) < 0 | as.matrix(retained_numeric) > 100, na.rm = TRUE)

if (nonnumeric_count > 0L) {
  stop_with_report(
    sprintf("Retained Google Trends columns contain %d nonnumeric nonblank cells.", nonnumeric_count),
    manifest
  )
}

if (missing_count > 0L) {
  missing_locations <- character()
  for (nm in names(retained_numeric)) {
    idx <- which(is.na(retained_numeric[[nm]]))
    if (length(idx) > 0L) {
      missing_locations <- c(
        missing_locations,
        paste0(nm, " @ ", format(gt_dates[idx], "%Y-%m-%d"))
      )
    }
  }
  stop_with_report(
    sprintf("Retained Google Trends columns contain %d missing value(s). No filling or imputation is performed by Step 2.", missing_count),
    manifest,
    c(
      "Missing retained keyword values found:",
      paste0("- ", format_values(missing_locations, max_items = 25L))
    )
  )
}

if (out_of_range_count > 0L) {
  stop_with_report(
    sprintf("Retained Google Trends columns contain %d values outside the expected 0-100 range.", out_of_range_count),
    manifest
  )
}

# -----------------------------
# 5. Build clean monthly file
# -----------------------------
if (KEYWORD_NAMES == "column_name") {
  names(retained_numeric) <- keyword_dict$Column_Name
} else {
  names(retained_numeric) <- keyword_dict$Keyword
}

clean_gt <- data.frame(
  Date = format(gt_dates, "%Y-%m-%d"),
  Year = as.integer(format(gt_dates, "%Y")),
  Month = as.integer(format(gt_dates, "%m")),
  Quarter = month_to_quarter(gt_dates),
  retained_numeric,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

keyword_dict_clean <- keyword_dict
keyword_dict_clean$Retained_For_Analysis <- TRUE
keyword_dict_clean$Source_File <- GT_FILE
keyword_dict_clean$Source_Column <- keyword_dict_clean$Keyword
keyword_dict_clean$Clean_Column <- if (KEYWORD_NAMES == "column_name") keyword_dict_clean$Column_Name else keyword_dict_clean$Keyword
keyword_dict_clean$Match_Rule <- "exact keyword text"

# Diagnostics for included and dropped columns
manifest <- add_manifest_row(manifest, "status", "PASS", "Clean monthly Google Trends file generated successfully.")
manifest <- add_manifest_row(manifest, "rows_in_clean_google_trends_monthly", nrow(clean_gt), "Expected 177 monthly rows, 2010-01 to 2024-09.")
manifest <- add_manifest_row(manifest, "metadata_columns", 4, "Date, Year, Month, Quarter.")
manifest <- add_manifest_row(manifest, "retained_keyword_columns", ncol(clean_gt) - 4L, "Expected 71 retained modelling keywords.")
manifest <- add_manifest_row(manifest, "raw_keyword_columns_before_exclusion", length(raw_gt_cols), "Includes agreed dropped columns.")
manifest <- add_manifest_row(manifest, "output_keyword_column_names", KEYWORD_NAMES)
manifest <- add_manifest_row(manifest, "date_range", paste0(head(clean_gt$Date, 1L), " to ", tail(clean_gt$Date, 1L)))
manifest <- add_manifest_row(manifest, "quarter_range", paste0(head(clean_gt$Quarter, 1L), " to ", tail(clean_gt$Quarter, 1L)))
manifest <- add_manifest_row(manifest, "retained_values_missing_count", missing_count)
manifest <- add_manifest_row(manifest, "retained_values_out_of_range_count", out_of_range_count)
manifest <- add_manifest_row(manifest, "retained_values_nonnumeric_count", nonnumeric_count)

# -----------------------------
# 6. Write outputs
# -----------------------------
write_csv_utf8(clean_gt, OUTPUT_GT_FILE)
write_csv_utf8(keyword_dict_clean, OUTPUT_DICT_FILE)
write_csv_utf8(manifest, OUTPUT_MANIFEST_FILE)

report_lines <- c(
  "# Step 2 clean monthly Google Trends preparation",
  "",
  paste0("Generated on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "Overall status: **PASS**",
  "",
  "## Agreed cleaning rules applied",
  "",
  "- Raw files were read but not modified.",
  paste0("- Dropped from the clean modelling file: `", paste(DROP_GT_COLUMNS, collapse = "`, `"), "`."),
  "- No blank-cell correction, filling, interpolation, or imputation was performed.",
  "- Retained keyword columns were matched to the dictionary by exact keyword text, not by column position.",
  paste0("- Clean keyword column naming: `", KEYWORD_NAMES, "`."),
  "",
  "## Clean file summary",
  "",
  paste0("- Rows: ", nrow(clean_gt), " monthly observations."),
  paste0("- Period: ", head(clean_gt$Date, 1L), " to ", tail(clean_gt$Date, 1L), "."),
  paste0("- Quarter coverage: ", head(clean_gt$Quarter, 1L), " to ", tail(clean_gt$Quarter, 1L), "."),
  paste0("- Metadata columns: 4 (`Date`, `Year`, `Month`, `Quarter`)."),
  paste0("- Retained keyword columns: ", ncol(clean_gt) - 4L, "."),
  paste0("- Total columns: ", ncol(clean_gt), "."),
  "- Retained Google Trends values are numeric, complete, and within the 0-100 scale.",
  "",
  "## Output files",
  "",
  "- `google_trends_monthly_clean.csv`",
  "- `keyword_dictionary_clean.csv`",
  "- `01_prepare_clean_data_manifest.csv`",
  "- `01_prepare_clean_data_report.md`"
)
write_lines_utf8(report_lines, OUTPUT_REPORT_FILE)

message("Step 2 clean monthly Google Trends preparation complete.")
message("Raw directory used: ", RAW_DIR_USED)
message("Output directory: ", OUT_DIR)
message("Clean monthly file: ", OUTPUT_GT_FILE)
message("Rows: ", nrow(clean_gt), " | Retained keyword columns: ", ncol(clean_gt) - 4L)

invisible(clean_gt)
