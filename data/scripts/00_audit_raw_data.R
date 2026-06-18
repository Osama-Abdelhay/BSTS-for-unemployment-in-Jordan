#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# 00_audit_raw_data.R
# Step 1: Raw data audit for the Jordan unemployment-Google Trends pipeline
# -----------------------------------------------------------------------------
# Purpose:
#   Audit the three frozen raw inputs before any cleaning, aggregation, or
#   modelling is performed.
#
# Audit rules:
#   - Raw files are never modified by this script.
#   - KWMonthly.csv is treated as complete raw data except for the two agreed
#     columns excluded from analysis: "شغل حلال" and "فلسنا".
#   - No blank-cell correction, filling, or imputation is performed here.
#   - Keyword alignment is checked by exact keyword text, not column position.
#   - Main same-quarter Google Trends nowcasting overlap: 2010 Q1-2024 Q3.
#   - Post-Google-Trends unemployment-only validation block: 2024 Q4-2026 Q1.
#
# Usage from project root:
#   Rscript scripts/00_audit_raw_data.R
#
# Optional arguments:
#   Rscript scripts/00_audit_raw_data.R --raw-dir=data/raw --out-dir=data/processed/audit
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
  prefix <- paste0("--", key, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit) == 0L) return(default)
  sub(prefix, "", hit[[1]], fixed = TRUE)
}

PROJECT_ROOT <- normalizePath(get_arg("project-root", Sys.getenv("BSTS_PROJECT_ROOT", unset = getwd())), mustWork = FALSE)
RAW_DIR_DEFAULT <- file.path(PROJECT_ROOT, "data", "raw")
OUT_DIR_DEFAULT <- file.path(PROJECT_ROOT, "data", "processed", "audit")
RAW_DIR_ARG <- get_arg("raw-dir", Sys.getenv("BSTS_RAW_DIR", unset = RAW_DIR_DEFAULT))
OUT_DIR <- normalizePath(get_arg("out-dir", Sys.getenv("BSTS_AUDIT_DIR", unset = OUT_DIR_DEFAULT)), mustWork = FALSE)

DICT_FILE  <- "Arabic_Keywords_Translation.csv"
UNEMP_FILE <- "Jordanian Unemployment Rate 2010- Q1 2026.csv"
GT_FILE    <- "KWMonthly.csv"

DROP_GT_COLUMNS <- c("شغل حلال", "فلسنا")

EXPECTED_DICT_ROWS <- 71L
EXPECTED_RAW_GT_KEYWORD_COLS <- 73L
EXPECTED_RETAINED_GT_KEYWORD_COLS <- 71L
EXPECTED_UNEMP_START <- c(year = 2010L, quarter = 1L)
EXPECTED_UNEMP_END   <- c(year = 2026L, quarter = 1L)
EXPECTED_GT_MONTH_START <- as.Date("2010-01-01")
EXPECTED_GT_MONTH_END   <- as.Date("2024-09-01")
EXPECTED_GT_OVERLAP_START <- c(year = 2010L, quarter = 1L)
EXPECTED_GT_OVERLAP_END   <- c(year = 2024L, quarter = 3L)
EXPECTED_POST_GT_START    <- c(year = 2024L, quarter = 4L)
EXPECTED_POST_GT_END      <- c(year = 2026L, quarter = 1L)

STOP_ON_FAIL <- TRUE

# Candidate folders make the script usable from a project root, from a scripts
# folder, or from /mnt/data during development.
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

count_blank_cells <- function(df) {
  mat <- as.matrix(df)
  sum(is.na(mat) | trimws(as.character(mat)) == "")
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

span_quarters <- function(x) {
  if (length(x) == 0L || all(is.na(x))) return(NA_character_)
  paste0(head(x, 1L), " to ", tail(x, 1L), " (", length(x), " quarters)")
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

quarter_seq <- function(start_year, start_q, end_year, end_q) {
  start_index <- as.integer(start_year) * 4L + as.integer(start_q)
  end_index <- as.integer(end_year) * 4L + as.integer(end_q)
  ids <- seq.int(start_index, end_index)
  years <- (ids - 1L) %/% 4L
  quarters <- ((ids - 1L) %% 4L) + 1L
  paste0(years, "-", quarters)
}

parse_quarter <- function(x) {
  x_chr <- trimws(as.character(x))
  x_norm <- gsub("[[:space:]]+", "", x_chr)
  x_norm <- gsub("_", "-", x_norm)
  x_norm <- gsub("/", "-", x_norm)
  x_norm <- gsub("Q", "", x_norm, ignore.case = TRUE)

  matches <- regexec("^([0-9]{4})-?([1-4])$", x_norm)
  parts <- regmatches(x_norm, matches)

  year <- rep(NA_integer_, length(x_norm))
  q <- rep(NA_integer_, length(x_norm))

  for (i in seq_along(parts)) {
    if (length(parts[[i]]) == 3L) {
      year[i] <- as.integer(parts[[i]][2L])
      q[i] <- as.integer(parts[[i]][3L])
    }
  }

  data.frame(
    raw = x_chr,
    year = year,
    quarter = q,
    label = ifelse(!is.na(year) & !is.na(q), paste0(year, "-", q), NA_character_),
    index = ifelse(!is.na(year) & !is.na(q), year * 4L + q, NA_integer_),
    stringsAsFactors = FALSE
  )
}

month_to_quarter <- function(dates) {
  year <- as.integer(format(dates, "%Y"))
  month <- as.integer(format(dates, "%m"))
  quarter <- ((month - 1L) %/% 3L) + 1L
  paste0(year, "-", quarter)
}

as_numeric_df <- function(df) {
  as.data.frame(
    lapply(df, function(z) {
      z_chr <- trimws(as.character(z))
      z_chr[z_chr == ""] <- NA_character_
      suppressWarnings(as.numeric(z_chr))
    }),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

count_nonnumeric_cells <- function(raw_df, numeric_df) {
  total <- 0L
  for (nm in names(raw_df)) {
    total <- total + sum(!is_blank(raw_df[[nm]]) & is.na(numeric_df[[nm]]))
  }
  as.integer(total)
}

status_from <- function(ok) if (isTRUE(ok)) "PASS" else "FAIL"

json_escape <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub('"', '\\\\"', x)
  x
}

json_vector <- function(x) {
  paste0("[", paste0('"', json_escape(as.character(x)), '"', collapse = ", "), "]")
}

.audit <- data.frame(
  area = character(),
  check = character(),
  status = character(),
  observed = character(),
  expected = character(),
  details = character(),
  stringsAsFactors = FALSE
)

add_audit <- function(area, check, status, observed = NA, expected = NA, details = "") {
  .audit <<- rbind(
    .audit,
    data.frame(
      area = area,
      check = check,
      status = status,
      observed = as.character(observed),
      expected = as.character(expected),
      details = as.character(details),
      stringsAsFactors = FALSE
    )
  )
}

# -----------------------------
# 2. Locate and read raw inputs
# -----------------------------
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

path_dict <- locate_file(DICT_FILE)
path_unemp <- locate_file(UNEMP_FILE)
path_gt <- locate_file(GT_FILE)
RAW_DIR_USED <- dirname(path_gt)

add_audit("Raw files", "Keyword dictionary file present", "PASS", path_dict, DICT_FILE, "Raw file found.")
add_audit("Raw files", "Unemployment file present", "PASS", path_unemp, UNEMP_FILE, "Raw file found.")
add_audit("Raw files", "Monthly Google Trends file present", "PASS", path_gt, GT_FILE, "Raw file found.")

keyword_dict <- read_csv_utf8(path_dict)
unemp_raw <- read_csv_utf8(path_unemp)
gt_raw <- read_csv_utf8(path_gt)

# -----------------------------
# 3. Keyword dictionary audit
# -----------------------------
required_dict_cols <- c("Column_Name", "Keyword", "English_Translation")
missing_dict_cols <- setdiff(required_dict_cols, names(keyword_dict))
extra_dict_cols <- setdiff(names(keyword_dict), required_dict_cols)

add_audit(
  "Keyword dictionary",
  "Required columns present",
  status_from(length(missing_dict_cols) == 0L),
  observed = paste(names(keyword_dict), collapse = "; "),
  expected = paste(required_dict_cols, collapse = "; "),
  details = paste0("Missing: ", format_values(missing_dict_cols), "; extra: ", format_values(extra_dict_cols))
)

add_audit(
  "Keyword dictionary",
  "Expected number of rows",
  status_from(nrow(keyword_dict) == EXPECTED_DICT_ROWS),
  observed = nrow(keyword_dict),
  expected = EXPECTED_DICT_ROWS,
  details = "Dictionary rows should match the retained modelling keyword list."
)

if (all(required_dict_cols %in% names(keyword_dict))) {
  missing_cells <- count_blank_cells(keyword_dict[required_dict_cols])
  dup_keywords <- unique(keyword_dict$Keyword[duplicated(keyword_dict$Keyword)])
  dup_colnames <- unique(keyword_dict$Column_Name[duplicated(keyword_dict$Column_Name)])

  add_audit("Keyword dictionary", "No missing dictionary cells", status_from(missing_cells == 0L), missing_cells, 0L, "Checked Column_Name, Keyword, and English_Translation.")
  add_audit("Keyword dictionary", "No duplicate keyword text", status_from(length(dup_keywords) == 0L), length(dup_keywords), 0L, paste("Duplicates:", format_values(dup_keywords)))
  add_audit("Keyword dictionary", "No duplicate Column_Name values", status_from(length(dup_colnames) == 0L), length(dup_colnames), 0L, paste("Duplicates:", format_values(dup_colnames)))
}

# -----------------------------
# 4. Unemployment audit
# -----------------------------
unemp_quarter_col <- "Quarter"
unemp_value_candidates <- setdiff(names(unemp_raw), unemp_quarter_col)
unemp_value_col <- if (length(unemp_value_candidates) > 0L) unemp_value_candidates[[1L]] else NA_character_

add_audit("Unemployment", "Quarter column present", status_from(unemp_quarter_col %in% names(unemp_raw)), paste(names(unemp_raw), collapse = "; "), unemp_quarter_col, "Quarter labels should identify 2010 Q1 to 2026 Q1.")
add_audit("Unemployment", "One unemployment value column available", status_from(!is.na(unemp_value_col) && length(unemp_value_candidates) == 1L), ifelse(is.na(unemp_value_col), "none", paste(unemp_value_candidates, collapse = "; ")), "one numeric value column", "The non-Quarter column is used as the official unemployment-rate field.")

if (unemp_quarter_col %in% names(unemp_raw)) {
  unemp_q <- parse_quarter(unemp_raw[[unemp_quarter_col]])
  expected_unemp_q <- quarter_seq(EXPECTED_UNEMP_START[["year"]], EXPECTED_UNEMP_START[["quarter"]], EXPECTED_UNEMP_END[["year"]], EXPECTED_UNEMP_END[["quarter"]])
  observed_unemp_q <- unemp_q$label
  missing_unemp_q <- setdiff(expected_unemp_q, observed_unemp_q)
  extra_unemp_q <- setdiff(observed_unemp_q[!is.na(observed_unemp_q)], expected_unemp_q)
  duplicate_unemp_q <- unique(observed_unemp_q[duplicated(observed_unemp_q) & !is.na(observed_unemp_q)])

  add_audit("Unemployment", "Quarter labels parse successfully", status_from(sum(is.na(unemp_q$label)) == 0L), sum(is.na(unemp_q$label)), 0L, "Number of unparsable quarter labels.")
  add_audit("Unemployment", "Expected number of quarterly observations", status_from(nrow(unemp_raw) == length(expected_unemp_q)), nrow(unemp_raw), length(expected_unemp_q), "Expected period is 2010 Q1 to 2026 Q1.")
  add_audit("Unemployment", "No missing expected quarters", status_from(length(missing_unemp_q) == 0L), length(missing_unemp_q), 0L, paste("Missing:", format_values(missing_unemp_q)))
  add_audit("Unemployment", "No unexpected extra quarters", status_from(length(extra_unemp_q) == 0L), length(extra_unemp_q), 0L, paste("Extra:", format_values(extra_unemp_q)))
  add_audit("Unemployment", "No duplicate quarters", status_from(length(duplicate_unemp_q) == 0L), length(duplicate_unemp_q), 0L, paste("Duplicates:", format_values(duplicate_unemp_q)))
  add_audit("Unemployment", "Quarters sorted chronologically", status_from(all(order(unemp_q$index) == seq_along(unemp_q$index))), span_quarters(observed_unemp_q), "ascending quarter order", "Raw file should be ordered from oldest to newest.")
}

if (!is.na(unemp_value_col)) {
  unemp_values <- suppressWarnings(as.numeric(trimws(as.character(unemp_raw[[unemp_value_col]]))))
  unemp_nonnumeric <- sum(!is_blank(unemp_raw[[unemp_value_col]]) & is.na(unemp_values))
  unemp_missing <- sum(is.na(unemp_values))
  unemp_out_of_range <- sum(unemp_values < 0 | unemp_values > 100, na.rm = TRUE)

  add_audit("Unemployment", "Unemployment values are numeric", status_from(unemp_nonnumeric == 0L), unemp_nonnumeric, 0L, "Nonblank cells that could not be converted to numeric.")
  add_audit("Unemployment", "No missing unemployment values", status_from(unemp_missing == 0L), unemp_missing, 0L, "Official unemployment-rate series should be complete.")
  add_audit("Unemployment", "Unemployment values are in 0-100 range", status_from(unemp_out_of_range == 0L), unemp_out_of_range, 0L, paste0("Observed min=", min(unemp_values, na.rm = TRUE), "; max=", max(unemp_values, na.rm = TRUE)))
}

# -----------------------------
# 5. Google Trends monthly audit
# -----------------------------
gt_date_col <- "Date"
add_audit("Google Trends monthly", "Date column present", status_from(gt_date_col %in% names(gt_raw)), paste(names(gt_raw)[seq_len(min(5L, ncol(gt_raw)))], collapse = "; "), gt_date_col, "Monthly file should contain one Date column and keyword columns.")

if (gt_date_col %in% names(gt_raw)) {
  gt_dates <- parse_month_date(gt_raw[[gt_date_col]])
  expected_months <- seq.Date(EXPECTED_GT_MONTH_START, EXPECTED_GT_MONTH_END, by = "month")
  missing_months <- setdiff(as.character(expected_months), as.character(gt_dates))
  extra_months <- setdiff(as.character(gt_dates[!is.na(gt_dates)]), as.character(expected_months))
  duplicate_months <- unique(as.character(gt_dates[duplicated(gt_dates) & !is.na(gt_dates)]))

  add_audit("Google Trends monthly", "Monthly dates parse successfully", status_from(sum(is.na(gt_dates)) == 0L), sum(is.na(gt_dates)), 0L, "Number of unparsable Date values.")
  add_audit("Google Trends monthly", "Expected number of monthly observations", status_from(nrow(gt_raw) == length(expected_months)), nrow(gt_raw), length(expected_months), "Expected period is January 2010 to September 2024.")
  add_audit("Google Trends monthly", "No missing expected months", status_from(length(missing_months) == 0L), length(missing_months), 0L, paste("Missing:", format_values(missing_months)))
  add_audit("Google Trends monthly", "No unexpected extra months", status_from(length(extra_months) == 0L), length(extra_months), 0L, paste("Extra:", format_values(extra_months)))
  add_audit("Google Trends monthly", "No duplicate months", status_from(length(duplicate_months) == 0L), length(duplicate_months), 0L, paste("Duplicates:", format_values(duplicate_months)))
  add_audit("Google Trends monthly", "Monthly rows sorted chronologically", status_from(all(order(gt_dates) == seq_along(gt_dates))), paste0(format(min(gt_dates, na.rm = TRUE), "%Y-%m"), " to ", format(max(gt_dates, na.rm = TRUE), "%Y-%m")), "2010-01 to 2024-09", "Raw file should be ordered from oldest to newest.")
}

raw_gt_cols <- setdiff(names(gt_raw), gt_date_col)
add_audit("Google Trends monthly", "Expected raw keyword-column count before exclusions", status_from(length(raw_gt_cols) == EXPECTED_RAW_GT_KEYWORD_COLS), length(raw_gt_cols), EXPECTED_RAW_GT_KEYWORD_COLS, "Count includes the two agreed excluded columns.")

missing_drop_cols <- setdiff(DROP_GT_COLUMNS, names(gt_raw))
add_audit("Google Trends monthly", "Agreed dropped columns present", status_from(length(missing_drop_cols) == 0L), paste(intersect(DROP_GT_COLUMNS, names(gt_raw)), collapse = "; "), paste(DROP_GT_COLUMNS, collapse = "; "), paste("Missing agreed drop columns:", format_values(missing_drop_cols)))

for (drop_col in intersect(DROP_GT_COLUMNS, names(gt_raw))) {
  nonblank_count <- sum(!is_blank(gt_raw[[drop_col]]))
  add_audit("Google Trends monthly", paste0("Dropped column is fully blank: ", drop_col), status_from(nonblank_count == 0L), nonblank_count, 0L, "Column is excluded from modelling but retained unchanged in the raw file.")
}

retained_gt_cols <- setdiff(raw_gt_cols, DROP_GT_COLUMNS)
add_audit("Google Trends monthly", "Expected retained keyword-column count", status_from(length(retained_gt_cols) == EXPECTED_RETAINED_GT_KEYWORD_COLS), length(retained_gt_cols), EXPECTED_RETAINED_GT_KEYWORD_COLS, "Retained columns are modelling candidate keywords.")

if ("Keyword" %in% names(keyword_dict)) {
  dict_keywords <- keyword_dict$Keyword
  missing_from_gt <- setdiff(dict_keywords, retained_gt_cols)
  extra_in_gt <- setdiff(retained_gt_cols, dict_keywords)

  add_audit("Keyword alignment", "All dictionary keywords present among retained GT columns", status_from(length(missing_from_gt) == 0L), length(missing_from_gt), 0L, paste("Missing from KWMonthly retained columns:", format_values(missing_from_gt)))
  add_audit("Keyword alignment", "No retained GT columns absent from dictionary", status_from(length(extra_in_gt) == 0L), length(extra_in_gt), 0L, paste("Extra retained KWMonthly columns:", format_values(extra_in_gt)))
  add_audit("Keyword alignment", "Retained GT columns match dictionary by exact text", status_from(length(missing_from_gt) == 0L && length(extra_in_gt) == 0L), paste0(length(intersect(dict_keywords, retained_gt_cols)), " matched"), paste0(length(dict_keywords), " dictionary keywords"), "This check intentionally does not rely on column position.")
}

if (length(retained_gt_cols) > 0L) {
  gt_retained_raw <- gt_raw[retained_gt_cols]
  gt_retained_num <- as_numeric_df(gt_retained_raw)
  gt_nonnumeric <- count_nonnumeric_cells(gt_retained_raw, gt_retained_num)
  gt_missing <- sum(is.na(as.matrix(gt_retained_num)))
  gt_out_of_range <- sum(as.matrix(gt_retained_num) < 0 | as.matrix(gt_retained_num) > 100, na.rm = TRUE)

  add_audit("Google Trends monthly", "Retained keyword values are numeric", status_from(gt_nonnumeric == 0L), gt_nonnumeric, 0L, "Nonblank retained keyword cells that could not be converted to numeric.")
  add_audit("Google Trends monthly", "No missing values in retained keyword columns", status_from(gt_missing == 0L), gt_missing, 0L, "No filling or imputation is performed. Any missing retained value fails this check.")
  add_audit("Google Trends monthly", "Retained keyword values are within 0-100 range", status_from(gt_out_of_range == 0L), gt_out_of_range, 0L, "Expected Google Trends scale is 0 to 100.")
}

# -----------------------------
# 6. Period alignment audit
# -----------------------------
period_audit <- data.frame(
  block = character(),
  start = character(),
  end = character(),
  n_periods = integer(),
  details = character(),
  stringsAsFactors = FALSE
)

add_period <- function(block, labels, details) {
  period_audit <<- rbind(
    period_audit,
    data.frame(
      block = block,
      start = ifelse(length(labels) > 0L, head(labels, 1L), NA_character_),
      end = ifelse(length(labels) > 0L, tail(labels, 1L), NA_character_),
      n_periods = length(labels),
      details = details,
      stringsAsFactors = FALSE
    )
  )
}

unemp_labels <- if (exists("unemp_q")) unemp_q$label else character(0L)
if (exists("gt_dates")) {
  gt_quarter_labels <- month_to_quarter(gt_dates)
  q_counts <- table(gt_quarter_labels)
  gt_complete_quarters <- names(q_counts[q_counts == 3L])
  gt_complete_q <- parse_quarter(gt_complete_quarters)
  gt_complete_quarters <- gt_complete_quarters[order(gt_complete_q$index)]
} else {
  gt_complete_quarters <- character(0L)
}

overlap_quarters <- intersect(unemp_labels, gt_complete_quarters)
overlap_q <- parse_quarter(overlap_quarters)
overlap_quarters <- overlap_quarters[order(overlap_q$index)]
expected_overlap <- quarter_seq(EXPECTED_GT_OVERLAP_START[["year"]], EXPECTED_GT_OVERLAP_START[["quarter"]], EXPECTED_GT_OVERLAP_END[["year"]], EXPECTED_GT_OVERLAP_END[["quarter"]])

post_gt_expected <- quarter_seq(EXPECTED_POST_GT_START[["year"]], EXPECTED_POST_GT_START[["quarter"]], EXPECTED_POST_GT_END[["year"]], EXPECTED_POST_GT_END[["quarter"]])
post_gt_quarters <- intersect(unemp_labels, post_gt_expected)
post_gt_q <- parse_quarter(post_gt_quarters)
post_gt_quarters <- post_gt_quarters[order(post_gt_q$index)]

add_audit("Period alignment", "Main GT overlap is 2010 Q1 to 2024 Q3", status_from(identical(overlap_quarters, expected_overlap)), span_quarters(overlap_quarters), span_quarters(expected_overlap), "Use this period for same-quarter Google Trends nowcasting.")
add_audit("Period alignment", "Post-GT validation block is 2024 Q4 to 2026 Q1", status_from(identical(post_gt_quarters, post_gt_expected)), span_quarters(post_gt_quarters), span_quarters(post_gt_expected), "Use this period for unemployment-only forecasting validation, not GT nowcasting.")

add_period("Unemployment full series", unemp_labels, "Official outcome series available through 2026 Q1.")
add_period("Google Trends complete-quarter coverage", gt_complete_quarters, "Complete quarterly Google Trends coverage derived from monthly file.")
add_period("Main GT nowcasting overlap", overlap_quarters, "Use for same-quarter Google Trends nowcasting.")
add_period("Post-GT validation block", post_gt_quarters, "Use for unemployment-only forecasting validation.")

# -----------------------------
# 7. Keyword alignment and diagnostics outputs
# -----------------------------
if ("Keyword" %in% names(keyword_dict)) {
  keyword_alignment <- data.frame(
    Column_Name = if ("Column_Name" %in% names(keyword_dict)) keyword_dict$Column_Name else NA_character_,
    Keyword = keyword_dict$Keyword,
    English_Translation = if ("English_Translation" %in% names(keyword_dict)) keyword_dict$English_Translation else NA_character_,
    in_KWMonthly_raw = keyword_dict$Keyword %in% raw_gt_cols,
    retained_for_analysis = keyword_dict$Keyword %in% retained_gt_cols,
    dropped_from_analysis = keyword_dict$Keyword %in% DROP_GT_COLUMNS,
    match_rule = "exact keyword text",
    stringsAsFactors = FALSE
  )

  dropped_alignment <- data.frame(
    Column_Name = NA_character_,
    Keyword = DROP_GT_COLUMNS,
    English_Translation = NA_character_,
    in_KWMonthly_raw = DROP_GT_COLUMNS %in% raw_gt_cols,
    retained_for_analysis = FALSE,
    dropped_from_analysis = TRUE,
    match_rule = "agreed exclusion",
    stringsAsFactors = FALSE
  )

  keyword_alignment <- rbind(keyword_alignment, dropped_alignment)
} else {
  keyword_alignment <- data.frame()
}

if (length(retained_gt_cols) > 0L) {
  gt_num <- as_numeric_df(gt_raw[retained_gt_cols])
  keyword_diagnostics <- data.frame(
    Keyword = retained_gt_cols,
    retained_for_analysis = TRUE,
    dropped_from_analysis = FALSE,
    n_months = nrow(gt_num),
    missing_count = vapply(gt_num, function(z) sum(is.na(z)), integer(1L)),
    zero_count = vapply(gt_num, function(z) sum(z == 0, na.rm = TRUE), integer(1L)),
    zero_proportion = vapply(gt_num, function(z) mean(z == 0, na.rm = TRUE), numeric(1L)),
    min_value = vapply(gt_num, function(z) suppressWarnings(min(z, na.rm = TRUE)), numeric(1L)),
    max_value = vapply(gt_num, function(z) suppressWarnings(max(z, na.rm = TRUE)), numeric(1L)),
    mean_value = vapply(gt_num, function(z) suppressWarnings(mean(z, na.rm = TRUE)), numeric(1L)),
    stringsAsFactors = FALSE
  )

  if ("Keyword" %in% names(keyword_dict)) {
    keyword_diagnostics$Column_Name <- if ("Column_Name" %in% names(keyword_dict)) keyword_dict$Column_Name[match(keyword_diagnostics$Keyword, keyword_dict$Keyword)] else NA_character_
    keyword_diagnostics$English_Translation <- if ("English_Translation" %in% names(keyword_dict)) keyword_dict$English_Translation[match(keyword_diagnostics$Keyword, keyword_dict$Keyword)] else NA_character_
    keyword_diagnostics <- keyword_diagnostics[, c(
      "Column_Name", "Keyword", "English_Translation", "retained_for_analysis",
      "dropped_from_analysis", "n_months", "missing_count", "zero_count",
      "zero_proportion", "min_value", "max_value", "mean_value"
    )]
  }

  dropped_diagnostics <- data.frame(
    Column_Name = NA_character_,
    Keyword = DROP_GT_COLUMNS,
    English_Translation = NA_character_,
    retained_for_analysis = FALSE,
    dropped_from_analysis = TRUE,
    n_months = nrow(gt_raw),
    missing_count = vapply(DROP_GT_COLUMNS, function(nm) if (nm %in% names(gt_raw)) sum(is_blank(gt_raw[[nm]])) else NA_integer_, integer(1L)),
    zero_count = NA_integer_,
    zero_proportion = NA_real_,
    min_value = NA_real_,
    max_value = NA_real_,
    mean_value = NA_real_,
    stringsAsFactors = FALSE
  )

  keyword_diagnostics <- rbind(keyword_diagnostics, dropped_diagnostics)
} else {
  keyword_diagnostics <- data.frame()
}

# -----------------------------
# 8. Write outputs
# -----------------------------
write_csv_utf8(.audit, file.path(OUT_DIR, "00_raw_data_audit.csv"))
write_csv_utf8(keyword_alignment, file.path(OUT_DIR, "00_keyword_alignment.csv"))
write_csv_utf8(keyword_diagnostics, file.path(OUT_DIR, "00_keyword_diagnostics.csv"))
write_csv_utf8(period_audit, file.path(OUT_DIR, "00_period_audit.csv"))

config_lines <- c(
  "{",
  sprintf('  "generated_at": "%s",', json_escape(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))),
  sprintf('  "project_root": "%s",', json_escape(PROJECT_ROOT)),
  sprintf('  "raw_dir_used": "%s",', json_escape(RAW_DIR_USED)),
  sprintf('  "audit_dir": "%s",', json_escape(OUT_DIR)),
  sprintf('  "keyword_dictionary_file": "%s",', json_escape(DICT_FILE)),
  sprintf('  "unemployment_file": "%s",', json_escape(UNEMP_FILE)),
  sprintf('  "google_monthly_file": "%s",', json_escape(GT_FILE)),
  sprintf('  "agreed_dropped_gt_columns": %s,', json_vector(DROP_GT_COLUMNS)),
  '  "keyword_matching_rule": "exact keyword text; no positional matching",',
  '  "blank_cell_filling_or_imputation_by_script": false,',
  '  "retained_keyword_missing_values_allowed": false,',
  '  "main_google_trends_nowcasting_overlap": "2010 Q1 to 2024 Q3",',
  '  "post_google_trends_validation_block": "2024 Q4 to 2026 Q1"',
  "}"
)
write_lines_utf8(config_lines, file.path(OUT_DIR, "00_audit_config.json"))

n_pass <- sum(.audit$status == "PASS")
n_fail <- sum(.audit$status == "FAIL")
n_warn <- sum(.audit$status == "WARN")
n_info <- sum(.audit$status == "INFO")

overall_status <- if (n_fail == 0L) "PASS" else "FAIL"

report_lines <- c(
  "# Step 1 raw data audit",
  "",
  paste0("Generated on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("Project root: `", PROJECT_ROOT, "`"),
  paste0("Raw data directory used: `", RAW_DIR_USED, "`"),
  paste0("Output directory: `", OUT_DIR, "`"),
  "",
  paste0("Overall status: **", overall_status, "**"),
  "",
  "## Agreed audit rules",
  "",
  "- Raw files are audited but not modified.",
  paste0("- The agreed Google Trends exclusions are: `", paste(DROP_GT_COLUMNS, collapse = "`, `"), "`."),
  "- No blank-cell correction, filling, or imputation is performed by this script.",
  "- Keyword alignment is checked by exact keyword text, not by column position.",
  "- Main same-quarter Google Trends nowcasting overlap is 2010 Q1 to 2024 Q3.",
  "- Post-Google-Trends unemployment-only validation block is 2024 Q4 to 2026 Q1.",
  "",
  "## Summary",
  "",
  paste0("- PASS checks: ", n_pass),
  paste0("- FAIL checks: ", n_fail),
  paste0("- WARN checks: ", n_warn),
  paste0("- INFO checks: ", n_info),
  "",
  "## Failed checks, if any",
  ""
)

if (n_fail == 0L) {
  report_lines <- c(report_lines, "No failed checks.")
} else {
  failed <- .audit[.audit$status == "FAIL", ]
  report_lines <- c(report_lines, paste0(
    "- **", failed$area, " / ", failed$check, "**: observed `",
    failed$observed, "`, expected `", failed$expected, "`. ", failed$details
  ))
}

report_lines <- c(
  report_lines,
  "",
  "## Output files",
  "",
  "- `00_raw_data_audit.csv`",
  "- `00_keyword_alignment.csv`",
  "- `00_keyword_diagnostics.csv`",
  "- `00_period_audit.csv`",
  "- `00_audit_config.json`",
  "- `00_raw_data_audit_report.md`"
)

write_lines_utf8(report_lines, file.path(OUT_DIR, "00_raw_data_audit_report.md"))

message("Step 1 raw data audit complete.")
message("Raw directory used: ", RAW_DIR_USED)
message("Output directory: ", OUT_DIR)
message("PASS: ", n_pass, " | FAIL: ", n_fail, " | WARN: ", n_warn, " | INFO: ", n_info)

if (STOP_ON_FAIL && n_fail > 0L) {
  stop(
    "Raw data audit failed. See ",
    file.path(OUT_DIR, "00_raw_data_audit_report.md"),
    " and ", file.path(OUT_DIR, "00_raw_data_audit.csv"),
    call. = FALSE
  )
}

invisible(.audit)
