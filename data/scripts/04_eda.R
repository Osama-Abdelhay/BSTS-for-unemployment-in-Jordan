#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# 04_eda.R
# Step 5: Exploratory diagnostics before modelling
# -----------------------------------------------------------------------------
# Purpose:
#   Produce descriptive diagnostics for the clean unemployment and Google Trends
#   modelling panels before any model fitting begins.
#
# Inputs:
#   data/processed/modeling_panel_gt_overlap.csv
#   data/processed/modeling_panel_unemployment_full.csv
#   data/processed/keyword_dictionary_clean.csv
#   data/processed/google_trends_quarterly_MA.csv
#   data/processed/google_trends_quarterly_EMA.csv
#
# Outputs:
#   data/processed/eda/tables/eda_unemployment_summary.csv
#   data/processed/eda/tables/eda_keyword_sparsity.csv
#   data/processed/eda/tables/eda_ma_ema_comparison.csv
#   data/processed/eda/tables/eda_keyword_correlations_with_unemployment.csv
#   data/processed/eda/tables/eda_keyword_dictionary_for_interpretation.csv
#   data/processed/eda/tables/eda_theme_summary.csv
#   data/processed/eda/tables/eda_selected_keywords_for_plots.csv
#   data/processed/eda/figures/eda_unemployment_rate_full.png
#   data/processed/eda/figures/eda_unemployment_rate_gt_overlap.png
#   data/processed/eda/figures/eda_keyword_zero_proportions.png
#   data/processed/eda/figures/eda_top_keyword_correlations.png
#   data/processed/eda/figures/eda_ma_ema_difference.png
#   data/processed/eda/figures/eda_ma_vs_ema_selected_keywords.png
#   data/processed/eda/04_eda_manifest.csv
#   data/processed/eda/04_eda_report.md
#
# Agreed pipeline rules:
#   - This script is diagnostic only.
#   - It does not modify raw files.
#   - It does not fill missing values, impute, interpolate, or create future GT
#     predictors.
#   - It does not perform global feature selection or global standardisation.
#   - Sparse-keyword results are descriptive; final filtering must be estimated
#     inside each rolling-validation training window.
#   - MA and EMA are analysed. SAWA remains deferred until the rolling-validation
#     structure is stable.
#
# Usage from project root:
#   Rscript scripts/04_eda.R
#
# Optional arguments:
#   Rscript scripts/04_eda.R \
#     --processed-dir=data/processed \
#     --eda-dir=data/processed/eda
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

EDA_DIR <- normalizePath(get_arg("eda-dir", file.path(PROCESSED_DIR_DEFAULT, "eda")), mustWork = FALSE)
TABLES_DIR <- normalizePath(get_arg("tables-dir", file.path(EDA_DIR, "tables")), mustWork = FALSE)
FIGURES_DIR <- normalizePath(get_arg("figures-dir", file.path(EDA_DIR, "figures")), mustWork = FALSE)

GT_PANEL_FILE <- "modeling_panel_gt_overlap.csv"
UNEMP_FULL_FILE <- "modeling_panel_unemployment_full.csv"
KEYWORD_DICTIONARY_FILE <- "keyword_dictionary_clean.csv"
GT_MA_FILE <- "google_trends_quarterly_MA.csv"
GT_EMA_FILE <- "google_trends_quarterly_EMA.csv"

EXPECTED_GT_START <- "2010-1"
EXPECTED_GT_END <- "2024-3"
EXPECTED_GT_QUARTERS <- 59L
EXPECTED_FULL_START <- "2010-1"
EXPECTED_FULL_END <- "2026-1"
EXPECTED_FULL_QUARTERS <- 65L
EXPECTED_KEYWORDS <- 71L

OUTPUT_UNEMP_SUMMARY <- file.path(TABLES_DIR, "eda_unemployment_summary.csv")
OUTPUT_KEYWORD_SPARSITY <- file.path(TABLES_DIR, "eda_keyword_sparsity.csv")
OUTPUT_MA_EMA_COMPARISON <- file.path(TABLES_DIR, "eda_ma_ema_comparison.csv")
OUTPUT_CORRELATIONS <- file.path(TABLES_DIR, "eda_keyword_correlations_with_unemployment.csv")
OUTPUT_KEYWORD_INTERPRETATION <- file.path(TABLES_DIR, "eda_keyword_dictionary_for_interpretation.csv")
OUTPUT_THEME_SUMMARY <- file.path(TABLES_DIR, "eda_theme_summary.csv")
OUTPUT_SELECTED_KEYWORDS <- file.path(TABLES_DIR, "eda_selected_keywords_for_plots.csv")
OUTPUT_MANIFEST <- file.path(EDA_DIR, "04_eda_manifest.csv")
OUTPUT_REPORT <- file.path(EDA_DIR, "04_eda_report.md")

FIG_UNEMP_FULL <- file.path(FIGURES_DIR, "eda_unemployment_rate_full.png")
FIG_UNEMP_GT <- file.path(FIGURES_DIR, "eda_unemployment_rate_gt_overlap.png")
FIG_ZERO_PROP <- file.path(FIGURES_DIR, "eda_keyword_zero_proportions.png")
FIG_TOP_COR <- file.path(FIGURES_DIR, "eda_top_keyword_correlations.png")
FIG_MA_EMA_DIFF <- file.path(FIGURES_DIR, "eda_ma_ema_difference.png")
FIG_MA_EMA_SELECTED <- file.path(FIGURES_DIR, "eda_ma_vs_ema_selected_keywords.png")

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

as_numeric_vector <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  suppressWarnings(as.numeric(x_chr))
}

as_logical_vector <- function(x) {
  if (is.logical(x)) return(x)
  x_chr <- tolower(trimws(as.character(x)))
  x_chr %in% c("true", "t", "1", "yes", "y")
}

safe_cor <- function(x, y) {
  x <- as_numeric_vector(x)
  y <- as_numeric_vector(y)
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3L) return(NA_real_)
  if (sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  cor(x[ok], y[ok])
}

safe_sd <- function(x) {
  x <- as_numeric_vector(x)
  if (sum(is.finite(x)) < 2L) return(NA_real_)
  sd(x, na.rm = TRUE)
}

quarter_start_date <- function(year, q) {
  month <- (as.integer(q) - 1L) * 3L + 1L
  as.Date(sprintf("%04d-%02d-01", as.integer(year), month))
}

contains_any <- function(text, patterns) {
  text <- tolower(paste(text, collapse = " "))
  any(vapply(patterns, function(p) grepl(tolower(p), text, fixed = TRUE), logical(1L)))
}

theme_for_keyword <- function(keyword, english_translation) {
  text <- paste(keyword, english_translation)

  if (contains_any(text, c("bayt", "بيت", "akhtaboot", "اخطبوط"))) {
    return("Job portals")
  }
  if (contains_any(text, c("social security", "unemployment benefit", "ضمان", "بدل تعطل"))) {
    return("Social protection and benefits")
  }
  if (contains_any(text, c("layoff", "fired", "arbitrary", "cut off", "dismissal", "termination", "تسريح", "تعسفي", "مقطوع"))) {
    return("Separation and labour rights")
  }
  if (contains_any(text, c("salary", "salaries", "wage", "wages", "income", "payment", "bonus", "deduction", "tax", "taxes", "spending", "money", "رواتب", "راتب", "اجر", "اجور", "مصاري", "فلوس", "ضريبة", "ضرائب", "حسم", "اقتطاع", "انفاق"))) {
    return("Wages, income and finances")
  }
  if (contains_any(text, c("day labor", "day labour", "per diem", "work from home", "freelance", "مياومة", "من المنزل"))) {
    return("Informal and flexible work")
  }
  if (contains_any(text, c("should we steal", "where do we get", "not a life", "broke", "quick gain", "legitimate earning", "ظايل نسرق", "من وين نجيب", "مش عيشة", "مفلس", "كسب"))) {
    return("Economic hardship and distress")
  }
  if (contains_any(text, c("human resources", "hr", "employer", "employee", "worker", "boss", "my job", "my work", "موارد بشرية", "عامل", "وظيفتي", "شغلي", "عملي"))) {
    return("Employment relationship and HR")
  }
  if (contains_any(text, c("training", "trainee", "تدريب", "متدرب"))) {
    return("Training and employability")
  }
  if (contains_any(text, c("job hunting", "looking for a job", "need a job", "want to work", "employ us", "resume", "cv", "jobs for women", "job", "work", "unemployment", "unemployed", "بطالة", "unemploy", "وظيفة", "شغل", "عمل", "بدي", "بدور", "ساعدوني"))) {
    return("Job search and employment")
  }

  "Other / needs review"
}

nice_keyword_label <- function(keyword_id, english_translation, max_chars = 42L) {
  label <- paste0(keyword_id, " — ", english_translation)
  ifelse(nchar(label) > max_chars, paste0(substr(label, 1L, max_chars - 3L), "..."), label)
}

make_unemp_summary <- function(df, label) {
  y <- as_numeric_vector(df$unemployment_rate_nationals)
  dy <- c(NA_real_, diff(y))
  data.frame(
    sample = label,
    n_quarters = nrow(df),
    start_quarter = df$Quarter[[1]],
    end_quarter = df$Quarter[[nrow(df)]],
    mean_unemployment_rate = mean(y, na.rm = TRUE),
    sd_unemployment_rate = safe_sd(y),
    min_unemployment_rate = min(y, na.rm = TRUE),
    min_quarter = df$Quarter[[which.min(y)]],
    max_unemployment_rate = max(y, na.rm = TRUE),
    max_quarter = df$Quarter[[which.max(y)]],
    first_value = y[[1]],
    last_value = y[[length(y)]],
    absolute_change_first_to_last = y[[length(y)]] - y[[1]],
    average_quarterly_change = mean(diff(y), na.rm = TRUE),
    largest_quarterly_increase = max(dy, na.rm = TRUE),
    largest_quarterly_increase_quarter = df$Quarter[[which.max(dy)]],
    largest_quarterly_decrease = min(dy, na.rm = TRUE),
    largest_quarterly_decrease_quarter = df$Quarter[[which.min(dy)]],
    stringsAsFactors = FALSE
  )
}

format_key_rows <- function(df, columns, n = 5L) {
  if (nrow(df) == 0L) return("none")
  df <- df[seq_len(min(n, nrow(df))), columns, drop = FALSE]
  rows <- apply(df, 1L, function(z) paste(paste(names(z), z, sep = ": "), collapse = "; "))
  paste0("- ", rows)
}

# -----------------------------
# 2. Load inputs and validate
# -----------------------------
ensure_dir(EDA_DIR)
ensure_dir(TABLES_DIR)
ensure_dir(FIGURES_DIR)

path_gt_panel <- locate_file(GT_PANEL_FILE, CANDIDATE_PROCESSED_DIRS)
path_unemp_full <- locate_file(UNEMP_FULL_FILE, CANDIDATE_PROCESSED_DIRS)
path_keyword_dictionary <- locate_file(KEYWORD_DICTIONARY_FILE, CANDIDATE_PROCESSED_DIRS)
path_ma <- locate_file(GT_MA_FILE, CANDIDATE_PROCESSED_DIRS)
path_ema <- locate_file(GT_EMA_FILE, CANDIDATE_PROCESSED_DIRS)

gt_panel <- read_csv_utf8(path_gt_panel)
unemp_full <- read_csv_utf8(path_unemp_full)
keyword_dictionary <- read_csv_utf8(path_keyword_dictionary)
ma_quarterly <- read_csv_utf8(path_ma)
ema_quarterly <- read_csv_utf8(path_ema)

required_panel_cols <- c("Quarter", "Year", "Q", "quarter_start", "unemployment_rate_nationals")
missing_panel_cols <- setdiff(required_panel_cols, names(gt_panel))
if (length(missing_panel_cols) > 0L) stop("GT panel is missing required columns: ", paste(missing_panel_cols, collapse = ", "), call. = FALSE)

required_full_cols <- c("Quarter", "Year", "Q", "quarter_start", "unemployment_rate_nationals", "gt_available", "post_gt_endpoint")
missing_full_cols <- setdiff(required_full_cols, names(unemp_full))
if (length(missing_full_cols) > 0L) stop("Full unemployment panel is missing required columns: ", paste(missing_full_cols, collapse = ", "), call. = FALSE)

required_dictionary_cols <- c("Column_Name", "Keyword", "English_Translation")
missing_dictionary_cols <- setdiff(required_dictionary_cols, names(keyword_dictionary))
if (length(missing_dictionary_cols) > 0L) stop("Keyword dictionary is missing required columns: ", paste(missing_dictionary_cols, collapse = ", "), call. = FALSE)

if (nrow(gt_panel) != EXPECTED_GT_QUARTERS) stop("GT overlap panel should have ", EXPECTED_GT_QUARTERS, " rows; found ", nrow(gt_panel), call. = FALSE)
if (gt_panel$Quarter[[1]] != EXPECTED_GT_START || gt_panel$Quarter[[nrow(gt_panel)]] != EXPECTED_GT_END) {
  stop("GT overlap panel expected period ", EXPECTED_GT_START, " to ", EXPECTED_GT_END, "; found ", gt_panel$Quarter[[1]], " to ", gt_panel$Quarter[[nrow(gt_panel)]], call. = FALSE)
}
if (nrow(unemp_full) != EXPECTED_FULL_QUARTERS) stop("Full unemployment panel should have ", EXPECTED_FULL_QUARTERS, " rows; found ", nrow(unemp_full), call. = FALSE)
if (unemp_full$Quarter[[1]] != EXPECTED_FULL_START || unemp_full$Quarter[[nrow(unemp_full)]] != EXPECTED_FULL_END) {
  stop("Full unemployment panel expected period ", EXPECTED_FULL_START, " to ", EXPECTED_FULL_END, "; found ", unemp_full$Quarter[[1]], " to ", unemp_full$Quarter[[nrow(unemp_full)]], call. = FALSE)
}
if (nrow(keyword_dictionary) != EXPECTED_KEYWORDS) stop("Keyword dictionary should have ", EXPECTED_KEYWORDS, " rows; found ", nrow(keyword_dictionary), call. = FALSE)

keyword_ids <- keyword_dictionary$Column_Name
ma_cols <- paste0("MA__", keyword_ids)
ema_cols <- paste0("EMA__", keyword_ids)
missing_ma <- setdiff(ma_cols, names(gt_panel))
missing_ema <- setdiff(ema_cols, names(gt_panel))
if (length(missing_ma) > 0L) stop("Missing MA columns in GT panel: ", paste(missing_ma, collapse = ", "), call. = FALSE)
if (length(missing_ema) > 0L) stop("Missing EMA columns in GT panel: ", paste(missing_ema, collapse = ", "), call. = FALSE)

# Date helpers for figures
gt_panel$quarter_start_date <- as.Date(gt_panel$quarter_start)
unemp_full$quarter_start_date <- as.Date(unemp_full$quarter_start)
unemp_full$gt_available <- as_logical_vector(unemp_full$gt_available)
unemp_full$post_gt_endpoint <- as_logical_vector(unemp_full$post_gt_endpoint)

# -----------------------------
# 3. Keyword dictionary for interpretation
# -----------------------------
keyword_interpretation <- data.frame(
  Keyword_ID = keyword_dictionary$Column_Name,
  Arabic_or_Source_Keyword = keyword_dictionary$Keyword,
  English_Translation = keyword_dictionary$English_Translation,
  Suggested_Theme = vapply(
    seq_len(nrow(keyword_dictionary)),
    function(i) theme_for_keyword(keyword_dictionary$Keyword[[i]], keyword_dictionary$English_Translation[[i]]),
    character(1L)
  ),
  Theme_Source = "rule-based draft for manual review",
  stringsAsFactors = FALSE
)
write_csv_utf8(keyword_interpretation, OUTPUT_KEYWORD_INTERPRETATION)

# -----------------------------
# 4. Unemployment diagnostics
# -----------------------------
gt_overlap_full <- unemp_full[unemp_full$gt_available, , drop = FALSE]
post_gt <- unemp_full[unemp_full$post_gt_endpoint, , drop = FALSE]

unemployment_summary <- rbind(
  make_unemp_summary(unemp_full, "full_unemployment_series_2010Q1_2026Q1"),
  make_unemp_summary(gt_overlap_full, "google_trends_overlap_2010Q1_2024Q3"),
  make_unemp_summary(post_gt, "post_gt_validation_block_2024Q4_2026Q1")
)
write_csv_utf8(unemployment_summary, OUTPUT_UNEMP_SUMMARY)

# -----------------------------
# 5. Keyword diagnostics
# -----------------------------
y <- as_numeric_vector(gt_panel$unemployment_rate_nationals)
keyword_rows <- vector("list", length(keyword_ids))

for (i in seq_along(keyword_ids)) {
  kid <- keyword_ids[[i]]
  ma_col <- paste0("MA__", kid)
  ema_col <- paste0("EMA__", kid)
  x_ma <- as_numeric_vector(gt_panel[[ma_col]])
  x_ema <- as_numeric_vector(gt_panel[[ema_col]])
  diff_abs <- abs(x_ma - x_ema)
  max_diff_idx <- which.max(diff_abs)

  keyword_rows[[i]] <- data.frame(
    Keyword_ID = kid,
    Arabic_or_Source_Keyword = keyword_dictionary$Keyword[[i]],
    English_Translation = keyword_dictionary$English_Translation[[i]],
    Suggested_Theme = keyword_interpretation$Suggested_Theme[[i]],
    n_quarters = nrow(gt_panel),
    zero_count_MA = sum(x_ma == 0, na.rm = TRUE),
    zero_prop_MA = mean(x_ma == 0, na.rm = TRUE),
    zero_count_EMA = sum(x_ema == 0, na.rm = TRUE),
    zero_prop_EMA = mean(x_ema == 0, na.rm = TRUE),
    nonzero_count_MA = sum(x_ma != 0, na.rm = TRUE),
    nonzero_count_EMA = sum(x_ema != 0, na.rm = TRUE),
    all_zero_MA = all(x_ma == 0, na.rm = TRUE),
    all_zero_EMA = all(x_ema == 0, na.rm = TRUE),
    mean_MA = mean(x_ma, na.rm = TRUE),
    sd_MA = safe_sd(x_ma),
    min_MA = min(x_ma, na.rm = TRUE),
    max_MA = max(x_ma, na.rm = TRUE),
    mean_EMA = mean(x_ema, na.rm = TRUE),
    sd_EMA = safe_sd(x_ema),
    min_EMA = min(x_ema, na.rm = TRUE),
    max_EMA = max(x_ema, na.rm = TRUE),
    ma_ema_correlation = safe_cor(x_ma, x_ema),
    mean_abs_MA_EMA_difference = mean(diff_abs, na.rm = TRUE),
    median_abs_MA_EMA_difference = median(diff_abs, na.rm = TRUE),
    max_abs_MA_EMA_difference = max(diff_abs, na.rm = TRUE),
    max_abs_difference_quarter = gt_panel$Quarter[[max_diff_idx]],
    cor_MA_same_quarter = safe_cor(x_ma, y),
    cor_EMA_same_quarter = safe_cor(x_ema, y),
    cor_MA_lag1_search_to_unemployment = safe_cor(head(x_ma, -1L), tail(y, -1L)),
    cor_EMA_lag1_search_to_unemployment = safe_cor(head(x_ema, -1L), tail(y, -1L)),
    stringsAsFactors = FALSE
  )
}

keyword_diagnostics <- do.call(rbind, keyword_rows)
write_csv_utf8(keyword_diagnostics, OUTPUT_KEYWORD_SPARSITY)

ma_ema_comparison <- keyword_diagnostics[, c(
  "Keyword_ID", "Arabic_or_Source_Keyword", "English_Translation", "Suggested_Theme",
  "n_quarters", "ma_ema_correlation", "mean_abs_MA_EMA_difference",
  "median_abs_MA_EMA_difference", "max_abs_MA_EMA_difference", "max_abs_difference_quarter"
)]
write_csv_utf8(ma_ema_comparison, OUTPUT_MA_EMA_COMPARISON)

correlation_diagnostics <- keyword_diagnostics[, c(
  "Keyword_ID", "Arabic_or_Source_Keyword", "English_Translation", "Suggested_Theme",
  "cor_MA_same_quarter", "cor_EMA_same_quarter",
  "cor_MA_lag1_search_to_unemployment", "cor_EMA_lag1_search_to_unemployment"
)]
write_csv_utf8(correlation_diagnostics, OUTPUT_CORRELATIONS)

# Theme summary is descriptive and draft. It is not used for modelling decisions.
theme_values <- sort(unique(keyword_diagnostics$Suggested_Theme))
theme_rows <- lapply(theme_values, function(th) {
  d <- keyword_diagnostics[keyword_diagnostics$Suggested_Theme == th, , drop = FALSE]
  data.frame(
    Suggested_Theme = th,
    n_keywords = nrow(d),
    mean_zero_prop_MA = mean(d$zero_prop_MA, na.rm = TRUE),
    median_zero_prop_MA = median(d$zero_prop_MA, na.rm = TRUE),
    n_all_zero_MA = sum(d$all_zero_MA, na.rm = TRUE),
    mean_abs_same_quarter_cor_MA = mean(abs(d$cor_MA_same_quarter), na.rm = TRUE),
    mean_abs_MA_EMA_difference = mean(d$mean_abs_MA_EMA_difference, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
})
theme_summary <- do.call(rbind, theme_rows)
theme_summary <- theme_summary[order(-theme_summary$n_keywords, theme_summary$Suggested_Theme), , drop = FALSE]
write_csv_utf8(theme_summary, OUTPUT_THEME_SUMMARY)

selected_keywords <- keyword_diagnostics[!keyword_diagnostics$all_zero_MA & keyword_diagnostics$zero_prop_MA <= 0.90 & !is.na(keyword_diagnostics$cor_MA_same_quarter), , drop = FALSE]
selected_keywords$abs_cor_MA_same_quarter <- abs(selected_keywords$cor_MA_same_quarter)
selected_keywords <- selected_keywords[order(-selected_keywords$abs_cor_MA_same_quarter), , drop = FALSE]
selected_keywords <- head(selected_keywords, 6L)
if (nrow(selected_keywords) < 6L) {
  fill <- keyword_diagnostics[!keyword_diagnostics$Keyword_ID %in% selected_keywords$Keyword_ID, , drop = FALSE]
  fill <- fill[order(-fill$mean_abs_MA_EMA_difference), , drop = FALSE]
  selected_keywords <- rbind(selected_keywords, head(fill, 6L - nrow(selected_keywords)))
}
selected_keywords <- selected_keywords[, c(
  "Keyword_ID", "Arabic_or_Source_Keyword", "English_Translation", "Suggested_Theme",
  "cor_MA_same_quarter", "cor_EMA_same_quarter", "zero_prop_MA", "mean_abs_MA_EMA_difference"
), drop = FALSE]
write_csv_utf8(selected_keywords, OUTPUT_SELECTED_KEYWORDS)

# -----------------------------
# 6. Figures
# -----------------------------
# Full unemployment series
png(FIG_UNEMP_FULL, width = 1600, height = 900, res = 160)
par(mar = c(5, 5, 4, 2))
plot(
  unemp_full$quarter_start_date,
  as_numeric_vector(unemp_full$unemployment_rate_nationals),
  type = "o",
  pch = 16,
  lwd = 2,
  xlab = "Quarter",
  ylab = "Unemployment rate (%)",
  main = "Quarterly unemployment rate, full official series"
)
abline(v = as.Date("2024-10-01"), lty = 2)
text(as.Date("2024-10-01"), par("usr")[[4]], "Post-GT validation begins", srt = 90, pos = 2, cex = 0.8)
grid()
dev.off()

# GT overlap unemployment series
png(FIG_UNEMP_GT, width = 1600, height = 900, res = 160)
par(mar = c(5, 5, 4, 2))
plot(
  gt_overlap_full$quarter_start_date,
  as_numeric_vector(gt_overlap_full$unemployment_rate_nationals),
  type = "o",
  pch = 16,
  lwd = 2,
  xlab = "Quarter",
  ylab = "Unemployment rate (%)",
  main = "Quarterly unemployment rate, Google Trends overlap sample"
)
grid()
dev.off()

# Keyword zero proportions
zero_plot <- keyword_diagnostics[order(-keyword_diagnostics$zero_prop_MA), , drop = FALSE]
zero_plot <- head(zero_plot, 30L)
zero_plot <- zero_plot[rev(seq_len(nrow(zero_plot))), , drop = FALSE]
zero_labels <- nice_keyword_label(zero_plot$Keyword_ID, zero_plot$English_Translation, max_chars = 45L)
png(FIG_ZERO_PROP, width = 1600, height = 1200, res = 160)
par(mar = c(5, 12, 4, 2))
barplot(
  zero_plot$zero_prop_MA,
  horiz = TRUE,
  names.arg = zero_labels,
  las = 1,
  cex.names = 0.65,
  xlab = "Zero proportion in MA quarterly series",
  main = "Most sparse Google Trends keywords in the GT overlap sample",
  xlim = c(0, 1)
)
grid(nx = NA, ny = NULL)
dev.off()

# Top same-quarter correlations by absolute value
cor_plot <- keyword_diagnostics[!is.na(keyword_diagnostics$cor_MA_same_quarter), , drop = FALSE]
cor_plot$abs_cor <- abs(cor_plot$cor_MA_same_quarter)
cor_plot <- cor_plot[order(-cor_plot$abs_cor), , drop = FALSE]
cor_plot <- head(cor_plot, 25L)
cor_plot <- cor_plot[rev(seq_len(nrow(cor_plot))), , drop = FALSE]
cor_labels <- nice_keyword_label(cor_plot$Keyword_ID, cor_plot$English_Translation, max_chars = 45L)
png(FIG_TOP_COR, width = 1600, height = 1200, res = 160)
par(mar = c(5, 12, 4, 2))
barplot(
  cor_plot$cor_MA_same_quarter,
  horiz = TRUE,
  names.arg = cor_labels,
  las = 1,
  cex.names = 0.65,
  xlab = "Same-quarter correlation with unemployment (MA)",
  main = "Top absolute same-quarter correlations with unemployment",
  xlim = c(-1, 1)
)
abline(v = 0, lwd = 1)
grid(nx = NA, ny = NULL)
dev.off()

# MA/EMA difference
ma_ema_plot <- keyword_diagnostics[order(-keyword_diagnostics$mean_abs_MA_EMA_difference), , drop = FALSE]
ma_ema_plot <- head(ma_ema_plot, 30L)
ma_ema_plot <- ma_ema_plot[rev(seq_len(nrow(ma_ema_plot))), , drop = FALSE]
diff_labels <- nice_keyword_label(ma_ema_plot$Keyword_ID, ma_ema_plot$English_Translation, max_chars = 45L)
png(FIG_MA_EMA_DIFF, width = 1600, height = 1200, res = 160)
par(mar = c(5, 12, 4, 2))
barplot(
  ma_ema_plot$mean_abs_MA_EMA_difference,
  horiz = TRUE,
  names.arg = diff_labels,
  las = 1,
  cex.names = 0.65,
  xlab = "Mean absolute difference between MA and EMA",
  main = "Keywords most affected by EMA recency weighting"
)
grid(nx = NA, ny = NULL)
dev.off()

# MA versus EMA selected keywords
png(FIG_MA_EMA_SELECTED, width = 1800, height = 1400, res = 160)
par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
for (kid in selected_keywords$Keyword_ID[seq_len(min(6L, nrow(selected_keywords)))]) {
  row <- keyword_diagnostics[keyword_diagnostics$Keyword_ID == kid, , drop = FALSE]
  ma_series <- as_numeric_vector(gt_panel[[paste0("MA__", kid)]])
  ema_series <- as_numeric_vector(gt_panel[[paste0("EMA__", kid)]])
  y_lim <- range(c(ma_series, ema_series), na.rm = TRUE)
  plot(
    gt_panel$quarter_start_date,
    ma_series,
    type = "l",
    lwd = 2,
    ylim = y_lim,
    xlab = "Quarter",
    ylab = "Search interest",
    main = paste0(kid, ": ", row$English_Translation[[1]])
  )
  lines(gt_panel$quarter_start_date, ema_series, lwd = 2, lty = 2)
  legend("topright", legend = c("MA", "EMA"), lty = c(1, 2), lwd = c(2, 2), bty = "n", cex = 0.8)
  grid()
}
mtext("MA versus EMA for selected high-signal keywords", outer = TRUE, cex = 1.2, font = 2)
dev.off()

# -----------------------------
# 7. Manifest and report
# -----------------------------
manifest_files <- c(
  list.files(TABLES_DIR, pattern = "\\.csv$", full.names = TRUE),
  list.files(FIGURES_DIR, pattern = "\\.png$", full.names = TRUE)
)
manifest_rows <- lapply(manifest_files, function(path) {
  ext <- tools::file_ext(path)
  if (tolower(ext) == "csv") {
    temp <- read_csv_utf8(path)
    rows <- nrow(temp)
    cols <- ncol(temp)
    file_type <- "table"
  } else {
    rows <- NA_integer_
    cols <- NA_integer_
    file_type <- "figure"
  }
  data.frame(
    file = basename(path),
    relative_path = sub(paste0(normalizePath(EDA_DIR, mustWork = FALSE), "/?"), "", normalizePath(path, mustWork = FALSE)),
    type = file_type,
    rows = rows,
    columns = cols,
    stringsAsFactors = FALSE
  )
})
manifest <- do.call(rbind, manifest_rows)
write_csv_utf8(manifest, OUTPUT_MANIFEST)

all_zero_keywords <- keyword_diagnostics$Keyword_ID[keyword_diagnostics$all_zero_MA]
most_sparse <- keyword_diagnostics[order(-keyword_diagnostics$zero_prop_MA), , drop = FALSE]
most_sparse <- head(most_sparse, 5L)
top_cor <- keyword_diagnostics[!is.na(keyword_diagnostics$cor_MA_same_quarter), , drop = FALSE]
top_cor$abs_cor <- abs(top_cor$cor_MA_same_quarter)
top_cor <- top_cor[order(-top_cor$abs_cor), , drop = FALSE]
top_cor <- head(top_cor, 5L)

report_lines <- c(
  "# Step 5 EDA report",
  "",
  "## Inputs",
  "",
  paste0("- ", basename(path_gt_panel)),
  paste0("- ", basename(path_unemp_full)),
  paste0("- ", basename(path_keyword_dictionary)),
  paste0("- ", basename(path_ma)),
  paste0("- ", basename(path_ema)),
  "",
  "## Scope",
  "",
  "This is a descriptive diagnostic layer only. It does not perform modelling, global feature selection, global standardisation, imputation, interpolation, or SAWA generation. All feature filtering decisions remain reserved for the rolling-validation training windows.",
  "",
  "## Unemployment series",
  "",
  paste0("- Full official unemployment series: ", nrow(unemp_full), " quarters, ", unemp_full$Quarter[[1]], " to ", unemp_full$Quarter[[nrow(unemp_full)]], "."),
  paste0("- Google Trends overlap sample: ", nrow(gt_overlap_full), " quarters, ", gt_overlap_full$Quarter[[1]], " to ", gt_overlap_full$Quarter[[nrow(gt_overlap_full)]], "."),
  paste0("- Post-GT unemployment-only validation block: ", nrow(post_gt), " quarters, ", post_gt$Quarter[[1]], " to ", post_gt$Quarter[[nrow(post_gt)]], "."),
  paste0("- Full-series unemployment range: ", sprintf("%.1f", min(as_numeric_vector(unemp_full$unemployment_rate_nationals), na.rm = TRUE)), "% to ", sprintf("%.1f", max(as_numeric_vector(unemp_full$unemployment_rate_nationals), na.rm = TRUE)), "%."),
  "",
  "## Google Trends keyword diagnostics",
  "",
  paste0("- Retained keyword series: ", length(keyword_ids), "."),
  paste0("- All-zero MA keyword series: ", length(all_zero_keywords), "."),
  paste0("- All-zero keywords: ", ifelse(length(all_zero_keywords) == 0L, "none", paste(all_zero_keywords, collapse = ", ")), "."),
  "- Tables include zero proportions, basic distribution statistics, MA/EMA differences, and same-quarter/lagged correlations with unemployment.",
  "",
  "## Most sparse keywords by MA zero proportion",
  "",
  format_key_rows(most_sparse, c("Keyword_ID", "English_Translation", "zero_prop_MA"), n = 5L),
  "",
  "## Top same-quarter MA correlations with unemployment by absolute value",
  "",
  format_key_rows(top_cor, c("Keyword_ID", "English_Translation", "cor_MA_same_quarter"), n = 5L),
  "",
  "## Generated outputs",
  "",
  "See `04_eda_manifest.csv` for a complete list of tables and figures."
)
write_lines_utf8(report_lines, OUTPUT_REPORT)

cat("Step 5 EDA complete.\n")
cat("Tables written to: ", TABLES_DIR, "\n", sep = "")
cat("Figures written to: ", FIGURES_DIR, "\n", sep = "")
cat("Report written to: ", OUTPUT_REPORT, "\n", sep = "")
