# ==============================================================================
# 00_packages.R
# Project: Bayesian nowcasting framework for unemployment monitoring in Jordan
# Purpose: Package loading, project paths, default configuration, reproducibility
# ============================================================================== 

# ---- Project-root discovery ----------------------------------------------------
# This allows scripts to run from either the project root or the scripts/ folder.
find_project_root <- function(start = getwd()) {
  start <- normalizePath(start, winslash = "/", mustWork = FALSE)
  candidates <- unique(c(
    start,
    dirname(start),
    dirname(dirname(start)),
    dirname(dirname(dirname(start)))
  ))

  is_project_root <- vapply(
    candidates,
    function(x) {
      dir.exists(file.path(x, "R")) && dir.exists(file.path(x, "scripts"))
    },
    logical(1)
  )

  if (any(is_project_root)) {
    return(candidates[which(is_project_root)[1]])
  }

  # Fallback: if this file is sourced interactively from R/, parent is root.
  if (dir.exists("R") && dir.exists("scripts")) {
    return(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
  }

  stop(
    "Could not locate the project root. Run scripts from the project root, ",
    "or set PROJECT_ROOT to the project directory.",
    call. = FALSE
  )
}

PROJECT_ROOT <- Sys.getenv("PROJECT_ROOT", unset = find_project_root())
PROJECT_ROOT <- normalizePath(PROJECT_ROOT, winslash = "/", mustWork = FALSE)

path_abs <- function(...) {
  normalizePath(file.path(PROJECT_ROOT, ...), winslash = "/", mustWork = FALSE)
}

# ---- Locale and reproducibility ------------------------------------------------
# UTF-8 is important because the dataset includes Arabic keywords and translations.
options(
  stringsAsFactors = FALSE,
  encoding = "UTF-8",
  scipen = 999
)

try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE)

# ---- Package management --------------------------------------------------------
required_packages <- c(
  "readr",
  "dplyr",
  "tidyr",
  "tibble",
  "stringr",
  "purrr",
  "lubridate",
  "ggplot2",
  "janitor",
  "yaml",
  "fs",
  "glue",
  "cli",
  "rlang"
)

# Packages used in later modelling scripts. They are checked here but not required
# for the data-preparation script to run.
optional_packages <- c(
  "forecast",
  "bsts",
  "Boom",
  "BoomSpikeSlab",
  "TTR",
  "scales",
  "patchwork",
  "sessioninfo"
)

install_missing <- identical(
  tolower(Sys.getenv("BSTS_INSTALL_MISSING", unset = "false")),
  "true"
)

check_and_install_packages <- function(packages, install = FALSE) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing) > 0 && install) {
    install.packages(missing, repos = "https://cloud.r-project.org")
    missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  }

  missing
}

missing_required <- check_and_install_packages(required_packages, install = install_missing)

if (length(missing_required) > 0) {
  stop(
    "Missing required R packages: ", paste(missing_required, collapse = ", "),
    "\nInstall them with install.packages(c(",
    paste(sprintf('"%s"', missing_required), collapse = ", "),
    ")) or run with environment variable BSTS_INSTALL_MISSING=true.",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(purrr)
  library(lubridate)
  library(ggplot2)
  library(janitor)
  library(yaml)
  library(fs)
  library(glue)
  library(cli)
  library(rlang)
})

missing_optional <- optional_packages[
  !vapply(optional_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_optional) > 0) {
  message(
    "Optional modelling packages not currently installed: ",
    paste(missing_optional, collapse = ", "),
    ". This is fine for data preparation, but install them before running models."
  )
}

# ---- Default configuration -----------------------------------------------------
make_default_cfg <- function(project_root = PROJECT_ROOT) {
  list(
    project = list(
      name = "unemployment-nowcasting-jordan",
      seed = 20260614,
      timezone = "Asia/Amman",
      quarter_col = "quarter",
      outcome_col = "unemployment_rate",
      start_quarter = "2010-1",
      end_quarter = "2024-3"
    ),
    data = list(
      raw_dir = "data/raw",
      interim_dir = "data/interim",
      processed_dir = "data/processed",
      unemployment_file = "data/raw/Jordanian Unemployment Rate 2010- Q3 2024.csv",
      gt_ma_file = "data/raw/Quarterly Mean Aggregation Google Trends Keywords.csv",
      gt_ema_file = "data/raw/Quarterly Exponentially Weighted Moving Average Aggregation Google Trends Keywords.csv",
      gt_sawa_file = "data/raw/Quarterly Seasonally Adjusted Weighted Average Aggregated Google Trends Keywords.csv",
      keyword_translation_file = "data/raw/Arabic Keywords Translation.csv"
    ),
    outputs = list(
      root_dir = "outputs",
      data_checks_dir = "outputs/data_checks",
      figures_dir = "outputs/figures",
      tables_dir = "outputs/tables",
      logs_dir = "outputs/logs"
    ),
    preprocessing = list(
      expected_keyword_count = 88,
      strict_keyword_count = FALSE,
      strict_quarter_coverage = TRUE,
      drop_all_missing_predictors = TRUE,
      warn_if_predictors_outside_0_100 = TRUE,
      zero_thresholds = c(0.50, 0.70, 0.90),
      primary_zero_threshold = 0.70,
      standardize_predictors = TRUE
    ),
    validation = list(
      initial_train_quarters = 32,
      horizons = c(0, 1),
      final_check_quarter = "2024-3"
    ),
    bsts = list(
      niter = 10000,
      burn = 2000,
      expected_model_size = 5,
      state_spec = "local_linear_trend_plus_seasonal"
    )
  )
}

cfg_default <- make_default_cfg(PROJECT_ROOT)

# Optional YAML override. This is useful later, but the scripts work without it.
config_file <- path_abs("config", "config.yml")
if (file.exists(config_file)) {
  cfg_user <- yaml::read_yaml(config_file)
  cfg <- utils::modifyList(cfg_default, cfg_user)
} else {
  cfg <- cfg_default
}

set.seed(cfg$project$seed)

# ---- Directory helpers ---------------------------------------------------------
ensure_project_dirs <- function() {
  dirs <- c(
    cfg$data$raw_dir,
    cfg$data$interim_dir,
    cfg$data$processed_dir,
    cfg$outputs$root_dir,
    cfg$outputs$data_checks_dir,
    cfg$outputs$figures_dir,
    cfg$outputs$tables_dir,
    cfg$outputs$logs_dir
  )

  purrr::walk(path_abs(dirs), fs::dir_create)
  invisible(dirs)
}

# ---- Lightweight logging -------------------------------------------------------
# Glue expressions must be evaluated in the caller's environment. This is why the
# caller environment is captured explicitly before calling glue(). For example,
# log_info("Reading: {file}") should find `file` inside read_csv_utf8(), not the
# base R function file().
log_info <- function(...) {
  caller_env <- parent.frame()
  msg <- glue::glue(..., .envir = caller_env)
  cli::cli_inform(c("i" = as.character(msg)))
}

log_done <- function(...) {
  caller_env <- parent.frame()
  msg <- glue::glue(..., .envir = caller_env)
  cli::cli_inform(c("v" = as.character(msg)))
}

log_warn <- function(...) {
  caller_env <- parent.frame()
  msg <- glue::glue(..., .envir = caller_env)
  cli::cli_warn(as.character(msg))
}


# ---- Reproducibility helpers --------------------------------------------------
write_session_info <- function(file = path_abs(cfg$outputs$logs_dir, "session_info.txt")) {
  fs::dir_create(dirname(file))

  info <- utils::capture.output({
    cat("Project root:", PROJECT_ROOT, "\n")
    cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n\n")

    if (requireNamespace("sessioninfo", quietly = TRUE)) {
      print(sessioninfo::session_info())
    } else {
      print(utils::sessionInfo())
    }
  })

  writeLines(info, con = file, useBytes = TRUE)
  invisible(file)
}
