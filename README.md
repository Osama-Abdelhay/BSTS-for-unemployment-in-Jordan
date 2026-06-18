# A Bayesian Nowcasting Framework for Unemployment Monitoring in Jordan Using Multilingual Google Trends

This repository contains the R code and manuscript outputs for the study:

> **A Bayesian Nowcasting Framework for Unemployment Monitoring in Jordan Using Multilingual Google Trends**

The project evaluates whether multilingual Google Trends indicators can provide a timely, interpretable, and uncertainty-aware supplementary signal for monitoring Jordan's quarterly unemployment rate. The analysis is designed for a policy-facing manuscript submitted to *Data & Policy*.

## Repository purpose

The code implements a reproducible nowcasting pipeline that:

1. prepares the published Jordan unemployment and Google Trends dataset;
2. aligns quarterly unemployment outcomes with Google Trends predictors;
3. applies rolling-origin validation for same-quarter nowcasting;
4. compares simple benchmarks, conventional time-series models, BSTS without Google Trends, and BSTS with Google Trends;
5. evaluates Google Trends aggregation and sparsity-threshold sensitivity;
6. reports point accuracy and probabilistic forecast quality;
7. summarises posterior inclusion probabilities for interpretable keyword signals;
8. exports manuscript-ready tables and figures.

The central interpretation is deliberately cautious: Google Trends is evaluated as **supplementary labour-market intelligence**, not as a replacement for official unemployment statistics.

## Data source

The analysis uses the peer-reviewed public dataset:

> Abdelhay O, Altamimi T. *Non-traditional data for macroeconomic estimation: unemployment in Jordan as an application*. Scientific Data. 2025;12:449. https://doi.org/10.1038/s41597-025-04721-6

Dataset DOI:

> https://doi.org/10.17632/sdvs4nbgym.5

The source dataset includes:

- official quarterly unemployment rates for Jordan;
- multilingual Google Trends search-interest indicators related to unemployment, job search, wages, job portals, social protection, labour rights, and economic hardship;
- quarterly Google Trends aggregation variants, including mean aggregation, exponentially weighted moving average, and seasonally adjusted weighted average;
- Arabic keyword translation files.

Google Trends values are relative, normalised search-interest indices, not counts of searches, searchers, unemployed persons, or job seekers. See Google Trends documentation: https://support.google.com/trends/answer/4365533

## Repository structure

```text
.
├── README.md
├── config/
│   └── config.yml                    # paths, seeds, thresholds, validation settings
├── data/raw/                         # not committed; downloaded source files
│   ├── Arabic_Keywords_Translation.csv
│   ├── Jordanian Unemployment Rate 2010- Q1 2026.csv
│   └── KWMonthly.csv
├── data/processed/                   # harmonised modelling datasets
├── data/scripts/                  
│   ├── 00_audit_raw_data.R
│   ├── 00_packages.R
│   ├── 01_prepare_clean_data.R
│   ├── 02_build_quarterly_google_trends.R
│   ├── 03_build_modeling_panels.R
│   ├── 04_eda.R
│   ├── 05_define_rolling_validation.R
│   ├── 06_run_benchmarks.R
│   ├── 07_run_bsts_baseline.R
│   ├── 07b_compare_bsts_state_specs.R
│   ├── 08_run_bsts_gt_MA_EMA.R
│   ├── 09_evaluate_models.R
│   ├── 10_mcmc_stability_check.R
│   └── 11_build_manuscript_assets.R
└── manuscript_assets/
    ├── tables/
    └── figures/
```

##  Input files
The input files are stored in data/raw

- **published source keyword dictionary:** 88 original Google Trends keywords;
- **final modelling dictionary:** retained candidate predictors after modelling preparation and sparsity filtering.

The retained number can differ from 88 because sparse or structurally uninformative predictors may be excluded during model preparation or within rolling-origin folds.

## Software requirements

The analysis is written in R. Recommended version:

```text
R >= 4.4.0
```

Core packages:

```r
install.packages(c(
  "tidyverse",
  "readr",
  "dplyr",
  "tidyr",
  "stringr",
  "lubridate",
  "zoo",
  "ggplot2",
  "scales",
  "forecast",
  "bsts",
  "CausalImpact",
  "purrr",
  "furrr",
  "future",
  "here",
  "janitor",
  "knitr"
))
```

For full reproducibility, use `renv`:

```r
install.packages("renv")
renv::restore()
```

## Reproducing the manuscript outputs

From the repository root, run:

```bash
Rscript scripts/00_run_all.R
```

Alternatively, run the pipeline step by step:

```bash
Rscript data/scripts/00_aduit_raw_data.R
Rscript data/scripts/00_packages.R
Rscript data/scripts/01_prepare_clean_data.R
Rscript data/scripts/02_build_quarterly_google_trends.R
Rscript data/scripts/03_build_modeling_panels.R
Rscript data/scripts/04_eda.R
Rscript data/scripts/05_define_rolling_validation.R
Rscript data/scripts/06_run_benchmarks.R
Rscript data/scripts/07_run_bsts_baseline.R
Rscript data/scripts/07b_compare_bsts_state_specs.R
Rscript data/scripts/08_run_bsts_gt_MA_EMA.R
Rscript data/scripts/09_evaluate_models.R
Rscript data/scripts/10_mcmc_stability_check.R
Rscript data/scripts/11_build_manuscript_assets.R
```

The final step writes manuscript-ready tables and figures to `manuscript_assets`, `manuscript_assets/tables/`, `manuscript_assets/figures/`, and `manuscript_assets/text`.

## Validation design

The main validation design is expanding-window rolling-origin evaluation.

For each validation origin:

1. fit models using only information available up to that origin;
2. apply zero-proportion filtering and standardisation inside the training window;
3. generate the same-quarter nowcast or forecast;
4. store the point estimate, 95% predictive interval, and error;
5. move the origin forward by one quarter.

The primary horizon is:

| Horizon | Interpretation |
|---|---|
| `h = 0` | Same-quarter nowcast using target-quarter Google Trends indicators while withholding the official unemployment rate for that quarter. |

Additional analyses may include:

| Horizon/block | Interpretation |
|---|---|
| `h = 1` | One-quarter-ahead forecast using only information available at the forecast origin. |
| Post-Google-Trends block | Unemployment-only validation after the Google Trends overlap endpoint. This is not evidence about Google Trends predictive contribution. |

## Model families

The pipeline compares the following model families:

| Label | Description | Role |
|---|---|---|
| `RW` | Random walk | Minimal persistence benchmark |
| `RW_DRIFT` | Random walk with drift | Persistence benchmark with drift |
| `SNAIVE` | Seasonal naïve | Seasonal benchmark |
| `ARIMA` | ARIMA benchmark | Conventional time-series model |
| `ETS` | Exponential smoothing benchmark | Conventional time-series model |
| `BSTS_NO_GT_*` | BSTS using the unemployment series only | Bayesian structural baseline |
| `BSTS_GT_MA_*` | BSTS with mean-aggregated Google Trends | Search-augmented model |
| `BSTS_GT_EMA_*` | BSTS with EMA-aggregated Google Trends | Search-augmented model |
| `BSTS_GT_SAWA_*` | BSTS with SAWA-aggregated Google Trends | Optional search-augmented model if regenerated |

The final manuscript results primarily emphasise the `h = 0` nowcast comparison.

## Performance metrics

Point accuracy metrics:

- MAE: mean absolute error, in unemployment-rate percentage points;
- RMSE: root mean squared error;
- MAPE: mean absolute percentage error;
- sMAPE: symmetric mean absolute percentage error.

Probabilistic metrics:

- 95% interval coverage;
- average 95% interval width;
- interval score.

Probabilistic metrics are reported because BSTS models produce predictive distributions, not only point estimates.

## Manuscript output files

The main manuscript tables are expected to include:

| File | Description |
|---|---|
| `Table_1_data_components.csv` | Data components and modelling variables |
| `Table_2_validation_design.csv` | Validation design summary |
| `Table_3_main_h0_model_comparison.csv` | Main same-quarter nowcasting comparison |
| `Table_4_google_trends_threshold_sensitivity.csv` | Google Trends aggregation and zero-threshold sensitivity |
| `Table_5_period_specific_errors.csv` | Pre-COVID, COVID-adjustment, and post-COVID errors |
| `Table_6_post_gt_validation.csv` | Post-Google-Trends unemployment-only validation |
| `Table_7_top_predictor_inclusion.csv` | Top posterior inclusion probability summaries |

The main manuscript figures are expected to include:

| File | Description |
|---|---|
| `final_h0_model_comparison.png` | Main `h = 0` model comparison |
| `final_h0_actual_vs_predicted_key_models.png` | Actual versus predicted unemployment trajectories |
| `final_h0_rolling_errors_key_models.png` | Rolling-origin errors for key models |
| `final_gt_threshold_sensitivity.png` | Google Trends threshold and aggregation sensitivity |
| `final_interval_quality_h0.png` | Coverage and interval width for `h = 0` models |
| `final_period_specific_mae.png` | MAE by validation period |
| `final_post_gt_unemployment_only_mae.png` | Post-Google-Trends unemployment-only validation |
| `final_predictor_inclusion_top_keywords.png` | Top keyword posterior inclusion probabilities |
| `mcmc_stability_mae_by_seed.png` | MCMC seed stability for MAE |
| `mcmc_stability_predictor_inclusion_top_keywords.png` | MCMC seed stability for keyword inclusion |

## Reproducibility safeguards

The code should preserve the following safeguards:

- no random train/test splitting for time-series validation;
- all preprocessing performed inside each rolling-origin training window;
- no future official unemployment values used in model fitting;
- no target-quarter unemployment value used for `h = 0` fitting;
- no target-quarter Google Trends values used for `h = 1` forecasts unless explicitly forecasted or lagged;
- fixed random seeds for final model runs;
- repeated MCMC seed checks for the preferred Google Trends models;
- clear separation of Google Trends overlap validation and post-Google-Trends unemployment-only validation;
- UTF-8 encoding preserved for Arabic keyword files.

## Key manuscript-specific notes

1. The published dataset contains 88 source keywords. The final modelling analysis may use a smaller retained candidate set after sparsity filtering and modelling preparation.
2. The final Google Trends sensitivity results compare MA and EMA specifications. Do not claim final SAWA superiority unless SAWA is rerun under the same validation design and included in the outputs.
3. The post-Google-Trends validation block evaluates unemployment-only models. It should not be interpreted as evidence for or against Google Trends.
4. Posterior inclusion probabilities are predictive-association summaries. They should not be interpreted causally.
5. Google Trends nowcasts should be interpreted as supplementary policy intelligence, not as official unemployment estimates.

## Citation

Please cite the data source:

```text
Abdelhay O, Altamimi T. Non-traditional data for macroeconomic estimation:
unemployment in Jordan as an application. Scientific Data. 2025;12:449.
https://doi.org/10.1038/s41597-025-04721-6
```

Please also cite the dataset:

```text
Abdelhay O, Altamimi T. Jordan Unemployment Rates and Google Trends Data
(2010-2024): Analyzing Economic Sentiment Through Search Behavior in Arabic
and English. Mendeley Data. 2025. https://doi.org/10.17632/sdvs4nbgym.5
```

When the manuscript is published or available as a preprint, add the manuscript citation here.

## Ethics and privacy

The analysis uses aggregate official unemployment statistics and aggregate Google Trends search-interest indices. No individual-level, personally identifiable, or private search data are analysed.

## License

- code: MIT License or Apache-2.0;
- documentation: CC BY 4.0;
- data: follow the license and terms of the original Mendeley Data release and Google Trends terms of use.

## Contact

For questions about the code or manuscript, contact:

```text
Osama Abdelhay
Department of Data Science and Artificial Intelligence
Princess Sumaya University for Technology
Amman, Jordan
Email: o.abdelhay@psut.edu.jo
ORCID: https://orcid.org/0000-0003-2339-1406
```
