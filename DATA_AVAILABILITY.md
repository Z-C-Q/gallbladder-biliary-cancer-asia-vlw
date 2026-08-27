# Data availability

## Materials included in this repository

This repository includes:

- the processed analytical inputs needed to reproduce the main and supplementary analyses;
- all R scripts used for data processing, analysis, visualization, and table generation;
- data dictionaries, source-access instructions, licensing information, and reproducibility documentation.

Generated figures, tables, figure-level datasets, and supplementary result files are intentionally not versioned. They can be recreated locally by running `R/run_analysis.R`.

The processed inputs contain aggregate country-, year-, age-, and sex-level estimates only. They contain no participant-level or personally identifiable information.

## Source data not redistributed

The complete downloaded GBD 2023 DALY and HALE source files are not redistributed in this repository. They are available through the Institute for Health Metrics and Evaluation (IHME) and the Global Health Data Exchange under IHME's applicable access and use terms. The files used locally were:

- `IHME-GBD_2023_DATA_merged.csv`
- `HALE_204_countries_1990_2023.csv`
- `SDI2023.csv`

Users who obtain these files under the applicable IHME agreement can reproduce the public processed inputs with `R/01_calculate_age_sex_vlw.R` and `R/00_prepare_public_inputs.R`.

## World Bank data

The World Bank World Development Indicators extract used in the analysis is included as `data/external/world_bank_wdi_1990_2024.csv`. The study uses:

- GDP per capita, PPP (current international dollars); and
- GDP, PPP (constant 2021 international dollars).

World Bank attribution and licensing details are recorded in `LICENSES/DATA_LICENSES.md`.

## Standardized data types and accession numbers

The study is based on secondary aggregate epidemiological and economic indicators. It did not generate nucleic-acid sequencing, proteomics, crystallography, or another community-standardized experimental data type. A data-type-specific accession number is therefore not applicable.

The versioned repository release used for the revised manuscript is archived at Zenodo: https://doi.org/10.5281/zenodo.22133469.
