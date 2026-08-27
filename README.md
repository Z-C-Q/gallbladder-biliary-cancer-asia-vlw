# Welfare-based economic burden of gallbladder and biliary tract cancer in Asia

This repository contains the analysis input data, R code, and documentation for the manuscript:

> Trends and Geographic Disparities in the Welfare-Based Economic Burden of Gallbladder and Biliary Tract Cancer in Asia, 1990–2023

The study is a retrospective analysis of 48 Asian countries using Global Burden of Disease Study 2023 estimates, World Bank economic indicators, healthy life expectancy (HALE), and the Sociodemographic Index (SDI). Disability-adjusted life-years (DALYs) were monetized using a value-of-statistical-life/value-of-statistical-life-year framework to estimate the value of lost welfare (VLW).

## Repository contents

| Path | Contents |
|---|---|
| `R/` | Data preparation, main-figure, table, and supplementary-analysis scripts |
| `data/processed/` | Minimum processed inputs needed to reproduce the reported analyses |
| `data/external/` | World Bank World Development Indicators input used in the study |
| `data/raw/` | Instructions for obtaining source-level GBD and HALE files; restricted source files are not redistributed here |
| `docs/` | Data-sharing language, provenance notes, and upload checklist |

## Reproduce the analysis

Run the following commands from the repository root:

```bash
Rscript R/check_dependencies.R
Rscript R/run_analysis.R
```

`R/run_analysis.R` reproduces all seven main figures, the manuscript tables, supplementary analyses, and the study flowchart from the processed inputs included in this repository. Generated files are written under `results/`, which is intentionally excluded from version control. On the system used for release verification, the complete workflow finished in approximately 15 seconds.

The principal tested environment was R 4.5.0 with the package versions recorded in `DESCRIPTION`. Later compatible package versions may also work.

## Rebuild the processed inputs from source files

The full source-level GBD and HALE files are not included because they are governed by IHME access terms and one source file exceeds GitHub's standard single-file limit. To rebuild the processed inputs:

1. Obtain the files described in `data/raw/README.md` under the applicable IHME agreement.
2. Place them in `data/raw/` using the expected filenames.
3. Run:

```bash
Rscript R/01_calculate_age_sex_vlw.R
Rscript R/00_prepare_public_inputs.R
Rscript R/run_analysis.R
```

The World Bank input is included at `data/external/world_bank_wdi_1990_2024.csv` with source attribution and license information in `LICENSES/DATA_LICENSES.md`.

## Manuscript analysis mapping

| Manuscript item | Script |
|---|---|
| Figure 1: temporal trends | `R/02_figure1_trends.R` |
| Figure 2: country maps | `R/03_figure2_maps.R` |
| Figure 3: rank changes | `R/04_figure3_rank_changes.R` |
| Figure 4: age and sex | `R/05_figure4_age_sex.R` |
| Figure 5: SDI associations | `R/06_figure5_sdi_associations.R` |
| Figure 6: burden growth | `R/07_figure6_growth.R` |
| Figure 7: priority matrix | `R/08_figure7_priority_matrix.R` |

The scripts generate their figure-level data and graphical outputs locally under `results/`; generated results are not included in the public repository.

## Data availability and licensing

This study did not generate a community-standardized experimental data type such as sequencing, proteomics, or crystallography data. No repository accession number is therefore applicable to the data type itself.

The code is released under the MIT License. Data files are not covered by the code license. IHME-derived material remains subject to the IHME Free-of-Charge Non-Commercial User Agreement and applicable GBD citation requirements. World Bank data are provided under the applicable World Bank dataset terms, generally CC BY 4.0 for World Bank-produced open data. See `DATA_AVAILABILITY.md` and `LICENSES/DATA_LICENSES.md` before reuse.

## Contact

Requests for further information and resources should be directed to the lead contact listed in the manuscript: Gang Wang (`wang1123@ustc.edu.cn`).

## Citation

If you use these materials, cite the associated article and the underlying GBD 2023 and World Bank data sources. The archived repository release is available at https://doi.org/10.5281/zenodo.22133469. Repository citation metadata are provided in `CITATION.cff`; add the article DOI after publication.
