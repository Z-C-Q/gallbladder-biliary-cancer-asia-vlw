# Source data required for a full rebuild

The following source files are intentionally not tracked in Git:

| Expected filename | Source | Required contents |
|---|---|---|
| `IHME-GBD_2023_DATA_merged.csv` | IHME GBD 2023 / GBD Results Tool | Gallbladder and biliary tract cancer; DALYs; Number and Rate; 1990–2023; 48 Asian countries; both-sex/all-age and age-standardized estimates, plus age- and sex-specific estimates |
| `HALE_204_countries_1990_2023.csv` | IHME GBD 2023 | HALE in years by location, year, age, and sex, including uncertainty bounds |
| `SDI2023.csv` | IHME GBD 2023 | SDI by location and year |
| `country.csv` | Study country crosswalk | World Bank country name/code matched to GBD location ID and name |

Obtain the IHME files through the Global Health Data Exchange or GBD Results Tool and accept the applicable IHME agreement. Do not commit credentials, downloaded account metadata, or unfiltered source downloads to this repository.

After placing the files here, run:

```bash
Rscript R/01_calculate_age_sex_vlw.R
Rscript R/00_prepare_public_inputs.R
Rscript R/run_analysis.R
```

The source filenames can be kept outside the repository when preparing the minimum public subset:

```bash
SOURCE_DATA_DIR=/path/to/source/files Rscript R/00_prepare_public_inputs.R
```
