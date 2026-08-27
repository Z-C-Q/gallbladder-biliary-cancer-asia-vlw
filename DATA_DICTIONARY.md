# Data dictionary

## Processed analytical inputs

### `data/processed/asia_age_sex_vlw_1990_2023.csv.gz`

Age-, sex-, country-, and year-specific DALY, HALE, valuation, and VLW estimates. The compressed CSV has 97,920 data rows and can be read directly by `readr::read_csv()`.

| Variable | Definition |
|---|---|
| `location_id`, `location_name` | GBD location identifier and location name |
| `country_code`, `country_name_wb` | World Bank three-letter country code and country name |
| `asia_subregion` | Central, East, South, Southeast, or Western Asia |
| `sex_id`, `sex_name` | GBD sex identifier and label |
| `age_id`, `age_name` | GBD age-group identifier and label |
| `cause_id`, `cause_name` | GBD cause identifier and label; cause 453 in the source data |
| `year` | Calendar year, 1990–2023 |
| `dalys` | DALY point estimate |
| `dalys_lower`, `dalys_upper` | Lower and upper bounds of the DALY 95% uncertainty interval |
| `hale` | HALE point estimate in years for the corresponding age and sex group |
| `hale_lower`, `hale_upper` | Lower and upper HALE bounds |
| `gdp_per_capita_ppp_current` | GDP per capita, PPP, current international dollars |
| `gdp_total_ppp_constant_2021` | Total GDP, PPP, constant 2021 international dollars |
| `vsl` | Country-year value of a statistical life after income transfer from the 2023 US reference value |
| `vsly` | Value of a statistical life-year, calculated as VSL divided by HALE |
| `vsly_lower`, `vsly_upper` | VSLY bounds induced by the HALE bounds |
| `economic_burden_vlw` | Value of lost welfare, calculated as DALYs multiplied by VSLY |
| `economic_burden_vlw_lower`, `economic_burden_vlw_upper` | Conditional VLW bounds propagated from source uncertainty |
| `economic_burden_vlw_trillion` | VLW in trillion international dollars |
| `economic_burden_vlw_lower_trillion`, `economic_burden_vlw_upper_trillion` | Conditional VLW bounds in trillion international dollars |
| `economic_burden_pct_gdp` | VLW divided by total GDP, multiplied by 100 |
| `economic_burden_pct_gdp_lower`, `economic_burden_pct_gdp_upper` | Conditional lower and upper bounds for VLW/GDP |

### Other processed inputs

| File | Rows excluding header | Purpose |
|---|---:|---|
| `gbd_analysis_subset.csv` | 7,968 | Minimum GBD-derived DALY rows needed for the main analysis scripts |
| `sdi_asia_1990_2023.csv` | 1,700 | SDI by Asian location and year |
| `country_mapping_asia.csv` | 48 | World Bank/GBD country crosswalk |
| `asia_age_vlw_summary_1990_2023.csv` | 680 | Asia-wide age-specific annual summary |
| `asia_country_age_vlw_both_sexes_1990_2023.csv` | 32,640 | Country-age-year VLW estimates for both sexes |
| `analysis_missingness.csv` | 48 | Country-level completeness counts for the processed VLW data |

Uncertainty intervals for VLW are conditional on the GBD/HALE inputs and specified valuation assumptions; they are not comprehensive probabilistic uncertainty intervals for GDP, VSL, income elasticity, or other fixed valuation parameters.
