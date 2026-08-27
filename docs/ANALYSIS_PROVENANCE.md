# Analysis provenance

## Data flow

1. GBD 2023 DALY estimates, GBD 2023 HALE and SDI, and World Bank economic indicators were harmonized by country and year.
2. A US 2023 reference VSL of 13.2 million dollars was transferred to each country-year using PPP-adjusted GDP per capita and the specified income elasticity.
3. VSL was annualized using HALE to obtain VSLY.
4. DALYs were multiplied by VSLY to estimate VLW, with conditional lower and upper estimates propagated from the source bounds used in the analysis.
5. Country-, subregional-, age-, sex-, trend-, ranking-, correlation-, and robustness analyses were performed in R.
6. Figure-level CSV files were written before plotting so that every graphic has a directly inspectable tabular source.

## Public-release transformation

The release directory was constructed from the final analysis workspace. Only files needed to understand, reproduce, or verify the reported results were retained. The following materials were excluded:

- contracts and administrative documents;
- manuscript and response-letter drafts;
- generated figures, tables, figure-level datasets, supplementary results, and duplicate result folders;
- operating-system metadata and temporary files;
- source downloads that are governed by external access terms; and
- workstation-specific paths.

File names were normalized to the numbering used in the current manuscript. Numerical values in regenerated figure-level datasets were verified against the final pre-release analysis outputs.

The public country crosswalk includes an explicit record for Türkiye (`location_id` 155; ISO3 code `TUR`) because the source-name crosswalk did not use the same location label. This addition completes the 48-country mapping and does not change calculated estimates, rankings, tests, or figures.
