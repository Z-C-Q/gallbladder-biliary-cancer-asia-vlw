# Prepare the minimum GBD-derived inputs used by the analysis scripts.
#
# Set SOURCE_DATA_DIR to the directory containing the files listed below,
# or place the files in data/raw before running this script:
#   IHME-GBD_2023_DATA_merged.csv
#   SDI2023.csv
#   country.csv

library(dplyr)
library(readr)

source_dir <- Sys.getenv("SOURCE_DATA_DIR", unset = "data/raw")
output_dir <- "data/processed"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

asia_countries <- c(
  "Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh",
  "Bhutan", "Brunei Darussalam", "Cambodia", "China", "Cyprus",
  "Democratic People's Republic of Korea", "Georgia", "India", "Indonesia",
  "Iran (Islamic Republic of)", "Iraq", "Israel", "Japan", "Jordan",
  "Kazakhstan", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic",
  "Lebanon", "Malaysia", "Maldives", "Mongolia", "Myanmar", "Nepal",
  "Oman", "Pakistan", "Palestine", "Philippines", "Qatar",
  "Republic of Korea", "Saudi Arabia", "Singapore", "Sri Lanka",
  "Syrian Arab Republic", "Tajikistan", "Thailand", "Timor-Leste",
  "Türkiye", "Turkmenistan", "United Arab Emirates", "Uzbekistan",
  "Viet Nam", "Yemen"
)

age_levels <- c(
  "20-24 years", "25-29 years", "30-34 years", "35-39 years",
  "40-44 years", "45-49 years", "50-54 years", "55-59 years",
  "60-64 years", "65-69 years", "70-74 years", "75-79 years",
  "80-84 years", "85-89 years", "90-94 years", "95+ years"
)

required_files <- c(
  gbd = "IHME-GBD_2023_DATA_merged.csv",
  sdi = "SDI2023.csv",
  country = "country.csv"
)

input_paths <- file.path(source_dir, required_files)
names(input_paths) <- names(required_files)
missing_files <- input_paths[!file.exists(input_paths)]
if (length(missing_files) > 0) {
  stop(
    "Missing source file(s): ",
    paste(missing_files, collapse = ", "),
    ". See data/raw/README.md for access instructions."
  )
}

gbd <- read_csv(input_paths[["gbd"]], show_col_types = FALSE)

gbd_analysis <- gbd %>%
  filter(
    location_name %in% asia_countries,
    cause_name == "Gallbladder and biliary tract cancer",
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    year >= 1990,
    year <= 2023,
    (
      sex_name == "Both" &
        age_name == "All ages" &
        metric_name %in% c("Number", "Rate")
    ) |
      (
        sex_name == "Both" &
          age_name == "Age-standardized" &
          metric_name == "Rate"
      ) |
      (
        year == 2023 &
          sex_name %in% c("Male", "Female") &
          age_name %in% age_levels &
          metric_name %in% c("Number", "Rate")
      )
  )

write_csv(gbd_analysis, file.path(output_dir, "gbd_analysis_subset.csv"))

sdi <- read_csv(input_paths[["sdi"]], show_col_types = FALSE) %>%
  filter(
    location_name %in% asia_countries,
    year >= 1990,
    year <= 2023,
    !is.na(location_id)
  ) %>%
  transmute(
    location_id = as.integer(location_id),
    location_name,
    year = as.integer(year),
    sdi = as.numeric(sdi)
  )

write_csv(sdi, file.path(output_dir, "sdi_asia_1990_2023.csv"))

country_mapping <- read_csv(input_paths[["country"]], show_col_types = FALSE) %>%
  filter(location_name %in% asia_countries) %>%
  select(Country.Name, Country.Code, location_id, location_name) %>%
  bind_rows(
    tibble::tribble(
      ~Country.Name, ~Country.Code, ~location_id, ~location_name,
      "Türkiye", "TUR", 155L, "Türkiye",
      "Yemen", "YEM", 157L, "Yemen"
    )
  ) %>%
  distinct(location_name, .keep_all = TRUE) %>%
  arrange(location_name)

write_csv(country_mapping, file.path(output_dir, "country_mapping_asia.csv"))

message("Prepared ", nrow(gbd_analysis), " GBD analysis rows, ",
        nrow(sdi), " SDI rows, and ", nrow(country_mapping),
        " country-mapping rows in ", output_dir, ".")
