# ============================================
# Asia age-specific VLW economic burden
# Disease: Gallbladder and biliary tract cancer
# GBD: 1990-2023
# VSL benchmark: USA 2023, USD 13,200,000
# Implements the study's age- and sex-specific valuation workflow
# ============================================

library(dplyr)
library(tidyr)
library(readr)

base_year <- 2023
vsl_usa_2023 <- 13200000
income_elasticity <- 1.0
years <- 1990:2023

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

sum_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

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

asia_subregion <- tibble::tribble(
  ~location_name, ~asia_subregion,
  "China", "East Asia",
  "Democratic People's Republic of Korea", "East Asia",
  "Japan", "East Asia",
  "Mongolia", "East Asia",
  "Republic of Korea", "East Asia",
  "Brunei Darussalam", "Southeast Asia",
  "Cambodia", "Southeast Asia",
  "Indonesia", "Southeast Asia",
  "Lao People's Democratic Republic", "Southeast Asia",
  "Malaysia", "Southeast Asia",
  "Myanmar", "Southeast Asia",
  "Philippines", "Southeast Asia",
  "Singapore", "Southeast Asia",
  "Thailand", "Southeast Asia",
  "Timor-Leste", "Southeast Asia",
  "Viet Nam", "Southeast Asia",
  "Afghanistan", "South Asia",
  "Bangladesh", "South Asia",
  "Bhutan", "South Asia",
  "India", "South Asia",
  "Maldives", "South Asia",
  "Nepal", "South Asia",
  "Pakistan", "South Asia",
  "Sri Lanka", "South Asia",
  "Armenia", "Central Asia",
  "Azerbaijan", "Central Asia",
  "Georgia", "Central Asia",
  "Kazakhstan", "Central Asia",
  "Kyrgyzstan", "Central Asia",
  "Tajikistan", "Central Asia",
  "Turkmenistan", "Central Asia",
  "Uzbekistan", "Central Asia",
  "Bahrain", "Western Asia",
  "Cyprus", "Western Asia",
  "Iran (Islamic Republic of)", "Western Asia",
  "Iraq", "Western Asia",
  "Israel", "Western Asia",
  "Jordan", "Western Asia",
  "Kuwait", "Western Asia",
  "Lebanon", "Western Asia",
  "Oman", "Western Asia",
  "Palestine", "Western Asia",
  "Qatar", "Western Asia",
  "Saudi Arabia", "Western Asia",
  "Syrian Arab Republic", "Western Asia",
  "Türkiye", "Western Asia",
  "United Arab Emirates", "Western Asia",
  "Yemen", "Western Asia"
)

cat("========================================\n")
cat("Asia age-specific VLW economic burden\n")
cat("Disease: Gallbladder and biliary tract cancer\n")
cat("Taiwan excluded from country analysis\n")
cat("========================================\n\n")

gbd <- read_csv("data/raw/IHME-GBD_2023_DATA_merged.csv", show_col_types = FALSE)
hale <- read_csv("data/raw/HALE_204_countries_1990_2023.csv", show_col_types = FALSE)
country <- read_csv("data/raw/country.csv", show_col_types = FALSE)
gdp_raw <- read_csv("data/external/world_bank_wdi_1990_2024.csv", show_col_types = FALSE, na = c("..", "NA", ""))

country_map <- country %>%
  filter(!is.na(location_id), location_id != "") %>%
  transmute(
    location_id = as.integer(location_id),
    country_code = Country.Code,
    country_name_wb = Country.Name
  )

country_by_location <- country_map %>%
  arrange(location_id) %>%
  distinct(location_id, .keep_all = TRUE)

country_by_code <- country_map %>%
  filter(!is.na(country_code), country_code != "") %>%
  arrange(country_code) %>%
  distinct(country_code, .keep_all = TRUE)

gdp_long <- gdp_raw %>%
  pivot_longer(matches("^\\d{4} \\[YR\\d{4}\\]$"), names_to = "year_name", values_to = "gdp_value") %>%
  mutate(
    year = parse_number(year_name),
    gdp_value = parse_number(as.character(gdp_value))
  ) %>%
  filter(
    year %in% years,
    `Series Name` %in% c(
      "GDP per capita, PPP (current international $)",
      "GDP, PPP (constant 2021 international $)"
    )
  ) %>%
  transmute(
    country_code = `Country Code`,
    year,
    series = case_when(
      `Series Name` == "GDP per capita, PPP (current international $)" ~ "gdp_per_capita_ppp_current",
      `Series Name` == "GDP, PPP (constant 2021 international $)" ~ "gdp_total_ppp_constant_2021",
      TRUE ~ NA_character_
    ),
    gdp_value
  ) %>%
  filter(!is.na(series), !is.na(gdp_value)) %>%
  pivot_wider(names_from = series, values_from = gdp_value)

gdp_by_location <- gdp_long %>%
  left_join(country_by_code, by = "country_code") %>%
  filter(!is.na(location_id))

gdp_pc_usa_2023 <- gdp_by_location %>%
  filter(location_id == 102, year == base_year) %>%
  pull(gdp_per_capita_ppp_current)

if (length(gdp_pc_usa_2023) != 1 || is.na(gdp_pc_usa_2023)) {
  stop("Unable to identify USA 2023 GDP per capita PPP from the World Bank input file.")
}

cat(sprintf("USA GDPpc PPP current, 2023: %.2f\n", gdp_pc_usa_2023))
cat(sprintf("VSL USA 2023: %.2f\n\n", vsl_usa_2023))

gbd_age <- gbd %>%
  filter(
    location_name %in% asia_countries,
    cause_name == "Gallbladder and biliary tract cancer",
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    metric_name == "Number",
    year %in% years,
    age_name != "All ages",
    age_name != "Age-standardized"
  ) %>%
  mutate(age_id = as.integer(age_id)) %>%
  select(
    location_id, location_name, sex_id, sex_name,
    age_id, age_name, cause_id, cause_name, year,
    dalys = val, dalys_upper = upper, dalys_lower = lower
  )

hale_age <- hale %>%
  filter(
    location_name %in% asia_countries,
    metric_name == "Years",
    year %in% years,
    age_name != "All ages",
    age_name != "Age-standardized"
  ) %>%
  mutate(age_id = as.integer(age_id)) %>%
  transmute(
    location_id, location_name, sex_id, sex_name,
    age_id, age_name, year,
    hale = val, hale_upper = upper, hale_lower = lower
  )

vsl_by_location <- gdp_by_location %>%
  filter(year %in% years) %>%
  mutate(
    vsl = vsl_usa_2023 * (gdp_per_capita_ppp_current / gdp_pc_usa_2023) ^ income_elasticity
  ) %>%
  select(
    location_id, year,
    gdp_per_capita_ppp_current, gdp_total_ppp_constant_2021, vsl
  )

result <- gbd_age %>%
  left_join(asia_subregion, by = "location_name") %>%
  left_join(country_by_location, by = "location_id") %>%
  left_join(hale_age, by = c("location_id", "location_name", "sex_id", "sex_name", "age_id", "age_name", "year")) %>%
  left_join(vsl_by_location, by = c("location_id", "year")) %>%
  mutate(
    vsly = vsl / hale,
    vsly_lower = vsl / hale_upper,
    vsly_upper = vsl / hale_lower,
    economic_burden_vlw = case_when(
      dalys == 0 ~ 0,
      !is.na(vsly) ~ dalys * vsly,
      TRUE ~ NA_real_
    ),
    economic_burden_vlw_lower = case_when(
      dalys_lower == 0 ~ 0,
      !is.na(vsly_lower) ~ dalys_lower * vsly_lower,
      TRUE ~ NA_real_
    ),
    economic_burden_vlw_upper = case_when(
      dalys_upper == 0 ~ 0,
      !is.na(vsly_upper) ~ dalys_upper * vsly_upper,
      TRUE ~ NA_real_
    ),
    economic_burden_vlw_trillion = economic_burden_vlw / 1e12,
    economic_burden_vlw_lower_trillion = economic_burden_vlw_lower / 1e12,
    economic_burden_vlw_upper_trillion = economic_burden_vlw_upper / 1e12,
    economic_burden_pct_gdp = economic_burden_vlw / gdp_total_ppp_constant_2021 * 100,
    economic_burden_pct_gdp_lower = economic_burden_vlw_lower / gdp_total_ppp_constant_2021 * 100,
    economic_burden_pct_gdp_upper = economic_burden_vlw_upper / gdp_total_ppp_constant_2021 * 100
  ) %>%
  select(
    location_id, location_name, country_code, country_name_wb, asia_subregion,
    sex_id, sex_name, age_id, age_name, cause_id, cause_name, year,
    dalys, dalys_lower, dalys_upper,
    hale, hale_lower, hale_upper,
    gdp_per_capita_ppp_current, gdp_total_ppp_constant_2021,
    vsl, vsly, vsly_lower, vsly_upper,
    economic_burden_vlw, economic_burden_vlw_lower, economic_burden_vlw_upper,
    economic_burden_vlw_trillion, economic_burden_vlw_lower_trillion, economic_burden_vlw_upper_trillion,
    economic_burden_pct_gdp, economic_burden_pct_gdp_lower, economic_burden_pct_gdp_upper
  ) %>%
  arrange(location_name, year, sex_id, age_id)

output_detail <- "data/processed/asia_age_sex_vlw_1990_2023.csv.gz"
write_csv(result, output_detail)

summary_age <- result %>%
  filter(sex_name == "Both") %>%
  group_by(year, age_id, age_name) %>%
  summarise(
    countries_with_dalys = n_distinct(location_id),
    countries_with_vlw = n_distinct(location_id[!is.na(economic_burden_vlw)]),
    total_dalys = sum_or_na(dalys),
    total_dalys_lower = sum_or_na(dalys_lower),
    total_dalys_upper = sum_or_na(dalys_upper),
    total_economic_burden_vlw = sum_or_na(economic_burden_vlw),
    total_economic_burden_vlw_lower = sum_or_na(economic_burden_vlw_lower),
    total_economic_burden_vlw_upper = sum_or_na(economic_burden_vlw_upper),
    total_economic_burden_vlw_trillion = total_economic_burden_vlw / 1e12,
    total_economic_burden_vlw_lower_trillion = total_economic_burden_vlw_lower / 1e12,
    total_economic_burden_vlw_upper_trillion = total_economic_burden_vlw_upper / 1e12,
    .groups = "drop"
  ) %>%
  arrange(year, age_id)

output_summary_age <- "data/processed/asia_age_vlw_summary_1990_2023.csv"
write_csv(summary_age, output_summary_age)

summary_country_age <- result %>%
  filter(sex_name == "Both") %>%
  group_by(location_id, location_name, country_code, asia_subregion, year, age_id, age_name) %>%
  summarise(
    dalys = sum_or_na(dalys),
    dalys_lower = sum_or_na(dalys_lower),
    dalys_upper = sum_or_na(dalys_upper),
    economic_burden_vlw = sum_or_na(economic_burden_vlw),
    economic_burden_vlw_lower = sum_or_na(economic_burden_vlw_lower),
    economic_burden_vlw_upper = sum_or_na(economic_burden_vlw_upper),
    economic_burden_vlw_trillion = economic_burden_vlw / 1e12,
    economic_burden_vlw_lower_trillion = economic_burden_vlw_lower / 1e12,
    economic_burden_vlw_upper_trillion = economic_burden_vlw_upper / 1e12,
    .groups = "drop"
  ) %>%
  arrange(location_name, year, age_id)

output_country_age <- "data/processed/asia_country_age_vlw_both_sexes_1990_2023.csv"
write_csv(summary_country_age, output_country_age)

missing_report <- result %>%
  group_by(location_id, location_name, country_code) %>%
  summarise(
    rows = n(),
    missing_hale_rows = sum(is.na(hale)),
    missing_gdp_rows = sum(is.na(gdp_per_capita_ppp_current) | is.na(gdp_total_ppp_constant_2021)),
    missing_vlw_rows = sum(is.na(economic_burden_vlw)),
    .groups = "drop"
  ) %>%
  arrange(desc(missing_vlw_rows), location_name)

output_missing <- "data/processed/analysis_missingness.csv"
write_csv(missing_report, output_missing)

cat("Saved outputs:\n")
cat(sprintf("- %s\n", output_detail))
cat(sprintf("- %s\n", output_summary_age))
cat(sprintf("- %s\n", output_country_age))
cat(sprintf("- %s\n", output_missing))
cat("\nData summary:\n")
cat(sprintf("- Asian countries in DALYs data: %d\n", n_distinct(result$location_id)))
cat(sprintf("- Rows in detailed output: %d\n", nrow(result)))
cat(sprintf("- Rows with VLW available: %d\n", sum(!is.na(result$economic_burden_vlw))))
cat(sprintf("- Age groups: %d\n", n_distinct(result$age_name)))
cat(sprintf("- Years: %d-%d\n", min(result$year), max(result$year)))
