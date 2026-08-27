library(dplyr)
library(tidyr)
library(readr)
library(openxlsx)

dir.create("results", showWarnings = FALSE)

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

sum_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

weighted_mean_complete <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  total_w <- sum(w[ok], na.rm = TRUE)
  if (sum(ok) == 0 || !is.finite(total_w) || total_w <= 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

first_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  x[1]
}

fmt_num <- function(x, digits = 1) {
  ifelse(is.na(x), NA_character_, format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE))
}

fmt_ui <- function(est, low, high, digits = 1) {
  ifelse(
    is.na(est),
    NA_character_,
    paste0(fmt_num(est, digits), " (", fmt_num(low, digits), "-", fmt_num(high, digits), ")")
  )
}

pct_change <- function(new, old) {
  ifelse(is.na(new) | is.na(old) | old == 0, NA_real_, (new - old) / old * 100)
}

calc_eapc_vec <- function(year, value) {
  ok <- !is.na(year) & !is.na(value) & value > 0
  if (sum(ok) < 3) return(c(eapc = NA_real_, lower = NA_real_, upper = NA_real_, p = NA_real_))
  fit <- lm(log(value[ok]) ~ year[ok])
  beta <- unname(coef(fit)[2])
  ci <- suppressMessages(confint(fit))[2, ]
  p <- summary(fit)$coefficients[2, 4]
  c(
    eapc = 100 * (exp(beta) - 1),
    lower = 100 * (exp(unname(ci[1])) - 1),
    upper = 100 * (exp(unname(ci[2])) - 1),
    p = unname(p)
  )
}

iqr_text <- function(x, digits = 2) {
  qs <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  paste0(fmt_num(qs[1], digits), "-", fmt_num(qs[2], digits))
}

gbd <- read_csv("data/processed/gbd_analysis_subset.csv", show_col_types = FALSE)
vlw_age <- read_csv("data/processed/asia_age_sex_vlw_1990_2023.csv.gz", show_col_types = FALSE)
sdi_raw <- read_csv("data/processed/sdi_asia_1990_2023.csv", show_col_types = FALSE)
country_map_raw <- read_csv("data/processed/country_mapping_asia.csv", show_col_types = FALSE)

fig2 <- read_csv("results/figure_data/figure1_trends.csv", show_col_types = FALSE)
fig3 <- read_csv("results/figure_data/figure2_maps.csv", show_col_types = FALSE)
fig5 <- read_csv("results/figure_data/figure4_age_sex.csv", show_col_types = FALSE)
fig6 <- read_csv("results/figure_data/figure5_sdi_associations.csv", show_col_types = FALSE)
fig8 <- read_csv("results/figure_data/figure7_priority_matrix.csv", show_col_types = FALSE)

table1 <- tibble::tribble(
  ~Data_source, ~Variable, ~Definition, ~Unit, ~Years, ~Use,
  "GBD 2023", "DALYs number", "Number of disability-adjusted life years due to gallbladder and biliary tract cancer.", "DALYs", "1990-2023", "Disease burden and economic burden estimation",
  "GBD 2023", "DALYs rate", "All-age DALYs rate; population denominator inferred from DALYs number and rate where needed.", "per 100,000 population", "1990-2023", "Temporal trend description",
  "GBD 2023", "Age-standardized DALYs rate", "Age-standardized DALYs rate for between-country and trend comparison.", "per 100,000 population", "1990-2023", "Trend analysis, EAPC and association analysis",
  "World Bank", "GDP per capita PPP", "Gross domestic product per capita based on purchasing power parity.", "current international US$", "1990-2023", "VSL/VSLY derivation and sensitivity analysis",
  "World Bank", "Total GDP PPP", "Total gross domestic product based on purchasing power parity.", "constant 2021 international US$", "1990-2023", "Economic burden as percentage of GDP",
  "GBD covariates", "SDI", "Socio-demographic Index.", "index, 0-1", "1990-2023", "Development-level grouping and association analysis",
  "GBD HALE", "HALE", "Healthy life expectancy by country, year, sex and age group.", "years", "1990-2023", "VSLY derivation",
  "Derived", "Economic burden", "Value of lost welfare estimated as DALYs multiplied by value of a statistical life-year.", "US$", "1990-2023", "Main economic burden outcome",
  "Derived", "Economic burden as % of GDP", "Economic burden divided by total GDP.", "%", "1990-2023", "Relative macroeconomic burden"
)

dalys_all <- gbd %>%
  filter(
    location_name %in% asia_countries,
    cause_name == "Gallbladder and biliary tract cancer",
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    sex_name == "Both",
    age_name == "All ages",
    metric_name %in% c("Number", "Rate")
  ) %>%
  select(location_id, location_name, year, metric_name, val, lower, upper) %>%
  pivot_wider(names_from = metric_name, values_from = c(val, lower, upper), names_glue = "{.value}_{metric_name}") %>%
  transmute(
    location_id, location_name, year,
    dalys_number = val_Number,
    dalys_number_lower = lower_Number,
    dalys_number_upper = upper_Number,
    dalys_rate = val_Rate,
    dalys_rate_lower = lower_Rate,
    dalys_rate_upper = upper_Rate,
    population_est = ifelse(!is.na(val_Rate) & val_Rate > 0, val_Number / val_Rate * 100000, NA_real_)
  )

asr_country <- gbd %>%
  filter(
    location_name %in% asia_countries,
    cause_name == "Gallbladder and biliary tract cancer",
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    sex_name == "Both",
    age_name == "Age-standardized",
    metric_name == "Rate"
  ) %>%
  transmute(location_id, location_name, year, asr = val, asr_lower = lower, asr_upper = upper)

country_year_both <- vlw_age %>%
  filter(sex_name == "Both") %>%
  group_by(location_id, location_name, country_code, asia_subregion, year) %>%
  summarise(
    economic_burden = sum_or_na(economic_burden_vlw),
    economic_burden_lower = sum_or_na(economic_burden_vlw_lower),
    economic_burden_upper = sum_or_na(economic_burden_vlw_upper),
    gdp_total = first_or_na(gdp_total_ppp_constant_2021),
    gdp_pc = first_or_na(gdp_per_capita_ppp_current),
    .groups = "drop"
  ) %>%
  mutate(
    economic_burden = ifelse(is.na(gdp_total), NA_real_, economic_burden),
    economic_burden_lower = ifelse(is.na(gdp_total), NA_real_, economic_burden_lower),
    economic_burden_upper = ifelse(is.na(gdp_total), NA_real_, economic_burden_upper),
    economic_burden_pct_gdp = economic_burden / gdp_total * 100,
    economic_burden_pct_gdp_lower = economic_burden_lower / gdp_total * 100,
    economic_burden_pct_gdp_upper = economic_burden_upper / gdp_total * 100
  )

country_year_sex <- vlw_age %>%
  filter(sex_name %in% c("Male", "Female")) %>%
  group_by(location_id, location_name, asia_subregion, year, sex_name) %>%
  summarise(economic_burden = sum_or_na(economic_burden_vlw), .groups = "drop") %>%
  pivot_wider(names_from = sex_name, values_from = economic_burden)

country_panel <- dalys_all %>%
  left_join(asr_country, by = c("location_id", "location_name", "year")) %>%
  left_join(country_year_both, by = c("location_id", "location_name", "year")) %>%
  mutate(
    economic_burden_per_100k = economic_burden / population_est * 100000,
    economic_burden_per_100k_lower = economic_burden_lower / population_est * 100000,
    economic_burden_per_100k_upper = economic_burden_upper / population_est * 100000
  )

table2_regions <- country_panel %>%
  filter(year == 2023) %>%
  group_by(Region = asia_subregion) %>%
  summarise(
    Countries = n_distinct(location_id),
    Population = sum_or_na(population_est),
    DALYs = sum_or_na(dalys_number),
    DALYs_lower = sum_or_na(dalys_number_lower),
    DALYs_upper = sum_or_na(dalys_number_upper),
    ASR = weighted_mean_complete(asr, population_est),
    ASR_lower = weighted_mean_complete(asr_lower, population_est),
    ASR_upper = weighted_mean_complete(asr_upper, population_est),
    Economic_burden = sum_or_na(economic_burden),
    Economic_burden_lower = sum_or_na(economic_burden_lower),
    Economic_burden_upper = sum_or_na(economic_burden_upper),
    GDP_total = sum_or_na(gdp_total[!is.na(economic_burden)]),
    .groups = "drop"
  )

table2_asia <- country_panel %>%
  filter(year == 2023) %>%
  summarise(
    Region = "Asia",
    Countries = n_distinct(location_id),
    Population = sum_or_na(population_est),
    DALYs = sum_or_na(dalys_number),
    DALYs_lower = sum_or_na(dalys_number_lower),
    DALYs_upper = sum_or_na(dalys_number_upper),
    ASR = weighted_mean_complete(asr, population_est),
    ASR_lower = weighted_mean_complete(asr_lower, population_est),
    ASR_upper = weighted_mean_complete(asr_upper, population_est),
    Economic_burden = sum_or_na(economic_burden),
    Economic_burden_lower = sum_or_na(economic_burden_lower),
    Economic_burden_upper = sum_or_na(economic_burden_upper),
    GDP_total = sum_or_na(gdp_total[!is.na(economic_burden)])
  )

sex_region <- country_year_sex %>%
  filter(year == 2023) %>%
  bind_rows(country_year_sex %>% filter(year == 2023) %>% mutate(asia_subregion = "Asia")) %>%
  group_by(Region = asia_subregion) %>%
  summarise(
    Male_burden = sum_or_na(Male),
    Female_burden = sum_or_na(Female),
    .groups = "drop"
  )

table2 <- bind_rows(table2_asia, table2_regions) %>%
  mutate(
    Economic_burden_per_100k = Economic_burden / Population * 100000,
    Economic_burden_per_100k_lower = Economic_burden_lower / Population * 100000,
    Economic_burden_per_100k_upper = Economic_burden_upper / Population * 100000,
    Economic_burden_pct_GDP = Economic_burden / GDP_total * 100,
    Economic_burden_pct_GDP_lower = Economic_burden_lower / GDP_total * 100,
    Economic_burden_pct_GDP_upper = Economic_burden_upper / GDP_total * 100
  ) %>%
  left_join(sex_region, by = "Region") %>%
  mutate(
    Female_to_male_ratio = Female_burden / Male_burden,
    `DALYs number (95% UI)` = fmt_ui(DALYs, DALYs_lower, DALYs_upper, 0),
    `Age-standardized DALYs rate (95% UI)` = fmt_ui(ASR, ASR_lower, ASR_upper, 2),
    `Economic burden, billion US$ (95% UI)` = fmt_ui(Economic_burden / 1e9, Economic_burden_lower / 1e9, Economic_burden_upper / 1e9, 2),
    `Economic burden per 100,000, US$ (95% UI)` = fmt_ui(Economic_burden_per_100k, Economic_burden_per_100k_lower, Economic_burden_per_100k_upper, 0),
    `Economic burden as % of GDP (95% UI)` = fmt_ui(Economic_burden_pct_GDP, Economic_burden_pct_GDP_lower, Economic_burden_pct_GDP_upper, 3),
    `Male burden, billion US$` = Male_burden / 1e9,
    `Female burden, billion US$` = Female_burden / 1e9
  ) %>%
  select(
    Region, Countries,
    `DALYs number (95% UI)`, `Age-standardized DALYs rate (95% UI)`,
    `Economic burden, billion US$ (95% UI)`,
    `Economic burden per 100,000, US$ (95% UI)`,
    `Economic burden as % of GDP (95% UI)`,
    `Male burden, billion US$`, `Female burden, billion US$`, Female_to_male_ratio,
    DALYs, DALYs_lower, DALYs_upper, ASR, ASR_lower, ASR_upper,
    Economic_burden, Economic_burden_lower, Economic_burden_upper
  )

asia_year <- country_panel %>%
  group_by(year) %>%
  summarise(
    dalys_number = sum_or_na(dalys_number),
    dalys_number_lower = sum_or_na(dalys_number_lower),
    dalys_number_upper = sum_or_na(dalys_number_upper),
    asr = weighted_mean_complete(asr, population_est),
    asr_lower = weighted_mean_complete(asr_lower, population_est),
    asr_upper = weighted_mean_complete(asr_upper, population_est),
    population_est = sum_or_na(population_est),
    economic_burden = sum_or_na(economic_burden),
    economic_burden_lower = sum_or_na(economic_burden_lower),
    economic_burden_upper = sum_or_na(economic_burden_upper),
    gdp_total = sum_or_na(gdp_total[!is.na(economic_burden)]),
    .groups = "drop"
  ) %>%
  mutate(
    dalys_rate = dalys_number / population_est * 100000,
    dalys_rate_lower = dalys_number_lower / population_est * 100000,
    dalys_rate_upper = dalys_number_upper / population_est * 100000,
    economic_burden_per_100k = economic_burden / population_est * 100000,
    economic_burden_per_100k_lower = economic_burden_lower / population_est * 100000,
    economic_burden_per_100k_upper = economic_burden_upper / population_est * 100000,
    economic_burden_pct_gdp = economic_burden / gdp_total * 100,
    economic_burden_pct_gdp_lower = economic_burden_lower / gdp_total * 100,
    economic_burden_pct_gdp_upper = economic_burden_upper / gdp_total * 100
  )

indicator_map <- tibble::tribble(
  ~Indicator, ~est, ~low, ~high, ~unit, ~digits,
  "DALYs number", "dalys_number", "dalys_number_lower", "dalys_number_upper", "DALYs", 0,
  "DALYs rate", "dalys_rate", "dalys_rate_lower", "dalys_rate_upper", "per 100,000", 2,
  "Age-standardized DALYs rate", "asr", "asr_lower", "asr_upper", "per 100,000", 2,
  "Economic burden", "economic_burden", "economic_burden_lower", "economic_burden_upper", "US$", 0,
  "Economic burden per 100,000 population", "economic_burden_per_100k", "economic_burden_per_100k_lower", "economic_burden_per_100k_upper", "US$ per 100,000", 0,
  "Economic burden as % of GDP", "economic_burden_pct_gdp", "economic_burden_pct_gdp_lower", "economic_burden_pct_gdp_upper", "%", 3
)

table3 <- lapply(seq_len(nrow(indicator_map)), function(i) {
  m <- indicator_map[i, ]
  y1990 <- asia_year %>% filter(year == 1990)
  y2023 <- asia_year %>% filter(year == 2023)
  values <- asia_year[[m$est]]
  e <- calc_eapc_vec(asia_year$year, values)
  tibble(
    Indicator = m$Indicator,
    Unit = m$unit,
    `1990 value (95% UI)` = fmt_ui(y1990[[m$est]], y1990[[m$low]], y1990[[m$high]], m$digits),
    `2023 value (95% UI)` = fmt_ui(y2023[[m$est]], y2023[[m$low]], y2023[[m$high]], m$digits),
    `Absolute change` = y2023[[m$est]] - y1990[[m$est]],
    `Percentage change, %` = pct_change(y2023[[m$est]], y1990[[m$est]]),
    `EAPC, % (95% CI)` = fmt_ui(e["eapc"], e["lower"], e["upper"], 2),
    `EAPC P value` = e["p"],
    value_1990 = y1990[[m$est]],
    value_2023 = y2023[[m$est]]
  )
}) %>% bind_rows()

ranking_base <- fig3 %>%
  left_join(fig6 %>% select(location_id, sdi, asr, asr_lower, asr_upper, dalys_number, dalys_number_lower, dalys_number_upper), by = "location_id") %>%
  mutate(
    `DALYs number (95% UI)` = fmt_ui(dalys_number, dalys_number_lower, dalys_number_upper, 0),
    `Age-standardized DALYs rate (95% UI)` = fmt_ui(asr, asr_lower, asr_upper, 2),
    `Economic burden, billion US$ (95% UI)` = fmt_ui(economic_burden_billion, economic_burden_lower_billion, economic_burden_upper_billion, 2),
    `Economic burden per 100,000, US$ (95% UI)` = fmt_ui(economic_burden_per_100k, economic_burden_per_100k_lower, economic_burden_per_100k_upper, 0),
    `Economic burden as % of GDP (95% UI)` = fmt_ui(economic_burden_pct_gdp, economic_burden_pct_gdp_lower, economic_burden_pct_gdp_upper, 3)
  )

make_rank <- function(data, rank_by, section) {
  data %>%
    filter(!is.na(.data[[rank_by]])) %>%
    arrange(desc(.data[[rank_by]])) %>%
    slice_head(n = 10) %>%
    mutate(Section = section, Rank = row_number()) %>%
    select(
      Section, Rank, Country = location_name, Subregion = asia_subregion, SDI = sdi,
      `DALYs number (95% UI)`, `Age-standardized DALYs rate (95% UI)`,
      `Economic burden, billion US$ (95% UI)`,
      `Economic burden per 100,000, US$ (95% UI)`,
      `Economic burden as % of GDP (95% UI)`,
      dalys_number, asr, economic_burden_billion, economic_burden_per_100k, economic_burden_pct_gdp
    )
}

table4 <- bind_rows(
  make_rank(ranking_base, "dalys_number", "DALYs number top 10"),
  make_rank(ranking_base, "economic_burden_billion", "Economic burden top 10"),
  make_rank(ranking_base, "economic_burden_pct_gdp", "Economic burden as % of GDP top 10")
)

eapc_country <- asr_country %>%
  group_by(location_id, location_name) %>%
  summarise(e = list(calc_eapc_vec(year, asr)), .groups = "drop") %>%
  mutate(
    eapc = vapply(e, function(x) x["eapc"], numeric(1)),
    eapc_lower = vapply(e, function(x) x["lower"], numeric(1)),
    eapc_upper = vapply(e, function(x) x["upper"], numeric(1))
  ) %>%
  select(-e)

sdi_change <- sdi_raw %>%
  filter(location_name %in% asia_countries, year %in% c(1990, 2023), !is.na(location_id)) %>%
  transmute(location_id = as.integer(location_id), location_name, year, sdi = as.numeric(sdi)) %>%
  group_by(location_id, location_name, year) %>%
  summarise(sdi = mean(sdi, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = sdi, names_prefix = "sdi_") %>%
  mutate(sdi_change = sdi_2023 - sdi_1990)

table5 <- country_panel %>%
  filter(year %in% c(1990, 2023)) %>%
  select(location_id, location_name, country_code, asia_subregion, year, dalys_number, dalys_number_lower, dalys_number_upper, economic_burden, economic_burden_lower, economic_burden_upper) %>%
  pivot_wider(
    names_from = year,
    values_from = c(dalys_number, dalys_number_lower, dalys_number_upper, economic_burden, economic_burden_lower, economic_burden_upper),
    names_sep = "_"
  ) %>%
  left_join(eapc_country, by = c("location_id", "location_name")) %>%
  left_join(sdi_change, by = c("location_id", "location_name")) %>%
  mutate(
    `1990 DALYs (95% UI)` = fmt_ui(dalys_number_1990, dalys_number_lower_1990, dalys_number_upper_1990, 0),
    `2023 DALYs (95% UI)` = fmt_ui(dalys_number_2023, dalys_number_lower_2023, dalys_number_upper_2023, 0),
    `DALYs percentage change, %` = pct_change(dalys_number_2023, dalys_number_1990),
    `EAPC of ASDR, % (95% CI)` = fmt_ui(eapc, eapc_lower, eapc_upper, 2),
    `1990 economic burden, billion US$ (95% UI)` = fmt_ui(economic_burden_1990 / 1e9, economic_burden_lower_1990 / 1e9, economic_burden_upper_1990 / 1e9, 2),
    `2023 economic burden, billion US$ (95% UI)` = fmt_ui(economic_burden_2023 / 1e9, economic_burden_lower_2023 / 1e9, economic_burden_upper_2023 / 1e9, 2),
    `Economic burden percentage change, %` = pct_change(economic_burden_2023, economic_burden_1990),
    `SDI change` = sdi_change
  ) %>%
  arrange(desc(economic_burden_2023)) %>%
  select(
    Country = location_name, Country_code = country_code, Subregion = asia_subregion,
    `1990 DALYs (95% UI)`, `2023 DALYs (95% UI)`, `DALYs percentage change, %`,
    `EAPC of ASDR, % (95% CI)`,
    `1990 economic burden, billion US$ (95% UI)`,
    `2023 economic burden, billion US$ (95% UI)`,
    `Economic burden percentage change, %`, `SDI change`,
    everything()
  )

fig8_table6 <- fig8 %>%
  left_join(
    asr_country %>%
      filter(year == 2023) %>%
      select(location_id, asr_2023 = asr),
    by = "location_id"
  )

kw_asr <- kruskal.test(asr_2023 ~ sdi_group, data = fig8_table6)$p.value
kw_econ <- kruskal.test(economic_burden_pct_gdp_2023 ~ sdi_group, data = fig8_table6)$p.value
kw_eapc <- kruskal.test(eapc ~ sdi_group, data = fig8_table6)$p.value

table6 <- fig8_table6 %>%
  filter(!is.na(sdi_group)) %>%
  group_by(`SDI group` = sdi_group) %>%
  summarise(
    `Number of countries` = n(),
    `Median age-standardized DALYs rate` = median(asr_2023, na.rm = TRUE),
    `IQR of age-standardized DALYs rate` = iqr_text(asr_2023, 2),
    `Median economic burden as % of GDP` = median(economic_burden_pct_gdp_2023, na.rm = TRUE),
    `IQR of economic burden as % of GDP` = iqr_text(economic_burden_pct_gdp_2023, 3),
    `Median EAPC` = median(eapc, na.rm = TRUE),
    `IQR of EAPC` = iqr_text(eapc, 2),
    .groups = "drop"
  ) %>%
  mutate(
    `Kruskal-Wallis P for ASDR` = kw_asr,
    `Kruskal-Wallis P for economic burden % GDP` = kw_econ,
    `Kruskal-Wallis P for EAPC` = kw_eapc
  )

s1 <- country_panel %>%
  left_join(sdi_raw %>% filter(location_name %in% asia_countries, !is.na(location_id)) %>% transmute(location_id = as.integer(location_id), year, sdi = as.numeric(sdi)), by = c("location_id", "year")) %>%
  mutate(
    DALYs_UI = fmt_ui(dalys_number, dalys_number_lower, dalys_number_upper, 0),
    ASR_UI = fmt_ui(asr, asr_lower, asr_upper, 2),
    Economic_burden_billion_UI = fmt_ui(economic_burden / 1e9, economic_burden_lower / 1e9, economic_burden_upper / 1e9, 2)
  ) %>%
  select(location_id, location_name, country_code, asia_subregion, year, sdi, DALYs_UI, ASR_UI, Economic_burden_billion_UI, everything())

s2 <- fig5 %>%
  select(metric, sex_name, age_id, age_name, value, lower, upper, unit, value_ui, countries, countries_economic)

sensitivity <- dalys_all %>%
  filter(year == 2023) %>%
  left_join(country_year_both %>% filter(year == 2023) %>% select(location_id, gdp_pc, economic_burden), by = "location_id") %>%
  mutate(
    HCM_burden = dalys_number * gdp_pc,
    HCM_burden_lower = dalys_number_lower * gdp_pc,
    HCM_burden_upper = dalys_number_upper * gdp_pc,
    HCM_burden_billion_UI = fmt_ui(HCM_burden / 1e9, HCM_burden_lower / 1e9, HCM_burden_upper / 1e9, 2),
    VLW_burden_billion = economic_burden / 1e9,
    HCM_to_VLW_ratio = HCM_burden / economic_burden
  ) %>%
  select(location_id, location_name, year, DALYs = dalys_number, GDPpc_PPP_current = gdp_pc, HCM_burden_billion_UI, VLW_burden_billion, HCM_to_VLW_ratio)

s4 <- read_csv("results/figure_data/figure5_spearman_results.csv", show_col_types = FALSE)

s5 <- country_map_raw %>%
  filter(location_name %in% asia_countries) %>%
  select(Country.Name, Country.Code, location_id, location_name) %>%
  arrange(location_name)

wb <- createWorkbook()

add_sheet <- function(wb, name, data, note = NULL) {
  addWorksheet(wb, name)
  start_row <- 1
  if (!is.null(note)) {
    writeData(wb, name, note, startRow = 1, startCol = 1)
    mergeCells(wb, name, cols = 1:min(8, max(1, ncol(data))), rows = 1)
    addStyle(wb, name, createStyle(textDecoration = "italic", fontColour = "#555555", wrapText = TRUE), rows = 1, cols = 1, gridExpand = TRUE)
    start_row <- 3
  }
  writeData(wb, name, data, startRow = start_row, withFilter = TRUE)
  header_style <- createStyle(fgFill = "#1F4E5F", fontColour = "white", textDecoration = "bold", halign = "center", valign = "center", wrapText = TRUE, border = "TopBottom", borderColour = "#D9E2E3")
  body_style <- createStyle(valign = "top", wrapText = TRUE)
  addStyle(wb, name, header_style, rows = start_row, cols = 1:ncol(data), gridExpand = TRUE)
  if (nrow(data) > 0) addStyle(wb, name, body_style, rows = (start_row + 1):(start_row + nrow(data)), cols = 1:ncol(data), gridExpand = TRUE)
  freezePane(wb, name, firstActiveRow = start_row + 1, firstActiveCol = 2)
  setColWidths(wb, name, cols = 1:ncol(data), widths = "auto")
  widths <- pmin(pmax(sapply(data, function(x) min(max(nchar(as.character(x), keepNA = FALSE), na.rm = TRUE) + 2, 12)), 10), 45)
  setColWidths(wb, name, cols = 1:ncol(data), widths = widths)
}

add_sheet(wb, "Table 1 Variables", table1, "Table 1. Data sources and variable definitions.")
add_sheet(wb, "Table 2 2023 Regions", table2, "Table 2. Overall and subregional disease burden and economic burden in Asia, 2023.")
add_sheet(wb, "Table 3 Overall Change", table3, "Table 3. Changes in overall disease burden and economic burden in Asia from 1990 to 2023.")
add_sheet(wb, "Table 4 Rankings", table4, "Table 4. Country-level top 10 rankings in 2023.")
add_sheet(wb, "Table 5 Country Trends", table5, "Table 5. Country-level trend indicators from 1990 to 2023.")
add_sheet(wb, "Table 6 SDI Groups", table6, "Table 6. Comparison across SDI groups. P values from Kruskal-Wallis tests.")
add_sheet(wb, "Supp Table 1 Full Panel", s1, "Supplementary Table 1. Country-year panel of DALYs and economic burden.")
add_sheet(wb, "Supp Table 2 Age Sex", s2, "Supplementary Table 2. Age- and sex-specific 2023 results used for Figure 5.")
add_sheet(wb, "Supp Table 3 Sensitivity", sensitivity, "Supplementary Table 3. Sensitivity analysis using DALYs × GDP per capita PPP current international dollars in 2023.")
add_sheet(wb, "Supp Table 4 Spearman", s4, "Supplementary Table 4. Spearman correlation results.")
add_sheet(wb, "Supp Table 5 Mapping", s5, "Supplementary Table 5. GBD location and country code mapping.")

currency_cols <- function(data, pattern) grep(pattern, names(data), ignore.case = TRUE)
for (sheet in names(wb)) {
  setRowHeights(wb, sheet, rows = 1:2, heights = 24)
}

dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
output_file <- "results/tables/manuscript_tables.xlsx"
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("Saved workbook:\n")
cat(output_file, "\n")
