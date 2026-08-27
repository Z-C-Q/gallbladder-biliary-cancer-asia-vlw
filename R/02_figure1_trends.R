library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)
library(scales)

dir.create("results", showWarnings = FALSE)
dir.create("results/figure_data", recursive = TRUE, showWarnings = FALSE)
dir.create("results/figures/main", recursive = TRUE, showWarnings = FALSE)
dir.create("results/figures/panels", recursive = TRUE, showWarnings = FALSE)

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

weighted_mean_or_na <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

fmt_ui <- function(est, low, high, digits = 2) {
  paste0(
    format(round(est, digits), big.mark = ",", scientific = FALSE),
    " (",
    format(round(low, digits), big.mark = ",", scientific = FALSE),
    "-",
    format(round(high, digits), big.mark = ",", scientific = FALSE),
    ")"
  )
}

gbd <- read_csv("data/processed/gbd_analysis_subset.csv", show_col_types = FALSE)
vlw_age <- read_csv("data/processed/asia_age_sex_vlw_1990_2023.csv.gz", show_col_types = FALSE)

dalys_all <- gbd %>%
  filter(
    location_name %in% asia_countries,
    cause_name == "Gallbladder and biliary tract cancer",
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    age_name == "All ages",
    sex_name == "Both",
    metric_name %in% c("Number", "Rate")
  ) %>%
  select(location_id, location_name, year, metric_name, val, lower, upper) %>%
  pivot_wider(
    names_from = metric_name,
    values_from = c(val, lower, upper),
    names_glue = "{.value}_{metric_name}"
  ) %>%
  transmute(
    location_id, location_name, year,
    dalys_number = val_Number,
    dalys_number_lower = lower_Number,
    dalys_number_upper = upper_Number,
    dalys_rate = val_Rate,
    population_est = ifelse(!is.na(val_Rate) & val_Rate > 0, val_Number / val_Rate * 100000, NA_real_)
  )

asr_country <- gbd %>%
  filter(
    location_name %in% asia_countries,
    cause_name == "Gallbladder and biliary tract cancer",
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    age_name == "Age-standardized",
    sex_name == "Both",
    metric_name == "Rate"
  ) %>%
  transmute(
    location_id, location_name, year,
    asr = val,
    asr_lower = lower,
    asr_upper = upper
  )

burden_health <- dalys_all %>%
  left_join(asr_country, by = c("location_id", "location_name", "year")) %>%
  group_by(year) %>%
  summarise(
    countries_dalys = n_distinct(location_id),
    dalys_number = sum_or_na(dalys_number),
    dalys_number_lower = sum_or_na(dalys_number_lower),
    dalys_number_upper = sum_or_na(dalys_number_upper),
    asr = weighted_mean_or_na(asr, population_est),
    asr_lower = weighted_mean_or_na(asr_lower, population_est),
    asr_upper = weighted_mean_or_na(asr_upper, population_est),
    population_est = sum_or_na(population_est),
    .groups = "drop"
  )

burden_econ <- vlw_age %>%
  filter(sex_name == "Both") %>%
  group_by(year, location_id, location_name) %>%
  summarise(
    economic_burden = sum_or_na(economic_burden_vlw),
    economic_burden_lower = sum_or_na(economic_burden_vlw_lower),
    economic_burden_upper = sum_or_na(economic_burden_vlw_upper),
    gdp_total = ifelse(all(is.na(gdp_total_ppp_constant_2021)), NA_real_, dplyr::first(na.omit(gdp_total_ppp_constant_2021))),
    .groups = "drop"
  ) %>%
  mutate(
    economic_burden = ifelse(is.na(gdp_total), NA_real_, economic_burden),
    economic_burden_lower = ifelse(is.na(gdp_total), NA_real_, economic_burden_lower),
    economic_burden_upper = ifelse(is.na(gdp_total), NA_real_, economic_burden_upper)
  ) %>%
  group_by(year) %>%
  summarise(
    countries_economic = n_distinct(location_id[!is.na(economic_burden)]),
    economic_burden = sum_or_na(economic_burden),
    economic_burden_lower = sum_or_na(economic_burden_lower),
    economic_burden_upper = sum_or_na(economic_burden_upper),
    gdp_total = sum_or_na(gdp_total[!is.na(economic_burden)]),
    economic_burden_pct_gdp = economic_burden / gdp_total * 100,
    economic_burden_pct_gdp_lower = economic_burden_lower / gdp_total * 100,
    economic_burden_pct_gdp_upper = economic_burden_upper / gdp_total * 100,
    .groups = "drop"
  )

fig2 <- burden_health %>%
  left_join(burden_econ, by = "year") %>%
  mutate(
    dalys_number_million = dalys_number / 1e6,
    dalys_number_lower_million = dalys_number_lower / 1e6,
    dalys_number_upper_million = dalys_number_upper / 1e6,
    economic_burden_billion = economic_burden / 1e9,
    economic_burden_lower_billion = economic_burden_lower / 1e9,
    economic_burden_upper_billion = economic_burden_upper / 1e9,
    dalys_number_ui = fmt_ui(dalys_number, dalys_number_lower, dalys_number_upper, 0),
    asr_ui = fmt_ui(asr, asr_lower, asr_upper, 2),
    economic_burden_ui = fmt_ui(economic_burden, economic_burden_lower, economic_burden_upper, 0),
    economic_burden_pct_gdp_ui = fmt_ui(economic_burden_pct_gdp, economic_burden_pct_gdp_lower, economic_burden_pct_gdp_upper, 4)
  )

write_csv(fig2, "results/figure_data/figure1_trends.csv")

base_theme <- theme_classic(base_family = "Helvetica", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8, color = "#263238"),
    axis.line = element_line(linewidth = 0.35, color = "#263238"),
    axis.ticks = element_line(linewidth = 0.3, color = "#263238"),
    plot.margin = margin(7, 7, 7, 7)
  )

line_col <- "#1F6F78"
ribbon_col <- "#9CCDD2"

p_a <- ggplot(fig2, aes(year, dalys_number_million)) +
  geom_ribbon(aes(ymin = dalys_number_lower_million, ymax = dalys_number_upper_million), fill = ribbon_col, alpha = 0.38) +
  geom_line(color = line_col, linewidth = 0.75) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2023)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(title = "A. DALYs number", x = NULL, y = "DALYs, millions") +
  base_theme

p_b <- ggplot(fig2, aes(year, asr)) +
  geom_ribbon(aes(ymin = asr_lower, ymax = asr_upper), fill = ribbon_col, alpha = 0.38) +
  geom_line(color = line_col, linewidth = 0.75) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2023)) +
  labs(title = "B. Age-standardized DALYs rate", x = NULL, y = "Rate per 100,000") +
  base_theme

p_c <- ggplot(fig2, aes(year, economic_burden_billion)) +
  geom_ribbon(aes(ymin = economic_burden_lower_billion, ymax = economic_burden_upper_billion), fill = ribbon_col, alpha = 0.38) +
  geom_line(color = line_col, linewidth = 0.75) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2023)) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(title = "C. Economic burden", x = "Year", y = "VLW, billion US$") +
  base_theme

p_d <- ggplot(fig2, aes(year, economic_burden_pct_gdp)) +
  geom_ribbon(aes(ymin = economic_burden_pct_gdp_lower, ymax = economic_burden_pct_gdp_upper), fill = ribbon_col, alpha = 0.38) +
  geom_line(color = line_col, linewidth = 0.75) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2023)) +
  labs(title = "D. Economic burden as percentage of GDP", x = "Year", y = "% of GDP") +
  base_theme

fig2_plot <- (p_a | p_b) / (p_c | p_d) +
  plot_annotation(
    title = "Figure 1. Trends in DALYs and economic burden in Asia, 1990-2023",
    theme = theme(
      plot.title = element_text(family = "Helvetica", face = "bold", size = 13, hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5)
    )
  )

ggsave("results/figures/main/figure1_trends.pdf", fig2_plot, width = 9.2, height = 6.2, device = cairo_pdf)
ggsave("results/figures/main/figure1_trends.png", fig2_plot, width = 9.2, height = 6.2, dpi = 300, bg = "white")

ggsave("results/figures/panels/figure1A_dalys_number.pdf", p_a, width = 4.6, height = 3.1, device = cairo_pdf)
ggsave("results/figures/panels/figure1B_age_standardized_dalys_rate.pdf", p_b, width = 4.6, height = 3.1, device = cairo_pdf)
ggsave("results/figures/panels/figure1C_economic_burden.pdf", p_c, width = 4.6, height = 3.1, device = cairo_pdf)
ggsave("results/figures/panels/figure1D_economic_burden_pct_gdp.pdf", p_d, width = 4.6, height = 3.1, device = cairo_pdf)

cat("Saved Figure 1 outputs in results/figures and results/figure_data.\n")
