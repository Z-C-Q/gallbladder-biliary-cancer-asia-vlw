library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)
library(scales)
library(sf)
library(rnaturalearth)

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

first_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  x[1]
}

fmt_ui <- function(est, low, high, digits = 2) {
  ifelse(
    is.na(est),
    NA_character_,
    paste0(
      format(round(est, digits), big.mark = ",", scientific = FALSE),
      " (",
      format(round(low, digits), big.mark = ",", scientific = FALSE),
      "-",
      format(round(high, digits), big.mark = ",", scientific = FALSE),
      ")"
    )
  )
}

gbd <- read_csv("data/processed/gbd_analysis_subset.csv", show_col_types = FALSE)
vlw_age <- read_csv("data/processed/asia_age_sex_vlw_1990_2023.csv.gz", show_col_types = FALSE)

population_2023 <- gbd %>%
  filter(
    location_name %in% asia_countries,
    cause_name == "Gallbladder and biliary tract cancer",
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    age_name == "All ages",
    sex_name == "Both",
    metric_name %in% c("Number", "Rate"),
    year == 2023
  ) %>%
  select(location_id, location_name, metric_name, val) %>%
  pivot_wider(names_from = metric_name, values_from = val) %>%
  transmute(
    location_id,
    location_name,
    population_est = ifelse(!is.na(Rate) & Rate > 0, Number / Rate * 100000, NA_real_)
  )

fig3 <- vlw_age %>%
  filter(year == 2023, sex_name == "Both") %>%
  group_by(location_id, location_name, country_code, asia_subregion) %>%
  summarise(
    economic_burden = sum_or_na(economic_burden_vlw),
    economic_burden_lower = sum_or_na(economic_burden_vlw_lower),
    economic_burden_upper = sum_or_na(economic_burden_vlw_upper),
    gdp_total = first_or_na(gdp_total_ppp_constant_2021),
    .groups = "drop"
  ) %>%
  left_join(population_2023, by = c("location_id", "location_name")) %>%
  mutate(
    economic_burden = ifelse(is.na(gdp_total), NA_real_, economic_burden),
    economic_burden_lower = ifelse(is.na(gdp_total), NA_real_, economic_burden_lower),
    economic_burden_upper = ifelse(is.na(gdp_total), NA_real_, economic_burden_upper),
    economic_burden_billion = economic_burden / 1e9,
    economic_burden_lower_billion = economic_burden_lower / 1e9,
    economic_burden_upper_billion = economic_burden_upper / 1e9,
    economic_burden_per_100k = economic_burden / population_est * 100000,
    economic_burden_per_100k_lower = economic_burden_lower / population_est * 100000,
    economic_burden_per_100k_upper = economic_burden_upper / population_est * 100000,
    economic_burden_per_100k_million = economic_burden_per_100k / 1e6,
    economic_burden_per_100k_lower_million = economic_burden_per_100k_lower / 1e6,
    economic_burden_per_100k_upper_million = economic_burden_per_100k_upper / 1e6,
    economic_burden_pct_gdp = economic_burden / gdp_total * 100,
    economic_burden_pct_gdp_lower = economic_burden_lower / gdp_total * 100,
    economic_burden_pct_gdp_upper = economic_burden_upper / gdp_total * 100,
    economic_burden_ui = fmt_ui(economic_burden, economic_burden_lower, economic_burden_upper, 0),
    economic_burden_billion_ui = fmt_ui(economic_burden_billion, economic_burden_lower_billion, economic_burden_upper_billion, 2),
    economic_burden_per_100k_ui = fmt_ui(economic_burden_per_100k, economic_burden_per_100k_lower, economic_burden_per_100k_upper, 0),
    economic_burden_pct_gdp_ui = fmt_ui(economic_burden_pct_gdp, economic_burden_pct_gdp_lower, economic_burden_pct_gdp_upper, 3)
  ) %>%
  arrange(desc(economic_burden))

write_csv(fig3, "results/figure_data/figure2_maps.csv")

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid() %>%
  mutate(map_code = ifelse(iso_a3 == "-99" | is.na(iso_a3), adm0_a3, iso_a3))

map_data <- world %>%
  filter(map_code %in% fig3$country_code) %>%
  left_join(fig3, by = c("map_code" = "country_code"))

unmatched <- fig3 %>%
  filter(!country_code %in% map_data$map_code) %>%
  select(location_name, country_code)
write_csv(unmatched, "results/figure_data/figure2_unmatched_map_codes.csv")

base_map_theme <- theme_void(base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 10.5, hjust = 0),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.position = "bottom",
    plot.margin = margin(5, 5, 5, 5),
    panel.border = element_rect(color = "#9AA7AA", fill = NA, linewidth = 0.35)
  )

map_scale <- function(title) {
  scale_fill_gradientn(
    colours = c("#FFF7BC", "#FEC44F", "#FE9929", "#EC7014", "#CC4C02", "#662506"),
    na.value = "#E5E5E5",
    labels = label_number(accuracy = 0.1),
    name = title
  )
}

make_map <- function(fill_var, title, legend_title) {
  ggplot(map_data) +
    geom_sf(aes(fill = .data[[fill_var]]), color = "white", linewidth = 0.16) +
    coord_sf(xlim = c(24, 150), ylim = c(-12, 56), expand = FALSE) +
    map_scale(legend_title) +
    labs(title = title) +
    guides(fill = guide_colorbar(barwidth = unit(35, "mm"), barheight = unit(3, "mm"), title.position = "top")) +
    base_map_theme
}

p_a <- make_map(
  "economic_burden_billion",
  "A. Economic burden",
  "VLW, billion US$"
)

p_b <- make_map(
  "economic_burden_per_100k_million",
  "B. Economic burden per 100,000 population",
  "million US$ per 100,000"
)

p_c <- make_map(
  "economic_burden_pct_gdp",
  "C. Economic burden as percentage of GDP",
  "% of GDP"
)

fig3_plot <- p_a | p_b | p_c
fig3_plot <- fig3_plot +
  plot_annotation(
    title = "Figure 2. Country-level economic burden of gallbladder and biliary tract cancer in Asia, 2023",
    theme = theme(
      plot.title = element_text(family = "Helvetica", face = "bold", size = 13, hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5)
    )
  )

ggsave("results/figures/main/figure2_maps.pdf", fig3_plot, width = 12, height = 4.6, device = cairo_pdf)
ggsave("results/figures/main/figure2_maps.png", fig3_plot, width = 12, height = 4.6, dpi = 300, bg = "white")

ggsave("results/figures/panels/figure2A_economic_burden_map.pdf", p_a, width = 4.2, height = 4.2, device = cairo_pdf)
ggsave("results/figures/panels/figure2B_economic_burden_per_100k_map.pdf", p_b, width = 4.2, height = 4.2, device = cairo_pdf)
ggsave("results/figures/panels/figure2C_economic_burden_pct_gdp_map.pdf", p_c, width = 4.2, height = 4.2, device = cairo_pdf)

cat("Saved Figure 2 outputs in results/figures and results/figure_data.\n")
