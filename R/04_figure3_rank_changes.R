library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)
library(ggrepel)
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

dalys <- gbd %>%
  filter(
    location_name %in% asia_countries,
    cause_name == "Gallbladder and biliary tract cancer",
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    age_name == "All ages",
    sex_name == "Both",
    metric_name == "Number",
    year %in% c(1990, 2023)
  ) %>%
  transmute(
    location_id,
    location_name,
    year,
    dalys_number = val,
    dalys_number_lower = lower,
    dalys_number_upper = upper
  )

econ <- vlw_age %>%
  filter(year %in% c(1990, 2023), sex_name == "Both") %>%
  group_by(location_id, location_name, country_code, asia_subregion, year) %>%
  summarise(
    economic_burden = sum_or_na(economic_burden_vlw),
    economic_burden_lower = sum_or_na(economic_burden_vlw_lower),
    economic_burden_upper = sum_or_na(economic_burden_vlw_upper),
    gdp_total = first_or_na(gdp_total_ppp_constant_2021),
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

country_data <- dalys %>%
  left_join(econ, by = c("location_id", "location_name", "year")) %>%
  mutate(
    dalys_number_million = dalys_number / 1e6,
    dalys_number_lower_million = dalys_number_lower / 1e6,
    dalys_number_upper_million = dalys_number_upper / 1e6,
    economic_burden_billion = economic_burden / 1e9,
    economic_burden_lower_billion = economic_burden_lower / 1e9,
    economic_burden_upper_billion = economic_burden_upper / 1e9
  )

make_indicator <- function(data, indicator, value_col, lower_col, upper_col, unit, digits) {
  out <- data %>%
    transmute(
      indicator = indicator,
      location_id,
      location_name,
      country_code,
      asia_subregion,
      year,
      value = .data[[value_col]],
      lower = .data[[lower_col]],
      upper = .data[[upper_col]],
      unit = unit
    ) %>%
    group_by(indicator, year) %>%
    arrange(desc(value), .by_group = TRUE) %>%
    mutate(rank = ifelse(is.na(value), NA_integer_, min_rank(desc(value)))) %>%
    ungroup() %>%
    mutate(value_ui = fmt_ui(value, lower, upper, digits))

  top_2023 <- out %>%
    filter(year == 2023, !is.na(rank), rank <= 15) %>%
    pull(location_id)

  out %>%
    filter(location_id %in% top_2023) %>%
    group_by(indicator, location_id) %>%
    mutate(rank_change = rank[year == 1990][1] - rank[year == 2023][1]) %>%
    ungroup()
}

fig4 <- bind_rows(
  make_indicator(country_data, "DALYs number", "dalys_number", "dalys_number_lower", "dalys_number_upper", "DALYs", 0),
  make_indicator(country_data, "Economic burden", "economic_burden_billion", "economic_burden_lower_billion", "economic_burden_upper_billion", "billion US$", 2),
  make_indicator(country_data, "Economic burden as % of GDP", "economic_burden_pct_gdp", "economic_burden_pct_gdp_lower", "economic_burden_pct_gdp_upper", "% of GDP", 3)
) %>%
  arrange(indicator, year, rank)

write_csv(fig4, "results/figure_data/figure3_rank_changes.csv")

plot_df <- fig4 %>%
  mutate(
    year_factor = factor(year, levels = c(1990, 2023)),
    label_2023 = ifelse(year == 2023, location_name, NA_character_),
    label_1990 = ifelse(year == 1990, as.character(rank), NA_character_)
  )

indicator_titles <- c(
  "DALYs number" = "A. Top 15 countries by DALYs number",
  "Economic burden" = "B. Top 15 countries by economic burden",
  "Economic burden as % of GDP" = "C. Top 15 countries by economic burden as % of GDP"
)

make_slope <- function(indicator_name) {
  d <- plot_df %>% filter(indicator == indicator_name)
  max_rank <- max(d$rank, na.rm = TRUE)
  rank_breaks <- sort(unique(c(1, 5, 10, 15, max_rank)))
  ggplot(d, aes(x = year_factor, y = rank, group = location_name)) +
    geom_line(aes(color = asia_subregion), linewidth = 0.55, alpha = 0.8) +
    geom_point(aes(color = asia_subregion), size = 1.9) +
    geom_text(
      data = d %>% filter(year == 1990),
      aes(label = rank),
      nudge_x = -0.09,
      size = 2.3,
      family = "Helvetica",
      color = "#37474F"
    ) +
    geom_text_repel(
      data = d %>% filter(year == 2023),
      aes(label = paste0(rank, ". ", location_name), color = asia_subregion),
      nudge_x = 0.28,
      direction = "y",
      hjust = 0,
      segment.size = 0.18,
      segment.color = "#A0A0A0",
      size = 2.3,
      family = "Helvetica",
      max.overlaps = Inf,
      box.padding = 0.12,
      min.segment.length = 0
    ) +
    scale_y_reverse(breaks = rank_breaks, limits = c(max_rank + 0.7, 0.7)) +
    scale_x_discrete(expand = expansion(mult = c(0.16, 0.58))) +
    labs(
      title = indicator_titles[[indicator_name]],
      x = NULL,
      y = "Rank",
      color = NULL
    ) +
    theme_classic(base_family = "Helvetica", base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 10.5, hjust = 0),
      axis.text = element_text(color = "#263238", size = 8),
      axis.title = element_text(size = 8.5),
      axis.line = element_line(linewidth = 0.35, color = "#263238"),
      axis.ticks = element_line(linewidth = 0.3, color = "#263238"),
      legend.position = "none",
      legend.text = element_text(size = 7),
      plot.margin = margin(6, 26, 6, 6)
    )
}

p_a <- make_slope("DALYs number")
p_b <- make_slope("Economic burden")
p_c <- make_slope("Economic burden as % of GDP")

fig4_plot <- p_a | p_b | p_c
fig4_plot <- fig4_plot +
  plot_annotation(
    title = "Figure 3. Changes in country rankings between 1990 and 2023",
    theme = theme(
      plot.title = element_text(family = "Helvetica", face = "bold", size = 13, hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5)
    )
  )

ggsave("results/figures/main/figure3_rank_changes.pdf", fig4_plot, width = 13.5, height = 6.2, device = cairo_pdf)
ggsave("results/figures/main/figure3_rank_changes.png", fig4_plot, width = 13.5, height = 6.2, dpi = 300, bg = "white")

ggsave("results/figures/panels/figure3A_dalys_rank_change.pdf", p_a, width = 5.0, height = 5.8, device = cairo_pdf)
ggsave("results/figures/panels/figure3B_economic_burden_rank_change.pdf", p_b, width = 5.0, height = 5.8, device = cairo_pdf)
ggsave("results/figures/panels/figure3C_economic_burden_pct_gdp_rank_change.pdf", p_c, width = 5.0, height = 5.8, device = cairo_pdf)

cat("Saved Figure 3 outputs in results/figures and results/figure_data.\n")
