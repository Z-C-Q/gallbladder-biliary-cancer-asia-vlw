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

pct_change <- function(new, old) {
  ifelse(is.na(new) | is.na(old) | old <= 0, NA_real_, (new - old) / old * 100)
}

vlw_age <- read_csv("data/processed/asia_age_sex_vlw_1990_2023.csv.gz", show_col_types = FALSE)

country_year <- vlw_age %>%
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
    economic_burden_billion = economic_burden / 1e9,
    economic_burden_lower_billion = economic_burden_lower / 1e9,
    economic_burden_upper_billion = economic_burden_upper / 1e9,
    economic_burden_pct_gdp = economic_burden / gdp_total * 100,
    economic_burden_pct_gdp_lower = economic_burden_lower / gdp_total * 100,
    economic_burden_pct_gdp_upper = economic_burden_upper / gdp_total * 100
  )

wide <- country_year %>%
  select(
    location_id, location_name, country_code, asia_subregion, year,
    economic_burden, economic_burden_lower, economic_burden_upper,
    economic_burden_billion, economic_burden_lower_billion, economic_burden_upper_billion,
    economic_burden_pct_gdp, economic_burden_pct_gdp_lower, economic_burden_pct_gdp_upper
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = c(
      economic_burden, economic_burden_lower, economic_burden_upper,
      economic_burden_billion, economic_burden_lower_billion, economic_burden_upper_billion,
      economic_burden_pct_gdp, economic_burden_pct_gdp_lower, economic_burden_pct_gdp_upper
    ),
    names_sep = "_"
  ) %>%
  mutate(
    economic_burden_growth_pct = pct_change(economic_burden_2023, economic_burden_1990),
    economic_burden_growth_pct_lower = pct_change(economic_burden_lower_2023, economic_burden_upper_1990),
    economic_burden_growth_pct_upper = pct_change(economic_burden_upper_2023, economic_burden_lower_1990),
    economic_burden_pct_gdp_growth_pct = pct_change(economic_burden_pct_gdp_2023, economic_burden_pct_gdp_1990),
    economic_burden_pct_gdp_growth_pct_lower = pct_change(economic_burden_pct_gdp_lower_2023, economic_burden_pct_gdp_upper_1990),
    economic_burden_pct_gdp_growth_pct_upper = pct_change(economic_burden_pct_gdp_upper_2023, economic_burden_pct_gdp_lower_1990),
    economic_burden_pct_gdp_change_pp = economic_burden_pct_gdp_2023 - economic_burden_pct_gdp_1990,
    economic_burden_pct_gdp_change_pp_lower = economic_burden_pct_gdp_lower_2023 - economic_burden_pct_gdp_upper_1990,
    economic_burden_pct_gdp_change_pp_upper = economic_burden_pct_gdp_upper_2023 - economic_burden_pct_gdp_lower_1990,
    economic_burden_1990_ui = fmt_ui(economic_burden_billion_1990, economic_burden_lower_billion_1990, economic_burden_upper_billion_1990, 2),
    economic_burden_2023_ui = fmt_ui(economic_burden_billion_2023, economic_burden_lower_billion_2023, economic_burden_upper_billion_2023, 2),
    economic_burden_pct_gdp_1990_ui = fmt_ui(economic_burden_pct_gdp_1990, economic_burden_pct_gdp_lower_1990, economic_burden_pct_gdp_upper_1990, 3),
    economic_burden_pct_gdp_2023_ui = fmt_ui(economic_burden_pct_gdp_2023, economic_burden_pct_gdp_lower_2023, economic_burden_pct_gdp_upper_2023, 3),
    economic_burden_growth_pct_ui = fmt_ui(economic_burden_growth_pct, economic_burden_growth_pct_lower, economic_burden_growth_pct_upper, 1),
    economic_burden_pct_gdp_growth_pct_ui = fmt_ui(economic_burden_pct_gdp_growth_pct, economic_burden_pct_gdp_growth_pct_lower, economic_burden_pct_gdp_growth_pct_upper, 1)
  )

top_burden <- wide %>%
  filter(!is.na(economic_burden_growth_pct)) %>%
  slice_max(economic_burden_growth_pct, n = 10, with_ties = FALSE) %>%
  mutate(panel = "Economic burden growth")

top_pct_gdp <- wide %>%
  filter(!is.na(economic_burden_pct_gdp_growth_pct)) %>%
  slice_max(economic_burden_pct_gdp_growth_pct, n = 10, with_ties = FALSE) %>%
  mutate(panel = "Economic burden as % of GDP growth")

fig7 <- bind_rows(top_burden, top_pct_gdp) %>%
  arrange(panel, desc(ifelse(panel == "Economic burden growth", economic_burden_growth_pct, economic_burden_pct_gdp_growth_pct)))

write_csv(wide, "results/figure_data/figure6_country_growth_all.csv")
write_csv(fig7, "results/figure_data/figure6_growth.csv")

palette_subregion <- c(
  "Central Asia" = "#F8766D",
  "East Asia" = "#A3A500",
  "South Asia" = "#00BF7D",
  "Southeast Asia" = "#00B0F6",
  "Western Asia" = "#E76BF3"
)

base_theme <- theme_classic(base_family = "Helvetica", base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = 10.5, hjust = 0),
    axis.text = element_text(color = "#263238", size = 8),
    axis.title = element_text(size = 8.5),
    axis.line = element_line(linewidth = 0.35, color = "#263238"),
    axis.ticks = element_line(linewidth = 0.3, color = "#263238"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    plot.margin = margin(7, 7, 7, 7)
  )

make_bar <- function(data, value_col, lower_col, upper_col, title, xlab) {
  d <- data %>%
    arrange(.data[[value_col]]) %>%
    mutate(location_name = factor(location_name, levels = location_name))

  ggplot(d, aes(x = .data[[value_col]], y = location_name, fill = asia_subregion)) +
    geom_col(width = 0.72) +
    geom_errorbarh(
      aes(xmin = .data[[lower_col]], xmax = .data[[upper_col]]),
      height = 0.24,
      linewidth = 0.32,
      color = "#37474F"
    ) +
    scale_fill_manual(values = palette_subregion, drop = FALSE) +
    scale_x_continuous(labels = label_number(accuracy = 1)) +
    labs(title = title, x = xlab, y = NULL) +
    base_theme
}

p_a <- make_bar(
  top_burden,
  "economic_burden_growth_pct",
  "economic_burden_growth_pct_lower",
  "economic_burden_growth_pct_upper",
  "A. Fastest growth in economic burden",
  "Growth from 1990 to 2023, %"
)

p_b <- make_bar(
  top_pct_gdp,
  "economic_burden_pct_gdp_growth_pct",
  "economic_burden_pct_gdp_growth_pct_lower",
  "economic_burden_pct_gdp_growth_pct_upper",
  "B. Fastest growth in economic burden as % of GDP",
  "Growth from 1990 to 2023, %"
)

fig7_plot <- p_a | p_b
fig7_plot <- fig7_plot +
  plot_annotation(
    title = "Figure 6. Countries with the fastest growth in economic burden, 1990-2023",
    theme = theme(
      plot.title = element_text(family = "Helvetica", face = "bold", size = 13, hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5)
    )
  )

ggsave("results/figures/main/figure6_growth.pdf", fig7_plot, width = 11.8, height = 5.6, device = cairo_pdf)
ggsave("results/figures/main/figure6_growth.png", fig7_plot, width = 11.8, height = 5.6, dpi = 300, bg = "white")

ggsave("results/figures/panels/figure6A_economic_burden_growth.pdf", p_a, width = 5.7, height = 5.1, device = cairo_pdf)
ggsave("results/figures/panels/figure6B_economic_burden_pct_gdp_growth.pdf", p_b, width = 5.7, height = 5.1, device = cairo_pdf)

cat("Saved Figure 6 outputs in results/figures and results/figure_data.\n")
