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

age_levels <- c(
  "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years",
  "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years",
  "75-79 years", "80-84 years", "85-89 years", "90-94 years", "95+ years"
)

sum_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
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

dalys_age <- gbd %>%
  filter(
    location_name %in% asia_countries,
    cause_name == "Gallbladder and biliary tract cancer",
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    metric_name %in% c("Number", "Rate"),
    year == 2023,
    sex_name %in% c("Male", "Female"),
    age_name %in% age_levels
  ) %>%
  select(location_id, location_name, sex_name, age_id, age_name, metric_name, val, lower, upper) %>%
  pivot_wider(
    names_from = metric_name,
    values_from = c(val, lower, upper),
    names_glue = "{.value}_{metric_name}"
  ) %>%
  mutate(
    population_est = ifelse(!is.na(val_Rate) & val_Rate > 0, val_Number / val_Rate * 100000, NA_real_)
  ) %>%
  group_by(sex_name, age_id, age_name) %>%
  summarise(
    countries = n_distinct(location_id),
    population_est = sum_or_na(population_est),
    dalys_number = sum_or_na(val_Number),
    dalys_number_lower = sum_or_na(lower_Number),
    dalys_number_upper = sum_or_na(upper_Number),
    dalys_rate = ifelse(dalys_number == 0, 0, dalys_number / population_est * 100000),
    dalys_rate_lower = ifelse(dalys_number_lower == 0, 0, dalys_number_lower / population_est * 100000),
    dalys_rate_upper = ifelse(dalys_number_upper == 0, 0, dalys_number_upper / population_est * 100000),
    .groups = "drop"
  )

econ_age <- vlw_age %>%
  filter(
    year == 2023,
    sex_name %in% c("Male", "Female"),
    age_name %in% age_levels
  ) %>%
  group_by(sex_name, age_id, age_name) %>%
  summarise(
    countries_economic = n_distinct(location_id[!is.na(economic_burden_vlw)]),
    economic_burden = sum_or_na(economic_burden_vlw),
    economic_burden_lower = sum_or_na(economic_burden_vlw_lower),
    economic_burden_upper = sum_or_na(economic_burden_vlw_upper),
    .groups = "drop"
  )

summary_age_sex <- dalys_age %>%
  left_join(econ_age, by = c("sex_name", "age_id", "age_name")) %>%
  mutate(
    age_name = factor(age_name, levels = age_levels),
    economic_burden_billion = economic_burden / 1e9,
    economic_burden_lower_billion = economic_burden_lower / 1e9,
    economic_burden_upper_billion = economic_burden_upper / 1e9,
    dalys_number_ui = fmt_ui(dalys_number, dalys_number_lower, dalys_number_upper, 0),
    dalys_rate_ui = fmt_ui(dalys_rate, dalys_rate_lower, dalys_rate_upper, 2),
    economic_burden_ui = fmt_ui(economic_burden, economic_burden_lower, economic_burden_upper, 0),
    economic_burden_billion_ui = fmt_ui(economic_burden_billion, economic_burden_lower_billion, economic_burden_upper_billion, 2)
  ) %>%
  arrange(sex_name, age_id)

fig5 <- bind_rows(
  summary_age_sex %>%
    transmute(
      metric = "DALYs number",
      sex_name, age_id, age_name, countries, countries_economic,
      value = dalys_number,
      lower = dalys_number_lower,
      upper = dalys_number_upper,
      plot_value = dalys_number / 1e3,
      plot_lower = dalys_number_lower / 1e3,
      plot_upper = dalys_number_upper / 1e3,
      unit = "thousand DALYs",
      value_ui = dalys_number_ui
    ),
  summary_age_sex %>%
    transmute(
      metric = "DALYs rate",
      sex_name, age_id, age_name, countries, countries_economic,
      value = dalys_rate,
      lower = dalys_rate_lower,
      upper = dalys_rate_upper,
      plot_value = dalys_rate,
      plot_lower = dalys_rate_lower,
      plot_upper = dalys_rate_upper,
      unit = "rate per 100,000",
      value_ui = dalys_rate_ui
    ),
  summary_age_sex %>%
    transmute(
      metric = "Economic burden",
      sex_name, age_id, age_name, countries, countries_economic,
      value = economic_burden,
      lower = economic_burden_lower,
      upper = economic_burden_upper,
      plot_value = economic_burden_billion,
      plot_lower = economic_burden_lower_billion,
      plot_upper = economic_burden_upper_billion,
      unit = "VLW, billion US$",
      value_ui = economic_burden_ui
    )
) %>%
  mutate(
    age_name = factor(age_name, levels = age_levels),
    sex_direction = ifelse(sex_name == "Male", -1, 1),
    plot_value_signed = plot_value * sex_direction,
    plot_lower_signed = ifelse(sex_name == "Male", -plot_upper, plot_lower),
    plot_upper_signed = ifelse(sex_name == "Male", -plot_lower, plot_upper)
  )

write_csv(summary_age_sex, "results/figure_data/figure4_age_sex_summary_wide.csv")
write_csv(fig5, "results/figure_data/figure4_age_sex.csv")

metric_titles <- c(
  "DALYs number" = "A. DALYs number",
  "DALYs rate" = "B. DALYs rate",
  "Economic burden" = "C. Economic burden"
)

metric_units <- c(
  "DALYs number" = "thousand DALYs",
  "DALYs rate" = "rate per 100,000",
  "Economic burden" = "VLW, billion US$"
)

max_abs <- fig5 %>%
  group_by(metric) %>%
  summarise(max_abs = max(abs(c(plot_lower_signed, plot_upper_signed)), na.rm = TRUE), .groups = "drop")

make_pyramid <- function(metric_name) {
  d <- fig5 %>% filter(metric == metric_name)
  lim <- max_abs$max_abs[max_abs$metric == metric_name] * 1.08

  ggplot(d, aes(y = age_name, x = plot_value_signed, fill = sex_name)) +
    geom_col(width = 0.72, color = "white", linewidth = 0.15) +
    geom_errorbarh(
      aes(xmin = plot_lower_signed, xmax = plot_upper_signed),
      height = 0.24,
      linewidth = 0.3,
      color = "#37474F"
    ) +
    geom_vline(xintercept = 0, color = "#263238", linewidth = 0.35) +
    scale_x_continuous(
      limits = c(-lim, lim),
      labels = function(x) label_number(accuracy = 0.1)(abs(x))
    ) +
    scale_y_discrete(limits = rev(age_levels)) +
    scale_fill_manual(values = c("Male" = "#3B82B8", "Female" = "#D95F7E")) +
    labs(
      title = metric_titles[[metric_name]],
      x = metric_units[[metric_name]],
      y = NULL,
      fill = NULL
    ) +
    theme_classic(base_family = "Helvetica", base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 10.5, hjust = 0),
      axis.text = element_text(size = 7.5, color = "#263238"),
      axis.title.x = element_text(size = 8.5),
      axis.line = element_line(linewidth = 0.35, color = "#263238"),
      axis.ticks = element_line(linewidth = 0.3, color = "#263238"),
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      plot.margin = margin(6, 8, 6, 6)
    )
}

p_a <- make_pyramid("DALYs number")
p_b <- make_pyramid("DALYs rate")
p_c <- make_pyramid("Economic burden")

fig5_plot <- p_a | p_b | p_c
fig5_plot <- fig5_plot +
  plot_annotation(
    title = "Figure 4. Age- and sex-specific distribution of DALYs and economic burden in Asia, 2023",
    theme = theme(
      plot.title = element_text(family = "Helvetica", face = "bold", size = 13, hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5)
    )
  )

ggsave("results/figures/main/figure4_age_sex.pdf", fig5_plot, width = 13.5, height = 6.6, device = cairo_pdf)
ggsave("results/figures/main/figure4_age_sex.png", fig5_plot, width = 13.5, height = 6.6, dpi = 300, bg = "white")

ggsave("results/figures/panels/figure4A_age_sex_dalys_number.pdf", p_a, width = 5.0, height = 6.2, device = cairo_pdf)
ggsave("results/figures/panels/figure4B_age_sex_dalys_rate.pdf", p_b, width = 5.0, height = 6.2, device = cairo_pdf)
ggsave("results/figures/panels/figure4C_age_sex_economic_burden.pdf", p_c, width = 5.0, height = 6.2, device = cairo_pdf)

cat("Saved Figure 4 outputs in results/figures and results/figure_data.\n")
