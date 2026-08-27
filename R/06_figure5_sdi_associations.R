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
sdi_raw <- read_csv("data/processed/sdi_asia_1990_2023.csv", show_col_types = FALSE)
vlw_age <- read_csv("data/processed/asia_age_sex_vlw_1990_2023.csv.gz", show_col_types = FALSE)

dalys_2023 <- gbd %>%
  filter(
    location_name %in% asia_countries,
    cause_name == "Gallbladder and biliary tract cancer",
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    sex_name == "Both",
    year == 2023,
    age_name %in% c("All ages", "Age-standardized"),
    metric_name %in% c("Number", "Rate")
  ) %>%
  mutate(indicator = case_when(
    age_name == "All ages" & metric_name == "Number" ~ "dalys_number",
    age_name == "Age-standardized" & metric_name == "Rate" ~ "asr",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(indicator)) %>%
  select(location_id, location_name, indicator, val, lower, upper) %>%
  pivot_wider(
    names_from = indicator,
    values_from = c(val, lower, upper),
    names_glue = "{indicator}_{.value}"
  ) %>%
  rename(
    dalys_number = dalys_number_val,
    dalys_number_lower = dalys_number_lower,
    dalys_number_upper = dalys_number_upper,
    asr = asr_val,
    asr_lower = asr_lower,
    asr_upper = asr_upper
  )

sdi_2023 <- sdi_raw %>%
  filter(year == 2023, location_name %in% asia_countries, !is.na(location_id)) %>%
  transmute(location_id = as.integer(location_id), location_name, sdi)

econ_2023 <- vlw_age %>%
  filter(year == 2023, sex_name == "Both") %>%
  group_by(location_id, location_name, country_code, asia_subregion) %>%
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
    economic_burden_pct_gdp = economic_burden / gdp_total * 100,
    economic_burden_pct_gdp_lower = economic_burden_lower / gdp_total * 100,
    economic_burden_pct_gdp_upper = economic_burden_upper / gdp_total * 100
  )

fig6 <- dalys_2023 %>%
  left_join(sdi_2023, by = c("location_id", "location_name")) %>%
  left_join(econ_2023, by = c("location_id", "location_name")) %>%
  mutate(
    dalys_number_million = dalys_number / 1e6,
    asr_ui = fmt_ui(asr, asr_lower, asr_upper, 2),
    dalys_number_ui = fmt_ui(dalys_number, dalys_number_lower, dalys_number_upper, 0),
    economic_burden_ui = fmt_ui(economic_burden, economic_burden_lower, economic_burden_upper, 0),
    economic_burden_pct_gdp_ui = fmt_ui(economic_burden_pct_gdp, economic_burden_pct_gdp_lower, economic_burden_pct_gdp_upper, 3)
  ) %>%
  arrange(sdi)

write_csv(fig6, "results/figure_data/figure5_sdi_associations.csv")

cor_results <- tibble::tibble(
  panel = c("A: SDI vs ASR", "B: SDI vs economic burden as % of GDP"),
  n = c(
    sum(complete.cases(fig6$sdi, fig6$asr)),
    sum(complete.cases(fig6$sdi, fig6$economic_burden_pct_gdp))
  ),
  spearman_rho = c(
    cor(fig6$sdi, fig6$asr, method = "spearman", use = "complete.obs"),
    cor(fig6$sdi, fig6$economic_burden_pct_gdp, method = "spearman", use = "complete.obs")
  ),
  p_value = c(
    suppressWarnings(cor.test(fig6$sdi, fig6$asr, method = "spearman", exact = FALSE)$p.value),
    suppressWarnings(cor.test(fig6$sdi, fig6$economic_burden_pct_gdp, method = "spearman", exact = FALSE)$p.value)
  )
)
write_csv(cor_results, "results/figure_data/figure5_spearman_results.csv")

base_theme <- theme_classic(base_family = "Helvetica", base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = 10.5, hjust = 0),
    axis.text = element_text(color = "#263238", size = 8),
    axis.title = element_text(size = 8.5),
    axis.line = element_line(linewidth = 0.35, color = "#263238"),
    axis.ticks = element_line(linewidth = 0.3, color = "#263238"),
    legend.position = "right",
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 7.5),
    plot.margin = margin(7, 7, 7, 7)
  )

label_countries <- c(
  "China", "India", "Japan", "Republic of Korea", "Thailand",
  "Bangladesh", "Pakistan", "Singapore", "Saudi Arabia"
)

p_a_data <- fig6 %>% filter(!is.na(sdi), !is.na(asr))
p_b_data <- fig6 %>% filter(!is.na(sdi), !is.na(economic_burden_pct_gdp))

p_a <- ggplot(p_a_data, aes(sdi, asr)) +
  geom_point(aes(size = dalys_number_million, color = asia_subregion), alpha = 0.78) +
  geom_smooth(method = "loess", se = TRUE, color = "#263238", fill = "#D7E6EA", linewidth = 0.75, formula = y ~ x) +
  geom_text_repel(
    data = p_a_data %>% filter(location_name %in% label_countries),
    aes(label = location_name, color = asia_subregion),
    size = 2.4,
    family = "Helvetica",
    segment.size = 0.2,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  scale_size_continuous(range = c(1.8, 8), labels = label_number(accuracy = 0.1), name = "DALYs, million") +
  labs(
    title = "A. SDI and age-standardized DALYs rate",
    x = "SDI",
    y = "Age-standardized DALYs rate per 100,000",
    color = NULL
  ) +
  base_theme

p_b <- ggplot(p_b_data, aes(sdi, economic_burden_pct_gdp)) +
  geom_point(aes(size = economic_burden_billion, color = asia_subregion), alpha = 0.78) +
  geom_smooth(method = "loess", se = TRUE, color = "#263238", fill = "#D7E6EA", linewidth = 0.75, formula = y ~ x) +
  geom_text_repel(
    data = p_b_data %>% filter(location_name %in% label_countries),
    aes(label = location_name, color = asia_subregion),
    size = 2.4,
    family = "Helvetica",
    segment.size = 0.2,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  scale_size_continuous(range = c(1.8, 8), labels = label_number(accuracy = 0.1), name = "VLW, billion US$") +
  labs(
    title = "B. SDI and economic burden as percentage of GDP",
    x = "SDI",
    y = "Economic burden, % of GDP",
    color = NULL
  ) +
  base_theme

fig6_plot <- p_a | p_b
fig6_plot <- fig6_plot +
  plot_annotation(
    title = "Figure 5. Association of SDI with disease burden and economic burden in Asia, 2023",
    theme = theme(
      plot.title = element_text(family = "Helvetica", face = "bold", size = 13, hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5)
    )
  )

ggsave("results/figures/main/figure5_sdi_associations.pdf", fig6_plot, width = 12.5, height = 5.3, device = cairo_pdf)
ggsave("results/figures/main/figure5_sdi_associations.png", fig6_plot, width = 12.5, height = 5.3, dpi = 300, bg = "white")

ggsave("results/figures/panels/figure5A_sdi_asr.pdf", p_a, width = 6.2, height = 4.8, device = cairo_pdf)
ggsave("results/figures/panels/figure5B_sdi_economic_burden_pct_gdp.pdf", p_b, width = 6.2, height = 4.8, device = cairo_pdf)

cat("Saved Figure 5 outputs in results/figures and results/figure_data.\n")
