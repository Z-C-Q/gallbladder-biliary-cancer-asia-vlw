library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
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

calc_eapc <- function(year, rate) {
  ok <- !is.na(year) & !is.na(rate) & rate > 0
  if (sum(ok) < 3) {
    return(tibble(eapc = NA_real_, eapc_lower = NA_real_, eapc_upper = NA_real_, eapc_p = NA_real_))
  }
  fit <- lm(log(rate[ok]) ~ year[ok])
  beta <- unname(coef(fit)[2])
  ci <- suppressMessages(confint(fit))[2, ]
  p <- summary(fit)$coefficients[2, 4]
  tibble(
    eapc = 100 * (exp(beta) - 1),
    eapc_lower = 100 * (exp(ci[1]) - 1),
    eapc_upper = 100 * (exp(ci[2]) - 1),
    eapc_p = unname(p)
  )
}

gbd <- read_csv("data/processed/gbd_analysis_subset.csv", show_col_types = FALSE)
sdi_raw <- read_csv("data/processed/sdi_asia_1990_2023.csv", show_col_types = FALSE)
vlw_age <- read_csv("data/processed/asia_age_sex_vlw_1990_2023.csv.gz", show_col_types = FALSE)

asr_year <- gbd %>%
  filter(
    location_name %in% asia_countries,
    cause_name == "Gallbladder and biliary tract cancer",
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    sex_name == "Both",
    age_name == "Age-standardized",
    metric_name == "Rate",
    year >= 1990,
    year <= 2023
  ) %>%
  transmute(location_id, location_name, year, asr = val, asr_lower = lower, asr_upper = upper)

eapc <- asr_year %>%
  group_by(location_id, location_name) %>%
  summarise(eapc_tbl = list(calc_eapc(year, asr)), .groups = "drop") %>%
  tidyr::unnest(eapc_tbl)

dalys_2023 <- gbd %>%
  filter(
    location_name %in% asia_countries,
    cause_name == "Gallbladder and biliary tract cancer",
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    sex_name == "Both",
    age_name == "All ages",
    metric_name == "Number",
    year == 2023
  ) %>%
  transmute(
    location_id,
    location_name,
    dalys_number_2023 = val,
    dalys_number_2023_lower = lower,
    dalys_number_2023_upper = upper,
    dalys_number_2023_million = val / 1e6
  )

econ_2023 <- vlw_age %>%
  filter(year == 2023, sex_name == "Both") %>%
  group_by(location_id, location_name, country_code, asia_subregion) %>%
  summarise(
    economic_burden_2023 = sum_or_na(economic_burden_vlw),
    economic_burden_2023_lower = sum_or_na(economic_burden_vlw_lower),
    economic_burden_2023_upper = sum_or_na(economic_burden_vlw_upper),
    gdp_total_2023 = first_or_na(gdp_total_ppp_constant_2021),
    .groups = "drop"
  ) %>%
  mutate(
    economic_burden_2023 = ifelse(is.na(gdp_total_2023), NA_real_, economic_burden_2023),
    economic_burden_2023_lower = ifelse(is.na(gdp_total_2023), NA_real_, economic_burden_2023_lower),
    economic_burden_2023_upper = ifelse(is.na(gdp_total_2023), NA_real_, economic_burden_2023_upper),
    economic_burden_2023_billion = economic_burden_2023 / 1e9,
    economic_burden_2023_lower_billion = economic_burden_2023_lower / 1e9,
    economic_burden_2023_upper_billion = economic_burden_2023_upper / 1e9,
    economic_burden_pct_gdp_2023 = economic_burden_2023 / gdp_total_2023 * 100,
    economic_burden_pct_gdp_2023_lower = economic_burden_2023_lower / gdp_total_2023 * 100,
    economic_burden_pct_gdp_2023_upper = economic_burden_2023_upper / gdp_total_2023 * 100
  )

sdi_2023 <- sdi_raw %>%
  filter(year == 2023, location_name %in% asia_countries, !is.na(location_id)) %>%
  transmute(location_id = as.integer(location_id), location_name, sdi = as.numeric(sdi)) %>%
  mutate(
    sdi_group = cut(
      sdi,
      breaks = quantile(sdi, probs = seq(0, 1, 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Lowest SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "Highest SDI")
    )
  )

fig8 <- eapc %>%
  left_join(dalys_2023, by = c("location_id", "location_name")) %>%
  left_join(econ_2023, by = c("location_id", "location_name")) %>%
  left_join(sdi_2023, by = c("location_id", "location_name")) %>%
  mutate(
    eapc_ui = fmt_ui(eapc, eapc_lower, eapc_upper, 2),
    dalys_number_2023_ui = fmt_ui(dalys_number_2023, dalys_number_2023_lower, dalys_number_2023_upper, 0),
    economic_burden_2023_ui = fmt_ui(economic_burden_2023, economic_burden_2023_lower, economic_burden_2023_upper, 0),
    economic_burden_2023_billion_ui = fmt_ui(economic_burden_2023_billion, economic_burden_2023_lower_billion, economic_burden_2023_upper_billion, 2),
    economic_burden_pct_gdp_2023_ui = fmt_ui(economic_burden_pct_gdp_2023, economic_burden_pct_gdp_2023_lower, economic_burden_pct_gdp_2023_upper, 3)
  ) %>%
  arrange(desc(economic_burden_2023_billion))

write_csv(fig8, "results/figure_data/figure7_priority_matrix.csv")

plot_data <- fig8 %>%
  filter(!is.na(eapc), !is.na(economic_burden_2023_billion), !is.na(dalys_number_2023_million), !is.na(sdi_group))

y_cut <- median(plot_data$economic_burden_2023_billion, na.rm = TRUE)

priority_labels <- plot_data %>%
  filter(
    economic_burden_2023_billion >= y_cut | eapc > 0.5 | location_name %in% c("China", "India", "Japan", "Republic of Korea", "Thailand")
  )

sdi_palette <- c(
  "Lowest SDI" = "#2C7BB6",
  "Low-middle SDI" = "#00A6CA",
  "Middle SDI" = "#ABDDA4",
  "High-middle SDI" = "#FDAE61",
  "Highest SDI" = "#D7191C"
)

p <- ggplot(plot_data, aes(x = eapc, y = economic_burden_2023_billion)) +
  geom_hline(yintercept = y_cut, linetype = "dashed", linewidth = 0.38, color = "#6B6B6B") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.38, color = "#6B6B6B") +
  annotate("text", x = min(plot_data$eapc, na.rm = TRUE), y = y_cut * 1.08, label = "Median 2023 burden", hjust = 0, size = 2.6, family = "Helvetica", color = "#555555") +
  geom_point(aes(size = dalys_number_2023_million, color = sdi_group), alpha = 0.82) +
  geom_text_repel(
    data = priority_labels,
    aes(label = location_name, color = sdi_group),
    size = 2.45,
    family = "Helvetica",
    segment.size = 0.2,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  scale_y_log10(labels = label_number(accuracy = 0.1)) +
  scale_size_continuous(range = c(2, 10), labels = label_number(accuracy = 0.1), name = "DALYs, million") +
  scale_color_manual(values = sdi_palette, drop = FALSE, name = "SDI group") +
  labs(
    title = "Figure 7. Priority matrix of DALYs rate trend and economic burden in Asia",
    x = "EAPC of age-standardized DALYs rate, 1990-2023 (%)",
    y = "Economic burden in 2023, VLW billion US$"
  ) +
  theme_classic(base_family = "Helvetica", base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.text = element_text(color = "#263238", size = 8),
    axis.title = element_text(size = 8.8),
    axis.line = element_line(linewidth = 0.35, color = "#263238"),
    axis.ticks = element_line(linewidth = 0.3, color = "#263238"),
    legend.position = "right",
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 7.5),
    plot.margin = margin(7, 7, 7, 7)
  )

ggsave("results/figures/main/figure7_priority_matrix.pdf", p, width = 8.8, height = 6.2, device = cairo_pdf)
ggsave("results/figures/main/figure7_priority_matrix.png", p, width = 8.8, height = 6.2, dpi = 300, bg = "white")
ggsave("results/figures/panels/figure7_priority_matrix.pdf", p, width = 8.8, height = 6.2, device = cairo_pdf)

cat("Saved Figure 7 outputs in results/figures and results/figure_data.\n")
