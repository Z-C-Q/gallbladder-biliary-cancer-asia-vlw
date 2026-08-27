library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(openxlsx)
library(patchwork)

project_root <- getwd()
out_dir <- file.path(project_root, "results")
data_dir <- file.path(out_dir, "supplementary_data")
pdf_dir <- file.path(out_dir, "figures", "supplementary")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)

theme_supp <- function(base_size = 9) {
  theme_classic(base_family = "Helvetica", base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 3, hjust = 0),
      plot.subtitle = element_text(size = base_size, color = "#444444"),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1, color = "#263238"),
      axis.line = element_line(linewidth = 0.35, color = "#263238"),
      axis.ticks = element_line(linewidth = 0.3, color = "#263238"),
      legend.title = element_text(size = base_size - 1),
      legend.text = element_text(size = base_size - 2),
      plot.margin = margin(8, 8, 8, 8)
    )
}

sum_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

fmt_num <- function(x, digits = 1) {
  ifelse(is.na(x), NA_character_, format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE))
}

pct_change <- function(new, old) {
  ifelse(is.na(new) | is.na(old) | old == 0, NA_real_, (new - old) / old * 100)
}

get_usa_gdp_pc_2023 <- function() {
  gdp_raw <- read_csv(file.path(project_root, "data/external/world_bank_wdi_1990_2024.csv"), show_col_types = FALSE, na = c("..", "NA", ""))
  value <- gdp_raw %>%
    filter(
      `Country Code` == "USA",
      `Series Name` == "GDP per capita, PPP (current international $)"
    ) %>%
    pull(`2023 [YR2023]`) %>%
    as.numeric()
  if (length(value) != 1 || is.na(value)) stop("Could not identify USA GDP per capita PPP for 2023.")
  value
}

fig2 <- read_csv(file.path(project_root, "results/figure_data/figure1_trends.csv"), show_col_types = FALSE)
fig3 <- read_csv(file.path(project_root, "results/figure_data/figure2_maps.csv"), show_col_types = FALSE)
fig8 <- read_csv(file.path(project_root, "results/figure_data/figure7_priority_matrix.csv"), show_col_types = FALSE)
vlw_age <- read_csv(file.path(project_root, "data/processed/asia_age_sex_vlw_1990_2023.csv.gz"), show_col_types = FALSE)
missingness <- read_csv(file.path(project_root, "data/processed/analysis_missingness.csv"), show_col_types = FALSE)

usa_gdp_pc_2023 <- get_usa_gdp_pc_2023()
ie_values <- c(0.5, 0.8, 1.0, 1.2, 1.5)

country_2023 <- vlw_age %>%
  filter(year == 2023, sex_name == "Both") %>%
  group_by(location_id, location_name, country_code, asia_subregion) %>%
  summarise(
    dalys = sum_or_na(dalys),
    dalys_lower = sum_or_na(dalys_lower),
    dalys_upper = sum_or_na(dalys_upper),
    economic_burden_ie1 = sum_or_na(economic_burden_vlw),
    economic_burden_ie1_lower = sum_or_na(economic_burden_vlw_lower),
    economic_burden_ie1_upper = sum_or_na(economic_burden_vlw_upper),
    gdp_pc = dplyr::first(na.omit(gdp_per_capita_ppp_current)),
    gdp_total = dplyr::first(na.omit(gdp_total_ppp_constant_2021)),
    population_est = dplyr::first(na.omit(fig3$population_est[match(location_id[1], fig3$location_id)])),
    .groups = "drop"
  ) %>%
  mutate(
    economic_burden_ie1 = ifelse(is.na(gdp_total), NA_real_, economic_burden_ie1),
    economic_burden_ie1_lower = ifelse(is.na(gdp_total), NA_real_, economic_burden_ie1_lower),
    economic_burden_ie1_upper = ifelse(is.na(gdp_total), NA_real_, economic_burden_ie1_upper)
  ) %>%
  left_join(fig8 %>% select(location_id, eapc, eapc_lower, eapc_upper, sdi, sdi_group), by = "location_id") %>%
  filter(!is.na(economic_burden_ie1), !is.na(gdp_pc), !is.na(gdp_total), !is.na(eapc))

ie_sensitivity <- tidyr::crossing(country_2023, income_elasticity = ie_values) %>%
  mutate(
    ie_multiplier = (gdp_pc / usa_gdp_pc_2023) ^ (income_elasticity - 1),
    economic_burden = economic_burden_ie1 * ie_multiplier,
    economic_burden_lower = economic_burden_ie1_lower * ie_multiplier,
    economic_burden_upper = economic_burden_ie1_upper * ie_multiplier,
    economic_burden_billion = economic_burden / 1e9,
    economic_burden_pct_gdp = economic_burden / gdp_total * 100
  ) %>%
  group_by(income_elasticity) %>%
  arrange(desc(economic_burden), .by_group = TRUE) %>%
  mutate(rank_vlw = row_number()) %>%
  ungroup() %>%
  select(
    location_id, location_name, country_code, asia_subregion, sdi, sdi_group,
    income_elasticity, gdp_pc, gdp_total, dalys, eapc, eapc_lower, eapc_upper,
    economic_burden, economic_burden_lower, economic_burden_upper,
    economic_burden_billion, economic_burden_pct_gdp, rank_vlw
  )

write_csv(ie_sensitivity, file.path(data_dir, "supp_ie_sensitivity_country_2023.csv"))

ie_top10 <- ie_sensitivity %>%
  filter(rank_vlw <= 10) %>%
  arrange(income_elasticity, rank_vlw) %>%
  mutate(
    economic_burden_billion_label = fmt_num(economic_burden_billion, 2),
    economic_burden_pct_gdp_label = fmt_num(economic_burden_pct_gdp, 3)
  )
write_csv(ie_top10, file.path(data_dir, "supp_ie_sensitivity_top10_2023.csv"))

rank_plot_data <- ie_sensitivity %>%
  filter(location_name %in% unique(ie_sensitivity$location_name[ie_sensitivity$income_elasticity == 1.0 & ie_sensitivity$rank_vlw <= 12]))

p_ie_rank <- ggplot(rank_plot_data, aes(x = factor(income_elasticity), y = rank_vlw, group = location_name, color = location_name)) +
  geom_line(linewidth = 0.55, alpha = 0.85) +
  geom_point(size = 1.8) +
  scale_y_reverse(breaks = 1:12) +
  labs(
    title = "Supplementary Figure S1. Country VLW ranking under alternative income elasticity values",
    subtitle = "Countries shown are the top 12 by 2023 VLW under the main IE = 1.0 scenario.",
    x = "Income elasticity of VSL",
    y = "Rank by total VLW in 2023",
    color = "Country"
  ) +
  theme_supp(9) +
  theme(legend.position = "right")

ggsave(file.path(pdf_dir, "supp_fig_s1_ie_sensitivity_rankings.pdf"), p_ie_rank, width = 8.8, height = 5.8, device = cairo_pdf)

top5_stability <- ie_sensitivity %>%
  filter(rank_vlw <= 5) %>%
  arrange(income_elasticity, rank_vlw) %>%
  group_by(income_elasticity) %>%
  summarise(top5_countries = paste(location_name, collapse = "; "), .groups = "drop")
write_csv(top5_stability, file.path(data_dir, "supp_ie_top5_stability_summary.csv"))

# Priority matrix robustness.
priority_base <- country_2023 %>%
  select(location_id, location_name, country_code, asia_subregion, eapc, eapc_lower, sdi, sdi_group) %>%
  left_join(
    ie_sensitivity %>%
      select(location_id, income_elasticity, economic_burden, economic_burden_billion, economic_burden_pct_gdp),
    by = "location_id"
  )

priority_scenarios <- list(
  "Median VLW + EAPC > 0" = priority_base %>%
    filter(income_elasticity == 1.0) %>%
    mutate(in_high_priority = economic_burden >= median(economic_burden, na.rm = TRUE) & eapc > 0),
  "75th percentile VLW + EAPC > 0" = priority_base %>%
    filter(income_elasticity == 1.0) %>%
    mutate(in_high_priority = economic_burden >= quantile(economic_burden, 0.75, na.rm = TRUE) & eapc > 0),
  "Median VLW + significant EAPC > 0" = priority_base %>%
    filter(income_elasticity == 1.0) %>%
    mutate(in_high_priority = economic_burden >= median(economic_burden, na.rm = TRUE) & eapc_lower > 0),
  "IE 0.8 median VLW + EAPC > 0" = priority_base %>%
    filter(income_elasticity == 0.8) %>%
    mutate(in_high_priority = economic_burden >= median(economic_burden, na.rm = TRUE) & eapc > 0),
  "IE 1.2 median VLW + EAPC > 0" = priority_base %>%
    filter(income_elasticity == 1.2) %>%
    mutate(in_high_priority = economic_burden >= median(economic_burden, na.rm = TRUE) & eapc > 0),
  "IE 1.5 median VLW + EAPC > 0" = priority_base %>%
    filter(income_elasticity == 1.5) %>%
    mutate(in_high_priority = economic_burden >= median(economic_burden, na.rm = TRUE) & eapc > 0)
)

priority_robustness <- bind_rows(lapply(names(priority_scenarios), function(nm) {
  priority_scenarios[[nm]] %>%
    mutate(scenario = nm) %>%
    select(scenario, location_id, location_name, country_code, asia_subregion, income_elasticity, economic_burden_billion, economic_burden_pct_gdp, eapc, eapc_lower, in_high_priority)
}))

priority_country_summary <- priority_robustness %>%
  group_by(location_id, location_name, country_code, asia_subregion) %>%
  summarise(
    high_priority_scenarios = sum(in_high_priority, na.rm = TRUE),
    scenarios_tested = n(),
    robustness_score = high_priority_scenarios / scenarios_tested,
    scenario_names = paste(scenario[in_high_priority], collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(desc(robustness_score), location_name)

write_csv(priority_robustness, file.path(data_dir, "supp_priority_matrix_robustness_long.csv"))
write_csv(priority_country_summary, file.path(data_dir, "supp_priority_matrix_robustness_country_summary.csv"))

heat_data <- priority_robustness %>%
  filter(in_high_priority | location_name %in% priority_country_summary$location_name[priority_country_summary$high_priority_scenarios > 0]) %>%
  mutate(
    location_name = factor(location_name, levels = rev(priority_country_summary$location_name[priority_country_summary$high_priority_scenarios > 0])),
    scenario = factor(scenario, levels = names(priority_scenarios))
  )

p_priority <- ggplot(heat_data, aes(x = scenario, y = location_name, fill = in_high_priority)) +
  geom_tile(color = "white", linewidth = 0.45) +
  scale_fill_manual(values = c("TRUE" = "#1B7F79", "FALSE" = "#E8ECEF"), labels = c("FALSE" = "No", "TRUE" = "Yes"), name = "High priority") +
  labs(
    title = "Supplementary Figure S2. Robustness of high-priority country assignment",
    subtitle = "High-priority assignment was tested under alternative VLW thresholds, EAPC criteria, and IE scenarios.",
    x = NULL,
    y = NULL
  ) +
  theme_supp(8) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "bottom"
  )

ggsave(file.path(pdf_dir, "supp_fig_s2_priority_matrix_robustness.pdf"), p_priority, width = 9.5, height = 5.8, device = cairo_pdf)

# Growth decomposition: total VLW = DALYs x unit VLW per DALY.
growth_series <- fig2 %>%
  mutate(
    unit_vlw_per_daly = economic_burden / dalys_number,
    dalys_index_1990 = dalys_number / dalys_number[year == 1990] * 100,
    unit_vlw_index_1990 = unit_vlw_per_daly / unit_vlw_per_daly[year == 1990] * 100,
    total_vlw_index_1990 = economic_burden / economic_burden[year == 1990] * 100
  )

growth_decomposition <- growth_series %>%
  filter(year %in% c(1990, 2023)) %>%
  summarise(
    dalys_1990 = dalys_number[year == 1990],
    dalys_2023 = dalys_number[year == 2023],
    vlw_1990 = economic_burden[year == 1990],
    vlw_2023 = economic_burden[year == 2023],
    unit_vlw_per_daly_1990 = unit_vlw_per_daly[year == 1990],
    unit_vlw_per_daly_2023 = unit_vlw_per_daly[year == 2023],
    dalys_pct_change = pct_change(dalys_2023, dalys_1990),
    vlw_pct_change = pct_change(vlw_2023, vlw_1990),
    unit_vlw_pct_change = pct_change(unit_vlw_per_daly_2023, unit_vlw_per_daly_1990),
    log_share_dalys = log(dalys_2023 / dalys_1990) / log(vlw_2023 / vlw_1990),
    log_share_unit_vlw = log(unit_vlw_per_daly_2023 / unit_vlw_per_daly_1990) / log(vlw_2023 / vlw_1990)
  )

write_csv(growth_series, file.path(data_dir, "supp_growth_decomposition_annual_series.csv"))
write_csv(growth_decomposition, file.path(data_dir, "supp_growth_decomposition_1990_2023_summary.csv"))

growth_long <- growth_series %>%
  select(year, dalys_index_1990, unit_vlw_index_1990, total_vlw_index_1990) %>%
  pivot_longer(-year, names_to = "component", values_to = "index") %>%
  mutate(component = recode(component, dalys_index_1990 = "DALYs", unit_vlw_index_1990 = "Unit VLW per DALY", total_vlw_index_1990 = "Total VLW"))

p_growth_line <- ggplot(growth_long, aes(year, index, color = component)) +
  geom_line(linewidth = 0.75) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  scale_color_manual(values = c("DALYs" = "#3B6EA8", "Unit VLW per DALY" = "#B55D3B", "Total VLW" = "#1B7F79")) +
  labs(title = "A. Indexed change since 1990", x = "Year", y = "Index, 1990 = 100", color = NULL) +
  theme_supp(9) +
  theme(legend.position = "bottom")

share_data <- tibble(
  component = c("DALYs", "Unit VLW per DALY"),
  log_share = c(growth_decomposition$log_share_dalys, growth_decomposition$log_share_unit_vlw)
)
p_growth_share <- ggplot(share_data, aes(component, log_share, fill = component)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = percent(log_share, accuracy = 0.1)), vjust = -0.35, size = 3, family = "Helvetica") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.75)) +
  scale_fill_manual(values = c("DALYs" = "#3B6EA8", "Unit VLW per DALY" = "#B55D3B")) +
  labs(title = "B. Log-change contribution to VLW growth", x = NULL, y = "Share of 1990-2023 log change") +
  theme_supp(9) +
  theme(legend.position = "none")

ggsave(
  file.path(pdf_dir, "supp_fig_s3_vlw_growth_decomposition.pdf"),
  p_growth_line | p_growth_share,
  width = 9.5,
  height = 4.8,
  device = cairo_pdf
)

# VSLY annualization sensitivity: main age-specific VSL/HALE output vs doubled VSL/(0.5*HALE) scenario.
vsly_annualization <- country_2023 %>%
  transmute(
    location_id, location_name, country_code, asia_subregion, sdi, sdi_group,
    main_vsly_assumption = "VSL / age-specific HALE",
    economic_burden_main = economic_burden_ie1,
    economic_burden_main_billion = economic_burden_ie1 / 1e9,
    economic_burden_half_hale = economic_burden_ie1 * 2,
    economic_burden_half_hale_billion = economic_burden_ie1 * 2 / 1e9,
    pct_gdp_main = economic_burden_ie1 / gdp_total * 100,
    pct_gdp_half_hale = economic_burden_ie1 * 2 / gdp_total * 100,
    rank_main = rank(-economic_burden_ie1, ties.method = "first"),
    rank_half_hale = rank(-(economic_burden_ie1 * 2), ties.method = "first")
  ) %>%
  arrange(rank_main)
write_csv(vsly_annualization, file.path(data_dir, "supp_vsly_annualization_sensitivity_2023.csv"))

vsly_asia <- fig2 %>%
  transmute(
    year,
    main_vlw_billion = economic_burden / 1e9,
    half_hale_vlw_billion = economic_burden * 2 / 1e9
  ) %>%
  pivot_longer(-year, names_to = "scenario", values_to = "vlw_billion") %>%
  mutate(scenario = recode(scenario, main_vlw_billion = "Main output: VSL / HALE", half_hale_vlw_billion = "Alternative: VSL / (0.5 x HALE)"))

p_vsly <- ggplot(vsly_asia, aes(year, vlw_billion, color = scenario)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Main output: VSL / HALE" = "#1B7F79", "Alternative: VSL / (0.5 x HALE)" = "#B55D3B")) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(
    title = "Supplementary Figure S4. Sensitivity to VSLY annualization assumption",
    subtitle = "The alternative 0.5 x HALE denominator doubles the absolute VLW level but does not change country rankings.",
    x = "Year",
    y = "Asia total VLW, billion US$",
    color = NULL
  ) +
  theme_supp(9) +
  theme(legend.position = "bottom")

ggsave(file.path(pdf_dir, "supp_fig_s4_vsly_annualization_sensitivity.pdf"), p_vsly, width = 8.2, height = 4.8, device = cairo_pdf)

# Data completeness and uncertainty proxy.
data_quality_proxy <- missingness %>%
  mutate(
    missing_hale_pct = missing_hale_rows / rows * 100,
    missing_gdp_pct = missing_gdp_rows / rows * 100,
    missing_vlw_pct = missing_vlw_rows / rows * 100
  ) %>%
  left_join(
    fig8 %>%
      select(
        location_id, eapc, eapc_lower, economic_burden_2023,
        economic_burden_2023_lower, economic_burden_2023_upper,
        dalys_number_2023, dalys_number_2023_lower, dalys_number_2023_upper
      ),
    by = "location_id"
  ) %>%
  mutate(
    dalys_relative_ui_width = (dalys_number_2023_upper - dalys_number_2023_lower) / dalys_number_2023,
    vlw_relative_ui_width = (economic_burden_2023_upper - economic_burden_2023_lower) / economic_burden_2023,
    data_completeness_score = 1 - missing_vlw_pct / 100,
    high_priority_main = economic_burden_2023 >= median(economic_burden_2023, na.rm = TRUE) & eapc > 0
  ) %>%
  arrange(desc(high_priority_main), desc(missing_vlw_pct), desc(vlw_relative_ui_width))
write_csv(data_quality_proxy, file.path(data_dir, "supp_data_completeness_uncertainty_proxy.csv"))

quality_plot_data <- data_quality_proxy %>%
  filter(high_priority_main | location_name %in% c("Japan", "China", "India", "Republic of Korea", "Thailand")) %>%
  mutate(
    country_order = location_name[order(high_priority_main, vlw_relative_ui_width, missing_vlw_pct, decreasing = TRUE)],
    location_name = factor(location_name, levels = unique(country_order))
  )

p_quality_missing <- ggplot(quality_plot_data, aes(x = reorder(location_name, missing_vlw_pct), y = missing_vlw_pct, fill = high_priority_main)) +
  geom_col(width = 0.65) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#1B7F79", "FALSE" = "#B8C2CC"), labels = c("FALSE" = "Comparator country", "TRUE" = "Main high-priority country"), name = NULL) +
  labs(
    title = "A. Analytic data completeness proxy",
    x = NULL,
    y = "Missing VLW rows, %"
  ) +
  theme_supp(9) +
  theme(legend.position = "none")

p_quality_ui <- ggplot(quality_plot_data, aes(x = reorder(location_name, vlw_relative_ui_width), y = vlw_relative_ui_width, fill = high_priority_main)) +
  geom_col(width = 0.65) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("TRUE" = "#1B7F79", "FALSE" = "#B8C2CC"), labels = c("FALSE" = "Comparator country", "TRUE" = "Main high-priority country"), name = NULL) +
  labs(
    title = "B. 2023 VLW relative uncertainty interval width",
    x = NULL,
    y = "(Upper - lower) / point estimate"
  ) +
  theme_supp(9) +
  theme(legend.position = "bottom")

p_quality <- (p_quality_missing | p_quality_ui) +
  plot_annotation(
    title = "Supplementary Figure S5. Data completeness and uncertainty proxy for selected countries",
    subtitle = "These proxies summarize analytic missingness and DALY-derived VLW interval width; they are not GBD registry-quality scores.",
    theme = theme(
      plot.title = element_text(family = "Helvetica", face = "bold", size = 12, hjust = 0),
      plot.subtitle = element_text(family = "Helvetica", size = 9, color = "#444444")
    )
  )

ggsave(file.path(pdf_dir, "supp_fig_s5_data_completeness_proxy.pdf"), p_quality, width = 11.2, height = 5.6, device = cairo_pdf)

# Workbook bundle.
wb <- createWorkbook()
add_sheet <- function(name, data, note) {
  addWorksheet(wb, name)
  writeData(wb, name, note, startRow = 1, startCol = 1)
  mergeCells(wb, name, cols = 1:min(ncol(data), 8), rows = 1)
  writeData(wb, name, data, startRow = 3, withFilter = TRUE)
  header_style <- createStyle(fgFill = "#1F4E5F", fontColour = "white", textDecoration = "bold", halign = "center", valign = "center", wrapText = TRUE)
  body_style <- createStyle(valign = "top", wrapText = TRUE)
  addStyle(wb, name, createStyle(textDecoration = "italic", fontColour = "#555555", wrapText = TRUE), rows = 1, cols = 1, gridExpand = TRUE)
  addStyle(wb, name, header_style, rows = 3, cols = 1:ncol(data), gridExpand = TRUE)
  if (nrow(data) > 0) addStyle(wb, name, body_style, rows = 4:(3 + nrow(data)), cols = 1:ncol(data), gridExpand = TRUE)
  freezePane(wb, name, firstActiveRow = 4, firstActiveCol = 2)
  widths <- pmin(pmax(sapply(data, function(x) min(max(nchar(as.character(x), keepNA = FALSE), na.rm = TRUE) + 2, 12)), 10), 45)
  setColWidths(wb, name, cols = 1:ncol(data), widths = widths)
}

add_sheet("IE sensitivity all", ie_sensitivity, "Supplementary Table. Country-level 2023 VLW estimates and rankings under alternative income elasticity assumptions.")
add_sheet("IE sensitivity top10", ie_top10, "Supplementary Table. Top 10 countries by 2023 VLW under each income elasticity scenario.")
add_sheet("Priority robustness", priority_robustness, "Supplementary Table. High-priority assignment under alternative priority-matrix thresholds and IE scenarios.")
add_sheet("Priority country summary", priority_country_summary, "Supplementary Table. Country-level robustness score across priority-matrix scenarios.")
add_sheet("Growth annual series", growth_series, "Supplementary Table. Annual series for VLW growth decomposition.")
add_sheet("Growth summary", growth_decomposition, "Supplementary Table. 1990-2023 decomposition of Asia total VLW growth.")
add_sheet("VSLY sensitivity", vsly_annualization, "Supplementary Table. Country-level sensitivity to VSLY annualization denominator.")
add_sheet("Data quality proxy", data_quality_proxy, "Supplementary Table. Data completeness and uncertainty proxy indicators.")

workbook_file <- file.path(data_dir, "supplementary_analysis_tables.xlsx")
saveWorkbook(wb, workbook_file, overwrite = TRUE)

analysis_index <- tibble::tribble(
  ~analysis_id, ~analysis, ~data_file, ~figure_file,
  "S1", "Income-elasticity sensitivity analysis for IE = 0.5, 0.8, 1.0, 1.2, and 1.5.", "supp_ie_sensitivity_country_2023.csv; supp_ie_sensitivity_top10_2023.csv", "supp_fig_s1_ie_sensitivity_rankings.pdf",
  "S2", "Priority-matrix robustness under alternative VLW thresholds, a stricter EAPC criterion, and IE scenarios.", "supp_priority_matrix_robustness_long.csv; supp_priority_matrix_robustness_country_summary.csv", "supp_fig_s2_priority_matrix_robustness.pdf",
  "S3", "VLW growth decomposition into DALY growth and unit VLW per DALY growth.", "supp_growth_decomposition_annual_series.csv; supp_growth_decomposition_1990_2023_summary.csv", "supp_fig_s3_vlw_growth_decomposition.pdf",
  "S4", "Sensitivity to the VSLY annualization denominator.", "supp_vsly_annualization_sensitivity_2023.csv", "supp_fig_s4_vsly_annualization_sensitivity.pdf",
  "S5", "Data completeness and uncertainty proxy analysis.", "supp_data_completeness_uncertainty_proxy.csv", "supp_fig_s5_data_completeness_proxy.pdf"
)
write_csv(analysis_index, file.path(data_dir, "supplementary_analysis_index.csv"))

cat("Generated supplementary analyses in:\n")
cat(out_dir, "\n")
cat("Data files:\n")
print(list.files(data_dir, full.names = TRUE))
cat("PDF files:\n")
print(list.files(pdf_dir, full.names = TRUE))
