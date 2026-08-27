library(grid)
library(readr)

dir.create("results", showWarnings = FALSE)
dir.create("results/figure_data", recursive = TRUE, showWarnings = FALSE)
dir.create("results/figures/supplementary", recursive = TRUE, showWarnings = FALSE)

nodes <- tibble::tribble(
  ~id, ~stage, ~label, ~detail,
  "D1", "Data sources", "GBD 2023", "DALYs number, crude rate and ASR for gallbladder and biliary tract cancer, 1990-2023",
  "D2", "Data sources", "World Bank GDP", "GDP per capita PPP and total GDP, 1990-2023",
  "D3", "Data sources", "SDI and HALE", "Socio-demographic index and healthy life expectancy",
  "S1", "Study sample", "Eligibility", "Asian countries with available GBD estimates",
  "S2", "Study sample", "Dimensions", "Country, year, sex, age group and Asian subregion",
  "P1", "Data processing", "Harmonization", "location_id-country code matching; GDP, SDI and HALE linked by country-year",
  "P2", "Data processing", "Uncertainty", "95% uncertainty intervals retained from GBD and HALE",
  "E1", "Economic burden", "VLW estimation", "VLW = DALYs x VSLY",
  "E2", "Economic burden", "VSLY derivation", "VSL = VSL_USA_2023 x (GDPpc / GDPpc_USA_2023)^IE; VSLY = VSL / HALE",
  "E3", "Economic burden", "Relative metrics", "Burden per population and burden as percentage of GDP",
  "A1", "Statistical analysis", "Temporal trends", "1990-2023 changes and EAPC",
  "A2", "Statistical analysis", "Heterogeneity", "Country, subregion, sex and age-specific comparisons",
  "A3", "Statistical analysis", "Associations", "SDI, HALE and GDP associations with disease and economic burden",
  "O1", "Outputs", "Study findings", "Regional trends, country differences, age-sex patterns and socioeconomic gradients"
)

edges <- tibble::tribble(
  ~from, ~to,
  "D1", "S1",
  "D2", "P1",
  "D3", "P1",
  "S1", "S2",
  "S2", "P1",
  "P1", "P2",
  "P2", "E1",
  "E1", "E2",
  "E2", "E3",
  "E3", "A1",
  "E3", "A2",
  "E3", "A3",
  "A1", "O1",
  "A2", "O1",
  "A3", "O1"
)

write_csv(nodes, "results/figure_data/study_flowchart_nodes.csv")
write_csv(edges, "results/figure_data/study_flowchart_edges.csv")

wrap_label <- function(x, width = 26) {
  parts <- unlist(strsplit(x, "\n", fixed = TRUE))
  paste(vapply(parts, function(part) paste(strwrap(part, width = width), collapse = "\n"), character(1)), collapse = "\n")
}

draw_box <- function(x, y, w, h, title, body, fill, border, title_size = 9.7, body_size = 7.3) {
  grid.roundrect(
    x = unit(x, "npc"), y = unit(y, "npc"),
    width = unit(w, "npc"), height = unit(h, "npc"),
    r = unit(0.025, "npc"),
    gp = gpar(fill = fill, col = border, lwd = 1.25)
  )
  grid.text(
    title,
    x = unit(x, "npc"), y = unit(y + h * 0.22, "npc"),
    gp = gpar(fontfamily = "Helvetica", fontface = "bold", fontsize = title_size, col = "#16262B")
  )
  grid.text(
    wrap_label(body, 28),
    x = unit(x, "npc"), y = unit(y - h * 0.12, "npc"),
    gp = gpar(fontfamily = "Helvetica", fontsize = body_size, col = "#26393E", lineheight = 0.92)
  )
}

draw_stage <- function(x, label, color) {
  grid.text(
    label,
    x = unit(x, "npc"), y = unit(0.875, "npc"),
    gp = gpar(fontfamily = "Helvetica", fontface = "bold", fontsize = 9.4, col = color)
  )
  grid.lines(
    x = unit(c(x - 0.065, x + 0.065), "npc"),
    y = unit(c(0.855, 0.855), "npc"),
    gp = gpar(col = color, lwd = 2)
  )
}

draw_arrow <- function(x0, y0, x1, y1) {
  grid.segments(
    x0 = unit(x0, "npc"), y0 = unit(y0, "npc"),
    x1 = unit(x1, "npc"), y1 = unit(y1, "npc"),
    arrow = arrow(length = unit(0.11, "inches"), type = "closed"),
    gp = gpar(col = "#6C7F86", lwd = 1.05)
  )
}

draw_bracket_arrow <- function(x0, ys, x1, y1) {
  for (y in ys) {
    grid.segments(
      x0 = unit(x0, "npc"), y0 = unit(y, "npc"),
      x1 = unit((x0 + x1) / 2, "npc"), y1 = unit(y, "npc"),
      gp = gpar(col = "#6C7F86", lwd = 1.0)
    )
  }
  grid.segments(
    x0 = unit((x0 + x1) / 2, "npc"), y0 = unit(min(ys), "npc"),
    x1 = unit((x0 + x1) / 2, "npc"), y1 = unit(max(ys), "npc"),
    gp = gpar(col = "#6C7F86", lwd = 1.0)
  )
  grid.segments(
    x0 = unit((x0 + x1) / 2, "npc"), y0 = unit(y1, "npc"),
    x1 = unit(x1, "npc"), y1 = unit(y1, "npc"),
    arrow = arrow(length = unit(0.11, "inches"), type = "closed"),
    gp = gpar(col = "#6C7F86", lwd = 1.0)
  )
}

draw_fig <- function() {
  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))
  grid.text(
    "Figure 1. Study design and analytic workflow",
    x = unit(0.5, "npc"), y = unit(0.948, "npc"),
    gp = gpar(fontfamily = "Helvetica", fontface = "bold", fontsize = 18, col = "#16262B")
  )

  xs <- c(0.095, 0.265, 0.435, 0.615, 0.795, 0.925)
  cols <- c("#31525B", "#9A6B16", "#6C5585", "#3D6E48", "#476A9A", "#8A4D4D")
  labs <- c("Data sources", "Study sample", "Data processing", "Economic estimation", "Statistical analysis", "Outputs")
  for (i in seq_along(xs)) draw_stage(xs[i], labs[i], cols[i])

  # Data sources
  draw_box(0.095, 0.755, 0.14, 0.105, "GBD 2023", "DALYs number, crude rate and ASR\n1990-2023", "#EAF3F4", "#31525B")
  draw_box(0.095, 0.610, 0.14, 0.105, "World Bank", "GDP per capita PPP\nand total GDP", "#EAF3F4", "#31525B")
  draw_box(0.095, 0.465, 0.14, 0.105, "SDI and HALE", "Development index and\nhealthy life expectancy", "#EAF3F4", "#31525B")

  # Study sample
  draw_box(0.265, 0.705, 0.145, 0.115, "Eligibility", "Asian countries\nwith available estimates", "#FFF5E6", "#9A6B16")
  draw_box(0.265, 0.535, 0.145, 0.115, "Dimensions", "Country, year, sex,\nage group and subregion", "#FFF5E6", "#9A6B16")

  # Processing
  draw_box(0.435, 0.705, 0.155, 0.115, "Harmonization", "location_id-country code\nGDP, SDI and HALE linked", "#F6F1FA", "#6C5585")
  draw_box(0.435, 0.535, 0.155, 0.115, "Burden measures", "DALYs number, rate and ASR\nwith 95% uncertainty", "#F6F1FA", "#6C5585")

  # Economic estimation
  draw_box(0.615, 0.755, 0.16, 0.105, "VSLY", "VSL = VSL_USA x\n(GDPpc/GDPpc_USA)^IE", "#EEF7EF", "#3D6E48", body_size = 6.8)
  draw_box(0.615, 0.610, 0.16, 0.105, "VLW", "Economic burden =\nDALYs x VSLY", "#EEF7EF", "#3D6E48")
  draw_box(0.615, 0.465, 0.16, 0.105, "95% UI", "Lower and upper estimates\nfrom DALYs and HALE UI", "#EEF7EF", "#3D6E48", body_size = 6.8)
  draw_box(0.615, 0.320, 0.16, 0.105, "Relative burden", "Burden per population\nand % of GDP", "#EEF7EF", "#3D6E48")

  # Analysis
  draw_box(0.795, 0.735, 0.155, 0.105, "Temporal trends", "1990-2023 changes\nand EAPC", "#F2F6FC", "#476A9A")
  draw_box(0.795, 0.575, 0.155, 0.105, "Heterogeneity", "Country, subregion,\nsex and age comparisons", "#F2F6FC", "#476A9A", body_size = 6.9)
  draw_box(0.795, 0.415, 0.155, 0.105, "Associations", "SDI, HALE and GDP\ncorrelation/smooth curves", "#F2F6FC", "#476A9A", body_size = 6.9)

  # Outputs
  draw_box(0.925, 0.575, 0.13, 0.145, "Study\nfindings", "Regional trends,\ncountry differences,\nage-sex patterns and\nsocioeconomic gradients", "#F8EEEE", "#8A4D4D", title_size = 9.0, body_size = 6.5)

  # Main flow arrows between stages
  draw_arrow(0.168, 0.610, 0.192, 0.610)
  draw_arrow(0.338, 0.610, 0.357, 0.610)
  draw_arrow(0.514, 0.610, 0.535, 0.610)
  draw_arrow(0.695, 0.610, 0.718, 0.610)
  draw_arrow(0.873, 0.575, 0.860, 0.575)

  # Within-stage arrows
  draw_arrow(0.265, 0.648, 0.265, 0.592)
  draw_arrow(0.435, 0.648, 0.435, 0.592)
  draw_arrow(0.615, 0.703, 0.615, 0.663)
  draw_arrow(0.615, 0.558, 0.615, 0.518)
  draw_arrow(0.615, 0.412, 0.615, 0.373)
  draw_arrow(0.795, 0.682, 0.795, 0.628)
  draw_arrow(0.795, 0.522, 0.795, 0.468)

  grid.text(
    wrap_label("ASR, age-standardized rate; DALYs, disability-adjusted life years; EAPC, estimated annual percentage change; GDP, gross domestic product; HALE, healthy life expectancy; SDI, Socio-demographic Index; UI, uncertainty interval; VLW, value of lost welfare; VSLY, value of a statistical life-year.", 165),
    x = unit(0.5, "npc"), y = unit(0.045, "npc"),
    gp = gpar(fontfamily = "Helvetica", fontsize = 7.4, col = "#3A494D", lineheight = 0.9)
  )
}

pdf("results/figures/supplementary/study_flowchart.pdf", width = 12, height = 7.2, family = "Helvetica", useDingbats = FALSE)
draw_fig()
dev.off()

png("results/figures/supplementary/study_flowchart.png", width = 3600, height = 2160, res = 300, bg = "white")
draw_fig()
dev.off()

cat("Saved study-flowchart outputs in results/figures/supplementary and results/figure_data.\n")
