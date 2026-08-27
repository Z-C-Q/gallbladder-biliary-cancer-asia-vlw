# Reproduce all main figures, manuscript tables, and supplementary analyses
# from the processed inputs included in this repository.

scripts <- c(
  "R/02_figure1_trends.R",
  "R/03_figure2_maps.R",
  "R/04_figure3_rank_changes.R",
  "R/05_figure4_age_sex.R",
  "R/06_figure5_sdi_associations.R",
  "R/07_figure6_growth.R",
  "R/08_figure7_priority_matrix.R",
  "R/09_create_manuscript_tables.R",
  "R/10_supplementary_analyses.R",
  "R/11_study_flowchart.R"
)

for (script in scripts) {
  message("Running ", script)
  source(script, echo = FALSE, chdir = FALSE)
}

message("Analysis completed. Outputs are in results/.")
