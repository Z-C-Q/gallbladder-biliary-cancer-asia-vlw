required_packages <- c(
  "dplyr", "tidyr", "readr", "ggplot2", "patchwork", "scales",
  "sf", "rnaturalearth", "rnaturalearthdata", "ggrepel", "openxlsx",
  "tibble"
)

available <- vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)

if (!all(available)) {
  stop(
    "Missing R package(s): ",
    paste(required_packages[!available], collapse = ", "),
    ". Install them before running R/run_analysis.R."
  )
}

versions <- vapply(
  required_packages,
  function(package) as.character(utils::packageVersion(package)),
  character(1)
)

cat("R ", as.character(getRversion()), "\n", sep = "")
cat(paste(required_packages, versions, sep = " "), sep = "\n")
cat("\nAll required packages are available.\n")
