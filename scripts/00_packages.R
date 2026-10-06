# Package setup ------------------------------------------------------------

required_packages <- c(
  here = "project-relative paths",
  haven = "labelled PIRLS data and missing values",
  dplyr = "data preparation",
  survey = "complex-survey estimation",
  broom = "tidy model output",
  ggplot2 = "figures"
)

install_requested <- "--install" %in% commandArgs(trailingOnly = TRUE)
missing_packages <- setdiff(names(required_packages), rownames(installed.packages()))

if (length(missing_packages) > 0L && install_requested) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
  missing_packages <- setdiff(
    names(required_packages),
    rownames(installed.packages())
  )
}

if (length(missing_packages) > 0L) {
  stop(
    "Missing package(s): ", paste(missing_packages, collapse = ", "), ".\n",
    "Run `Rscript scripts/00_packages.R --install` from the project root.",
    call. = FALSE
  )
}

package_versions <- vapply(
  names(required_packages),
  function(package) as.character(packageVersion(package)),
  character(1)
)

message(
  "All required packages are available:\n",
  paste(sprintf("- %s %s", names(package_versions), package_versions), collapse = "\n")
)
