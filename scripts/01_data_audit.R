# Audit the PIRLS 2021 EdSurvey data ---------------------------------------

source("scripts/00_packages.R")
library(EdSurvey)

# readPIRLS() requires the extracted SPSS files with their original filenames.
# The first run creates EdSurvey .txt and .meta cache files in data/raw.
pirls <- readPIRLS(
  path = "data/raw",
  countries = c("zaf", "bra")
)

analysis_variables <- c(
  "idstud",
  "idschool",
  "idclass",
  "asbg01",  # gender
  "asbghrl", # home resources for learning scale
  "asdghrl", # home resources for learning index
  "asbgsb",  # student bullying scale
  "asdgsb",  # student bullying index
  "rrea"     # expands to the five overall-reading plausible values
)

extract_country_data <- function(country_data) {
  getData(
    data = country_data,
    varnames = analysis_variables,
    # Preserve the full sample for the audit. Omitted categories remain labelled
    # and can be excluded explicitly for each later analysis.
    dropOmittedLevels = FALSE,
    addAttributes = TRUE
  )
}

analysis_data <- lapply(pirls$datalist, extract_country_data)
names(analysis_data) <- pirls$covs$country

expected_columns <- c(
  "idstud", "idschool", "idclass", "asbg01",
  "asbghrl", "asdghrl", "asbgsb", "asdgsb",
  sprintf("asrrea%02d", 1:5)
)

for (country in names(analysis_data)) {
  missing_columns <- setdiff(expected_columns, names(analysis_data[[country]]))

  if (length(missing_columns) > 0L) {
    stop(
      country, " is missing expected variable(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
}

audit_results <- do.call(
  rbind,
  lapply(names(analysis_data), function(country) {
    country_data <- analysis_data[[country]]

    data.frame(
      country = country,
      students = nrow(country_data),
      unique_student_ids = dplyr::n_distinct(country_data$idstud),
      schools = dplyr::n_distinct(country_data$idschool),
      extracted_variables = ncol(country_data),
      reading_plausible_values = sum(
        grepl("^asrrea0[1-5]$", names(country_data))
      ),
      stringsAsFactors = FALSE
    )
  })
)
rownames(audit_results) <- NULL

expected_counts <- data.frame(
  country = c("South Africa", "Brazil"),
  expected_students = c(12422L, 4941L),
  expected_schools = c(321L, 187L),
  stringsAsFactors = FALSE
)

count_check <- merge(audit_results, expected_counts, by = "country", sort = FALSE)

if (nrow(count_check) != nrow(expected_counts) ||
    any(count_check$students != count_check$expected_students) ||
    any(count_check$schools != count_check$expected_schools)) {
  stop("EdSurvey student or school counts do not match the expected files.", call. = FALSE)
}

if (any(audit_results$students != audit_results$unique_student_ids)) {
  stop("At least one country contains duplicate IDSTUD values.", call. = FALSE)
}

if (any(audit_results$reading_plausible_values != 5L)) {
  stop("EdSurvey did not return all five overall-reading plausible values.", call. = FALSE)
}

print(audit_results, row.names = FALSE)
message("EdSurvey data audit completed successfully.")
