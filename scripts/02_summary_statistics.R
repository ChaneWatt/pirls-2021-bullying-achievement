# Validate the survey design and inspect summary statistics ----------------

source("scripts/00_packages.R")
library(EdSurvey)

output_directory <- here::here("output", "summary_statistics")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

# Read the cached PIRLS files and assign country names to the country objects.
pirls <- readPIRLS(
  path = here::here("data", "raw"),
  countries = c("zaf", "bra")
)

country_data <- pirls$datalist
names(country_data) <- pirls$covs$country

expected_replicates <- c(
  "South Africa" = 250L,
  "Brazil" = 196L
)

if (!all(names(expected_replicates) %in% names(country_data))) {
  stop(
    "The expected South Africa and Brazil objects were not returned by readPIRLS().",
    call. = FALSE
  )
}

analysis_variables <- c(
  "idstud",
  "idschool",
  "idclass",
  "asbg01",  # gender
  "asbghrl", # home resources for learning scale
  "asdghrl", # home resources for learning index
  "asbgsb",  # student bullying scale: higher means less bullying
  "asdgsb",  # student bullying index
  "rrea",    # expands to all five reading plausible values
  "totwgt"   # full-sample student weight
)

categorical_variables <- c("asbg01", "asdghrl", "asdgsb")
continuous_variables <- c("asbghrl", "asbgsb", "rrea")

country_slug <- function(country) {
  tolower(gsub("[^[:alnum:]]+", "_", country))
}

save_summary_result <- function(result, country, variable, statistic_type) {
  filename_stem <- paste(
    country_slug(country),
    statistic_type,
    variable,
    sep = "_"
  )

  saveRDS(
    result,
    file = file.path(output_directory, paste0(filename_stem, ".rds"))
  )

  result_data_frame <- tryCatch(
    as.data.frame(result),
    error = function(error) NULL
  )

  if (!is.null(result_data_frame)) {
    write.csv(
      result_data_frame,
      file = file.path(output_directory, paste0(filename_stem, ".csv")),
      row.names = FALSE,
      na = ""
    )
  }
}

design_checks <- vector("list", length(country_data))
names(design_checks) <- names(country_data)

for (country in names(country_data)) {
  sdf <- country_data[[country]]
  slug <- country_slug(country)

  cat("\n", strrep("=", 78L), "\n", sep = "")
  cat(country, "\n")
  cat(strrep("=", 78L), "\n", sep = "")

  # EdSurvey's print method is the closest equivalent to Stata's describe for
  # the survey object. It reports metadata, omitted levels, and conditions.
  cat("\nSURVEY OBJECT DESCRIPTION\n")
  print(sdf)

  cat("\nAVAILABLE WEIGHTS\n")
  showWeights(sdf)

  cat("\nWEIGHT METADATA\n")
  print(getAttributes(data = sdf, attribute = "weights"))

  cat("\nREADING PLAUSIBLE VALUES\n")
  showPlausibleValues(data = sdf, verbose = TRUE)

  # Compare extraction with and without JK replicate weights. Using setdiff()
  # avoids relying on a country-specific replicate-weight naming convention.
  weight_without_replicates <- getData(
    data = sdf,
    varnames = "totwgt",
    dropOmittedLevels = FALSE,
    returnJKreplicates = FALSE
  )

  weight_with_replicates <- getData(
    data = sdf,
    varnames = "totwgt",
    dropOmittedLevels = FALSE,
    returnJKreplicates = TRUE
  )

  replicate_names <- setdiff(
    names(weight_with_replicates),
    names(weight_without_replicates)
  )
  replicate_count <- length(replicate_names)

  if (replicate_count != expected_replicates[[country]]) {
    stop(
      country, " returned ", replicate_count,
      " replicate weights; expected ", expected_replicates[[country]], ".",
      call. = FALSE
    )
  }

  cat("\nJACKKNIFE REPLICATE-WEIGHT CHECK\n")
  cat("Replicate weights found:", replicate_count, "\n")
  cat(
    "First replicate-weight names:",
    paste(utils::head(replicate_names), collapse = ", "),
    "\n"
  )

  # Extract a compact ordinary data frame for unweighted data inspection.
  # These summaries describe the realised sample, not the target population.
  inspection_data <- getData(
    data = sdf,
    varnames = analysis_variables,
    dropOmittedLevels = FALSE,
    includeNaLabel = FALSE,
    returnJKreplicates = FALSE
  )

  reading_columns <- grep(
    "^asrrea0[1-5]$",
    names(inspection_data),
    value = TRUE
  )

  sample_check <- data.frame(
    country = country,
    students = nrow(inspection_data),
    unique_student_ids = dplyr::n_distinct(inspection_data$idstud),
    schools = dplyr::n_distinct(inspection_data$idschool),
    reading_plausible_values = length(reading_columns),
    full_sample_weight = "TOTWGT",
    jackknife_replicates = replicate_count,
    stringsAsFactors = FALSE
  )
  design_checks[[country]] <- sample_check

  cat("\nUNWEIGHTED SAMPLE CHECK\n")
  print(sample_check, row.names = FALSE)

  cat("\nVARIABLE STRUCTURE\n")
  str(inspection_data)

  # This table counts literal NA values. Omitted response categories are also
  # displayed in the categorical frequency tables below and will receive a
  # fuller treatment in the dedicated missingness analysis.
  missingness <- data.frame(
    country = country,
    variable = names(inspection_data),
    missing_n = vapply(inspection_data, function(x) sum(is.na(x)), integer(1)),
    missing_percent = vapply(
      inspection_data,
      function(x) 100 * mean(is.na(x)),
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )

  write.csv(
    missingness,
    file = file.path(output_directory, paste0(slug, "_literal_na_summary.csv")),
    row.names = FALSE,
    na = ""
  )

  cat("\nLITERAL NA SUMMARY\n")
  print(missingness, row.names = FALSE)

  # Unweighted frequency tables are useful for checking labels and omitted
  # categories. They must not be used as the final population estimates.
  for (variable in categorical_variables) {
    frequency_table <- as.data.frame(
      table(inspection_data[[variable]], useNA = "ifany"),
      stringsAsFactors = FALSE
    )
    names(frequency_table) <- c("category", "unweighted_n")
    frequency_table$country <- country
    frequency_table$variable <- variable
    frequency_table <- frequency_table[
      c("country", "variable", "category", "unweighted_n")
    ]

    cat("\nUNWEIGHTED FREQUENCIES:", toupper(variable), "\n")
    print(frequency_table, row.names = FALSE)

    write.csv(
      frequency_table,
      file = file.path(
        output_directory,
        paste0(slug, "_unweighted_frequency_", variable, ".csv")
      ),
      row.names = FALSE,
      na = ""
    )
  }

  # summary2() uses the default PIRLS weight and survey design. Keeping omitted
  # levels here makes this a transparent validation step before exclusions.
  for (variable in categorical_variables) {
    weighted_summary <- summary2(
      data = sdf,
      variable = variable,
      dropOmittedLevels = FALSE
    )

    unweighted_summary <- summary2(
      data = sdf,
      variable = variable,
      weightVar = NULL,
      dropOmittedLevels = FALSE
    )

    cat("\nWEIGHTED SUMMARY:", toupper(variable), "\n")
    print(weighted_summary)
    cat("\nUNWEIGHTED SUMMARY:", toupper(variable), "\n")
    print(unweighted_summary)

    save_summary_result(
      weighted_summary,
      country,
      variable,
      "weighted"
    )
    save_summary_result(
      unweighted_summary,
      country,
      variable,
      "unweighted"
    )
  }

  # For rrea, EdSurvey pools all five reading plausible values. These are the
  # appropriate weighted descriptive statistics for reporting, unlike a base-R
  # mean of ASRREA01 alone.
  for (variable in continuous_variables) {
    weighted_summary <- summary2(
      data = sdf,
      variable = variable,
      dropOmittedLevels = FALSE
    )

    unweighted_summary <- summary2(
      data = sdf,
      variable = variable,
      weightVar = NULL,
      dropOmittedLevels = FALSE
    )

    cat("\nWEIGHTED SUMMARY:", toupper(variable), "\n")
    print(weighted_summary)
    cat("\nUNWEIGHTED SUMMARY:", toupper(variable), "\n")
    print(unweighted_summary)

    save_summary_result(
      weighted_summary,
      country,
      variable,
      "weighted"
    )
    save_summary_result(
      unweighted_summary,
      country,
      variable,
      "unweighted"
    )
  }
}

design_check_table <- do.call(rbind, design_checks)
rownames(design_check_table) <- NULL

if (any(design_check_table$students != design_check_table$unique_student_ids)) {
  stop("At least one country contains duplicate student identifiers.", call. = FALSE)
}

if (any(design_check_table$reading_plausible_values != 5L)) {
  stop("At least one country does not contain all five reading plausible values.", call. = FALSE)
}

write.csv(
  design_check_table,
  file = file.path(output_directory, "survey_design_checks.csv"),
  row.names = FALSE,
  na = ""
)

cat("\n", strrep("=", 78L), "\n", sep = "")
cat("FINAL SURVEY-DESIGN CHECKS\n")
cat(strrep("=", 78L), "\n", sep = "")
print(design_check_table, row.names = FALSE)
message(
  "Summary-statistics checks completed successfully. Outputs were written to ",
  output_directory,
  "."
)
view(design_check_table)