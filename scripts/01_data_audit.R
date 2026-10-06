# Audit the PIRLS 2021 student files ---------------------------------------

source("scripts/00_packages.R")

student_files <- c(
  Brazil = here::here("data", "raw", "ASGBRAR5.Rdata"),
  `South Africa` = here::here("data", "raw", "ASGZAFR5.Rdata")
)

required_variables <- c(
  "IDSTUD", "IDSCHOOL", "IDCLASS",
  "TOTWGT", "JKZONE", "JKREP",
  sprintf("ASRREA%02d", 1:5),
  "ASBGSB", "ASDGSB",
  paste0("ASBG11", LETTERS[1:10])
)

load_single_object <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }

  data_environment <- new.env(parent = emptyenv())
  object_names <- load(path, envir = data_environment)

  if (length(object_names) != 1L) {
    stop(
      basename(path), " must contain exactly one object; found ",
      length(object_names), ".",
      call. = FALSE
    )
  }

  data_environment[[object_names]]
}

audit_student_file <- function(country, path) {
  student_data <- load_single_object(path)
  missing_variables <- setdiff(required_variables, names(student_data))

  if (length(missing_variables) > 0L) {
    stop(
      country, " is missing required variable(s): ",
      paste(missing_variables, collapse = ", "),
      call. = FALSE
    )
  }

  cleaned_fields <- haven::zap_missing(student_data[required_variables])

  data.frame(
    country = country,
    students = nrow(student_data),
    unique_student_ids = dplyr::n_distinct(student_data$IDSTUD),
    schools = dplyr::n_distinct(student_data$IDSCHOOL),
    jackknife_zones = dplyr::n_distinct(cleaned_fields$JKZONE, na.rm = TRUE),
    missing_weights = sum(is.na(cleaned_fields$TOTWGT)),
    missing_bullying_index = sum(is.na(cleaned_fields$ASDGSB)),
    missing_reading_pv1 = sum(is.na(cleaned_fields$ASRREA01)),
    stringsAsFactors = FALSE
  )
}

audit_results <- do.call(
  rbind,
  Map(audit_student_file, names(student_files), unname(student_files))
)
rownames(audit_results) <- NULL

if (any(audit_results$students != audit_results$unique_student_ids)) {
  stop("At least one student file contains duplicate IDSTUD values.", call. = FALSE)
}

print(audit_results, row.names = FALSE)
message("Data audit completed successfully.")
