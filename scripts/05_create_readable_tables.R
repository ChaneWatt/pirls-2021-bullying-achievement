# =============================================================================
# PIRLS 2021: Create reader-friendly copies of output tables
# =============================================================================
# The original output files and PIRLS variable codes are preserved. This script
# writes renamed copies to output/readable_tables for interpretation and use in
# the paper.
# =============================================================================

source("scripts/00_packages.R")

input_directories <- c(
  here::here("output", "variable_audit"),
  here::here("output", "preliminary_descriptives")
)

output_directory <- here::here("output", "readable_tables")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

column_name_map <- c(
  rrea = "reading_achievement",
  asdgsb = "bullying_frequency",
  asbgsb = "bullying_scale_score",
  asbg01 = "gender",
  asdage = "student_age",
  asbhses = "home_socioeconomic_status",
  asdhses = "home_socioeconomic_status_category",
  asbghrl = "home_resources_scale",
  asdghrl = "home_resources_category",
  asdhedup = "highest_parental_education",
  asbg03 = "home_language",
  acbg05b = "school_location",
  acbg05c = "neighbourhood_income",
  acbg03a = "share_economically_disadvantaged",
  acbg03b = "share_economically_affluent",
  totwgt = "student_survey_weight",
  n = "unweighted_student_count",
  wtd_n = "weighted_student_count",
  weighted_n = "weighted_student_count",
  pct = "weighted_percent",
  se_pct = "weighted_percent_standard_error",
  weighted_percent = "weighted_percent",
  weighted_percent_se = "weighted_percent_standard_error",
  mean = "mean_reading_achievement",
  se_mean = "reading_mean_standard_error",
  sd = "standard_deviation",
  valid_n = "valid_student_count",
  omitted_or_missing_n = "missing_or_omitted_count",
  omitted_or_missing_pct = "missing_or_omitted_percent"
)

value_name_map <- c(
  RREA = "Reading achievement",
  ASDGSB = "Bullying frequency",
  ASBGSB = "Bullying scale score",
  ASBG01 = "Gender",
  ASDAGE = "Student age",
  ASBHSES = "Home socioeconomic status",
  ASDHSES = "Home socioeconomic status category",
  ASBGHRL = "Home resources scale",
  ASDGHRL = "Home resources category",
  ASDHEDUP = "Highest parental education",
  ASBG03 = "Home language",
  ACBG05B = "School location",
  ACBG05C = "Neighbourhood income",
  ACBG03A = "Share economically disadvantaged",
  ACBG03B = "Share economically affluent",
  TOTWGT = "Student survey weight"
)

make_names_readable <- function(data) {
  original_names <- names(data)
  lower_names <- tolower(original_names)
  matched <- lower_names %in% names(column_name_map)
  original_names[matched] <- unname(column_name_map[lower_names[matched]])
  names(data) <- make.unique(original_names, sep = "_")

  # Replace cells that consist solely of a PIRLS code. Category labels such as
  # "Never or Almost Never" are already readable and are left unchanged.
  data[] <- lapply(data, function(column) {
    if (is.factor(column)) column <- as.character(column)
    if (!is.character(column)) return(column)

    matched_values <- column %in% names(value_name_map)
    column[matched_values] <- unname(value_name_map[column[matched_values]])
    column
  })

  data
}

csv_files <- unlist(lapply(input_directories, function(directory) {
  if (!dir.exists(directory)) return(character(0))
  list.files(directory, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
}))

if (length(csv_files) == 0L) {
  stop(
    "No CSV outputs were found. Run scripts/03_variable_audit.R and ",
    "scripts/04_preliminary_descriptives.R first."
  )
}

conversion_log <- lapply(csv_files, function(input_file) {
  table <- read.csv(
    input_file,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )

  readable_table <- make_names_readable(table)
  output_file <- file.path(output_directory, basename(input_file))

  write.csv(readable_table, output_file, row.names = FALSE, na = "")

  data.frame(
    source_file = normalizePath(input_file, winslash = "/", mustWork = FALSE),
    readable_file = normalizePath(output_file, winslash = "/", mustWork = FALSE),
    rows = nrow(readable_table),
    columns = ncol(readable_table),
    stringsAsFactors = FALSE
  )
})

conversion_log <- dplyr::bind_rows(conversion_log)
write.csv(
  conversion_log,
  file.path(output_directory, "readable_table_conversion_log.csv"),
  row.names = FALSE
)

file.copy(
  from = here::here("docs", "variable_dictionary.csv"),
  to = file.path(output_directory, "variable_dictionary.csv"),
  overwrite = TRUE
)

cat("\n==============================================================================\n")
cat("READABLE OUTPUT TABLES CREATED\n")
cat("==============================================================================\n")
print(conversion_log, row.names = FALSE)

message(
  "Readable copies were written to ",
  normalizePath(output_directory, winslash = "/", mustWork = FALSE),
  ". The original technical tables were not changed."
)

