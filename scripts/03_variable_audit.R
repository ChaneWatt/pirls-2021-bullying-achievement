# =============================================================================
# PIRLS 2021: Candidate-variable audit (Step 4)
# =============================================================================
# Purpose:
#   1. Search the PIRLS codebook for candidate controls and mechanisms.
#   2. Check whether pre-specified candidate variables exist in both countries.
#   3. Record coding, missingness, and provisional analytical roles.
#
# This script does not run regressions and does not alter the source data.
# =============================================================================

source("scripts/00_packages.R")
source("scripts/pirls_load_helpers.R")

library(EdSurvey)
library(dplyr)

output_dir <- here::here("output", "variable_audit")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

zaf <- load_pirls_country("zaf")
bra <- load_pirls_country("bra")

country_data <- list(
  "South Africa" = zaf,
  "Brazil" = bra
)

# Search broadly first. Exact variables are retained only after inspecting the
# resulting labels, levels, and missingness.
search_terms <- c(
  age = "age|date of birth|birth year|birth month|older",
  grade_repetition = "repeat|repetition|retention|held back|over-age|over age",
  parental_education = "parent.*education|mother.*education|father.*education|highest education",
  home_language = "language.*home|speak.*home|test language",
  school_location = "school.*location|urban|rural|immediate area",
  school_ses_composition = "disadvantaged|affluent|income level|socioeconomic|socio-economic",
  school_resources = "school resources|resource shortage|resources.*instruction|instruction.*resources",
  school_belonging = "belonging|belong",
  absenteeism = "absent|absence|attendance|missed school",
  psychological_difficulties = "psychological|wellbeing|well-being|anxious|lonely",
  engagement_reading_attitudes = "engagement|like reading|reading attitude|confident in reading",
  school_safety = "safe and orderly|school safety|feel safe|discipline"
)

flatten_for_csv <- function(x) {
  if (!is.data.frame(x)) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  }

  x[] <- lapply(x, function(column) {
    if (is.list(column)) {
      vapply(column, function(value) paste(value, collapse = " | "), character(1))
    } else {
      as.character(column)
    }
  })

  x
}

search_one <- function(sdf, country, concept, pattern) {
  result <- tryCatch(
    # levels = FALSE keeps the result tabular for the audit CSV. Coding for
    # exact candidates is recorded below from getData(dropOmittedLevels = FALSE).
    searchSDF(string = pattern, data = sdf, levels = FALSE),
    error = function(error) {
      data.frame(search_error = conditionMessage(error), stringsAsFactors = FALSE)
    }
  )

  result <- flatten_for_csv(result)
  result$country <- country
  result$concept <- concept
  result$search_pattern <- pattern
  result[, c("country", "concept", "search_pattern", setdiff(names(result), c("country", "concept", "search_pattern")))]
}

search_results <- list()
search_index <- 1L

for (country in names(country_data)) {
  for (concept in names(search_terms)) {
    search_results[[search_index]] <- search_one(
      sdf = country_data[[country]],
      country = country,
      concept = concept,
      pattern = unname(search_terms[[concept]])
    )
    search_index <- search_index + 1L
  }
}

search_results <- bind_rows(search_results)

write.csv(
  search_results,
  file.path(output_dir, "candidate_codebook_search_results.csv"),
  row.names = FALSE,
  na = ""
)
saveRDS(search_results, file.path(output_dir, "candidate_codebook_search_results.rds"))

# Candidates supported by the literature and/or PIRLS documentation. Variables
# found by the broader searches above can be added after their labels are checked.
candidate_registry <- data.frame(
  concept = c(
    "Reading achievement", "Bullying frequency category", "Bullying continuous scale",
    "Gender", "Student age", "Home resources continuous scale",
    "Home resources category", "Highest parental education", "Home language",
    "School location", "Immediate-area income", "Economically disadvantaged pupils",
    "Economically affluent pupils"
  ),
  variable = tolower(c(
    "RREA", "ASDGSB", "ASBGSB", "ASBG01", "ASDAGE", "ASBGHRL",
    "ASDGHRL", "ASDHEDUP", "ASBG03", "ACBG05B", "ACBG05C", "ACBG03A", "ACBG03B"
  )),
  level = c(
    "Student", "Student", "Student", "Student", "Student", "Student",
    "Student", "Student/home", "Student", "School", "School", "School", "School"
  ),
  role = c(
    "Outcome", "Main exposure", "Robustness exposure", "Core confounder",
    "Core confounder", "Core socioeconomic confounder", "Descriptive/robustness SES",
    "Candidate socioeconomic confounder", "Candidate confounder",
    "Candidate school confounder", "Candidate school confounder",
    "Candidate school confounder", "Candidate school confounder"
  ),
  provisional_decision = c(
    "Retain", "Retain", "Retain", "Retain", "Retain if comparable",
    "Retain", "Retain for descriptives", "Decide after audit", "Decide after audit",
    "Decide after audit", "Decide after audit", "Decide after audit", "Decide after audit"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  candidate_registry,
  file.path(output_dir, "candidate_variable_registry.csv"),
  row.names = FALSE
)

find_name <- function(sdf, requested) {
  matched <- colnames(sdf)[tolower(colnames(sdf)) == tolower(requested)]
  if (length(matched) == 0L) NA_character_ else matched[[1L]]
}

student_n <- function(sdf) {
  weight_data <- getData(
    data = sdf,
    varnames = "totwgt",
    dropOmittedLevels = TRUE,
    returnJKreplicates = FALSE
  )
  nrow(weight_data)
}

collapse_values <- function(x, maximum = 30L) {
  if (is.factor(x)) {
    values <- levels(x)
  } else if (is.character(x)) {
    values <- sort(unique(x[!is.na(x)]))
  } else {
    nonmissing <- x[!is.na(x)]
    if (length(nonmissing) == 0L) return(NA_character_)
    return(paste0("Range: ", signif(min(nonmissing), 6), " to ", signif(max(nonmissing), 6)))
  }

  if (length(values) > maximum) {
    values <- c(values[seq_len(maximum)], paste0("... ", length(values) - maximum, " more"))
  }
  paste(values, collapse = " | ")
}

audit_one <- function(sdf, country, concept, requested_variable, level, role, decision) {
  actual_name <- find_name(sdf, requested_variable)

  base_result <- data.frame(
    country = country,
    concept = concept,
    requested_variable = toupper(requested_variable),
    actual_variable = actual_name,
    level = level,
    role = role,
    provisional_decision = decision,
    available = !is.na(actual_name),
    unweighted_n = NA_integer_,
    valid_n = NA_integer_,
    omitted_or_missing_n = NA_integer_,
    omitted_or_missing_pct = NA_real_,
    coding_or_range = NA_character_,
    audit_note = NA_character_,
    stringsAsFactors = FALSE
  )

  # RREA is an EdSurvey achievement scale rather than an ordinary single column.
  if (tolower(requested_variable) == "rrea") {
    pv_names <- grep("^asrrea[0-9]+$", colnames(sdf), ignore.case = TRUE, value = TRUE)
    base_result$available <- length(pv_names) == 5L
    base_result$actual_variable <- paste(pv_names, collapse = " | ")
    sample_n <- student_n(sdf)
    base_result$unweighted_n <- sample_n
    base_result$valid_n <- sample_n
    base_result$omitted_or_missing_n <- 0L
    base_result$omitted_or_missing_pct <- 0
    base_result$coding_or_range <- paste(length(pv_names), "reading plausible values")
    base_result$audit_note <- "Use RREA through EdSurvey; do not average plausible values manually."
    return(base_result)
  }

  if (is.na(actual_name)) {
    base_result$audit_note <- "Not found as an exact column name; inspect the codebook-search output."
    return(base_result)
  }

  raw <- tryCatch(
    getData(
      data = sdf,
      varnames = c(actual_name, "totwgt"),
      dropOmittedLevels = FALSE,
      includeNaLabel = FALSE,
      returnJKreplicates = FALSE
    ),
    error = function(error) error
  )

  clean <- tryCatch(
    getData(
      data = sdf,
      varnames = c(actual_name, "totwgt"),
      dropOmittedLevels = TRUE,
      includeNaLabel = FALSE,
      returnJKreplicates = FALSE
    ),
    error = function(error) error
  )

  if (inherits(raw, "error") || inherits(clean, "error")) {
    messages <- c(
      if (inherits(raw, "error")) conditionMessage(raw),
      if (inherits(clean, "error")) conditionMessage(clean)
    )
    base_result$audit_note <- paste(unique(messages), collapse = " | ")
    return(base_result)
  }

  raw_variable <- names(raw)[tolower(names(raw)) == tolower(actual_name)][1L]
  clean_variable <- names(clean)[tolower(names(clean)) == tolower(actual_name)][1L]

  if (is.na(raw_variable) || is.na(clean_variable)) {
    base_result$audit_note <- "getData returned data, but the requested column could not be matched."
    return(base_result)
  }

  total_n <- nrow(raw)
  valid_n <- sum(!is.na(clean[[clean_variable]]))

  base_result$unweighted_n <- total_n
  base_result$valid_n <- valid_n
  base_result$omitted_or_missing_n <- total_n - valid_n
  base_result$omitted_or_missing_pct <- round(100 * (total_n - valid_n) / total_n, 2)
  base_result$coding_or_range <- collapse_values(raw[[raw_variable]])
  base_result$audit_note <- "Missingness includes PIRLS omitted/not-administered responses after dropOmittedLevels = TRUE."
  base_result
}

audit_results <- list()
audit_index <- 1L

for (country in names(country_data)) {
  for (row_index in seq_len(nrow(candidate_registry))) {
    row <- candidate_registry[row_index, ]
    audit_results[[audit_index]] <- audit_one(
      sdf = country_data[[country]],
      country = country,
      concept = row$concept,
      requested_variable = row$variable,
      level = row$level,
      role = row$role,
      decision = row$provisional_decision
    )
    audit_index <- audit_index + 1L
  }
}

audit_results <- bind_rows(audit_results)

write.csv(
  audit_results,
  file.path(output_dir, "candidate_variable_audit.csv"),
  row.names = FALSE,
  na = ""
)
saveRDS(audit_results, file.path(output_dir, "candidate_variable_audit.rds"))

cat("\n==============================================================================\n")
cat("CANDIDATE-VARIABLE AUDIT\n")
cat("==============================================================================\n")
print(audit_results, row.names = FALSE)

core_variables <- c("rrea", "asdgsb", "asbgsb", "asbg01", "asbghrl", "asdghrl")
core_check <- audit_results |>
  filter(tolower(requested_variable) %in% core_variables) |>
  group_by(requested_variable) |>
  summarise(available_in_both = all(available), .groups = "drop")

if (any(!core_check$available_in_both)) {
  warning(
    "At least one core variable was not recognised in both countries. ",
    "Review candidate_variable_audit.csv before running Step 5."
  )
}

message(
  "Variable audit completed. Outputs were written to ",
  normalizePath(output_dir, winslash = "/", mustWork = FALSE),
  "."
)

