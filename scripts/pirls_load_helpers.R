# Shared PIRLS 2021 data loader.
#
# readPIRLS() needs the directory containing the extracted SPSS files plus a
# three-letter country code. This helper locates that directory inside the
# project's data folder, so analysis scripts do not depend on a machine-specific
# absolute path.

load_pirls_country <- function(country_code, data_root = here::here("data")) {
  country_code <- tolower(country_code)

  if (!dir.exists(data_root)) {
    stop("The project data folder does not exist: ", data_root)
  }

  sav_files <- list.files(
    data_root,
    pattern = "\\.sav$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(sav_files) == 0L) {
    stop(
      "No extracted PIRLS SPSS (.sav) files were found under ", data_root,
      ". Run the existing data-audit/import script first."
    )
  }

  candidate_directories <- unique(dirname(sav_files))

  # Try directories containing the requested country first. PIRLS filenames
  # normally include the three-letter country code.
  country_file <- grepl(country_code, basename(sav_files), ignore.case = TRUE)
  preferred_directories <- unique(dirname(sav_files[country_file]))
  candidate_directories <- unique(c(preferred_directories, candidate_directories))

  errors <- character(0)

  for (candidate in candidate_directories) {
    result <- tryCatch(
      EdSurvey::readPIRLS(
        path = candidate,
        countries = country_code,
        forceReread = FALSE,
        verbose = FALSE
      ),
      error = function(error) error
    )

    if (inherits(result, "edsurvey.data.frame")) {
      message(
        "Loaded PIRLS 2021 for '", country_code, "' from ",
        normalizePath(candidate, winslash = "/", mustWork = FALSE), "."
      )
      return(result)
    }

    if (inherits(result, "error")) {
      errors <- c(errors, paste(candidate, conditionMessage(result), sep = ": "))
    }
  }

  stop(
    "PIRLS files were found, but readPIRLS() could not load country '",
    country_code, "'. Directories tried: ",
    paste(candidate_directories, collapse = "; "),
    if (length(errors) > 0L) paste0(". Last error: ", tail(errors, 1L)) else ""
  )
}

