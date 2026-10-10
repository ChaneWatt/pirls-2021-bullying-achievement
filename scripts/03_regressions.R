# =============================================================================
# PIRLS 2021: Bullying and reading-achievement regressions
# =============================================================================
#
# This script is deliberately written in a Stata-friendly, step-by-step style.
# It uses EdSurvey::lm.sdf(), so PIRLS's five reading plausible values, student
# weight, and jackknife replicate weights are handled by EdSurvey.
#
# IMPORTANT FIRST RUN:
#   Leave RUN_REGRESSIONS as FALSE and source this file. It will print and check
#   the variable labels and reference categories, then stop before estimation.
#
# FULL RUN:
#   After checking the printed labels, change RUN_REGRESSIONS to TRUE and source
#   this file again. The complete set of models can take a long time because
#   JRR_I_MAX = Inf uses all five plausible values for jackknife sampling
#   variance estimation as well as for the coefficient estimates.
# =============================================================================

RUN_REGRESSIONS <- TRUE 
JRR_I_MAX <- Inf

source("scripts/00_packages.R")
source("scripts/pirls_load_helpers.R")

library(EdSurvey)
library(dplyr)

output_dir <- here::here("output", "regressions")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. Read the finalized audit dictionary. This preserves the variable choices
#    already made in Steps 3-5; this script does not select a new control set.
# -----------------------------------------------------------------------------

dictionary_path <- here::here("output", "readable_tables", "variable_dictionary.csv")
missingness_path <- here::here("output", "readable_tables", "candidate_variable_missingness.csv")

if (!file.exists(dictionary_path) || !file.exists(missingness_path)) {
  stop(
    "The readable audit outputs are missing. Run ",
    "source('scripts/05_create_readable_tables.R') before this script."
  )
}

variable_dictionary <- read.csv(dictionary_path, stringsAsFactors = FALSE)
audited_missingness <- read.csv(missingness_path, stringsAsFactors = FALSE)

required_dictionary_codes <- c(
  "RREA", "ASDGSB", "ASBGSB", "ASBG01", "ASDAGE", "ASBHSES",
  "ASBG03", "ACBG05B", "ACBG03A"
)

missing_dictionary_codes <- setdiff(
  required_dictionary_codes,
  toupper(variable_dictionary$pirls_variable)
)

if (length(missing_dictionary_codes) > 0L) {
  stop(
    "The finalized variable dictionary is missing: ",
    paste(missing_dictionary_codes, collapse = ", "),
    ". Update the audit rather than substituting variables silently."
  )
}

# Audited model variables. Do not replace these without revisiting the audit.
bullying_category <- "asdgsb"
bullying_scale <- "asbgsb"
gender <- "asbg01"
age <- "asdage"
home_ses <- "asbhses"
home_language <- "asbg03"
school_location <- "acbg05b"
school_disadvantage <- "acbg03a"

# The audit found no defensible grade-repetition variable. None is constructed.

# -----------------------------------------------------------------------------
# 2. Load the original PIRLS objects.
# -----------------------------------------------------------------------------

message("Loading South African PIRLS data...")
zaf <- load_pirls_country("zaf")
message("Loading Brazilian PIRLS data...")
bra <- load_pirls_country("bra")

country_data <- list(
  "South Africa" = zaf,
  "Brazil" = bra
)

# -----------------------------------------------------------------------------
# 3. Preflight: print and validate variable names, labels, and references.
#    Any unexpected result stops the script before a regression is estimated.
# -----------------------------------------------------------------------------

required_columns <- c(
  bullying_category, bullying_scale, gender, age, home_ses,
  home_language, school_location, school_disadvantage
)

expected_bullying_levels <- c("ALMOST NEVER", "ABOUT MONTHLY", "ABOUT WEEKLY")
expected_gender_levels <- list(
  "South Africa" = c("GIRL", "BOY"),
  "Brazil" = c("GIRL", "BOY", "<OTHER>")
)

factor_levels_from_sdf <- function(sdf, variable) {
  extracted <- getData(
    data = sdf,
    varnames = variable,
    dropOmittedLevels = TRUE,
    includeNaLabel = FALSE,
    returnJKreplicates = FALSE
  )
  actual <- names(extracted)[tolower(names(extracted)) == tolower(variable)][1L]
  if (is.na(actual)) stop("getData() did not return expected variable ", variable, ".")

  values <- extracted[[actual]]
  if (is.factor(values)) {
    return(levels(droplevels(values)))
  }
  sort(unique(as.character(values[!is.na(values)])))
}

preflight_rows <- list()
preflight_index <- 1L
preflight_errors <- character(0)

cat("\n==============================================================================\n")
cat("REGRESSION PREFLIGHT: LABELS AND REFERENCE CATEGORIES\n")
cat("==============================================================================\n")

for (country in names(country_data)) {
  sdf <- country_data[[country]]
  available <- tolower(colnames(sdf))
  absent <- setdiff(required_columns, available)

  if (length(absent) > 0L) {
    preflight_errors <- c(
      preflight_errors,
      paste0(country, " is missing required variables: ", paste(absent, collapse = ", "))
    )
    next
  }

  # Confirm that RREA expands to five plausible values.
  pv_check <- tryCatch(
    getData(
      data = sdf,
      varnames = "rrea",
      dropOmittedLevels = TRUE,
      returnJKreplicates = FALSE
    ),
    error = function(error) error
  )

  if (inherits(pv_check, "error")) {
    preflight_errors <- c(
      preflight_errors,
      paste0(country, " could not load RREA: ", conditionMessage(pv_check))
    )
  } else {
    pv_names <- grep("^asrrea[0-9]+$", names(pv_check), ignore.case = TRUE, value = TRUE)
    if (length(pv_names) != 5L) {
      preflight_errors <- c(
        preflight_errors,
        paste0(country, " returned ", length(pv_names), " RREA plausible values instead of five.")
      )
    }
    cat("\n", country, ": RREA plausible values = ", paste(pv_names, collapse = ", "), "\n", sep = "")
  }

  variables_to_print <- c(
    bullying_category, gender, home_language, school_location,
    school_disadvantage
  )

  for (variable in variables_to_print) {
    found_levels <- factor_levels_from_sdf(sdf, variable)
    cat(country, " | ", toupper(variable), " | ", paste(found_levels, collapse = " ; "), "\n", sep = "")

    for (level in found_levels) {
      preflight_rows[[preflight_index]] <- data.frame(
        country = country,
        variable = toupper(variable),
        level = level,
        stringsAsFactors = FALSE
      )
      preflight_index <- preflight_index + 1L
    }
  }

  bullying_levels <- factor_levels_from_sdf(sdf, bullying_category)
  if (!setequal(bullying_levels, expected_bullying_levels)) {
    preflight_errors <- c(
      preflight_errors,
      paste0(
        country, " has unexpected ASDGSB levels. Expected: ",
        paste(expected_bullying_levels, collapse = ", "),
        "; found: ", paste(bullying_levels, collapse = ", ")
      )
    )
  }

  gender_levels <- factor_levels_from_sdf(sdf, gender)
  if (!setequal(gender_levels, expected_gender_levels[[country]])) {
    preflight_errors <- c(
      preflight_errors,
      paste0(
        country, " has unexpected ASBG01 levels. Expected: ",
        paste(expected_gender_levels[[country]], collapse = ", "),
        "; found: ", paste(gender_levels, collapse = ", ")
      )
    )
  }
}

preflight_table <- bind_rows(preflight_rows)
write.csv(
  preflight_table,
  file.path(output_dir, "preflight_categories.csv"),
  row.names = FALSE,
  na = ""
)

cat("\nReference category for bullying: ALMOST NEVER\n")
cat("Reference category for gender where gender enters a model: GIRL\n")
cat("ASBHSES is continuous and will be measured in PIRLS home-SES scale points.\n")
cat("ASBGSB is continuous; higher values mean LESS bullying.\n")

if (length(preflight_errors) > 0L) {
  cat("\nPREFLIGHT FAILED:\n")
  cat(paste0("- ", preflight_errors, collapse = "\n"), "\n")
  stop("Regression preflight failed. No models were estimated.")
}

cat("\nPreflight passed: all required variables and reference categories were found.\n")

if (!RUN_REGRESSIONS) {
  cat("\nNo regressions were run because RUN_REGRESSIONS is FALSE.\n")
  cat("After checking the labels above, change RUN_REGRESSIONS to TRUE and source this file again.\n")
}

# =============================================================================
# Everything below this point runs only after explicit activation at the top.
# =============================================================================

if (RUN_REGRESSIONS) {

  # ---------------------------------------------------------------------------
  # 4. Define the requested model sequence.
  # ---------------------------------------------------------------------------

  main_formulas <- list(
    M1 = rrea ~ asdgsb,
    M2 = rrea ~ asdgsb + asbg01 + asdage,
    M3 = rrea ~ asdgsb + asbg01 + asdage + asbhses,
    M4 = rrea ~ asdgsb + asbg01 + asdage + asbhses + asbg03,
    M5 = rrea ~ asdgsb + asbg01 + asdage + asbhses + asbg03 + acbg05b + acbg03a
  )

  m5_covariates <- c(
    "asdgsb", "asbg01", "asdage", "asbhses", "asbg03", "acbg05b", "acbg03a"
  )

  reference_list <- function(formula) {
    formula_variables <- all.vars(formula)
    references <- list()
    if ("asdgsb" %in% formula_variables) references$asdgsb <- "ALMOST NEVER"
    if ("asbg01" %in% formula_variables) references$asbg01 <- "GIRL"
    references
  }

  run_one_model <- function(formula, sdf, country, model_name, sample_name,
                            default_conditions = TRUE) {
    message("Estimating ", country, " ", model_name, " [", sample_name, "]...")

    tryCatch(
      lm.sdf(
        formula = formula,
        data = sdf,
        weightVar = NULL,
        relevels = reference_list(formula),
        varMethod = "jackknife",
        jrrIMax = JRR_I_MAX,
        dropOmittedLevels = TRUE,
        defaultConditions = default_conditions,
        verbose = TRUE
      ),
      error = function(error) {
        stop(
          "Model ", model_name, " failed for ", country,
          " on sample '", sample_name, "': ", conditionMessage(error),
          ". No substitute variable or silent workaround was used."
        )
      }
    )
  }

  # The M5 common sample is a light.edsurvey.data.frame. It retains the PIRLS
  # weights, replicate weights, and plausible-value attributes required by
  # lm.sdf(), while fixing the same complete-case sample for M1-M4.
  make_m5_common_sample <- function(sdf) {
    extracted <- getData(
      data = sdf,
      varnames = c("rrea", m5_covariates),
      dropOmittedLevels = TRUE,
      includeNaLabel = FALSE,
      addAttributes = TRUE,
      returnJKreplicates = TRUE
    )

    keep <- complete.cases(extracted[, m5_covariates, drop = FALSE])
    common <- extracted[keep, , drop = FALSE]

    if (!inherits(common, "light.edsurvey.data.frame")) {
      class(common) <- unique(c("light.edsurvey.data.frame", class(common)))
    }
    common
  }

  weighted_ses_mean <- function(sdf) {
    data <- getData(
      data = sdf,
      varnames = c("asbhses", "totwgt"),
      dropOmittedLevels = TRUE,
      includeNaLabel = FALSE,
      returnJKreplicates = FALSE
    )
    valid <- complete.cases(data[, c("asbhses", "totwgt")]) & data$totwgt > 0
    weighted.mean(data$asbhses[valid], data$totwgt[valid])
  }

  models <- list()
  model_metadata <- list()
  model_index <- 1L
  ses_centres <- data.frame(
    country = character(0),
    asbhses_weighted_mean = numeric(0),
    units = character(0),
    stringsAsFactors = FALSE
  )

  add_model <- function(model, country, model_name, sample_name) {
    key <- paste(country, model_name, sample_name, sep = "__")
    models[[key]] <<- model
    model_metadata[[key]] <<- data.frame(
      key = key,
      country = country,
      model = model_name,
      sample = sample_name,
      stringsAsFactors = FALSE
    )
  }

  for (country in names(country_data)) {
    sdf <- country_data[[country]]

    # Main sequence: each model uses all cases available for its own variables.
    for (model_name in names(main_formulas)) {
      model <- run_one_model(
        formula = main_formulas[[model_name]],
        sdf = sdf,
        country = country,
        model_name = model_name,
        sample_name = "available_case"
      )
      add_model(model, country, model_name, "available_case")
    }

    # Common-sample sensitivity: M1-M4 all use the exact M5 estimation sample.
    common_sdf <- make_m5_common_sample(sdf)
    for (model_name in names(main_formulas)[1:4]) {
      common_name <- paste0(model_name, "_common")
      model <- run_one_model(
        formula = main_formulas[[model_name]],
        sdf = common_sdf,
        country = country,
        model_name = common_name,
        sample_name = "m5_common",
        default_conditions = FALSE
      )
      add_model(model, country, common_name, "m5_common")
    }

    # I1: M3 plus bullying x gender.
    # Brazil's <OTHER> gender category contains only 41 learners and is excluded
    # from this interaction model because its bullying subgroups are too small.
    i1_sdf <- if (country == "Brazil") {
      subset(x = sdf, subset = asbg01 != "<OTHER>")
    } else {
      sdf
    }

    i1 <- run_one_model(
      formula = rrea ~ asdgsb * asbg01 + asdage + asbhses,
      sdf = i1_sdf,
      country = country,
      model_name = "I1",
      sample_name = if (country == "Brazil") "excludes_other_gender" else "available_case"
    )
    add_model(
      i1, country, "I1",
      if (country == "Brazil") "excludes_other_gender" else "available_case"
    )

    # I2: M3 plus bullying x continuous SES. ASBHSES is centred at the
    # design-weighted country mean. One unit is one PIRLS ASBHSES scale point.
    ses_centre <- weighted_ses_mean(sdf)
    ses_centres <- bind_rows(
      ses_centres,
      data.frame(
        country = country,
        asbhses_weighted_mean = ses_centre,
        units = "PIRLS ASBHSES scale points",
        stringsAsFactors = FALSE
      )
    )

    i2_formula <- as.formula(
      paste0(
        "rrea ~ asdgsb * I(asbhses - ",
        format(ses_centre, digits = 15, scientific = FALSE, trim = TRUE),
        ") + asbg01 + asdage"
      )
    )

    i2 <- run_one_model(
      formula = i2_formula,
      sdf = sdf,
      country = country,
      model_name = "I2",
      sample_name = "available_case"
    )
    add_model(i2, country, "I2", "available_case")

    # R1: continuous bullying robustness model based on M3.
    # IMPORTANT: higher ASBGSB means LESS bullying. The variable is not flipped;
    # its exported label states the direction explicitly.
    r1 <- run_one_model(
      formula = rrea ~ asbgsb + asbg01 + asdage + asbhses,
      sdf = sdf,
      country = country,
      model_name = "R1",
      sample_name = "available_case"
    )
    add_model(r1, country, "R1", "available_case")
  }

  # ---------------------------------------------------------------------------
  # 5. Convert EdSurvey model objects to readable CSV rows.
  # ---------------------------------------------------------------------------

  label_component <- function(term) {
    if (term == "(Intercept)") return("Constant")
    if (term == "asdage") return("Age (years)")
    if (term == "asbhses") return("Home SES (ASBHSES scale points)")
    if (term == "asbgsb") return("Bullying scale (higher = less bullying)")
    if (grepl("^I\\(asbhses - ", term)) return("Home SES (centred scale points)")

    if (grepl("^asdgsb", term)) {
      level <- sub("^asdgsb", "", term)
      labels <- c(
        "ABOUT MONTHLY" = "Bullied about monthly",
        "ABOUT WEEKLY" = "Bullied about weekly"
      )
      if (level %in% names(labels)) return(unname(labels[level]))
    }

    if (grepl("^asbg01", term)) {
      level <- sub("^asbg01", "", term)
      if (level == "BOY") return("Boy")
      if (level == "<OTHER>") return("Other gender category")
    }

    if (grepl("^asbg03", term)) {
      level <- sub("^asbg03", "", term)
      home_language_labels <- c(
        "I ALMOST ALWAYS SPEAK <LANGUAGE OF TEST> AT HOME" = "Home language: almost always test language",
        "I SOMETIMES SPEAK <LANGUAGE OF TEST> AND SOMETIMES SPEAK ANOTHER LANGUAGE AT HOME" = "Home language: sometimes test language",
        "I NEVER SPEAK <LANGUAGE OF TEST> AT HOME" = "Home language: never test language"
      )
      if (level %in% names(home_language_labels)) return(unname(home_language_labels[level]))
      return(paste("Home language:", tolower(level)))
    }

    if (grepl("^acbg05b", term)) {
      level <- sub("^acbg05b", "", term)
      location_labels <- c(
        "SUBURBAN-ON FRINGE OR OUTSKIRTS OF URBAN AREA" = "School location: suburban",
        "MEDIUM SIZE CITY OR LARGE TOWN" = "School location: medium city/large town",
        "SMALL TOWN OR VILLAGE" = "School location: small town/village",
        "REMOTE RURAL" = "School location: remote rural"
      )
      if (level %in% names(location_labels)) return(unname(location_labels[level]))
      return(paste("School location:", tolower(level)))
    }

    if (grepl("^acbg03a", term)) {
      level <- sub("^acbg03a", "", term)
      return(paste("Economically disadvantaged:", tolower(level)))
    }

    term
  }

  readable_term <- function(term) {
    components <- strsplit(term, ":", fixed = TRUE)[[1L]]
    paste(vapply(components, label_component, character(1)), collapse = " × ")
  }

  extract_coefficients <- function(model, metadata) {
    coefficient_table <- as.data.frame(model$coefmat, stringsAsFactors = FALSE)
    data.frame(
      country = metadata$country,
      model = metadata$model,
      sample = metadata$sample,
      formula = paste(deparse(model$formula), collapse = " "),
      term_raw = rownames(coefficient_table),
      term_label = vapply(rownames(coefficient_table), readable_term, character(1)),
      estimate = coefficient_table$coef,
      standard_error = coefficient_table$se,
      t_statistic = coefficient_table$t,
      degrees_of_freedom = coefficient_table$dof,
      p_value = coefficient_table[["Pr(>|t|)"]],
      confidence_interval_lower = coefficient_table$coef - 1.96 * coefficient_table$se,
      confidence_interval_upper = coefficient_table$coef + 1.96 * coefficient_table$se,
      n_used = model$nUsed,
      r_squared = model$r.squared,
      plausible_values = model$npv,
      jrr_i_max = paste(model$jrrIMax, collapse = ","),
      weight = model$weight,
      variance_method = model$varMethod,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  coefficient_results <- bind_rows(lapply(names(models), function(key) {
    extract_coefficients(models[[key]], model_metadata[[key]])
  }))

  model_summary <- coefficient_results |>
    group_by(country, model, sample, formula) |>
    summarise(
      n_used = first(n_used),
      r_squared = first(r_squared),
      plausible_values = first(plausible_values),
      jrr_i_max = first(jrr_i_max),
      weight = first(weight),
      variance_method = first(variance_method),
      .groups = "drop"
    )

  main_results <- coefficient_results |>
    filter(model %in% paste0("M", 1:5), sample == "available_case")

  common_results <- coefficient_results |>
    filter(grepl("_common$", model))

  interaction_results <- coefficient_results |>
    filter(model %in% c("I1", "I2"))

  robustness_results <- coefficient_results |>
    filter(model == "R1")

  write.csv(coefficient_results, file.path(output_dir, "all_model_coefficients.csv"), row.names = FALSE, na = "")
  write.csv(model_summary, file.path(output_dir, "model_summary.csv"), row.names = FALSE, na = "")
  write.csv(main_results, file.path(output_dir, "main_sequence_results.csv"), row.names = FALSE, na = "")
  write.csv(common_results, file.path(output_dir, "common_sample_sensitivity.csv"), row.names = FALSE, na = "")
  write.csv(interaction_results, file.path(output_dir, "interaction_results.csv"), row.names = FALSE, na = "")
  write.csv(robustness_results, file.path(output_dir, "robustness_results.csv"), row.names = FALSE, na = "")
  write.csv(ses_centres, file.path(output_dir, "ses_centering_values.csv"), row.names = FALSE, na = "")

  # ---------------------------------------------------------------------------
  # 6. Create compact booktabs LaTeX tables for direct \input{} use.
  #    The paper needs \usepackage{booktabs} and \usepackage{graphicx}.
  # ---------------------------------------------------------------------------

  significance_stars <- function(p_value) {
    if (is.na(p_value)) return("")
    if (p_value < 0.01) return("***")
    if (p_value < 0.05) return("**")
    if (p_value < 0.10) return("*")
    ""
  }

  latex_escape <- function(text) {
    replacements <- c(
      "\\" = "\\textbackslash{}",
      "&" = "\\&",
      "%" = "\\%",
      "$" = "\\$",
      "#" = "\\#",
      "_" = "\\_",
      "{" = "\\{",
      "}" = "\\}",
      "<" = "\\textless{}",
      ">" = "\\textgreater{}"
    )
    output <- text
    for (pattern in names(replacements)) {
      output <- gsub(pattern, replacements[[pattern]], output, fixed = TRUE)
    }
    output
  }

  formatted_cell <- function(data, country, model, term_label) {
    row <- data[
      data$country == country & data$model == model & data$term_label == term_label,
      , drop = FALSE
    ]
    if (nrow(row) == 0L) return("")
    if (nrow(row) > 1L) stop("Duplicate coefficient row for ", country, ", ", model, ", ", term_label, ".")

    paste0(
      "\\shortstack{",
      sprintf("%.1f", row$estimate), significance_stars(row$p_value),
      "\\\\(", sprintf("%.1f", row$standard_error), ")}"
    )
  }

  make_latex_table <- function(data, model_names, caption, label, note, output_file) {
    countries <- c("South Africa", "Brazil")
    coefficient_terms <- unique(data$term_label[data$term_raw != "(Intercept)"])

    header_models <- paste(rep(model_names, times = length(countries)), collapse = " & ")
    lines <- c(
      "\\begin{table}[!htbp]",
      "\\centering",
      paste0("\\caption{", latex_escape(caption), "}"),
      paste0("\\label{", label, "}"),
      "\\scriptsize",
      "\\setlength{\\tabcolsep}{2.2pt}",
      "\\resizebox{\\textwidth}{!}{%",
      paste0("\\begin{tabular}{l", paste(rep("c", length(model_names) * 2L), collapse = ""), "}"),
      "\\toprule",
      paste0(
        " & \\multicolumn{", length(model_names), "}{c}{South Africa}",
        " & \\multicolumn{", length(model_names), "}{c}{Brazil} \\\\"
      ),
      paste0(
        "\\cmidrule(lr){2-", length(model_names) + 1L, "}",
        "\\cmidrule(lr){", length(model_names) + 2L, "-", length(model_names) * 2L + 1L, "}"
      ),
      paste0(" & ", header_models, " \\\\"),
      "\\midrule"
    )

    for (term in coefficient_terms) {
      cells <- c()
      for (country in countries) {
        for (model_name in model_names) {
          cells <- c(cells, formatted_cell(data, country, model_name, term))
        }
      }
      lines <- c(lines, paste0(latex_escape(term), " & ", paste(cells, collapse = " & "), " \\\\"))
    }

    lines <- c(lines, "\\midrule")

    n_cells <- c()
    r2_cells <- c()
    for (country in countries) {
      for (model_name in model_names) {
        model_rows <- data[data$country == country & data$model == model_name, , drop = FALSE]
        n_cells <- c(n_cells, if (nrow(model_rows) == 0L) "" else format(model_rows$n_used[1L], big.mark = ",", scientific = FALSE))
        r2_cells <- c(r2_cells, if (nrow(model_rows) == 0L) "" else sprintf("%.3f", model_rows$r_squared[1L]))
      }
    }

    lines <- c(
      lines,
      paste0("Learners & ", paste(n_cells, collapse = " & "), " \\\\"),
      paste0("$R^{2}$ & ", paste(r2_cells, collapse = " & "), " \\\\"),
      "\\bottomrule",
      "\\end{tabular}%",
      "}",
      paste0("\\parbox{0.98\\textwidth}{\\footnotesize \\textit{Notes:} ", latex_escape(note), "}"),
      "\\end{table}"
    )

    writeLines(lines, output_file, useBytes = TRUE)
  }

  make_latex_table(
    data = main_results,
    model_names = paste0("M", 1:5),
    caption = "Bullying and reading achievement: main model sequence",
    label = "tab:bullying-main-models",
    note = paste(
      "Entries are coefficients with jackknife standard errors in parentheses.",
      "The reference group is almost never bullied; the gender reference is girls.",
      "All models use the default PIRLS student weight and all five reading plausible values.",
      "M1 includes bullying; M2 adds gender and age; M3 adds continuous home SES;",
      "M4 adds home language; M5 adds school location and economically disadvantaged composition.",
      "Significance: *** p<0.01, ** p<0.05, * p<0.10."
    ),
    output_file = file.path(output_dir, "main_models.tex")
  )

  make_latex_table(
    data = interaction_results,
    model_names = c("I1", "I2"),
    caption = "Heterogeneity in the association between bullying and reading achievement",
    label = "tab:bullying-interactions",
    note = paste(
      "Entries are coefficients with jackknife standard errors in parentheses.",
      "I1 interacts bullying with gender and excludes Brazil's 41 learners in the Other gender category.",
      "I2 interacts bullying with continuous ASBHSES centred at the design-weighted country mean;",
      "one unit is one PIRLS ASBHSES scale point.",
      "The reference bullying category is almost never and the gender reference is girls.",
      "Significance: *** p<0.01, ** p<0.05, * p<0.10."
    ),
    output_file = file.path(output_dir, "interaction_models.tex")
  )

  cat("\n==============================================================================\n")
  cat("REGRESSION ANALYSIS COMPLETED\n")
  cat("==============================================================================\n")
  print(model_summary, row.names = FALSE)
  cat("\nASBHSES centring values (one unit = one PIRLS scale point):\n")
  print(ses_centres, row.names = FALSE)

  message(
    "Regression outputs were written to ",
    normalizePath(output_dir, winslash = "/", mustWork = FALSE),
    "."
  )
}

