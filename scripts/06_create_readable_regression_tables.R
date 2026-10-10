# =============================================================================
# Create human-readable and essay-ready regression tables
# =============================================================================

input_dir <- file.path("output", "regressions")
output_dir <- file.path(input_dir, "readable")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

read_result <- function(name) {
  path <- file.path(input_dir, name)
  if (!file.exists(path)) stop("Missing regression output: ", path)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

main <- read_result("main_sequence_results.csv")
common <- read_result("common_sample_sensitivity.csv")
interactions <- read_result("interaction_results.csv")
robustness <- read_result("robustness_results.csv")
summary_data <- read_result("model_summary.csv")

stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

md_cell <- function(row) {
  if (nrow(row) == 0L) return("")
  paste0(sprintf("%.1f", row$estimate[1L]), stars(row$p_value[1L]),
         "<br>(", sprintf("%.1f", row$standard_error[1L]), ")")
}

md_table_for_country <- function(data, country, models, include_intercept = FALSE) {
  country_data <- data[data$country == country & data$model %in% models, , drop = FALSE]
  terms <- unique(country_data$term_label)
  if (!include_intercept) terms <- setdiff(terms, "Constant")
  display_models <- sub("_common$", "", models)

  lines <- c(
    paste0("### ", country),
    "",
    paste0("| Variable | ", paste(display_models, collapse = " | "), " |"),
    paste0("|---|", paste(rep("---:", length(models)), collapse = "|"), "|")
  )

  for (term in terms) {
    cells <- vapply(models, function(model) {
      md_cell(country_data[country_data$model == model & country_data$term_label == term, , drop = FALSE])
    }, character(1))
    lines <- c(lines, paste0("| ", term, " | ", paste(cells, collapse = " | "), " |"))
  }

  n_cells <- vapply(models, function(model) {
    rows <- country_data[country_data$model == model, , drop = FALSE]
    if (nrow(rows) == 0L) "" else format(rows$n_used[1L], big.mark = ",", scientific = FALSE)
  }, character(1))
  r2_cells <- vapply(models, function(model) {
    rows <- country_data[country_data$model == model, , drop = FALSE]
    if (nrow(rows) == 0L) "" else sprintf("%.3f", rows$r_squared[1L])
  }, character(1))

  c(
    lines,
    paste0("| **Learners** | ", paste(n_cells, collapse = " | "), " |"),
    paste0("| **R-squared** | ", paste(r2_cells, collapse = " | "), " |"),
    ""
  )
}

write_md_results <- function(data, models, title, description, filename) {
  lines <- c(
    paste0("# ", title),
    "",
    description,
    "",
    "Cells report coefficients with jackknife standard errors in parentheses. ",
    "Significance: *** p < 0.01, ** p < 0.05, * p < 0.10.",
    "",
    md_table_for_country(data, "South Africa", models),
    md_table_for_country(data, "Brazil", models)
  )
  writeLines(lines, file.path(output_dir, filename), useBytes = TRUE)
}

write_md_results(
  main,
  paste0("M", 1:5),
  "Main regression sequence",
  paste(
    "M1 includes bullying; M2 adds gender and age; M3 adds home SES;",
    "M4 adds home language; M5 adds school location and school disadvantage.",
    "Each model uses all cases available for its own variables."
  ),
  "main_models.md"
)

write_md_results(
  common,
  paste0("M", 1:4, "_common"),
  "Common-sample sensitivity analysis",
  "M1-M4 are re-estimated on the exact M5 complete-case sample.",
  "common_sample_models.md"
)

write_md_results(
  interactions,
  c("I1", "I2"),
  "Interaction models",
  paste(
    "I1 tests whether the bullying association differs by gender.",
    "I2 tests whether it differs with home SES."
  ),
  "interaction_models.md"
)

write_md_results(
  robustness,
  "R1",
  "Continuous-bullying robustness model",
  paste(
    "R1 replaces the categorical bullying measure with ASBGSB.",
    "Higher ASBGSB values indicate less bullying, so a positive coefficient",
    "means that less bullying is associated with higher reading achievement."
  ),
  "robustness_model.md"
)

# Compact overview containing only the focal bullying coefficients.
key_terms <- c("Bullied about monthly", "Bullied about weekly")
overview <- main[main$term_label %in% key_terms, , drop = FALSE]
overview_lines <- c(
  "# Regression results: quick overview",
  "",
  "This is the easiest starting point for reading the results. Coefficients are",
  "PIRLS reading-score differences relative to learners who are almost never bullied.",
  "Standard errors appear in parentheses.",
  "",
  md_table_for_country(overview, "South Africa", paste0("M", 1:5)),
  md_table_for_country(overview, "Brazil", paste0("M", 1:5)),
  "## Model definitions",
  "",
  "- **M1:** bullying only.",
  "- **M2:** adds gender and age.",
  "- **M3:** adds home SES.",
  "- **M4:** adds home language.",
  "- **M5:** adds school location and school disadvantage.",
  "",
  "## Files",
  "",
  "- [Full main models](main_models.md)",
  "- [Common-sample sensitivity models](common_sample_models.md)",
  "- [Interaction models](interaction_models.md)",
  "- [Continuous-bullying robustness model](robustness_model.md)",
  "- [Model and sample summary](model_summary.md)",
  "- [Essay import instructions](essay_import.md)"
)
writeLines(overview_lines, file.path(output_dir, "results_overview.md"), useBytes = TRUE)

# Model/sample summary.
summary_data <- summary_data[order(summary_data$country, summary_data$model), , drop = FALSE]
summary_lines <- c(
  "# Model and sample summary",
  "",
  "| Country | Model | Sample | Learners | R-squared | Formula |",
  "|---|---|---|---:|---:|---|"
)
for (i in seq_len(nrow(summary_data))) {
  row <- summary_data[i, ]
  summary_lines <- c(summary_lines, paste0(
    "| ", row$country, " | ", row$model, " | ", row$sample, " | ",
    format(row$n_used, big.mark = ",", scientific = FALSE), " | ",
    sprintf("%.3f", row$r_squared), " | `", row$formula, "` |"
  ))
}
writeLines(summary_lines, file.path(output_dir, "model_summary.md"), useBytes = TRUE)

# LaTeX helpers for the two analyses that did not previously have .tex tables.
latex_escape <- function(text) {
  fixed_replace <- function(value, old, new) {
    paste(strsplit(value, old, fixed = TRUE)[[1L]], collapse = new)
  }
  vapply(text, function(value) {
    value <- fixed_replace(value, "&", "\\&")
    value <- fixed_replace(value, "%", "\\%")
    value <- fixed_replace(value, "_", "\\_")
    value <- fixed_replace(value, "<", "\\textless{}")
    fixed_replace(value, ">", "\\textgreater{}")
  }, character(1), USE.NAMES = FALSE)
}

latex_cell <- function(row) {
  if (nrow(row) == 0L) return("")
  paste0("\\shortstack{", sprintf("%.1f", row$estimate[1L]), stars(row$p_value[1L]),
         "\\\\(", sprintf("%.1f", row$standard_error[1L]), ")}")
}

write_latex_table <- function(data, models, caption, label, note, filename) {
  countries <- c("South Africa", "Brazil")
  terms <- setdiff(unique(data$term_label), "Constant")
  display_models <- sub("_common$", "", models)
  ncols <- length(models) * length(countries)
  first_end <- length(models) + 1L
  second_start <- first_end + 1L
  second_end <- ncols + 1L

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\caption{", latex_escape(caption), "}"),
    paste0("\\label{", label, "}"),
    "\\scriptsize",
    "\\setlength{\\tabcolsep}{2.2pt}",
    "\\resizebox{\\textwidth}{!}{%",
    paste0("\\begin{tabular}{l", paste(rep("c", ncols), collapse = ""), "}"),
    "\\toprule",
    paste0(" & \\multicolumn{", length(models), "}{c}{South Africa} & \\multicolumn{",
           length(models), "}{c}{Brazil} \\\\"),
    paste0("\\cmidrule(lr){2-", first_end, "}\\cmidrule(lr){", second_start, "-", second_end, "}"),
    paste0(" & ", paste(rep(display_models, times = 2L), collapse = " & "), " \\\\"),
    "\\midrule"
  )

  for (term in terms) {
    cells <- character(0)
    for (country in countries) {
      for (model in models) {
        row <- data[data$country == country & data$model == model & data$term_label == term, , drop = FALSE]
        cells <- c(cells, latex_cell(row))
      }
    }
    lines <- c(lines, paste0(latex_escape(term), " & ", paste(cells, collapse = " & "), " \\\\"))
  }

  lines <- c(lines, "\\midrule")
  n_cells <- r2_cells <- character(0)
  for (country in countries) {
    for (model in models) {
      rows <- data[data$country == country & data$model == model, , drop = FALSE]
      n_cells <- c(n_cells, format(rows$n_used[1L], big.mark = ",", scientific = FALSE))
      r2_cells <- c(r2_cells, sprintf("%.3f", rows$r_squared[1L]))
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
  writeLines(lines, file.path(input_dir, filename), useBytes = TRUE)
}

write_latex_table(
  common,
  paste0("M", 1:4, "_common"),
  "Common-sample sensitivity analysis",
  "tab:bullying-common-sample",
  paste(
    "Entries are coefficients with jackknife standard errors in parentheses.",
    "All models use the M5 complete-case sample.",
    "Significance: *** p<0.01, ** p<0.05, * p<0.10."
  ),
  "common_sample_models.tex"
)

write_latex_table(
  robustness,
  "R1",
  "Continuous-bullying robustness model",
  "tab:bullying-robustness",
  paste(
    "Entries are coefficients with jackknife standard errors in parentheses.",
    "Higher ASBGSB values indicate less bullying.",
    "Significance: *** p<0.01, ** p<0.05, * p<0.10."
  ),
  "robustness_model.tex"
)

import_lines <- c(
  "# Importing the regression tables into the essay",
  "",
  "The essay uses LaTeX. Add any of these commands to",
  "`scripts/paper/sections/results.tex`:",
  "",
  "```tex",
  "\\input{../../output/regressions/main_models.tex}",
  "\\input{../../output/regressions/interaction_models.tex}",
  "\\input{../../output/regressions/common_sample_models.tex}",
  "\\input{../../output/regressions/robustness_model.tex}",
  "```",
  "",
  "The main paper already loads the required `booktabs` and `graphicx` packages.",
  "Usually the main and interaction tables belong in the paper; the common-sample",
  "and robustness tables can go in an appendix if space is limited."
)
writeLines(import_lines, file.path(output_dir, "essay_import.md"), useBytes = TRUE)

message("Readable regression tables written to ", normalizePath(output_dir, winslash = "/"))
