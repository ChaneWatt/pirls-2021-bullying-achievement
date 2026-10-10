# =============================================================================
# PIRLS 2021: Design-correct preliminary descriptives (Step 5)
# =============================================================================
# All estimates use EdSurvey so that PIRLS weights, jackknife replicate weights,
# and all five reading plausible values are handled correctly.
#
# This script does not run regressions.
# =============================================================================

source("scripts/00_packages.R")
source("scripts/pirls_load_helpers.R")

library(EdSurvey)
library(dplyr)
library(ggplot2)

output_dir <- here::here("output", "preliminary_descriptives")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

message("Loading South African PIRLS data...")
zaf <- load_pirls_country("zaf")
message("Loading Brazilian PIRLS data...")
bra <- load_pirls_country("bra")

country_data <- list(
  "South Africa" = zaf,
  "Brazil" = bra
)

find_column <- function(data, requested) {
  available_names <- if (inherits(data, "edsurvey.data.frame")) colnames(data) else names(data)
  matched <- available_names[tolower(available_names) == tolower(requested)]
  if (length(matched) == 0L) NA_character_ else matched[[1L]]
}

normalise_output_names <- function(data) {
  names(data) <- tolower(gsub("[^A-Za-z0-9]+", "_", names(data)))
  names(data) <- gsub("_+$", "", names(data))
  data
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

extract_table <- function(result, country, analysis) {
  # EdSurvey objects such as summary2 and edsurveyTable store their printable
  # table in $data, but their S3 class may prevent as.data.frame() dispatch.
  # Remove the outer S3 class before extracting that component.
  result_data <- NULL

  if (inherits(result, "summary2") || inherits(result, "edsurveyTable")) {
    result_data <- unclass(result)[["data"]]
  } else if (is.list(result) && "data" %in% names(result)) {
    result_data <- result[["data"]]
  } else if (is.data.frame(result) || is.matrix(result)) {
    result_data <- result
  }

  if (is.null(result_data)) {
    stop("The result does not contain an exportable data table.")
  }

  # Strip any inherited EdSurvey class from the inner table as well.
  if (!is.data.frame(result_data)) {
    result_data <- as.data.frame(unclass(result_data), stringsAsFactors = FALSE)
  } else {
    result_data <- as.data.frame(result_data, stringsAsFactors = FALSE)
  }
  result_data <- normalise_output_names(result_data)
  result_data$country <- country
  result_data$analysis <- analysis
  result_data[, c("country", "analysis", setdiff(names(result_data), c("country", "analysis")))]
}

save_analysis <- function(result, country, analysis, stem) {
  saveRDS(result, file.path(output_dir, paste0(stem, "_", gsub(" ", "_", tolower(country)), ".rds")))
  table_data <- tryCatch(
    extract_table(result, country, analysis),
    error = function(error) {
      text_path <- file.path(
        output_dir,
        paste0(stem, "_", gsub(" ", "_", tolower(country)), ".txt")
      )
      writeLines(capture.output(print(result)), text_path)
      warning(
        "Could not convert ", stem, " for ", country,
        " to CSV; its printed output was saved to ", text_path,
        ". Reason: ", conditionMessage(error)
      )
      data.frame()
    }
  )

  if (nrow(table_data) == 0L) {
    return(table_data)
  }

  write.csv(
    table_data,
    file.path(output_dir, paste0(stem, "_", gsub(" ", "_", tolower(country)), ".csv")),
    row.names = FALSE,
    na = ""
  )
  table_data
}

run_summary <- function(sdf, variable, country, stem) {
  result <- summary2(
    data = sdf,
    variable = variable,
    dropOmittedLevels = TRUE
  )
  save_analysis(result, country, paste("Weighted summary of", variable), stem)
}

run_table <- function(sdf, formula, country, analysis, stem, aggregation_level = 0L) {
  result <- edsurveyTable(
    formula = formula,
    data = sdf,
    jrrIMax = Inf,
    pctAggregationLevel = aggregation_level
  )
  save_analysis(result, country, analysis, stem)
}

all_summaries <- list()
all_headline <- list()
all_gender_gradient <- list()
all_ses_gradient <- list()

for (country in names(country_data)) {
  sdf <- country_data[[country]]
  message("Running weighted summaries for ", country, "...")

  # Weighted distributions and overall reading achievement.
  summary_variables <- c("asdgsb", "asbgsb", "rrea", "asbg01", "asdghrl", "asbghrl")
  if (!is.na(find_column(sdf, "asdage"))) {
    summary_variables <- c(summary_variables, "asdage")
  }

  for (variable in summary_variables) {
    message("  Summary: ", variable)
    key <- paste(country, variable, sep = "__")
    all_summaries[[key]] <- run_summary(
      sdf = sdf,
      variable = variable,
      country = country,
      stem = paste0("weighted_summary_", variable)
    )
  }

  # Percentages and achievement means are produced together. Since all five
  # reading PVs are present, PCT describes the weighted bullying distribution.
  message("  Reading achievement by bullying category (full jackknife calculation)...")
  all_headline[[country]] <- run_table(
    sdf = sdf,
    formula = rrea ~ asdgsb,
    country = country,
    analysis = "Reading achievement and bullying prevalence by bullying category",
    stem = "reading_by_bullying",
    aggregation_level = 0L
  )

  # Within-gender and within-SES percentages plus achievement gradients.
  message("  Bullying gradient by gender (full jackknife calculation)...")
  all_gender_gradient[[country]] <- run_table(
    sdf = sdf,
    formula = rrea ~ asbg01 + asdgsb,
    country = country,
    analysis = "Reading and bullying distribution within gender",
    stem = "reading_bullying_by_gender",
    aggregation_level = 1L
  )

  message("  Bullying gradient by home resources (full jackknife calculation)...")
  all_ses_gradient[[country]] <- run_table(
    sdf = sdf,
    formula = rrea ~ asdghrl + asdgsb,
    country = country,
    analysis = "Reading and bullying distribution within home-resources category",
    stem = "reading_bullying_by_home_resources",
    aggregation_level = 1L
  )
}

headline_table <- bind_rows(all_headline)
gender_gradient_table <- bind_rows(all_gender_gradient)
ses_gradient_table <- bind_rows(all_ses_gradient)

write.csv(headline_table, file.path(output_dir, "table_reading_by_bullying_both_countries.csv"), row.names = FALSE, na = "")
write.csv(gender_gradient_table, file.path(output_dir, "table_bullying_by_gender_both_countries.csv"), row.names = FALSE, na = "")
write.csv(ses_gradient_table, file.path(output_dir, "table_bullying_by_home_resources_both_countries.csv"), row.names = FALSE, na = "")

# Missingness and omitted-response audit. This is deliberately unweighted:
# it reports how many sampled records would be lost before any model is fitted.
candidate_missingness_variables <- c(
  "asdgsb", "asbgsb", "asbg01", "asdage", "asbhses", "asdhses",
  "asbghrl", "asdghrl",
  "asdhedup", "asbg03", "acbg05b", "acbg05c", "acbg03a", "acbg03b"
)

missingness_one <- function(sdf, country, requested) {
  actual <- find_column(sdf, requested)
  full_sample_n <- student_n(sdf)
  if (is.na(actual)) {
    return(data.frame(
      country = country, variable = toupper(requested), available = FALSE,
      total_n = full_sample_n, valid_n = NA_integer_, omitted_or_missing_n = NA_integer_,
      omitted_or_missing_pct = NA_real_, stringsAsFactors = FALSE
    ))
  }

  cleaned <- tryCatch(
    getData(
      data = sdf,
      varnames = actual,
      dropOmittedLevels = TRUE,
      includeNaLabel = FALSE,
      returnJKreplicates = FALSE
    ),
    error = function(error) error
  )

  if (inherits(cleaned, "error")) {
    return(data.frame(
      country = country, variable = toupper(requested), available = TRUE,
      total_n = full_sample_n, valid_n = NA_integer_, omitted_or_missing_n = NA_integer_,
      omitted_or_missing_pct = NA_real_, stringsAsFactors = FALSE
    ))
  }

  clean_name <- names(cleaned)[tolower(names(cleaned)) == tolower(actual)][1L]
  valid_n <- sum(!is.na(cleaned[[clean_name]]))

  data.frame(
    country = country,
    variable = toupper(requested),
    available = TRUE,
    total_n = full_sample_n,
    valid_n = valid_n,
    omitted_or_missing_n = full_sample_n - valid_n,
    omitted_or_missing_pct = round(100 * (full_sample_n - valid_n) / full_sample_n, 2),
    stringsAsFactors = FALSE
  )
}

missingness <- bind_rows(lapply(names(country_data), function(country) {
  bind_rows(lapply(candidate_missingness_variables, function(variable) {
    missingness_one(country_data[[country]], country, variable)
  }))
}))

write.csv(missingness, file.path(output_dir, "candidate_variable_missingness.csv"), row.names = FALSE, na = "")

# Flag categories that are too small to interpret casually. The PIRLS reporting
# convention uses 2.5% as an important small-cell warning; n < 30 is an added
# practical warning for this project.
small_category_one <- function(sdf, country, requested) {
  actual <- find_column(sdf, requested)
  if (is.na(actual)) return(NULL)

  extracted <- tryCatch(
    getData(
      data = sdf,
      varnames = c(actual, "totwgt"),
      dropOmittedLevels = TRUE,
      includeNaLabel = FALSE,
      returnJKreplicates = FALSE
    ),
    error = function(error) NULL
  )
  if (is.null(extracted)) return(NULL)

  variable_name <- names(extracted)[tolower(names(extracted)) == tolower(actual)][1L]
  weight_name <- names(extracted)[tolower(names(extracted)) == "totwgt"][1L]
  if (is.na(variable_name) || is.na(weight_name)) return(NULL)

  data <- data.frame(
    category = as.character(extracted[[variable_name]]),
    weight = extracted[[weight_name]],
    stringsAsFactors = FALSE
  ) |>
    filter(!is.na(category), !is.na(weight)) |>
    group_by(category) |>
    summarise(unweighted_n = n(), weighted_n = sum(weight), .groups = "drop") |>
    mutate(
      weighted_pct = 100 * weighted_n / sum(weighted_n),
      small_category = unweighted_n < 30 | weighted_pct < 2.5,
      country = country,
      variable = toupper(requested)
    ) |>
    select(country, variable, category, unweighted_n, weighted_n, weighted_pct, small_category)

  data
}

categorical_variables <- c("asdgsb", "asbg01", "asdghrl", "asdhedup", "asbg03", "acbg05b", "acbg05c", "acbg03a", "acbg03b")
category_sizes <- bind_rows(lapply(names(country_data), function(country) {
  bind_rows(lapply(categorical_variables, function(variable) {
    small_category_one(country_data[[country]], country, variable)
  }))
}))

write.csv(category_sizes, file.path(output_dir, "category_size_checks.csv"), row.names = FALSE, na = "")
write.csv(filter(category_sizes, small_category), file.path(output_dir, "small_categories_flagged.csv"), row.names = FALSE, na = "")

# Validate South African 2021 means against the approximate published values:
# Never/almost never 359; about monthly 304; about weekly 243.
category_column <- find_column(headline_table, "asdgsb")
mean_column <- find_column(headline_table, "mean")
se_column <- find_column(headline_table, "se_mean")

sa_validation <- data.frame(
  published_category = c("Never or Almost Never", "About Monthly", "About Weekly"),
  published_mean_approx = c(359, 304, 243),
  estimated_mean = NA_real_,
  estimated_se = NA_real_,
  difference_from_published = NA_real_,
  absolute_difference = NA_real_,
  within_5_points = NA,
  stringsAsFactors = FALSE
)

if (!is.na(category_column) && !is.na(mean_column)) {
  sa_rows <- headline_table[headline_table$country == "South Africa", , drop = FALSE]
  labels <- tolower(as.character(sa_rows[[category_column]]))
  patterns <- c("never|almost never", "monthly|month", "weekly|week")

  for (index in seq_along(patterns)) {
    matched <- which(grepl(patterns[[index]], labels))[1L]
    if (!is.na(matched)) {
      sa_validation$estimated_mean[index] <- as.numeric(sa_rows[[mean_column]][matched])
      if (!is.na(se_column)) {
        sa_validation$estimated_se[index] <- as.numeric(sa_rows[[se_column]][matched])
      }
    }
  }

  sa_validation$difference_from_published <- sa_validation$estimated_mean - sa_validation$published_mean_approx
  sa_validation$absolute_difference <- abs(sa_validation$difference_from_published)
  sa_validation$within_5_points <- sa_validation$absolute_difference <= 5
}

write.csv(sa_validation, file.path(output_dir, "south_africa_published_mean_validation.csv"), row.names = FALSE, na = "")

# Paper-ready preliminary figures. These remain exploratory until the estimates
# and labels have been reviewed.
if (!is.na(category_column) && !is.na(mean_column)) {
  plot_data <- headline_table
  plot_data[[category_column]] <- factor(plot_data[[category_column]], levels = unique(plot_data[[category_column]]))

  if (!is.na(se_column)) {
    plot_data$lower_95 <- plot_data[[mean_column]] - 1.96 * plot_data[[se_column]]
    plot_data$upper_95 <- plot_data[[mean_column]] + 1.96 * plot_data[[se_column]]
  } else {
    plot_data$lower_95 <- NA_real_
    plot_data$upper_95 <- NA_real_
  }

  reading_plot <- ggplot(
    plot_data,
    aes(x = .data[[category_column]], y = .data[[mean_column]], colour = country, group = country)
  ) +
    geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.12, position = position_dodge(width = 0.2), na.rm = TRUE) +
    geom_point(size = 2.7, position = position_dodge(width = 0.2)) +
    geom_line(linewidth = 0.7, position = position_dodge(width = 0.2)) +
    labs(
      title = "Reading achievement by bullying frequency",
      subtitle = "PIRLS 2021 design-correct estimates; error bars are 95% confidence intervals",
      x = "Bullying frequency",
      y = "Mean reading achievement",
      colour = "Country"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))

  ggsave(file.path(output_dir, "figure_reading_by_bullying.png"), reading_plot, width = 8.5, height = 5.5, dpi = 300)
}

pct_column <- find_column(headline_table, "pct")
se_pct_column <- find_column(headline_table, "se_pct")

if (!is.na(category_column) && !is.na(pct_column)) {
  prevalence_plot_data <- headline_table
  prevalence_plot_data[[category_column]] <- factor(
    prevalence_plot_data[[category_column]],
    levels = unique(prevalence_plot_data[[category_column]])
  )

  prevalence_plot <- ggplot(
    prevalence_plot_data,
    aes(x = .data[[category_column]], y = .data[[pct_column]], fill = country)
  ) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    labs(
      title = "Bullying prevalence by country",
      subtitle = "PIRLS 2021 weighted estimates",
      x = "Bullying frequency",
      y = "Weighted percentage",
      fill = "Country"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))

  if (!is.na(se_pct_column)) {
    prevalence_plot <- prevalence_plot +
      geom_errorbar(
        aes(
          ymin = pmax(0, .data[[pct_column]] - 1.96 * .data[[se_pct_column]]),
          ymax = pmin(100, .data[[pct_column]] + 1.96 * .data[[se_pct_column]])
        ),
        position = position_dodge(width = 0.8),
        width = 0.15
      )
  }

  ggsave(file.path(output_dir, "figure_bullying_prevalence.png"), prevalence_plot, width = 8.5, height = 5.5, dpi = 300)
}

cat("\n==============================================================================\n")
cat("PRELIMINARY DESIGN-CORRECT DESCRIPTIVES\n")
cat("==============================================================================\n")
cat("\nReading achievement by bullying category:\n")
print(headline_table, row.names = FALSE)
cat("\nSouth African published-mean validation:\n")
print(sa_validation, row.names = FALSE)
cat("\nFlagged small categories:\n")
print(filter(category_sizes, small_category), row.names = FALSE)

message(
  "Preliminary descriptives completed. Outputs were written to ",
  normalizePath(output_dir, winslash = "/", mustWork = FALSE),
  "."
)

