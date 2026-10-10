# =============================================================================
# Create publication-ready regression figures
# =============================================================================

library(ggplot2)

input_dir <- file.path("output", "regressions")
figure_dir <- file.path(input_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

main <- read.csv(
  file.path(input_dir, "main_sequence_results.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
common <- read.csv(
  file.path(input_dir, "common_sample_sensitivity.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

bullying_terms <- c("Bullied about monthly", "Bullied about weekly")

main_plot <- main[main$term_label %in% bullying_terms, , drop = FALSE]
main_plot$sample_display <- "Available-case sample"

common_plot <- common[common$term_label %in% bullying_terms, , drop = FALSE]
common_plot$model <- sub("_common$", "", common_plot$model)
common_plot$sample_display <- "M5 common sample"

plot_data <- rbind(main_plot, common_plot)
plot_data$model_number <- as.integer(sub("M", "", plot_data$model))
plot_data$x_position <- plot_data$model_number + ifelse(
  plot_data$sample_display == "Available-case sample", -0.07, 0.07
)
plot_data$term_display <- factor(
  plot_data$term_label,
  levels = bullying_terms,
  labels = c("Bullied about monthly", "Bullied about weekly")
)
plot_data$sample_display <- factor(
  plot_data$sample_display,
  levels = c("Available-case sample", "M5 common sample")
)
plot_data$country <- factor(
  plot_data$country,
  levels = c("South Africa", "Brazil")
)

coefficient_plot <- ggplot(
  plot_data,
  aes(
    x = x_position,
    y = estimate,
    colour = term_display,
    shape = sample_display,
    linetype = sample_display,
    group = interaction(term_display, sample_display)
  )
) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.45) +
  geom_errorbar(
    aes(ymin = confidence_interval_lower, ymax = confidence_interval_upper),
    width = 0.08,
    linewidth = 0.55,
    alpha = 0.9
  ) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 2.8, stroke = 0.9, fill = "white") +
  facet_wrap(~country, nrow = 1) +
  scale_x_continuous(
    breaks = 1:5,
    labels = paste0("M", 1:5),
    limits = c(0.7, 5.3)
  ) +
  scale_colour_manual(
    values = c(
      "Bullied about monthly" = "#0072B2",
      "Bullied about weekly" = "#D55E00"
    ),
    name = "Bullying frequency"
  ) +
  scale_shape_manual(
    values = c("Available-case sample" = 16, "M5 common sample" = 17),
    name = "Estimation sample"
  ) +
  scale_linetype_manual(
    values = c("Available-case sample" = "solid", "M5 common sample" = "dashed"),
    name = "Estimation sample"
  ) +
  labs(
    title = "Association between bullying frequency and reading achievement",
    subtitle = "Coefficient estimates and 95% confidence intervals across model specifications",
    x = "Model specification",
    y = "Difference in PIRLS reading score",
    caption = paste(
      "Reference category: almost never bullied. M1-M5 successively add controls.",
      "Dashed common-sample estimates use the M5 complete-case sample and are available for M1-M4.",
      "All estimates use PIRLS weights, jackknife variance estimation, and five reading plausible values.",
      sep = "\n"
    )
  ) +
  guides(
    colour = guide_legend(order = 1),
    shape = guide_legend(order = 2),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(colour = "grey25"),
    strip.text = element_text(face = "bold", size = 11.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    plot.caption = element_text(hjust = 0, colour = "grey30", size = 8.5),
    plot.margin = margin(10, 14, 10, 10)
  )

ggsave(
  filename = file.path(figure_dir, "bullying_coefficients_across_models.png"),
  plot = coefficient_plot,
  width = 11,
  height = 7,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  filename = file.path(figure_dir, "bullying_coefficients_across_models.pdf"),
  plot = coefficient_plot,
  width = 11,
  height = 7,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

message(
  "Coefficient plot written to ",
  normalizePath(figure_dir, winslash = "/", mustWork = FALSE)
)
