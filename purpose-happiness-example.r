# Example Usage of the Purpose and Happiness Longitudinal Analysis
# This script demonstrates how to use the analysis functions with your data

# First, source the main analysis script
# source("purpose-happiness-analysis.R")

# Load required packages
library(dplyr)
library(ggplot2)
library(knitr)
library(lavaan)
library(tidyr)
library(corrplot)

# ===============================================================
# 1. Load and preprocess the data
# ===============================================================

# Load your dataset
data <- read.csv("alldatPvH4LLM.csv")

# Check if key variables are present
purpose_vars <- c("b_bpurp_1", "b_bpurp_2", "b_bpurp_3", "b_bpurp_4",
                 "fu1_bpurp_1", "fu1_bpurp_2", "fu1_bpurp_3", "fu1_bpurp_4",
                 "fu2_bpurp_1", "fu2_bpurp_2", "fu2_bpurp_3", "fu2_bpurp_4")

happiness_vars <- c("b_shs_gh_a", "b_shs_rh_a", "b_shs_ch_a", "b_shs_ch_b_r",
                   "fu1_shs_gh_a", "fu1_shs_rh_a", "fu1_shs_ch_a", "fu1_shs_ch_fu1_r", 
                   "fu2_shs_gh_a", "fu2_shs_rh_a", "fu2_shs_ch_a", "fu2_shs_ch_b_r")

# Check for any missing variables
missing_vars <- setdiff(c(purpose_vars, happiness_vars), names(data))
if(length(missing_vars) > 0) {
  stop("Missing key variables: ", paste(missing_vars, collapse = ", "))
}

# ===============================================================
# 2. Run the comprehensive analysis
# ===============================================================

# Create a directory for results
output_dir <- "purpose_happiness_results"
if(!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Run the full analysis (uncomment when ready)
# results <- run_purpose_happiness_analysis(data, output_dir)

# ===============================================================
# 3. Alternatively, run individual analysis components
# ===============================================================

# Compute scale scores for easier analysis
data_with_scores <- compute_scale_scores(data)

# Run measurement invariance testing
invariance_results <- run_measurement_invariance(data)

# Print model fit summary
cat("\nMeasurement Invariance Summary:\n")
print(invariance_results$fit_table)

cat("\nModel Comparisons:\n")
print(invariance_results$diff_table)

# Run stability analysis
stability_results <- run_stability_analysis(data_with_scores)

# Print stability coefficients
cat("\nTemporal Stability Coefficients:\n")
print(stability_results$test_retest)

# Display stability plots
print(stability_results$stability_plots$shs_plot)
print(stability_results$stability_plots$bpurp_plot)

# Run correlational analysis
correlate_results <- run_correlate_analysis(data_with_scores)

# Print correlations with key variables
cat("\nCorrelations with Key Psychological Variables:\n")
print(correlate_results$purpose_happiness_correlations)

# Run longitudinal analysis
longitudinal_results <- run_longitudinal_analysis(data_with_scores)

# Print longitudinal correlations
cat("\nLongitudinal Correlations with Outcomes:\n")
print(longitudinal_results$longitudinal_correlations$longitudinal_results)

# Print regression model summaries
cat("\nRegression Model Summaries:\n")
print(longitudinal_results$regression_summary)

# ===============================================================
# 4. Generate an HTML report
# ===============================================================

# Combine all results
all_results <- list(
  measurement_invariance = invariance_results,
  stability = stability_results,
  correlations = correlate_results,
  longitudinal = longitudinal_results
)

# Generate the report (uncomment when ready)
# generate_html_report(all_results, output_file = file.path(output_dir, "purpose_happiness_report.html"))

# ===============================================================
# 5. Custom Figure Creation: Focus on key findings
# ===============================================================

# Create a custom figure comparing purpose and happiness stability
stability_comparison <- ggplot(stability_results$test_retest, 
                             aes(x = Months, y = Correlation, color = Construct)) +
  geom_point(size = 3) +
  geom_line(aes(group = Construct)) +
  labs(title = "Temporal Stability of Purpose vs. Happiness",
       x = "Months between assessments",
       y = "Test-Retest Correlation") +
  theme_minimal() +
  scale_color_manual(values = c("darkgreen", "royalblue")) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

print(stability_comparison)

# Save the custom figure
ggsave(file.path(output_dir, "stability_comparison.png"), 
      stability_comparison, width = 8, height = 6)

# Create a figure highlighting the different correlational patterns
# Extract data for key correlates
key_correlates <- c("b_gshs_mean", "b_di_mean", "b_bmpns_A_tot", "b_swls_mean")
key_labels <- c("Hope", "Distress Tolerance", "Autonomy", "Life Satisfaction")

correlate_data <- data.frame(
  Correlate = key_labels,
  Purpose = sapply(key_correlates, function(var) {
    cor(data_with_scores$b_bpurp_mean, data_with_scores[[var]], 
        use = "pairwise.complete.obs")
  }),
  Happiness = sapply(key_correlates, function(var) {
    cor(data_with_scores$b_shs_mean, data_with_scores[[var]], 
        use = "pairwise.complete.obs")
  })
)

# Create a long format for plotting
correlate_long <- correlate_data %>%
  tidyr::pivot_longer(
    cols = c("Purpose", "Happiness"),
    names_to = "Construct",
    values_to = "Correlation"
  )

# Plot the comparison
correlate_comparison <- ggplot(correlate_long, 
                             aes(x = Correlate, y = Correlation, 
                                 fill = Construct)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Purpose vs. Happiness: Key Correlates",
       x = "",
       y = "Correlation Coefficient") +
  theme_minimal() +
  scale_fill_manual(values = c("royalblue", "darkgreen")) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 11, angle = 0),
    legend.position = "bottom"
  ) +
  ylim(0, 0.7)

print(correlate_comparison)

# Save the custom figure
ggsave(file.path(output_dir, "correlate_comparison.png"), 
      correlate_comparison, width = 8, height = 6)

# ===============================================================
# 6. Create clean measurement diagrams for the manuscript
# ===============================================================

# Extract factor loadings from the model
loadings_purpose <- invariance_results$loadings %>%
  filter(factor == "bpurp") %>%
  select(timepoint, item, loading) %>%
  mutate(
    time_label = case_when(
      timepoint == "1" ~ "Baseline",
      timepoint == "2" ~ "6-month",
      timepoint == "3" ~ "2-year"
    )
  ) %>%
  arrange(time_label, item)

loadings_happiness <- invariance_results$loadings %>%
  filter(factor == "shs") %>%
  select(timepoint, item, loading) %>%
  mutate(
    time_label = case_when(
      timepoint == "1" ~ "Baseline",
      timepoint == "2" ~ "6-month",
      timepoint == "3" ~ "2-year"
    )
  ) %>%
  arrange(time_label, item)

# Print nicely formatted tables
cat("\nPurpose Factor Loadings:\n")
print(loadings_purpose)

cat("\nHappiness Factor Loadings:\n")
print(loadings_happiness)

# Extract factor correlations
factor_correlations <- invariance_results$correlations %>%
  mutate(
    time_label = case_when(
      timepoint == "1" ~ "Baseline",
      timepoint == "2" ~ "6-month",
      timepoint == "3" ~ "2-year"
    ),
    correlation_formatted = sprintf("%.3f (%.3f)", correlation, se)
  ) %>%
  select(time_label, correlation_formatted) %>%
  arrange(time_label)

cat("\nPurpose-Happiness Correlations:\n")
print(factor_correlations)
