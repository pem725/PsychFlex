# Load required packages
library(quantreg)
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the data
data <- read.csv("tmpPvHitems4LLM.csv")

# Calculate POMP scores using a simpler approach: (mean score × 100) / maximum possible score
# Define the maximum possible scores
happiness_max <- 7  # Maximum score of 7 for happiness items
purpose_max <- 5    # Maximum score of 5 for purpose items

# Define the happiness and purpose item groups
happiness_items <- list(
  b = c("b_shs_gh_a", "b_shs_rh_a", "b_shs_ch_a", "b_shs_ch_b_r"),
  fu1 = c("fu1_shs_gh_a", "fu1_shs_rh_a", "fu1_shs_ch_a", "fu1_shs_ch_fu1_r"),
  fu2 = c("fu2_shs_gh_a", "fu2_shs_rh_a", "fu2_shs_ch_a", "fu2_shs_ch_b_r")
)

purpose_items <- list(
  b = c("b_bpurp_1", "b_bpurp_2", "b_bpurp_3", "b_bpurp_4"),
  fu1 = c("fu1_bpurp_1", "fu1_bpurp_2", "fu1_bpurp_3", "fu1_bpurp_4"),
  fu2 = c("fu2_bpurp_1", "fu2_bpurp_2", "fu2_bpurp_3", "fu2_bpurp_4")
)

# Calculate POMP scores for each timepoint
for (tp in c("b", "fu1", "fu2")) {
  # Calculate happiness POMP scores
  h_mean <- rowMeans(data[, happiness_items[[tp]]], na.rm = TRUE)
  data[[paste0(tp, "_happiness")]] <- (h_mean * 100) / happiness_max
  
  # Calculate purpose POMP scores
  p_mean <- rowMeans(data[, purpose_items[[tp]]], na.rm = TRUE)
  data[[paste0(tp, "_purpose")]] <- (p_mean * 100) / purpose_max
}

# Reshape data to long format for easier analysis
data_long <- data %>%
  select(id, contains("purpose"), contains("happiness")) %>%
  pivot_longer(
    cols = -id,
    names_to = c("timepoint", "measure"),
    names_pattern = "(b|fu1|fu2)_(purpose|happiness)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  )

# Define quantiles of interest
quantiles <- c(0.15, 0.30, 0.50, 0.70, 0.85)

# Function to run quantile regression for each timepoint
run_quantile_regression <- function(data_subset, tau) {
  model <- rq(happiness ~ purpose, tau = tau, data = data_subset)
  coef_data <- data.frame(
    quantile = tau,
    intercept = coef(model)[1],
    slope = coef(model)[2],
    timepoint = unique(data_subset$timepoint)
  )
  return(coef_data)
}

# Empty data frame to store results
results <- data.frame()

# Run quantile regression for each timepoint and quantile
for (tp in c("b", "fu1", "fu2")) {
  data_subset <- data_long %>% filter(timepoint == tp)
  
  for (q in quantiles) {
    results <- rbind(results, run_quantile_regression(data_subset, q))
  }
}

# Create data for plotting
# For creating scatterplots with fitted quantile regression lines
create_plot_data <- function(timepoint_data, timepoint_name) {
  plot_data <- data.frame()
  
  for (q in quantiles) {
    # Get the model for this quantile
    model_data <- results %>% 
      filter(timepoint == timepoint_name, quantile == q)
    
    # Generate points along the regression line
    x_range <- seq(min(timepoint_data$purpose, na.rm = TRUE), 
                   max(timepoint_data$purpose, na.rm = TRUE), 
                   length.out = 100)
    y_values <- model_data$intercept + model_data$slope * x_range
    
    temp_data <- data.frame(
      purpose = x_range,
      happiness = y_values,
      quantile = paste0(q*100, "th percentile"),
      timepoint = timepoint_name
    )
    
    plot_data <- rbind(plot_data, temp_data)
  }
  
  return(plot_data)
}

# Create plots for each timepoint
plot_b <- create_plot_data(data_long %>% filter(timepoint == "b"), "b")
plot_fu1 <- create_plot_data(data_long %>% filter(timepoint == "fu1"), "fu1")
plot_fu2 <- create_plot_data(data_long %>% filter(timepoint == "fu2"), "fu2")

plot_all <- rbind(plot_b, plot_fu1, plot_fu2)

# Generate scatterplots with quantile regression lines
ggplot() +
  # Add points (with transparency)
  geom_point(data = data_long, aes(x = purpose, y = happiness), alpha = 0.3) +
  # Add quantile regression lines
  geom_line(data = plot_all, aes(x = purpose, y = happiness, color = quantile, group = quantile), size = 1) +
  # Facet by timepoint
  facet_wrap(~ timepoint, labeller = labeller(timepoint = c(b = "Baseline", fu1 = "Follow-up 1", fu2 = "Follow-up 2"))) +
  labs(
    title = "Relationship Between Purpose and Happiness",
    subtitle = "Quantile Regression Analysis at Different Timepoints",
    x = "Purpose Score",
    y = "Happiness Score",
    color = "Percentile"
  ) +
  theme_minimal()

# Compare slopes across timepoints and quantiles
# Group by quantile to compare across timepoints
results_by_quantile <- results %>%
  group_by(quantile) %>%
  arrange(timepoint) %>%
  summarize(
    baseline_slope = slope[timepoint == "b"],
    fu1_slope = slope[timepoint == "fu1"],
    fu2_slope = slope[timepoint == "fu2"],
    change_b_to_fu1 = fu1_slope - baseline_slope,
    change_fu1_to_fu2 = fu2_slope - fu1_slope
  )

# Calculate the differences in slopes between lower and upper quantiles
# Similar to the analysis in Killingsworth paper
slope_comparison <- results %>%
  group_by(timepoint) %>%
  summarize(
    lowest_15_slope = slope[which(quantile == 0.15)],
    median_slope = slope[which(quantile == 0.50)],
    highest_85_slope = slope[which(quantile == 0.85)],
    diff_high_low = highest_85_slope - lowest_15_slope
  )

# Print the results
print("Quantile Regression Results:")
print(results)
print("\nSlope Comparisons Across Quantiles:")
print(slope_comparison)
print("\nSlope Comparisons Across Timepoints:")
print(results_by_quantile)

# Check for flattening or acceleration patterns
# Test if the slopes for the 15th percentile flatten after follow-up 1
# or if the slopes for the 85th percentile accelerate after follow-up 1
flattening_test <- results %>%
  filter(quantile == 0.15) %>%
  arrange(timepoint)

acceleration_test <- results %>%
  filter(quantile == 0.85) %>%
  arrange(timepoint)

print("\nTesting for flattening pattern in lowest quantile (15th):")
print(flattening_test)

print("\nTesting for acceleration pattern in highest quantile (85th):")
print(acceleration_test)
