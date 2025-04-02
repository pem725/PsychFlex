# Load required packages
library(psych)   # For reliability analysis
library(dplyr)
library(knitr)   # For table formatting
library(kableExtra) # For enhanced tables
library(tidyr)

# Read the data
data <- read.csv("tmpPvHitems4LLM.csv")

# Calculate POMP scores using a simpler approach: (mean score × 100) / maximum possible score
# Define the maximum possible scores
happiness_max <- 7  # Maximum score of 7 for happiness items
purpose_max <- 5    # Maximum score of 5 for purpose items

# Define the item groups
happiness_items <- list(
  baseline = c("b_shs_gh_a", "b_shs_rh_a", "b_shs_ch_a", "b_shs_ch_b_r"),
  follow_up1 = c("fu1_shs_gh_a", "fu1_shs_rh_a", "fu1_shs_ch_a", "fu1_shs_ch_fu1_r"),
  follow_up2 = c("fu2_shs_gh_a", "fu2_shs_rh_a", "fu2_shs_ch_a", "fu2_shs_ch_b_r")
)

purpose_items <- list(
  baseline = c("b_bpurp_1", "b_bpurp_2", "b_bpurp_3", "b_bpurp_4"),
  follow_up1 = c("fu1_bpurp_1", "fu1_bpurp_2", "fu1_bpurp_3", "fu1_bpurp_4"),
  follow_up2 = c("fu2_bpurp_1", "fu2_bpurp_2", "fu2_bpurp_3", "fu2_bpurp_4")
)

# Calculate POMP scores directly and store in a structured format
results <- data.frame(
  Measure = character(),
  Timepoint = character(),
  N = numeric(),
  Mean = numeric(),
  SD = numeric(),
  Min = numeric(),
  Max = numeric(),
  Alpha = numeric(),
  stringsAsFactors = FALSE
)

# Process happiness measures
for (tp_name in names(happiness_items)) {
  # Get display name for timepoint
  display_name <- switch(tp_name,
                         "baseline" = "Baseline",
                         "follow_up1" = "Follow-up 1",
                         "follow_up2" = "Follow-up 2")
  
  # Get items for this timepoint
  items <- happiness_items[[tp_name]]
  
  # Calculate alpha reliability
  alpha_result <- psych::alpha(data[, items], check.keys = TRUE)
  
  # Calculate POMP scores
  pomp_scores <- rowMeans(data[, items], na.rm = TRUE) * 100 / happiness_max
  
  # Calculate statistics
  n_valid <- sum(!is.na(pomp_scores))
  mean_val <- mean(pomp_scores, na.rm = TRUE)
  sd_val <- sd(pomp_scores, na.rm = TRUE)
  min_val <- min(pomp_scores, na.rm = TRUE)
  max_val <- max(pomp_scores, na.rm = TRUE)
  
  # Add to results
  results <- rbind(results, data.frame(
    Measure = "Happiness",
    Timepoint = display_name,
    N = n_valid,
    Mean = mean_val,
    SD = sd_val,
    Min = min_val,
    Max = max_val,
    Alpha = alpha_result$total$raw_alpha
  ))
}

# Process purpose measures
for (tp_name in names(purpose_items)) {
  # Get display name for timepoint
  display_name <- switch(tp_name,
                         "baseline" = "Baseline",
                         "follow_up1" = "Follow-up 1",
                         "follow_up2" = "Follow-up 2")
  
  # Get items for this timepoint
  items <- purpose_items[[tp_name]]
  
  # Calculate alpha reliability
  alpha_result <- psych::alpha(data[, items], check.keys = TRUE)
  
  # Calculate POMP scores
  pomp_scores <- rowMeans(data[, items], na.rm = TRUE) * 100 / purpose_max
  
  # Calculate statistics
  n_valid <- sum(!is.na(pomp_scores))
  mean_val <- mean(pomp_scores, na.rm = TRUE)
  sd_val <- sd(pomp_scores, na.rm = TRUE)
  min_val <- min(pomp_scores, na.rm = TRUE)
  max_val <- max(pomp_scores, na.rm = TRUE)
  
  # Add to results
  results <- rbind(results, data.frame(
    Measure = "Purpose",
    Timepoint = display_name,
    N = n_valid,
    Mean = mean_val,
    SD = sd_val,
    Min = min_val,
    Max = max_val,
    Alpha = alpha_result$total$raw_alpha
  ))
}

# Format the numeric columns to have consistent decimal places
results$Mean <- sprintf("%.2f", results$Mean)
results$SD <- sprintf("%.2f", results$SD)
results$Min <- sprintf("%.2f", results$Min)
results$Max <- sprintf("%.2f", results$Max)
results$Alpha <- sprintf("%.3f", results$Alpha)  # Use 3 decimal places for alpha

# Create the final formatted table
final_table <- kable(results, 
                   format = "html", 
                   caption = "Table 1. Psychometric Properties of Happiness and Purpose Measures Across Timepoints (POMP Scores)",
                   row.names = FALSE,
                   col.names = c("Measure", "Timepoint", "N", "Mean", "SD", "Min", "Max", "Cronbach's α")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
              full_width = FALSE) %>%
  row_spec(0, bold = TRUE)

# Function to export tables to CSV
export_tables <- function() {
  write.csv(results, "psychometric_properties.csv", row.names = FALSE)
  cat("Table exported to CSV file.\n")
}

print("Table with Cronbach's alpha and without skewness/kurtosis:")
final_table
