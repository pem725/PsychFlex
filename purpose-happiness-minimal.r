# Minimal Purpose and Happiness Analysis Script
# Focusing on core functionality with minimal dependencies

# Load essential packages
library(dplyr)
library(tidyr)
library(stringr)

# Function to compute composite scores for SHS and BPURP at each timepoint
compute_scale_scores <- function(data) {
  # Compute SHS composites
  data_with_scores <- data %>%
    mutate(
      # Baseline 
      b_shs_mean = rowMeans(select(., b_shs_gh_a, b_shs_rh_a, b_shs_ch_a, b_shs_ch_b_r), na.rm = TRUE),
      b_bpurp_mean = rowMeans(select(., b_bpurp_1, b_bpurp_2, b_bpurp_3, b_bpurp_4), na.rm = TRUE),
      
      # 6-month follow-up
      fu1_shs_mean = rowMeans(select(., fu1_shs_gh_a, fu1_shs_rh_a, fu1_shs_ch_a, fu1_shs_ch_fu1_r), na.rm = TRUE),
      fu1_bpurp_mean = rowMeans(select(., fu1_bpurp_1, fu1_bpurp_2, fu1_bpurp_3, fu1_bpurp_4), na.rm = TRUE),
      
      # 2-year follow-up
      fu2_shs_mean = rowMeans(select(., fu2_shs_gh_a, fu2_shs_rh_a, fu2_shs_ch_a, fu2_shs_ch_b_r), na.rm = TRUE),
      fu2_bpurp_mean = rowMeans(select(., fu2_bpurp_1, fu2_bpurp_2, fu2_bpurp_3, fu2_bpurp_4), na.rm = TRUE)
    )
  
  return(data_with_scores)
}

# Simple function to analyze temporal stability through correlations
analyze_stability <- function(data) {
  # Compute stability correlations
  stability_data <- data %>%
    select(
      # SHS at each timepoint
      b_shs_mean, fu1_shs_mean, fu2_shs_mean,
      # BPURP at each timepoint
      b_bpurp_mean, fu1_bpurp_mean, fu2_bpurp_mean
    )
  
  # Compute correlation matrix
  stability_corr <- cor(stability_data, use = "pairwise.complete.obs")
  
  # Create test-retest table
  test_retest <- data.frame(
    Construct = c("Happiness (SHS)", "Happiness (SHS)", "Happiness (SHS)",
                 "Purpose (BPURP)", "Purpose (BPURP)", "Purpose (BPURP)"),
    Time_Points = c("Baseline to 6-month", "Baseline to 2-year", "6-month to 2-year",
                   "Baseline to 6-month", "Baseline to 2-year", "6-month to 2-year"),
    Correlation = c(
      stability_corr["b_shs_mean", "fu1_shs_mean"],
      stability_corr["b_shs_mean", "fu2_shs_mean"],
      stability_corr["fu1_shs_mean", "fu2_shs_mean"],
      stability_corr["b_bpurp_mean", "fu1_bpurp_mean"],
      stability_corr["b_bpurp_mean", "fu2_bpurp_mean"],
      stability_corr["fu1_bpurp_mean", "fu2_bpurp_mean"]
    )
  )
  
  return(list(
    stability_corr = stability_corr,
    test_retest = test_retest
  ))
}

# Function to analyze correlates with other psychological variables
analyze_correlates <- function(data) {
  # Define key variables to correlate with purpose and happiness
  trait_vars <- c(
    # Hope
    "b_gshs_mean", 
    # Self-control
    "b_scs_mean",
    # Need satisfaction
    "b_bmpns_A_tot", "b_bmpns_C_tot", "b_bmpns_R_tot",
    # Positive/negative affect
    "b_bmis_PU_mean", "b_bmis_NT_mean",
    # Life satisfaction
    "b_swls_mean"
  )
  
  # Filter to only include variables that exist in the dataset
  trait_vars <- trait_vars[trait_vars %in% names(data)]
  
  # Extract baseline purpose and happiness measures
  outcome_vars <- c("b_bpurp_mean", "b_shs_mean")
  outcome_vars <- outcome_vars[outcome_vars %in% names(data)]
  
  # Select variables for correlation analysis
  correlation_data <- data %>%
    select(all_of(c(trait_vars, outcome_vars)))
  
  # Compute correlation matrix
  correlation_matrix <- cor(correlation_data, use = "pairwise.complete.obs")
  
  # Extract correlations with purpose and happiness
  purpose_correlations <- data.frame(
    Variable = trait_vars,
    Purpose_Correlation = correlation_matrix[trait_vars, "b_bpurp_mean"],
    Happiness_Correlation = correlation_matrix[trait_vars, "b_shs_mean"]
  )
  
  # Calculate the difference between purpose and happiness correlations
  purpose_correlations$Difference <- purpose_correlations$Purpose_Correlation - purpose_correlations$Happiness_Correlation
  
  # Round values for display
  purpose_correlations <- purpose_correlations %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  return(purpose_correlations)
}

# Function to analyze longitudinal relationships
analyze_longitudinal <- function(data) {
  # Define predictors (baseline measures)
  predictors <- c("b_bpurp_mean", "b_shs_mean")
  
  # Define follow-up outcomes
  outcomes_fu1 <- c(
    "fu1_swls_mean",  # Life satisfaction
    "fu1_phq_mean",   # Depression
    "fu1_bmis_PU_mean" # Positive affect
  )
  
  outcomes_fu2 <- c(
    "fu2_swls_mean",  # Life satisfaction
    "fu2_phq_mean",   # Depression
    "fu2_bmis_PU_mean" # Positive affect
  )
  
  # Filter variables that exist in the dataset
  predictors <- predictors[predictors %in% names(data)]
  outcomes_fu1 <- outcomes_fu1[outcomes_fu1 %in% names(data)]
  outcomes_fu2 <- outcomes_fu2[outcomes_fu2 %in% names(data)]
  
  # Results tables
  results_6mo <- data.frame()
  results_2yr <- data.frame()
  
  # Analyze 6-month outcomes
  if(length(predictors) > 0 && length(outcomes_fu1) > 0) {
    for(outcome in outcomes_fu1) {
      # Correlations with predictors
      purpose_r <- cor(data[[outcome]], data[["b_bpurp_mean"]], use = "pairwise.complete.obs")
      happiness_r <- cor(data[[outcome]], data[["b_shs_mean"]], use = "pairwise.complete.obs")
      
      # Add to results
      outcome_name <- gsub("fu1_|_mean", "", outcome)
      results_6mo <- rbind(results_6mo, data.frame(
        Outcome = outcome_name,
        Purpose_r = purpose_r,
        Happiness_r = happiness_r
      ))
    }
  }
  
  # Analyze 2-year outcomes
  if(length(predictors) > 0 && length(outcomes_fu2) > 0) {
    for(outcome in outcomes_fu2) {
      # Correlations with predictors
      purpose_r <- cor(data[[outcome]], data[["b_bpurp_mean"]], use = "pairwise.complete.obs")
      happiness_r <- cor(data[[outcome]], data[["b_shs_mean"]], use = "pairwise.complete.obs")
      
      # Add to results
      outcome_name <- gsub("fu2_|_mean", "", outcome)
      results_2yr <- rbind(results_2yr, data.frame(
        Outcome = outcome_name,
        Purpose_r = purpose_r,
        Happiness_r = happiness_r
      ))
    }
  }
  
  # Round values
  if(nrow(results_6mo) > 0) {
    results_6mo <- results_6mo %>% 
      mutate(across(where(is.numeric), ~round(., 3)))
  }
  
  if(nrow(results_2yr) > 0) {
    results_2yr <- results_2yr %>% 
      mutate(across(where(is.numeric), ~round(., 3)))
  }
  
  return(list(
    results_6mo = results_6mo,
    results_2yr = results_2yr
  ))
}

# Main function to run the analysis
run_purpose_happiness_analysis <- function(data, output_file = NULL) {
  # 1. Compute composite scores
  data_with_scores <- compute_scale_scores(data)
  
  # 2. Analyze stability
  stability_results <- analyze_stability(data_with_scores)
  
  # 3. Analyze correlates
  correlate_results <- analyze_correlates(data_with_scores)
  
  # 4. Analyze longitudinal relationships
  longitudinal_results <- analyze_longitudinal(data_with_scores)
  
  # Combine results
  all_results <- list(
    stability = stability_results,
    correlates = correlate_results,
    longitudinal = longitudinal_results
  )
  
  # Save results if output file is specified
  if(!is.null(output_file)) {
    save(all_results, file = output_file)
    message("Results saved to: ", output_file)
  }
  
  # Print key results
  cat("\n=== STABILITY ANALYSIS ===\n")
  print(stability_results$test_retest)
  
  cat("\n=== CORRELATIONAL ANALYSIS ===\n")
  print(correlate_results)
  
  cat("\n=== LONGITUDINAL ANALYSIS (6-month) ===\n")
  print(longitudinal_results$results_6mo)
  
  cat("\n=== LONGITUDINAL ANALYSIS (2-year) ===\n")
  print(longitudinal_results$results_2yr)
  
  return(all_results)
}

# Example usage:
# 1. Load your data
data <- read.csv("alldatPvH4LLM.csv")
# 
# 2. Run the analysis
results <- run_purpose_happiness_analysis(data, output_file = "purpose_happiness_results.RData")
