# Table Formatter for Google Docs
# This script takes the output from your purpose-happiness analysis
# and formats it for easy copying into Google Docs

library(dplyr)
library(knitr)
library(kableExtra)

# Function to format data frames for Google Docs
format_table_for_gdocs <- function(df, caption = NULL) {
  # Create a nicely formatted table using kable
  formatted_table <- knitr::kable(df, format = "html", caption = caption) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                             full_width = FALSE)
  
  # Return the HTML table that can be copied to Google Docs
  return(formatted_table)
}

# Load your results - replace with your actual path
# Uncomment and modify the path to load your saved results
# load("purpose_happiness_results.RData")

# Alternative: Run your analysis again with cleaned up output
run_formatted_analysis <- function(data_path) {
  # Load data
  data <- read.csv(data_path)
  
  # Run the original analysis functions from your script
  # Compute composite scores
  compute_scale_scores <- function(data) {
    # Core function from your original script
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
  
  analyze_stability <- function(data) {
    # Core function from your original script
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
    
    # Format the correlation values
    test_retest$Correlation <- sprintf("%.3f", test_retest$Correlation)
    
    return(list(
      stability_corr = stability_corr,
      test_retest = test_retest
    ))
  }
  
  analyze_correlates <- function(data) {
    # Core function from your original script
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
    
    # Rename variables to be more readable
    purpose_correlations$Variable <- case_when(
      purpose_correlations$Variable == "b_gshs_mean" ~ "Hope",
      purpose_correlations$Variable == "b_scs_mean" ~ "Self-Control",
      purpose_correlations$Variable == "b_bmpns_A_tot" ~ "Need Satisfaction (Autonomy)",
      purpose_correlations$Variable == "b_bmpns_C_tot" ~ "Need Satisfaction (Competence)",
      purpose_correlations$Variable == "b_bmpns_R_tot" ~ "Need Satisfaction (Relatedness)",
      purpose_correlations$Variable == "b_bmis_PU_mean" ~ "Positive Affect",
      purpose_correlations$Variable == "b_bmis_NT_mean" ~ "Negative Affect",
      purpose_correlations$Variable == "b_swls_mean" ~ "Life Satisfaction",
      TRUE ~ purpose_correlations$Variable
    )
    
    # Format the numeric columns to 3 decimal places
    purpose_correlations <- purpose_correlations %>%
      rename(
        "Purpose (r)" = Purpose_Correlation,
        "Happiness (r)" = Happiness_Correlation,
        "Difference" = Difference
      )
    
    return(purpose_correlations)
  }
  
  analyze_longitudinal <- function(data) {
    # Core function from your original script
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
    
    # Round values and rename variables
    if(nrow(results_6mo) > 0) {
      results_6mo <- results_6mo %>% 
        mutate(across(where(is.numeric), ~round(., 3))) %>%
        rename("Purpose (r)" = Purpose_r, 
               "Happiness (r)" = Happiness_r)
      
      # Rename outcomes for clarity
      results_6mo$Outcome <- case_when(
        results_6mo$Outcome == "swls" ~ "Life Satisfaction",
        results_6mo$Outcome == "phq" ~ "Depression",
        results_6mo$Outcome == "bmis_PU" ~ "Positive Affect",
        TRUE ~ results_6mo$Outcome
      )
    }
    
    if(nrow(results_2yr) > 0) {
      results_2yr <- results_2yr %>% 
        mutate(across(where(is.numeric), ~round(., 3))) %>%
        rename("Purpose (r)" = Purpose_r, 
               "Happiness (r)" = Happiness_r)
      
      # Rename outcomes for clarity
      results_2yr$Outcome <- case_when(
        results_2yr$Outcome == "swls" ~ "Life Satisfaction",
        results_2yr$Outcome == "phq" ~ "Depression",
        results_2yr$Outcome == "bmis_PU" ~ "Positive Affect",
        TRUE ~ results_2yr$Outcome
      )
    }
    
    return(list(
      results_6mo = results_6mo,
      results_2yr = results_2yr
    ))
  }
  
  # Process data and run analysis
  data_with_scores <- compute_scale_scores(data)
  stability_results <- analyze_stability(data_with_scores)
  correlate_results <- analyze_correlates(data_with_scores)
  longitudinal_results <- analyze_longitudinal(data_with_scores)
  
  # Format tables for Google Docs copying
  formatted_tables <- list(
    stability = format_table_for_gdocs(stability_results$test_retest, 
                                       "Table 1. Test-Retest Reliability of Purpose and Happiness Measures"),
    
    correlates = format_table_for_gdocs(correlate_results, 
                                       "Table 2. Correlations Between Purpose, Happiness, and Related Constructs"),
    
    longitudinal_6mo = format_table_for_gdocs(longitudinal_results$results_6mo, 
                                             "Table 3. Six-Month Longitudinal Correlations"),
    
    longitudinal_2yr = format_table_for_gdocs(longitudinal_results$results_2yr, 
                                             "Table 4. Two-Year Longitudinal Correlations")
  )
  
  # Also generate plain text/tab-delimited tables for easier copying
  tab_delimited_tables <- list(
    stability = capture.output(write.table(stability_results$test_retest, 
                                           quote = FALSE, sep = "\t", 
                                           row.names = FALSE)),
    
    correlates = capture.output(write.table(correlate_results, 
                                           quote = FALSE, sep = "\t", 
                                           row.names = FALSE)),
    
    longitudinal_6mo = capture.output(write.table(longitudinal_results$results_6mo, 
                                                 quote = FALSE, sep = "\t", 
                                                 row.names = FALSE)),
    
    longitudinal_2yr = capture.output(write.table(longitudinal_results$results_2yr, 
                                                 quote = FALSE, sep = "\t", 
                                                 row.names = FALSE))
  )
  
  # Return both HTML-formatted and tab-delimited tables
  return(list(
    html_tables = formatted_tables,
    tab_delimited = tab_delimited_tables
  ))
}

# Example usage:
# tables <- run_formatted_analysis("alldatPvH4LLM.csv")
# 
# # View HTML formatted tables (these can be copied to Google Docs)
# tables$html_tables$stability
# tables$html_tables$correlates
# tables$html_tables$longitudinal_6mo
# tables$html_tables$longitudinal_2yr
# 
# # Alternatively, for simple tab-delimited format:
# cat(tables$tab_delimited$stability, sep = "\n")
# cat(tables$tab_delimited$correlates, sep = "\n")
# cat(tables$tab_delimited$longitudinal_6mo, sep = "\n")
# cat(tables$tab_delimited$longitudinal_2yr, sep = "\n")

# Save formatted tables to files
save_tables_to_files <- function(tables, output_dir = ".") {
  # Create directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save HTML files
  writeLines(tables$html_tables$stability, 
             file.path(output_dir, "table1_stability.html"))
  
  writeLines(tables$html_tables$correlates, 
             file.path(output_dir, "table2_correlates.html"))
  
  writeLines(tables$html_tables$longitudinal_6mo, 
             file.path(output_dir, "table3_longitudinal_6mo.html"))
  
  writeLines(tables$html_tables$longitudinal_2yr, 
             file.path(output_dir, "table4_longitudinal_2yr.html"))
  
  # Save tab-delimited files (easy to copy to spreadsheets)
  writeLines(tables$tab_delimited$stability, 
             file.path(output_dir, "table1_stability.txt"))
  
  writeLines(tables$tab_delimited$correlates, 
             file.path(output_dir, "table2_correlates.txt"))
  
  writeLines(tables$tab_delimited$longitudinal_6mo, 
             file.path(output_dir, "table3_longitudinal_6mo.txt"))
  
  writeLines(tables$tab_delimited$longitudinal_2yr, 
             file.path(output_dir, "table4_longitudinal_2yr.txt"))
  
  message("Tables saved to ", output_dir)
}

# Main function with improved Google Docs format
create_gdocs_ready_tables <- function(data_path, output_dir = NULL) {
  # Load data and run analysis
  data <- read.csv(data_path)
  data_with_scores <- compute_scale_scores(data)
  stability_results <- analyze_stability(data_with_scores)
  correlate_results <- analyze_correlates(data_with_scores)
  longitudinal_results <- analyze_longitudinal(data_with_scores)
  
  # Create Google Docs-friendly tables (using markdown format which works well)
  tables <- list()
  
  # Table 1: Stability Analysis
  tables$stability <- knitr::kable(stability_results$test_retest, format = "markdown",
                                  caption = "Test-Retest Reliability of Purpose and Happiness Measures")
  
  # Table 2: Correlates
  tables$correlates <- knitr::kable(correlate_results, format = "markdown",
                                   caption = "Correlations Between Purpose, Happiness, and Related Constructs")
  
  # Table 3: 6-month Longitudinal
  tables$longitudinal_6mo <- knitr::kable(longitudinal_results$results_6mo, format = "markdown",
                                         caption = "Six-Month Longitudinal Correlations")
  
  # Table 4: 2-year Longitudinal
  tables$longitudinal_2yr <- knitr::kable(longitudinal_results$results_2yr, format = "markdown",
                                         caption = "Two-Year Longitudinal Correlations")
  
  # Save files if directory is provided
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    writeLines(tables$stability, file.path(output_dir, "table1_stability.md"))
    writeLines(tables$correlates, file.path(output_dir, "table2_correlates.md"))
    writeLines(tables$longitudinal_6mo, file.path(output_dir, "table3_longitudinal_6mo.md"))
    writeLines(tables$longitudinal_2yr, file.path(output_dir, "table4_longitudinal_2yr.md"))
    
    message("Markdown tables saved to ", output_dir)
  }
  
  return(tables)
}
