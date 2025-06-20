# Longitudinal Data Analysis: Psychometrics and Correlations
# Author: Generated for longitudinal study analysis
# Date: 2025

library(tidyverse)
library(psych)
library(corrr)
library(knitr)
library(kableExtra)

# Assuming your data frame is called 'dat'
# dat <- read.csv("your_data.csv") # Uncomment and modify as needed

# =============================================================================
# 1. VARIABLE IDENTIFICATION AND GROUPING
# =============================================================================

# Define time point prefixes
time_prefixes <- c("b_", "fu1_", "fu2_")

# Key measure suffixes for identification
key_measures <- list(
  shs = "shs",           # Subjective Happiness Scale
  bpurp = "bpurp",       # Brief Purpose (Purpose in Life)
  bmpns = "bmpns",       # Basic Psychological Needs
  vali = "vali",         # Values (importance)
  valc = "valc",         # Values (centrality)
  pf = "pf",             # Personal Focus/Goals
  gshs = "gshs",         # Hope Scale
  pilea = "pilea",       # Purpose in Life Engagement
  bmeaq = "bmeaq",       # Meaning/Authenticity
  scs = "scs",           # Self-Compassion
  ibaes = "ibaes"        # Intrinsic/Extrinsic aspirations
)

# =============================================================================
# 2. IDENTIFY VARIABLES NEEDING REVERSE CODING
# =============================================================================

identify_reverse_coded_vars <- function(var_names) {
  # Variables ending with _nr need reverse coding
  # Variables ending with _r are already reverse coded
  needs_reverse <- grep("_nr$", var_names, value = TRUE)
  already_reversed <- grep("_r$", var_names, value = TRUE)
  
  list(
    needs_reverse = needs_reverse,
    already_reversed = already_reversed
  )
}

reverse_coding_info <- identify_reverse_coded_vars(names(dat))
cat("Variables needing reverse coding:", length(reverse_coding_info$needs_reverse), "\n")
cat("Variables already reverse coded:", length(reverse_coding_info$already_reversed), "\n")

# =============================================================================
# 3. REVERSE CODE VARIABLES AS NEEDED
# =============================================================================

reverse_code_likert <- function(x, max_val = 7) {
  # Assumes Likert scale from 1 to max_val
  # You may need to adjust max_val based on your scales
  (max_val + 1) - x
}

# Create a working copy of the data
dat_processed <- dat

# Reverse code variables ending in _nr
for (var in reverse_coding_info$needs_reverse) {
  if (var %in% names(dat_processed)) {
    # Detect scale range automatically
    var_range <- range(dat_processed[[var]], na.rm = TRUE)
    max_val <- max(var_range)
    min_val <- min(var_range)
    
    if (max_val <= 7 && min_val >= 1) {
      dat_processed[[var]] <- reverse_code_likert(dat_processed[[var]], max_val)
      cat("Reverse coded:", var, "with max value:", max_val, "\n")
    }
  }
}

# =============================================================================
# 4. EXTRACT VARIABLES BY TIME POINT AND MEASURE
# =============================================================================

extract_vars_by_measure <- function(data, measure_name, time_point) {
  pattern <- paste0("^", time_point, ".*", measure_name)
  var_names <- grep(pattern, names(data), value = TRUE)
  return(var_names)
}

# Create comprehensive variable lists
measure_vars <- list()
for (time in time_prefixes) {
  measure_vars[[time]] <- list()
  for (measure in names(key_measures)) {
    vars <- extract_vars_by_measure(dat_processed, key_measures[[measure]], time)
    if (length(vars) > 0) {
      measure_vars[[time]][[measure]] <- vars
    }
  }
}

# =============================================================================
# 5. PSYCHOMETRIC ANALYSIS (CRONBACH'S ALPHA)
# =============================================================================

# =============================================================================
# 5a. HELPER FUNCTIONS
# =============================================================================

# Function to check if variable is numeric
is_numeric_var <- function(data, var_name) {
  if (var_name %in% names(data)) {
    return(is.numeric(data[[var_name]]) || is.integer(data[[var_name]]))
  }
  return(FALSE)
}

# Safe version of sapply that always returns logical vector
safe_sapply_logical <- function(X, FUN, ...) {
  result <- sapply(X, FUN, ..., USE.NAMES = FALSE, simplify = TRUE)
  if (is.list(result)) {
    result <- unlist(result)
  }
  return(as.logical(result))
}


# Function to identify item-level variables (exclude composites/means)
get_item_level_vars <- function(var_list) {
  # Exclude variables that are already computed composites/means/totals
  exclude_patterns <- c("_mean", "_tot", "_POMP", "_composite", "_diff", "_goals1_2")
  item_vars <- var_list
  
  for (pattern in exclude_patterns) {
    item_vars <- item_vars[!grepl(pattern, item_vars)]
  }
  
  return(item_vars)
}

# =============================================================================
# 5b. PSYCHOMETRIC ANALYSIS (CRONBACH'S ALPHA) - FIXED VERSION
# =============================================================================

calculate_alpha_for_scales_fixed <- function(data, var_lists) {
  alpha_results <- list()
  
  for (time_point in names(var_lists)) {
    alpha_results[[time_point]] <- list()
    
    for (measure in names(var_lists[[time_point]])) {
      vars <- var_lists[[time_point]][[measure]]
      
      # Get item-level variables only
      item_vars <- get_item_level_vars(vars)
      
      # Filter to only numeric variables that exist in the data
      numeric_check <- safe_sapply_logical(item_vars, function(x) is_numeric_var(data, x))
      numeric_vars <- item_vars[numeric_check]
      
      if (length(numeric_vars) >= 2) {
        # Extract data and handle missing values
        scale_data <- data[, numeric_vars, drop = FALSE]
        
        # Remove rows with all missing values
        scale_data <- scale_data[rowSums(!is.na(scale_data)) > 0, , drop = FALSE]
        
        # Remove columns with all missing values
        scale_data <- scale_data[, colSums(!is.na(scale_data)) > 0, drop = FALSE]
        
        if (nrow(scale_data) > 10 && ncol(scale_data) >= 2) {
          tryCatch({
            # Suppress the smc warnings that are cluttering output
            suppressWarnings({
              alpha_result <- psych::alpha(scale_data, check.keys = TRUE, warnings = FALSE)
            })
            alpha_results[[time_point]][[measure]] <- list(
              alpha = alpha_result$total$std.alpha,
              raw_alpha = alpha_result$total$raw_alpha,
              n_items = ncol(scale_data),
              n_cases = nrow(scale_data[complete.cases(scale_data), ]),
              variables = numeric_vars,
              reliability = ifelse(alpha_result$total$std.alpha >= 0.70, "Good", 
                                   ifelse(alpha_result$total$std.alpha >= 0.60, "Acceptable", "Poor"))
            )
          }, error = function(e) {
            cat("Error calculating alpha for", time_point, measure, ":", e$message, "\n")
          })
        }
      }
    }
  }
  return(alpha_results)
}

# Calculate alphas with fixed function
alpha_results <- calculate_alpha_for_scales_fixed(dat_processed, measure_vars)

# Create alpha summary table
create_alpha_table <- function(alpha_results) {
  alpha_df <- data.frame()
  
  for (time_point in names(alpha_results)) {
    for (measure in names(alpha_results[[time_point]])) {
      result <- alpha_results[[time_point]][[measure]]
      alpha_df <- rbind(alpha_df, data.frame(
        Time_Point = gsub("_$", "", time_point),  # Remove trailing underscore
        Measure = toupper(measure),
        Alpha = round(result$alpha, 3),
        Raw_Alpha = round(result$raw_alpha, 3),
        N_Items = result$n_items,
        N_Cases = result$n_cases,
        Reliability = result$reliability,
        stringsAsFactors = FALSE
      ))
    }
  }
  return(alpha_df)
}

# Create and display alpha table
if (length(alpha_results) > 0) {
  alpha_table <- create_alpha_table(alpha_results)
  cat("=== CRONBACH'S ALPHA RESULTS ===\n")
  print(kable(alpha_table, format = "pipe", align = c("l", "l", "r", "r", "r", "r", "l")))
  cat("\n")
} else {
  cat("No alpha results to display\n")
}

# =============================================================================
# 6. CREATE COMPOSITE SCORES
# =============================================================================

create_composite_scores <- function(data, var_lists) {
  composite_data <- data
  
  for (time_point in names(var_lists)) {
    for (measure in names(var_lists[[time_point]])) {
      vars <- var_lists[[time_point]][[measure]]
      
      # Filter to item-level variables (exclude already computed means)
      item_vars <- vars[!grepl("_mean|_tot|_POMP", vars)]
      
      if (length(item_vars) >= 2) {
        composite_name <- paste0(time_point, measure, "_composite")
        composite_data[[composite_name]] <- rowMeans(
          composite_data[, item_vars, drop = FALSE], 
          na.rm = TRUE
        )
      }
    }
  }
  return(composite_data)
}

# Create composite scores
dat_with_composites <- create_composite_scores(dat_processed, measure_vars)

# =============================================================================
# 7. CORRELATION ANALYSIS
# =============================================================================

# Extract key variables for correlation analysis
extract_key_variables <- function(data, time_points = c("b_", "fu1_", "fu2_")) {
  key_vars <- list()
  
  for (time in time_points) {
    # Extract happiness (SHS) and purpose (BPURP) as key outcomes
    shs_vars <- grep(paste0("^", time, "shs_mean"), names(data), value = TRUE)
    bpurp_vars <- grep(paste0("^", time, "bpurp_mean"), names(data), value = TRUE)
    
    # If mean variables don't exist, use composite scores
    if (length(shs_vars) == 0) {
      shs_vars <- grep(paste0("^", time, "shs_composite"), names(data), value = TRUE)
    }
    if (length(bpurp_vars) == 0) {
      bpurp_vars <- grep(paste0("^", time, "bpurp_composite"), names(data), value = TRUE)
    }
    
    key_vars[[paste0(time, "happiness")]] <- shs_vars[1]  # Take first match
    key_vars[[paste0(time, "purpose")]] <- bpurp_vars[1]  # Take first match
  }
  
  # Remove NULL entries
  key_vars <- key_vars[!sapply(key_vars, is.null)]
  key_vars <- key_vars[!sapply(key_vars, function(x) length(x) == 0)]
  
  return(key_vars)
}

# Extract correlates (other measures)
extract_correlates <- function(data, time_points = c("b_", "fu1_", "fu2_")) {
  correlate_vars <- list()
  
  measures_to_correlate <- c("bmpns", "vali", "valc", "gshs", "pilea", "bmeaq", "scs", "ibaes")
  
  for (time in time_points) {
    for (measure in measures_to_correlate) {
      # Look for mean or composite variables
      mean_vars <- grep(paste0("^", time, ".*", measure, ".*mean"), names(data), value = TRUE)
      composite_vars <- grep(paste0("^", time, measure, "_composite"), names(data), value = TRUE)
      tot_vars <- grep(paste0("^", time, ".*", measure, ".*tot"), names(data), value = TRUE)
      
      if (length(mean_vars) > 0) {
        correlate_vars[[paste0(time, measure)]] <- mean_vars
      } else if (length(composite_vars) > 0) {
        correlate_vars[[paste0(time, measure)]] <- composite_vars
      } else if (length(tot_vars) > 0) {
        correlate_vars[[paste0(time, measure)]] <- tot_vars[1]
      }
    }
  }
  
  return(correlate_vars)
}

# Get key variables and correlates
key_variables <- extract_key_variables(dat_with_composites)
correlate_variables <- extract_correlates(dat_with_composites)

# Create correlation matrix
create_correlation_table <- function(data, key_vars, correlate_vars) {
  # Combine all variables
  all_vars <- c(unlist(key_vars), unlist(correlate_vars))
  all_vars <- all_vars[!is.na(all_vars)]
  all_vars <- all_vars[all_vars %in% names(data)]
  
  if (length(all_vars) == 0) {
    cat("No variables found for correlation analysis\n")
    return(NULL)
  }
  
  # Calculate correlation matrix
  cor_data <- data[, all_vars, drop = FALSE]
  cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
  
  return(cor_matrix)
}

# Calculate correlations
correlation_matrix <- create_correlation_table(dat_with_composites, key_variables, correlate_variables)

if (!is.null(correlation_matrix)) {
  # Extract correlations with happiness and purpose
  happiness_vars <- unlist(key_variables)[grepl("happiness", names(unlist(key_variables)))]
  purpose_vars <- unlist(key_variables)[grepl("purpose", names(unlist(key_variables)))]
  
  # Create focused correlation table
  if (length(happiness_vars) > 0 || length(purpose_vars) > 0) {
    outcome_vars <- c(happiness_vars, purpose_vars)
    outcome_vars <- outcome_vars[outcome_vars %in% rownames(correlation_matrix)]
    
    if (length(outcome_vars) > 0) {
      correlate_names <- unlist(correlate_variables)
      correlate_names <- correlate_names[correlate_names %in% rownames(correlation_matrix)]
      
      if (length(correlate_names) > 0) {
        focused_cors <- correlation_matrix[correlate_names, outcome_vars, drop = FALSE]
        
        print("Correlations with Happiness and Purpose:")
        print(kable(round(focused_cors, 3), format = "markdown"))
      }
    }
  }
}

# =============================================================================
# 8. SUMMARY STATISTICS
# =============================================================================

create_descriptive_table <- function(data, variables) {
  desc_stats <- data.frame()
  
  for (var_name in names(variables)) {
    var <- variables[[var_name]]
    if (length(var) == 1 && var %in% names(data)) {
      stats <- data %>%
        summarise(
          Variable = var_name,
          N = sum(!is.na(.data[[var]])),
          Mean = round(mean(.data[[var]], na.rm = TRUE), 2),
          SD = round(sd(.data[[var]], na.rm = TRUE), 2),
          Min = round(min(.data[[var]], na.rm = TRUE), 2),
          Max = round(max(.data[[var]], na.rm = TRUE), 2)
        )
      desc_stats <- rbind(desc_stats, stats)
    }
  }
  return(desc_stats)
}

# Create descriptive statistics
all_key_vars <- c(key_variables, correlate_variables)
descriptive_table <- create_descriptive_table(dat_with_composites, all_key_vars)

print("Descriptive Statistics:")
print(kable(descriptive_table, format = "markdown"))

# =============================================================================
# 9. SAVE RESULTS
# =============================================================================

# Save processed data
# write.csv(dat_with_composites, "processed_longitudinal_data.csv", row.names = FALSE)

# Save alpha results
# write.csv(alpha_table, "cronbach_alpha_results.csv", row.names = FALSE)

# Save correlation matrix
# if (!is.null(correlation_matrix)) {
#   write.csv(correlation_matrix, "correlation_matrix.csv", row.names = TRUE)
# }

# Save descriptive statistics
# write.csv(descriptive_table, "descriptive_statistics.csv", row.names = FALSE)

cat("\nAnalysis complete! Check the tables above for results.\n")
cat("Uncomment the write.csv() lines to save results to files.\n")