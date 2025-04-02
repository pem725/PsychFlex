# Quantile Regression Analysis of Purpose and Happiness
# Based on Killingsworth et al. (2023) approach

# Load required packages
library(tidyverse)
library(quantreg)  # For quantile regression
library(ggplot2)   # For visualization
library(knitr)     # For table outputs
library(kableExtra)
library(data.table)

# Read the data
data <- read.csv("tmpPvHitems4LLM.csv")

# Create composite scores for happiness and purpose at each time point
# Using the code from your cross-lagged-panel-model.r
data <- data %>%
  mutate(
    # Baseline happiness score (average of items)
    b_happiness = rowMeans(cbind(b_shs_gh_a, b_shs_rh_a, b_shs_ch_a, b_shs_ch_b_r), na.rm = TRUE)*100/7,
    
    # Follow-up 1 happiness score
    fu1_happiness = rowMeans(cbind(fu1_shs_gh_a, fu1_shs_rh_a, fu1_shs_ch_a, fu1_shs_ch_fu1_r), na.rm = TRUE)*100/7,
    
    # Follow-up 2 happiness score
    fu2_happiness = rowMeans(cbind(fu2_shs_gh_a, fu2_shs_rh_a, fu2_shs_ch_a, fu2_shs_ch_b_r), na.rm = TRUE)*100/7,
    
    # Baseline purpose score (average of items)
    b_purpose = rowMeans(cbind(b_bpurp_1, b_bpurp_2, b_bpurp_3, b_bpurp_4), na.rm = TRUE)*100/5,
    
    # Follow-up 1 purpose score
    fu1_purpose = rowMeans(cbind(fu1_bpurp_1, fu1_bpurp_2, fu1_bpurp_3, fu1_bpurp_4), na.rm = TRUE)*100/5,
    
    # Follow-up 2 purpose score
    fu2_purpose = rowMeans(cbind(fu2_bpurp_1, fu2_bpurp_2, fu2_bpurp_3, fu2_bpurp_4), na.rm = TRUE)*100/5
  )

# Function to run quantile regression for a specific timepoint
run_quantile_regression <- function(data, purpose_var, happiness_var, quantiles = c(0.15, 0.30, 0.50, 0.70, 0.85)) {
  results <- list()
  
  # Create a binned version of purpose for visualization
  # Create custom breaks to avoid the "breaks are not unique" error
  purpose_values <- data[[purpose_var]][!is.na(data[[purpose_var]])]
  min_val <- min(purpose_values, na.rm = TRUE)
  max_val <- max(purpose_values, na.rm = TRUE)
  range_val <- max_val - min_val
  
  # Create 10 equally spaced breaks
  custom_breaks <- seq(min_val, max_val, length.out = 11)
  
  data_with_binned_purpose <- data %>%
    mutate(purpose_bin = cut(get(purpose_var), 
                            breaks = custom_breaks,
                            include.lowest = TRUE,
                            labels = FALSE)) %>%
    group_by(purpose_bin) %>%
    mutate(purpose_bin_value = mean(get(purpose_var), na.rm = TRUE)) %>%
    ungroup()
  
  # Run quantile regression for each specified quantile
  for (q in quantiles) {
    model <- rq(formula(paste(happiness_var, "~", purpose_var)), 
               tau = q, 
               data = data)
    
    # Get summary using nid method which is more robust
    model_summary <- summary(model, se = "nid")
    
    results[[as.character(q)]] <- list(
      model = model,
      coef = coef(model),
      summary = model_summary
    )
  }
  
  # Create a dataframe for plotting
  plot_data <- data_with_binned_purpose %>%
    select(purpose_bin_value, !!sym(happiness_var)) %>%
    filter(!is.na(purpose_bin_value), !is.na(!!sym(happiness_var)))
  
  list(
    results = results,
    plot_data = plot_data,
    binned_data = data_with_binned_purpose
  )
}

# Run quantile regression for each timepoint
b_qr <- run_quantile_regression(data, "b_purpose", "b_happiness")
fu1_qr <- run_quantile_regression(data, "fu1_purpose", "fu1_happiness")
fu2_qr <- run_quantile_regression(data, "fu2_purpose", "fu2_happiness")

# Create a summary table of quantile regression slopes
create_slope_table <- function(qr_results, timepoint) {
  # Create a data frame with consistent columns
  slopes_df <- data.frame(
    Quantile = numeric(),
    Slope = numeric(),
    t_value = numeric(),
    p_value = numeric(),
    Timepoint = character(),
    Significant = character(),
    stringsAsFactors = FALSE
  )
  
  # Add data for each quantile
  for (q in names(qr_results$results)) {
    model <- qr_results$results[[q]]
    
    # Use nid method for standard errors which is more reliable
    model_summary <- summary(model$model, se = "nid")
    
    # Extract coefficient info safely
    coef_index <- 2  # Index for the purpose variable coefficient
    slope <- model$coef[2]  # The slope is the coefficient for the purpose variable
    
    # Get t-value and p-value more safely
    t_value <- NA
    p_value <- NA
    
    if (!is.null(model_summary$coefficients) && 
        nrow(model_summary$coefficients) >= 2 && 
        ncol(model_summary$coefficients) >= 3) {
      
      # Standard format with t-values and p-values
      t_value <- model_summary$coefficients[2, 3]
      
      if (ncol(model_summary$coefficients) >= 4) {
        p_value <- model_summary$coefficients[2, 4]
      }
    }
    
    # Add row to the data frame
    new_row <- data.frame(
      Quantile = as.numeric(q),
      Slope = slope,
      t_value = t_value,
      p_value = p_value,
      Timepoint = timepoint,
      Significant = ifelse(!is.na(p_value) & p_value < 0.05, "*", ""),
      stringsAsFactors = FALSE
    )
    
    slopes_df <- rbind(slopes_df, new_row)
  }
  
  return(slopes_df)
}

b_slopes <- create_slope_table(b_qr, "Baseline")
fu1_slopes <- create_slope_table(fu1_qr, "Follow-up 1")
fu2_slopes <- create_slope_table(fu2_qr, "Follow-up 2")

# Combine all slope tables
all_slopes <- rbind(b_slopes, fu1_slopes, fu2_slopes)
all_slopes <- all_slopes[order(all_slopes$Timepoint, all_slopes$Quantile),]

# Format the table for display
formatted_table <- all_slopes %>%
  mutate(
    Quantile = paste0(Quantile * 100, "th"),
    Slope = sprintf("%.2f", Slope),
    t_value = sprintf("%.2f", t_value),
    p_value = sprintf("%.3f", p_value)
  )

# Print the formatted table
kable(formatted_table, 
      caption = "Quantile Regression Slopes: Happiness Regressed on Purpose",
      col.names = c("Quantile", "Slope", "t-value", "p-value", "Timepoint", "Significant")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
  column_spec(6, bold = TRUE)  # Make the significance column stand out

# Create a visualization similar to Figure 2 in Killingsworth et al. (2023)
# Function to create the plot for a specific timepoint
create_quantile_plot <- function(qr_results, purpose_var, happiness_var, timepoint) {
  # Get the binned data
  binned_data <- qr_results$binned_data
  
  # Calculate actual quantiles for each purpose bin
  quantile_data <- binned_data %>%
    group_by(purpose_bin_value) %>%
    summarise(
      q15 = quantile(get(happiness_var), 0.15, na.rm = TRUE),
      q30 = quantile(get(happiness_var), 0.30, na.rm = TRUE),
      q50 = quantile(get(happiness_var), 0.50, na.rm = TRUE),
      q70 = quantile(get(happiness_var), 0.70, na.rm = TRUE),
      q85 = quantile(get(happiness_var), 0.85, na.rm = TRUE),
      n = n()
    ) %>%
    filter(n >= 5)  # Only include bins with sufficient data
  
  # Create plot
  p <- ggplot() +
    # 15th percentile
    geom_point(data = quantile_data, aes(x = purpose_bin_value, y = q15), color = "blue", size = 3, alpha = 0.7) +
    geom_smooth(data = quantile_data, aes(x = purpose_bin_value, y = q15), method = "lm", color = "blue", se = FALSE) +
    
    # 30th percentile
    geom_point(data = quantile_data, aes(x = purpose_bin_value, y = q30), color = "green", size = 3, alpha = 0.7) +
    geom_smooth(data = quantile_data, aes(x = purpose_bin_value, y = q30), method = "lm", color = "green", se = FALSE) +
    
    # 50th percentile (median)
    geom_point(data = quantile_data, aes(x = purpose_bin_value, y = q50), color = "black", size = 3, alpha = 0.7) +
    geom_smooth(data = quantile_data, aes(x = purpose_bin_value, y = q50), method = "lm", color = "black", se = FALSE) +
    
    # 70th percentile
    geom_point(data = quantile_data, aes(x = purpose_bin_value, y = q70), color = "orange", size = 3, alpha = 0.7) +
    geom_smooth(data = quantile_data, aes(x = purpose_bin_value, y = q70), method = "lm", color = "orange", se = FALSE) +
    
    # 85th percentile
    geom_point(data = quantile_data, aes(x = purpose_bin_value, y = q85), color = "red", size = 3, alpha = 0.7) +
    geom_smooth(data = quantile_data, aes(x = purpose_bin_value, y = q85), method = "lm", color = "red", se = FALSE) +
    
    # Labels and theme
    labs(
      title = paste("Happiness by Purpose in Life:", timepoint),
      subtitle = "Showing the 15th, 30th, 50th, 70th, and 85th percentiles of happiness",
      x = "Purpose in Life Score",
      y = "Happiness Score"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom"
    ) +
    # Add annotations for the quantiles
    annotate("text", x = min(quantile_data$purpose_bin_value, na.rm = TRUE), 
             y = max(quantile_data$q85, na.rm = TRUE), 
             label = "85th percentile", hjust = -0.1, color = "red") +
    annotate("text", x = min(quantile_data$purpose_bin_value, na.rm = TRUE), 
             y = max(quantile_data$q70, na.rm = TRUE), 
             label = "70th percentile", hjust = -0.1, color = "orange") +
    annotate("text", x = min(quantile_data$purpose_bin_value, na.rm = TRUE), 
             y = max(quantile_data$q50, na.rm = TRUE), 
             label = "50th percentile", hjust = -0.1, color = "black") +
    annotate("text", x = min(quantile_data$purpose_bin_value, na.rm = TRUE), 
             y = max(quantile_data$q30, na.rm = TRUE), 
             label = "30th percentile", hjust = -0.1, color = "green") +
    annotate("text", x = min(quantile_data$purpose_bin_value, na.rm = TRUE), 
             y = max(quantile_data$q15, na.rm = TRUE), 
             label = "15th percentile", hjust = -0.1, color = "blue")
  
  return(p)
}

# Create the plots for each timepoint
b_plot <- create_quantile_plot(b_qr, "b_purpose", "b_happiness", "Baseline")
fu1_plot <- create_quantile_plot(fu1_qr, "fu1_purpose", "fu1_happiness", "Follow-up 1")
fu2_plot <- create_quantile_plot(fu2_qr, "fu2_purpose", "fu2_happiness", "Follow-up 2")

# Print the plots
print(b_plot)
print(fu1_plot)
print(fu2_plot)

# Piecewise Quantile Regression Analysis
# Function to run piecewise quantile regression with a breakpoint
run_piecewise_qr <- function(data, purpose_var, happiness_var, 
                             breakpoint = NULL, quantiles = c(0.15, 0.30, 0.50, 0.70, 0.85)) {
  
  # If breakpoint is NULL, determine it using the median
  if (is.null(breakpoint)) {
    breakpoint <- median(data[[purpose_var]], na.rm = TRUE)
  }
  
  # Create variables for piecewise regression
  data <- data %>%
    mutate(
      purpose_low = ifelse(get(purpose_var) <= breakpoint, get(purpose_var), breakpoint),
      purpose_high = ifelse(get(purpose_var) > breakpoint, get(purpose_var) - breakpoint, 0)
    )
  
  results <- list()
  
  # Run quantile regression for each specified quantile
  for (q in quantiles) {
    model <- rq(formula(paste(happiness_var, "~ purpose_low + purpose_high")), 
                tau = q, 
                data = data)
    
    # Test if the slopes are different
    low_slope <- coef(model)["purpose_low"]
    high_slope <- coef(model)["purpose_high"]
    
    # Construct a test for difference in slopes
    wald_test <- summary(model, se = "boot", R = 1000)
    
    results[[as.character(q)]] <- list(
      model = model,
      low_slope = low_slope,
      high_slope = high_slope,
      p_value = wald_test$coefficients["purpose_high", 4],
      breakpoint = breakpoint
    )
  }
  
  return(results)
}

# Run piecewise quantile regression for each timepoint
# Let's determine breakpoints at the median of purpose scores
b_breakpoint <- median(data$b_purpose, na.rm = TRUE)
fu1_breakpoint <- median(data$fu1_purpose, na.rm = TRUE)
fu2_breakpoint <- median(data$fu2_purpose, na.rm = TRUE)

b_piecewise <- run_piecewise_qr(data, "b_purpose", "b_happiness", b_breakpoint)
fu1_piecewise <- run_piecewise_qr(data, "fu1_purpose", "fu1_happiness", fu1_breakpoint)
fu2_piecewise <- run_piecewise_qr(data, "fu2_purpose", "fu2_happiness", fu2_breakpoint)

# Create a summary table for piecewise regression results
create_piecewise_table <- function(piecewise_results, timepoint) {
  result_df <- data.frame(
    Quantile = numeric(),
    Low_Slope = numeric(),
    High_Slope = numeric(),
    Slope_Diff = numeric(),
    p_value = numeric(),
    Breakpoint = numeric(),
    Timepoint = character(),
    stringsAsFactors = FALSE
  )
  
  for (q in names(piecewise_results)) {
    result <- piecewise_results[[q]]
    result_df <- rbind(result_df, data.frame(
      Quantile = as.numeric(q),
      Low_Slope = result$low_slope,
      High_Slope = result$high_slope,
      Slope_Diff = result$high_slope - result$low_slope,
      p_value = result$p_value,
      Breakpoint = result$breakpoint,
      Timepoint = timepoint
    ))
  }
  
  return(result_df)
}

b_piecewise_table <- create_piecewise_table(b_piecewise, "Baseline")
fu1_piecewise_table <- create_piecewise_table(fu1_piecewise, "Follow-up 1")
fu2_piecewise_table <- create_piecewise_table(fu2_piecewise, "Follow-up 2")

# Combine all piecewise tables
all_piecewise <- rbind(b_piecewise_table, fu1_piecewise_table, fu2_piecewise_table)
all_piecewise <- all_piecewise[order(all_piecewise$Timepoint, all_piecewise$Quantile),]

# Format the piecewise table for display
formatted_piecewise <- all_piecewise %>%
  mutate(
    Quantile = paste0(Quantile * 100, "th"),
    Low_Slope = ifelse(is.na(Low_Slope), "NA", sprintf("%.2f", Low_Slope)),
    High_Slope = ifelse(is.na(High_Slope), "NA", sprintf("%.2f", High_Slope)),
    Slope_Diff = ifelse(is.na(Slope_Diff), "NA", sprintf("%.2f", Slope_Diff)),
    p_value = ifelse(is.na(p_value), "NA", sprintf("%.3f", p_value)),
    Significant = ifelse(!is.na(p_value) & p_value < 0.05, "*", "")
  )

# Print the formatted piecewise table
kable(formatted_piecewise, 
      caption = "Piecewise Quantile Regression: Slopes Below and Above the Breakpoint",
      col.names = c("Quantile", "Low Slope", "High Slope", "Diff", "p-value", "Breakpoint", "Timepoint", "Significant")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

# Additional analysis: Compare the relationship across different timepoints
# This can help understand how the relationship evolves over time

# Create a comparative plot for the median (50th percentile)
create_comparative_plot <- function(data) {
  # Create custom breaks for each timepoint to avoid "breaks are not unique" error
  b_purpose_values <- data$b_purpose[!is.na(data$b_purpose)]
  b_min <- min(b_purpose_values, na.rm = TRUE)
  b_max <- max(b_purpose_values, na.rm = TRUE)
  b_breaks <- seq(b_min, b_max, length.out = 11)
  
  fu1_purpose_values <- data$fu1_purpose[!is.na(data$fu1_purpose)]
  fu1_min <- min(fu1_purpose_values, na.rm = TRUE)
  fu1_max <- max(fu1_purpose_values, na.rm = TRUE)
  fu1_breaks <- seq(fu1_min, fu1_max, length.out = 11)
  
  fu2_purpose_values <- data$fu2_purpose[!is.na(data$fu2_purpose)]
  fu2_min <- min(fu2_purpose_values, na.rm = TRUE)
  fu2_max <- max(fu2_purpose_values, na.rm = TRUE)
  fu2_breaks <- seq(fu2_min, fu2_max, length.out = 11)
  
  # Calculate median happiness for each purpose bin at each timepoint
  median_data <- data %>%
    mutate(
      b_purpose_bin = cut(b_purpose, breaks = b_breaks, include.lowest = TRUE, labels = FALSE),
      fu1_purpose_bin = cut(fu1_purpose, breaks = fu1_breaks, include.lowest = TRUE, labels = FALSE),
      fu2_purpose_bin = cut(fu2_purpose, breaks = fu2_breaks, include.lowest = TRUE, labels = FALSE)
    ) %>%
    group_by(b_purpose_bin) %>%
    mutate(b_purpose_value = mean(b_purpose, na.rm = TRUE),
           b_happiness_median = median(b_happiness, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(fu1_purpose_bin) %>%
    mutate(fu1_purpose_value = mean(fu1_purpose, na.rm = TRUE),
           fu1_happiness_median = median(fu1_happiness, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(fu2_purpose_bin) %>%
    mutate(fu2_purpose_value = mean(fu2_purpose, na.rm = TRUE),
           fu2_happiness_median = median(fu2_happiness, na.rm = TRUE)) %>%
    ungroup()
  
  # Create a long format dataset for plotting
  b_data <- median_data %>%
    select(purpose = b_purpose_value, happiness = b_happiness_median) %>%
    mutate(timepoint = "Baseline") %>%
    distinct(purpose, happiness, timepoint)
  
  fu1_data <- median_data %>%
    select(purpose = fu1_purpose_value, happiness = fu1_happiness_median) %>%
    mutate(timepoint = "Follow-up 1") %>%
    distinct(purpose, happiness, timepoint)
  
  fu2_data <- median_data %>%
    select(purpose = fu2_purpose_value, happiness = fu2_happiness_median) %>%
    mutate(timepoint = "Follow-up 2") %>%
    distinct(purpose, happiness, timepoint)
  
  plot_data <- rbind(b_data, fu1_data, fu2_data) %>%
    filter(!is.na(purpose), !is.na(happiness))
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = purpose, y = happiness, color = timepoint)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = "Relationship Between Purpose and Happiness Across Timepoints",
      subtitle = "Showing median happiness values",
      x = "Purpose in Life Score",
      y = "Happiness Score",
      color = "Timepoint"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
  
  return(p)
}

# Create the comparative plot
comparative_plot <- create_comparative_plot(data)
print(comparative_plot)

# Save the results and plots
# Uncomment and run these lines to save the outputs
# ggsave("baseline_quantile_plot.png", b_plot, width = 10, height = 8)
# ggsave("followup1_quantile_plot.png", fu1_plot, width = 10, height = 8)
# ggsave("followup2_quantile_plot.png", fu2_plot, width = 10, height = 8)
# ggsave("comparative_plot.png", comparative_plot, width = 10, height = 8)
# write.csv(all_slopes, "quantile_regression_slopes.csv", row.names = FALSE)
# write.csv(all_piecewise, "piecewise_quantile_regression.csv", row.names = FALSE)

# Print a summary of findings
cat("\n----- Summary of Quantile Regression Analysis -----\n")
cat("This analysis examines the relationship between purpose and happiness at different\n")
cat("quantiles of the happiness distribution, similar to the approach by Killingsworth et al. (2023).\n\n")

cat("Key findings:\n")
cat("1. Slope patterns across quantiles: ")
for (timepoint in unique(all_slopes$Timepoint)) {
  timepoint_slopes <- all_slopes[all_slopes$Timepoint == timepoint,]
  min_slope <- min(as.numeric(timepoint_slopes$Slope))
  max_slope <- max(as.numeric(timepoint_slopes$Slope))
  cat(sprintf("\n   - %s: Slopes range from %.2f to %.2f", timepoint, min_slope, max_slope))
}

cat("\n\n2. Piecewise regression results: ")
for (timepoint in unique(all_piecewise$Timepoint)) {
  timepoint_pw <- all_piecewise[all_piecewise$Timepoint == timepoint,]
  significant_diff <- any(as.numeric(gsub("0\\.", ".", timepoint_pw$p_value)) < 0.05)
  if (significant_diff) {
    cat(sprintf("\n   - %s: Significant differences in slopes around the breakpoint", timepoint))
  } else {
    cat(sprintf("\n   - %s: No significant differences in slopes around the breakpoint", timepoint))
  }
}

cat("\n\n3. Comparative analysis across timepoints:\n")
cat("   - The relationship between purpose and happiness ")
# This will be completed based on the results
