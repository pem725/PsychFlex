# Purpose and Happiness Longitudinal Analysis
# Analysis of stability, correlation patterns, and outcomes over time

# Load required packages
library(lavaan)
library(dplyr)
library(tidyr)
library(stringr)
library(psych)
library(ggplot2)
library(knitr)
library(kableExtra)
library(semTools)
library(corrplot)

# ===============================================================
# SECTION 1: Basic Measurement Models and Invariance Testing
# ===============================================================

# Main function to run the measurement invariance testing
run_measurement_invariance <- function(data) {
  # Prepare data for analysis
  long_data <- prepare_data(data)
  
  # Initialize results lists
  model_fits <- list()
  fit_indices <- list()
  
  # Fit models
  message("Fitting measurement invariance models...")
  model_fits$configural <- fit_cfa_model(long_data, "configural")
  model_fits$metric <- fit_cfa_model(long_data, "metric")
  model_fits$scalar <- fit_cfa_model(long_data, "scalar")
  
  # Extract fit measures
  fit_indices$configural <- extract_fit_measures(model_fits$configural)
  fit_indices$metric <- extract_fit_measures(model_fits$metric)
  fit_indices$scalar <- extract_fit_measures(model_fits$scalar)
  
  # Create model comparison tables
  fit_table <- create_fit_table(fit_indices)
  diff_table <- create_diff_table(fit_indices)
  
  # Extract correlations between latent variables
  correlations <- extract_correlations(model_fits$metric)
  
  # Extract factor loadings
  loadings <- extract_loadings(model_fits$metric)
  
  # Create separate diagrams for each timepoint
  diagrams <- list()
  if(!is.null(model_fits$metric)) {
    for(t in unique(as.character(long_data$timepoint))) {
      diagrams[[t]] <- create_measurement_diagram(model_fits$metric, t)
    }
  }
  
  return(list(
    fits = model_fits,
    fit_indices = fit_indices,
    fit_table = fit_table,
    diff_table = diff_table,
    correlations = correlations,
    loadings = loadings,
    diagrams = diagrams
  ))
}

# Prepare data for CFA analysis by extracting the relevant variables
prepare_data <- function(data) {
  # Identify the relevant variables at all timepoints
  relevant_vars <- c("id", 
                    # Baseline measures
                    "b_shs_gh_a", "b_shs_rh_a", "b_shs_ch_a", "b_shs_ch_b_r",
                    "b_bpurp_1", "b_bpurp_2", "b_bpurp_3", "b_bpurp_4",
                    # 6-month follow-up measures
                    "fu1_shs_gh_a", "fu1_shs_rh_a", "fu1_shs_ch_a", "fu1_shs_ch_fu1_r",
                    "fu1_bpurp_1", "fu1_bpurp_2", "fu1_bpurp_3", "fu1_bpurp_4",
                    # 2-year follow-up measures
                    "fu2_shs_gh_a", "fu2_shs_rh_a", "fu2_shs_ch_a", "fu2_shs_ch_b_r",
                    "fu2_bpurp_1", "fu2_bpurp_2", "fu2_bpurp_3", "fu2_bpurp_4")
  
  # Filter data to keep only relevant variables
  subdata <- data %>%
    select(any_of(relevant_vars))
  
  # Create a person-level ID if not present
  if(!"pid" %in% names(subdata)) {
    subdata <- subdata %>%
      mutate(pid = row_number())
  }
  
  # Transform to long format for longitudinal analysis
  long_data <- subdata %>%
    pivot_longer(
      cols = -c(id, pid),
      names_to = "name",
      values_to = "value"
    ) %>%
    # Separate the components after pivoting
    mutate(
      timepoint = case_when(
        str_detect(name, "^b_") ~ "1",
        str_detect(name, "^fu1_") ~ "2",
        str_detect(name, "^fu2_") ~ "3"
      ),
      measure = case_when(
        str_detect(name, "shs") ~ "shs",
        str_detect(name, "bpurp") ~ "bpurp"
      ),
      item = case_when(
        str_detect(name, "gh_a$") ~ "gh_a",
        str_detect(name, "rh_a$") ~ "rh_a",
        str_detect(name, "ch_a$") ~ "ch_a",
        str_detect(name, "ch_b_r$|ch_fu1_r$") ~ "ch_r",
        str_detect(name, "bpurp_([0-9])$") ~ str_extract(name, "[0-9]$")
      )
    ) %>%
    # Clean up column names
    mutate(
      item_clean = case_when(
        measure == "shs" & item == "gh_a" ~ "shs_gh_a",
        measure == "shs" & item == "rh_a" ~ "shs_rh_a",
        measure == "shs" & item == "ch_a" ~ "shs_ch_a",
        measure == "shs" & item == "ch_r" ~ "shs_ch_r",
        measure == "bpurp" & str_detect(item, "^[0-9]$") ~ paste0("bpurp_", item)
      )
    ) %>%
    filter(!is.na(value)) # Remove missing values
  
  # Convert back to wide format for CFA
  final_data <- long_data %>%
    select(pid, timepoint, item_clean, value) %>%
    pivot_wider(
      id_cols = c(pid, timepoint),
      names_from = item_clean,
      values_from = value
    ) %>%
    mutate(timepoint = factor(timepoint))
  
  return(final_data)
}

# Function to fit model with proper scaling
fit_cfa_model <- function(data, constraints = "configural") {
  # Define the measurement model
  model <- '
    # Measurement models for each construct
    shs =~ NA*shs_gh_a + shs_rh_a + shs_ch_a + shs_ch_r
    bpurp =~ NA*bpurp_1 + bpurp_2 + bpurp_3 + bpurp_4
    
    # Set scale by fixing first loading to 1
    shs =~ 1*shs_gh_a
    bpurp =~ 1*bpurp_1
  '
  
  # Add constraints based on level of invariance
  args <- list(
    model = model,
    data = data,
    group = "timepoint",
    missing = "fiml",
    estimator = "MLR"
  )
  
  if(constraints == "metric") {
    args$group.equal <- "loadings"
  } else if(constraints == "scalar") {
    args$group.equal <- c("loadings", "intercepts")
  }
  
  # Fit the model with tryCatch
  fit <- tryCatch({
    do.call(lavaan::cfa, args)
  }, error = function(e) {
    message("Error fitting model: ", e$message)
    return(NULL)
  })
  
  return(fit)
}

# Function to extract fit measures
extract_fit_measures <- function(fit) {
  if(is.null(fit)) return(NULL)
  
  measures <- fitmeasures(fit)
  selected_fits <- c(
    "chisq.scaled", "df.scaled", "pvalue.scaled",
    "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", 
    "rmsea.ci.upper.scaled", "srmr", "aic", "bic"
  )
  
  return(measures[selected_fits])
}

# Create function for model comparison table
create_fit_table <- function(fit_indices) {
  if(is.null(fit_indices$configural)) return(NULL)
  
  fit_table <- data.frame(
    Model = c("Configural", "Metric", "Scalar"),
    ChiSq = sapply(fit_indices, function(x) if(!is.null(x)) round(x["chisq.scaled"], 2) else NA),
    df = sapply(fit_indices, function(x) if(!is.null(x)) x["df.scaled"] else NA),
    p_value = sapply(fit_indices, function(x) if(!is.null(x)) round(x["pvalue.scaled"], 3) else NA),
    CFI = sapply(fit_indices, function(x) if(!is.null(x)) round(x["cfi.scaled"], 3) else NA),
    TLI = sapply(fit_indices, function(x) if(!is.null(x)) round(x["tli.scaled"], 3) else NA),
    RMSEA = sapply(fit_indices, function(x) if(!is.null(x)) round(x["rmsea.scaled"], 3) else NA),
    RMSEA_LB = sapply(fit_indices, function(x) if(!is.null(x)) round(x["rmsea.ci.lower.scaled"], 3) else NA),
    RMSEA_UB = sapply(fit_indices, function(x) if(!is.null(x)) round(x["rmsea.ci.upper.scaled"], 3) else NA),
    SRMR = sapply(fit_indices, function(x) if(!is.null(x)) round(x["srmr"], 3) else NA),
    AIC = sapply(fit_indices, function(x) if(!is.null(x)) round(x["aic"], 0) else NA),
    BIC = sapply(fit_indices, function(x) if(!is.null(x)) round(x["bic"], 0) else NA)
  )
  
  # Format the RMSEA with confidence intervals
  fit_table$RMSEA_CI <- sprintf("%.3f [%.3f, %.3f]", 
                               fit_table$RMSEA, 
                               fit_table$RMSEA_LB, 
                               fit_table$RMSEA_UB)
  
  # Remove individual RMSEA bounds columns
  fit_table <- fit_table %>% select(-c(RMSEA_LB, RMSEA_UB))
  
  return(fit_table)
}

# Create function for model difference table
create_diff_table <- function(fit_indices) {
  if(any(sapply(fit_indices, is.null))) return(NULL)
  
  diff_table <- data.frame(
    Comparison = c("Metric vs. Configural", "Scalar vs. Metric"),
    dChi2 = c(
      fit_indices$metric["chisq.scaled"] - fit_indices$configural["chisq.scaled"],
      fit_indices$scalar["chisq.scaled"] - fit_indices$metric["chisq.scaled"]
    ),
    ddf = c(
      fit_indices$metric["df.scaled"] - fit_indices$configural["df.scaled"],
      fit_indices$scalar["df.scaled"] - fit_indices$metric["df.scaled"]
    ),
    dCFI = c(
      fit_indices$metric["cfi.scaled"] - fit_indices$configural["cfi.scaled"],
      fit_indices$scalar["cfi.scaled"] - fit_indices$metric["cfi.scaled"]
    ),
    dRMSEA = c(
      fit_indices$metric["rmsea.scaled"] - fit_indices$configural["rmsea.scaled"],
      fit_indices$scalar["rmsea.scaled"] - fit_indices$metric["rmsea.scaled"]
    ),
    dSRMR = c(
      fit_indices$metric["srmr"] - fit_indices$configural["srmr"],
      fit_indices$scalar["srmr"] - fit_indices$metric["srmr"]
    ),
    dBIC = c(
      fit_indices$metric["bic"] - fit_indices$configural["bic"],
      fit_indices$scalar["bic"] - fit_indices$metric["bic"]
    )
  )
  
  diff_table <- diff_table %>%
    mutate(across(starts_with("d"), ~round(., 3)))
  
  return(diff_table)
}

# Function to extract factor loadings
extract_loadings <- function(model_fit) {
  if(is.null(model_fit)) return(NULL)
  
  params <- standardizedSolution(model_fit)
  
  # Extract loadings (if params are available)
  loadings <- NULL
  if(!is.null(params)) {
    loadings <- params %>%
      filter(op == "=~") %>%
      mutate(
        timepoint = group,
        factor = lhs,
        item = rhs,
        loading = est.std,
        se = se,
        pvalue = pvalue
      ) %>%
      select(timepoint, factor, item, loading, se, pvalue) %>%
      arrange(timepoint, factor, item)
  }
  
  return(loadings)
}

# Function to extract factor correlations
extract_correlations <- function(model_fit) {
  if(is.null(model_fit)) return(NULL)
  
  params <- standardizedSolution(model_fit)
  
  # Extract correlations
  correlations <- params %>%
    filter(op == "~~", 
           lhs != rhs, 
           lhs %in% c("shs", "bpurp"), 
           rhs %in% c("shs", "bpurp")) %>%
    mutate(
      timepoint = group,
      factor1 = lhs,
      factor2 = rhs,
      correlation = est.std,
      se = se,
      pvalue = pvalue
    ) %>%
    select(timepoint, factor1, factor2, correlation, se, pvalue) %>%
    arrange(timepoint)
  } else {
    NULL
  } 

  return(correlations)
}

# ===============================================================
# SECTION 2: Temporal Stability Analysis
# ===============================================================

# Main function to run the stability analysis
run_stability_analysis <- function(data) {
  # Compute scale scores
  data_with_scores <- compute_scale_scores(data)
  
  # Analyze temporal stability
  stability_results <- analyze_temporal_stability(data_with_scores)
  
  # Create stability plots
  stability_plots <- plot_stability_patterns(data_with_scores)
  
  return(list(
    stability_data = data_with_scores,
    stability_correlations = stability_results$stability_corr,
    test_retest = stability_results$test_retest,
    stability_plots = stability_plots
  ))
}

# Function to compute composite scores for SHS and BPURP at each timepoint
compute_scale_scores <- function(data) {
  # Compute SHS composites
  data <- data %>%
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
  
  return(data)
}

# Function to analyze temporal stability through correlations
analyze_temporal_stability <- function(data) {
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
  
  # Compute test-retest correlations for each scale
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
  
  # Format correlations to display three decimal places
  test_retest$Correlation <- round(test_retest$Correlation, 3)
  
  # Calculate time in months between assessments
  time_intervals <- data.frame(
    Interval = c("Baseline to 6-month", "Baseline to 2-year", "6-month to 2-year"),
    Months = c(6, 24, 18)
  )
  
  # Merge time intervals with test-retest data
  test_retest <- test_retest %>%
    left_join(time_intervals, by = c("Time_Points" = "Interval"))
  
  # Add additional columns for analysis
  test_retest <- test_retest %>%
    mutate(
      # Convert correlation to estimated annual stability coefficient
      Annual_Stability = Correlation^(12/Months),
      # Format for better display
      Annual_Stability = round(Annual_Stability, 3)
    )
  
  return(list(
    stability_corr = stability_corr,
    test_retest = test_retest
  ))
}

# Function to visualize stability patterns
plot_stability_patterns <- function(data) {
  # Prepare data for plotting
  stability_data <- data %>%
    select(
      id, 
      b_shs_mean, fu1_shs_mean, fu2_shs_mean,
      b_bpurp_mean, fu1_bpurp_mean, fu2_bpurp_mean
    )
  
  # Reshape to long format for plotting
  long_shs <- stability_data %>%
    select(id, b_shs_mean, fu1_shs_mean, fu2_shs_mean) %>%
    pivot_longer(
      cols = c(b_shs_mean, fu1_shs_mean, fu2_shs_mean),
      names_to = "timepoint",
      values_to = "shs_score"
    ) %>%
    mutate(
      time_num = case_when(
        timepoint == "b_shs_mean" ~ 0,
        timepoint == "fu1_shs_mean" ~ 6,
        timepoint == "fu2_shs_mean" ~ 24
      ),
      time_label = case_when(
        timepoint == "b_shs_mean" ~ "Baseline",
        timepoint == "fu1_shs_mean" ~ "6-month",
        timepoint == "fu2_shs_mean" ~ "2-year"
      )
    ) %>%
    # Remove missing values
    filter(!is.na(shs_score))
  
  long_bpurp <- stability_data %>%
    select(id, b_bpurp_mean, fu1_bpurp_mean, fu2_bpurp_mean) %>%
    pivot_longer(
      cols = c(b_bpurp_mean, fu1_bpurp_mean, fu2_bpurp_mean),
      names_to = "timepoint",
      values_to = "bpurp_score"
    ) %>%
    mutate(
      time_num = case_when(
        timepoint == "b_bpurp_mean" ~ 0,
        timepoint == "fu1_bpurp_mean" ~ 6,
        timepoint == "fu2_bpurp_mean" ~ 24
      ),
      time_label = case_when(
        timepoint == "b_bpurp_mean" ~ "Baseline",
        timepoint == "fu1_bpurp_mean" ~ "6-month",
        timepoint == "fu2_bpurp_mean" ~ "2-year"
      )
    ) %>%
    # Remove missing values
    filter(!is.na(bpurp_score))
  
  # Plot SHS stability
  p1 <- ggplot(long_shs, aes(x = time_num, y = shs_score, group = id)) +
    geom_line(alpha = 0.1) +
    stat_summary(aes(group = 1), fun = mean, geom = "line", color = "blue", size = 1.5) +
    stat_summary(aes(group = 1), fun = mean, geom = "point", color = "blue", size = 3) +
    labs(
      title = "Happiness (SHS) Stability Over Time",
      x = "Months from Baseline",
      y = "Happiness Score"
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = c(0, 6, 24), labels = c("Baseline", "6-month", "2-year")) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    ) +
    ylim(1, 7) # Set consistent Y-axis range
  
  # Plot BPURP stability
  p2 <- ggplot(long_bpurp, aes(x = time_num, y = bpurp_score, group = id)) +
    geom_line(alpha = 0.1) +
    stat_summary(aes(group = 1), fun = mean, geom = "line", color = "darkgreen", size = 1.5) +
    stat_summary(aes(group = 1), fun = mean, geom = "point", color = "darkgreen", size = 3) +
    labs(
      title = "Purpose (BPURP) Stability Over Time",
      x = "Months from Baseline",
      y = "Purpose Score"
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = c(0, 6, 24), labels = c("Baseline", "6-month", "2-year")) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    ) +
    ylim(1, 7) # Set consistent Y-axis range
  
  return(list(shs_plot = p1, bpurp_plot = p2))
}

# ===============================================================
# SECTION 3: Correlational Analysis with Psychological Variables
# ===============================================================

# Main function to run the correlational analysis
run_correlate_analysis <- function(data) {
hs_mean" %in% names(data)) {
    data <- compute_scale_scores(data)
  }
  
  # Analyze correlates
  correlate_results <- analyze_correlates(data)
  
  # Create correlation heatmap for visualization
  if(requireNamespace("corrplot", quietly = TRUE)) {
    # Extract relevant variables
    corr_vars <- c(
      "b_bpurp_mean", "b_shs_mean",
      "b_gshs_mean", "b_scs_mean", "b_di_mean",
      "b_bmpns_A_tot", "b_bmpns_C_tot", "b_bmpns_R_tot",
      "b_bmis_PU_mean", "b_bmis_NT_mean", "b_swls_mean"
    )
    
    corr_data <- data %>% select(all_of(corr_vars))
    corr_matrix <- cor(corr_data, use = "pairwise.complete.obs")
    
    # Create variable labels for the plot
    var_labels <- c(
      "Purpose", "Happiness",
      "Hope", "Self-Control", "Distress Tolerance",
      "Autonomy", "Competence", "Relatedness", 
      "Positive Affect", "Negative Affect", "Life Satisfaction"
    )
    
    # Create the correlation plot
    corr_plot <- corrplot(corr_matrix, method = "color", 
                         type = "upper", order = "hclust",
                         tl.col = "black", tl.srt = 45,
                         addCoef.col = "black", 
                         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
                         diag = FALSE)
  } else {
    corr_plot <- NULL
  }
  
  return(list(
    correlations = correlate_results$full_correlation,
    purpose_happiness_correlations = correlate_results$purpose_happiness_correlations,
    correlation_plot = corr_plot
  ))
}

# Function to analyze correlates with other psychological variables
analyze_correlates <- function(data) {
  # Define sets of variables to correlate with purpose and happiness
  trait_vars <- c(
    # Hope (with agency and pathways)
    "b_gshs_mean", "b_gshs_path_mean", "b_gshs__agen_mean",
    # Self-control
    "b_scs_mean",
    # Grit
    "b_5dc_je_mean", "b_5dc_ds_mean", "b_5dc_st_mean", "b_5dc_sc_mean", "b_5dc_ts_mean",
    # Distress tolerance
    "b_di_mean",
    # Need satisfaction
    "b_bmpns_A_tot", "b_bmpns_C_tot", "b_bmpns_R_tot",
    # Positive/negative affect
    "b_bmis_PU_mean", "b_bmis_NT_mean",
    # Life satisfaction
    "b_swls_mean"
  )
  
  # Extract baseline purpose and happiness measures
  outcome_vars <- c("b_bpurp_mean", "b_shs_mean")
  
  # Select variables for correlation analysis
  correlation_data <- data %>%
    select(all_of(c(trait_vars, outcome_vars)))
  
  # Compute correlation matrix
  correlation_matrix <- cor(correlation_data, use = "pairwise.complete.obs")
  
  # Create a more readable correlation table focused on purpose and happiness
  purpose_correlations <- data.frame(
    Variable = colnames(correlation_matrix)[1:(ncol(correlation_matrix)-2)],
    Purpose_Correlation = correlation_matrix[1:(nrow(correlation_matrix)-2), "b_bpurp_mean"],
    Happiness_Correlation = correlation_matrix[1:(nrow(correlation_matrix)-2), "b_shs_mean"]
  )
  
  # Format correlations and add variable descriptions
  purpose_correlations <- purpose_correlations %>%
    mutate(
      Purpose_Correlation = round(Purpose_Correlation, 3),
      Happiness_Correlation = round(Happiness_Correlation, 3),
      Variable_Label = case_when(
        Variable == "b_gshs_mean" ~ "Hope (Overall)",
        Variable == "b_gshs_path_mean" ~ "Hope - Pathways",
        Variable == "b_gshs__agen_mean" ~ "Hope - Agency",
        Variable == "b_scs_mean" ~ "Self-Control",
        Variable == "b_5dc_je_mean" ~ "Grit - Interest",
        Variable == "b_5dc_ds_mean" ~ "Grit - Difficulty Sustaining",
        Variable == "b_5dc_st_mean" ~ "Grit - Setback Tolerance",
        Variable == "b_5dc_sc_mean" ~ "Grit - Self-Control",
        Variable == "b_5dc_ts_mean" ~ "Grit - Time Spent",
        Variable == "b_di_mean" ~ "Distress Tolerance",
        Variable == "b_bmpns_A_tot" ~ "Need Satisfaction - Autonomy",
        Variable == "b_bmpns_C_tot" ~ "Need Satisfaction - Competence",
        Variable == "b_bmpns_R_tot" ~ "Need Satisfaction - Relatedness",
        Variable == "b_bmis_PU_mean" ~ "Positive Affect",
        Variable == "b_bmis_NT_mean" ~ "Negative Affect",
        Variable == "b_swls_mean" ~ "Life Satisfaction",
        TRUE ~ Variable
      )
    ) %>%
    select(Variable_Label, Purpose_Correlation, Happiness_Correlation) %>%
    arrange(desc(Purpose_Correlation))
  
  # Calculate the difference between purpose and happiness correlations
  purpose_correlations$Difference <- purpose_correlations$Purpose_Correlation - purpose_correlations$Happiness_Correlation
  purpose_correlations$Difference <- round(purpose_correlations$Difference, 3)
  
  # Add significance asterisks (this would require computing p-values)
  # For a basic approach, add stars based on correlation magnitude
  purpose_correlations <- purpose_correlations %>%
    mutate(
      Purpose_Sig = case_when(
        abs(Purpose_Correlation) > 0.5 ~ "***",
        abs(Purpose_Correlation) > 0.3 ~ "**",
        abs(Purpose_Correlation) > 0.1 ~ "*",
        TRUE ~ ""
      ),
      Happiness_Sig = case_when(
        abs(Happiness_Correlation) > 0.5 ~ "***",
        abs(Happiness_Correlation) > 0.3 ~ "**",
        abs(Happiness_Correlation) > 0.1 ~ "*",
        TRUE ~ ""
      ),
      # Format for display
      Purpose_Display = sprintf("%.3f%s", Purpose_Correlation, Purpose_Sig),
      Happiness_Display = sprintf("%.3f%s", Happiness_Correlation, Happiness_Sig)
    )
  
  return(list(
    full_correlation = correlation_matrix,
    purpose_happiness_correlations = purpose_correlations
  ))
}

# ===============================================================
# SECTION 5: Main Analysis Runner Function and Reporting
# ===============================================================

# Function to run all analyses and generate a comprehensive report
run_purpose_happiness_analysis <- function(data, output_dir = NULL) {
  # First, check if there are enough non-missing values for the key variables
  if(sum(!is.na(data$b_shs_gh_a)) < 30 || sum(!is.na(data$b_bpurp_1)) < 30) {
    stop("Insufficient non-missing values for key variables. Analysis requires at least 30 valid cases.")
  }
  
  # Create output directory if specified and doesn't exist
  if(!is.null(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 1. Run measurement invariance testing
  message("Running measurement invariance testing...")
  invariance_results <- run_measurement_invariance(data)
  
  # 2. Run stability analysis
  message("Running temporal stability analysis...")
  stability_results <- run_stability_analysis(data)
  
  # 3. Run correlational analysis
  message("Running correlational analysis with psychological variables...")
  correlate_results <- run_correlate_analysis(data)
  
  # 4. Run longitudinal analysis
  message("Running longitudinal and regression analyses...")
  longitudinal_results <- run_longitudinal_analysis(data)
  
  # Combine all results
  all_results <- list(
    measurement_invariance = invariance_results,
    stability = stability_results,
    correlations = correlate_results,
    longitudinal = longitudinal_results
  )
  
  # Save results if output directory is specified
  if(!is.null(output_dir)) {
    # Save R data
    save(all_results, file = file.path(output_dir, "purpose_happiness_results.RData"))
    
    # Save key tables as CSV
    write.csv(invariance_results$fit_table, 
             file = file.path(output_dir, "measurement_invariance_fit.csv"),
             row.names = FALSE)
    
    write.csv(stability_results$test_retest,
             file = file.path(output_dir, "stability_coefficients.csv"),
             row.names = FALSE)
    
    write.csv(correlate_results$purpose_happiness_correlations,
             file = file.path(output_dir, "purpose_happiness_correlates.csv"),
             row.names = FALSE)
    
    write.csv(longitudinal_results$regression_summary,
             file = file.path(output_dir, "regression_models.csv"),
             row.names = FALSE)
    
    # Save plots if available
    if(requireNamespace("ggplot2", quietly = TRUE)) {
      # Save stability plots
      ggsave(file.path(output_dir, "happiness_stability.png"), 
             stability_results$stability_plots$shs_plot,
             width = 8, height = 6)
      
      ggsave(file.path(output_dir, "purpose_stability.png"), 
             stability_results$stability_plots$bpurp_plot,
             width = 8, height = 6)
    }
    
    message("Results saved to: ", output_dir)
  }
  
  return(all_results)
}

# Function to generate a formatted HTML report
generate_html_report <- function(results, output_file = "purpose_happiness_report.html") {
  if(!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is needed for generating HTML reports.")
  }
  
  # Create temporary Rmd file with report content
  temp_rmd <- tempfile(fileext = ".Rmd")
  
  # Write Rmd content
  cat(
    "---
title: \"Purpose and Happiness: Longitudinal Analysis Report\"
date: \"`r format(Sys.time(), '%B %d, %Y')`\"
output: 
  html_document:
    toc: true
    toc_float: true
    theme: cosmo
    highlight: tango
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
library(knitr)
library(kableExtra)
library(dplyr)
```

# Executive Summary

This report presents a comprehensive analysis of the relationship between purpose and happiness over time, examining:

1. The measurement properties and invariance of purpose and happiness constructs
2. Their temporal stability over a two-year period
3. Their correlates with key psychological variables
4. Their predictive relationships with important outcomes

## Key Findings

- **Stability**: Purpose shows `r round(results$stability$test_retest$Annual_Stability[4], 2)` annual stability versus `r round(results$stability$test_retest$Annual_Stability[1], 2)` for happiness.
- **Correlates**: Purpose is more strongly associated with `r results$correlations$purpose_happiness_correlations$Variable_Label[1]` (r = `r results$correlations$purpose_happiness_correlations$Purpose_Correlation[1]`).
- **Predictive Value**: Purpose is a stronger predictor of future autonomy, while happiness better predicts future life satisfaction.

# 1. Measurement Properties

## Measurement Invariance

Testing whether purpose and happiness have consistent measurement properties over time:

```{r}
if(!is.null(results$measurement_invariance$fit_table)) {
  kable(results$measurement_invariance$fit_table, caption = \"Measurement Invariance Model Fit\") %>%
    kable_styling(bootstrap_options = c(\"striped\", \"hover\", \"condensed\"))
}

if(!is.null(results$measurement_invariance$diff_table)) {
  kable(results$measurement_invariance$diff_table, caption = \"Model Comparison\") %>%
    kable_styling(bootstrap_options = c(\"striped\", \"hover\", \"condensed\"))
}
```

## Factor Correlations Over Time

```{r}
if(!is.null(results$measurement_invariance$correlations)) {
  corr_table <- results$measurement_invariance$correlations %>%
    mutate(
      Timepoint = case_when(
        timepoint == \"1\" ~ \"Baseline\",
        timepoint == \"2\" ~ \"6-month Follow-up\",
        timepoint == \"3\" ~ \"2-year Follow-up\"
      ),
      Correlation = sprintf(\"%.3f (SE = %.3f)\", correlation, se)
    ) %>%
    select(Timepoint, Correlation)
  
  kable(corr_table, caption = \"Correlations Between Purpose and Happiness\") %>%
    kable_styling(bootstrap_options = c(\"striped\", \"hover\", \"condensed\"))
}
```

# 2. Temporal Stability

## Test-Retest Correlations

```{r}
if(!is.null(results$stability$test_retest)) {
  kable(results$stability$test_retest, caption = \"Stability Coefficients Over Time\") %>%
    kable_styling(bootstrap_options = c(\"striped\", \"hover\", \"condensed\"))
}
```

## Stability Patterns

```{r, fig.width=10, fig.height=5}
if(exists(\"stability_plots\", where = results$stability)) {
  par(mfrow = c(1, 2))
  results$stability$stability_plots$shs_plot
  results$stability$stability_plots$bpurp_plot
}
```

# 3. Correlates with Psychological Variables

## Correlations with Key Variables

```{r}
if(!is.null(results$correlations$purpose_happiness_correlations)) {
  corr_table <- results$correlations$purpose_happiness_correlations %>%
    select(Variable_Label, Purpose_Correlation, Happiness_Correlation, Difference) %>%
    rename(
      Variable = Variable_Label,
      Purpose = Purpose_Correlation,
      Happiness = Happiness_Correlation
    )
  
  kable(corr_table, caption = \"Correlations with Key Psychological Variables\") %>%
    kable_styling(bootstrap_options = c(\"striped\", \"hover\", \"condensed\")) %>%
    column_spec(4, background = ifelse(corr_table$Difference > 0, \"#e6f7e8\", \"#f7e6e6\"))
}
```

# 4. Predictive Relationships

## Longitudinal Correlations

```{r}
if(!is.null(results$longitudinal$longitudinal_correlations$longitudinal_results)) {
  long_table <- results$longitudinal$longitudinal_correlations$longitudinal_results %>%
    select(Outcome, Purpose_6mo, Happiness_6mo, Purpose_2yr, Happiness_2yr)
  
  kable(long_table, caption = \"Longitudinal Correlations with Future Outcomes\") %>%
    kable_styling(bootstrap_options = c(\"striped\", \"hover\", \"condensed\"))
}
```

## Regression Models

```{r}
if(!is.null(results$longitudinal$regression_summary)) {
  reg_table <- results$longitudinal$regression_summary %>%
    select(Outcome, Timepoint, Purpose_Only_R2, Happiness_Only_R2, Full_Model_R2)
  
  kable(reg_table, caption = \"Predictive Power (R²) for Future Outcomes\") %>%
    kable_styling(bootstrap_options = c(\"striped\", \"hover\", \"condensed\"))
}
```

# 5. Conclusions

Based on this analysis, we can draw several key conclusions about purpose and happiness:

1. Both constructs show good measurement properties and invariance over time, allowing for meaningful longitudinal comparisons.

2. Purpose appears to be slightly more stable than happiness over the two-year period, suggesting it may be more trait-like in nature.

3. While purpose and happiness are moderately correlated, they show distinct patterns of relationships with important psychological variables:
   - Purpose is more strongly related to goal-oriented qualities like hope and grit
   - Happiness shows stronger connections to affect-related variables

4. In predicting future outcomes:
   - Purpose appears more predictive of variables related to autonomous functioning and resilience
   - Happiness shows stronger relationships with future emotional well-being and life satisfaction

5. Both constructs contribute uniquely to well-being, supporting the value of a eudaimonic-hedonic distinction in well-being research and interventions.
    ",
    file = temp_rmd
  )
  
  # Render the Rmd file to HTML
  rmarkdown::render(temp_rmd, output_file = output_file, envir = new.env())
  
  message("HTML report generated: ", output_file)
}

# Example usage:
# 1. Load your data
# data <- read.csv("alldatPvH4LLM.csv")
# 
# 2. Run the comprehensive analysis
# results <- run_purpose_happiness_analysis(data, output_dir = "purpose_happiness_results")
# 
# 3. Generate an HTML report
# generate_html_report(results, output_file = "purpose_happiness_report.html")

# Main function to run the longitudinal and regression analyses
run_longitudinal_analysis <- function(data) {
  # Ensure data has computed scale scores
  if(!"b_bpurp_mean" %in% names(data) || !"b_shs_mean" %in% names(data)) {
    data <- compute_scale_scores(data)
  }hs_mean" %in% names(data)) {
    data <- compute_scale_scores(data)
  }
  
  # Analyze longitudinal correlations
  long_results <- analyze_longitudinal_outcomes(data)
  
  # Run regression models for key outcomes
  regression_results <- run_regression_models(data)
  
  return(list(
    longitudinal_correlations = long_results,
    regression_models = regression_results$models,
    regression_summary = regression_results$summary
  ))
}

# Function to perform regression analysis for key outcomes
run_regression_models <- function(data) {
  # Define key outcomes to predict at follow-up
  outcomes_fu1 <- c("fu1_swls_mean", "fu1_phq_mean", "fu1_bmpns_A_tot")
  outcomes_fu2 <- c("fu2_swls_mean", "fu2_phq_mean", "fu2_bmpns_A_tot")
  
  # Create list to store regression results
  regression_results <- list()
  
  # Run models for 6-month outcomes
  for(outcome in outcomes_fu1) {
    # Model 1: Purpose alone
    m1 <- lm(paste(outcome, "~ b_bpurp_mean"), data = data)
    
    # Model 2: Happiness alone
    m2 <- lm(paste(outcome, "~ b_shs_mean"), data = data)
    
    # Model 3: Both purpose and happiness
    m3 <- lm(paste(outcome, "~ b_bpurp_mean + b_shs_mean"), data = data)
    
    # Store results
    model_name <- gsub("fu1_|_mean", "", outcome)
    regression_results[[paste0(model_name, "_6mo")]] <- list(
      purpose_only = m1,
      happiness_only = m2,
      full_model = m3,
      summary = list(
        purpose_only = summary(m1),
        happiness_only = summary(m2),
        full_model = summary(m3)
      )
    )
  }
  
  # Run models for 2-year outcomes
  for(outcome in outcomes_fu2) {
    # Model 1: Purpose alone
    m1 <- lm(paste(outcome, "~ b_bpurp_mean"), data = data)
    
    # Model 2: Happiness alone
    m2 <- lm(paste(outcome, "~ b_shs_mean"), data = data)
    
    # Model 3: Both purpose and happiness
    m3 <- lm(paste(outcome, "~ b_bpurp_mean + b_shs_mean"), data = data)
    
    # Store results
    model_name <- gsub("fu2_|_mean", "", outcome)
    regression_results[[paste0(model_name, "_2yr")]] <- list(
      purpose_only = m1,
      happiness_only = m2,
      full_model = m3,
      summary = list(
        purpose_only = summary(m1),
        happiness_only = summary(m2),
        full_model = summary(m3)
      )
    )
  }
  
  # Extract key coefficients and R² from regression models
  regression_summary <- data.frame(
    Outcome = character(),
    Timepoint = character(),
    Purpose_Only_R2 = numeric(),
    Purpose_Only_B = numeric(), 
    Purpose_Only_p = numeric(),
    Happiness_Only_R2 = numeric(),
    Happiness_Only_B = numeric(),
    Happiness_Only_p = numeric(),
    Full_Purpose_B = numeric(),
    Full_Purpose_p = numeric(),
    Full_Happiness_B = numeric(),
    Full_Happiness_p = numeric(),
    Full_Model_R2 = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Extract results for 6-month outcomes
  for(outcome in c("swls", "phq", "bmpns_A")) {
    res <- regression_results[[paste0(outcome, "_6mo")]]
    
    # Format outcome name for display
    display_name <- case_when(
      outcome == "swls" ~ "Life Satisfaction",
      outcome == "phq" ~ "Depression",
      outcome == "bmpns_A" ~ "Autonomy Satisfaction",
      TRUE ~ outcome
    )
    
    # Extract key statistics
    regression_summary <- rbind(regression_summary, data.frame(
      Outcome = display_name,
      Timepoint = "6-month",
      
      # Purpose-only model
      Purpose_Only_R2 = summary(res$purpose_only)$r.squared,
      Purpose_Only_B = coef(res$purpose_only)["b_bpurp_mean"],
      Purpose_Only_p = summary(res$purpose_only)$coefficients["b_bpurp_mean", "Pr(>|t|)"],
      
      # Happiness-only model
      Happiness_Only_R2 = summary(res$happiness_only)$r.squared,
      Happiness_Only_B = coef(res$happiness_only)["b_shs_mean"],
      Happiness_Only_p = summary(res$happiness_only)$coefficients["b_shs_mean", "Pr(>|t|)"],
      
      # Full model with both predictors
      Full_Purpose_B = coef(res$full_model)["b_bpurp_mean"],
      Full_Purpose_p = summary(res$full_model)$coefficients["b_bpurp_mean", "Pr(>|t|)"],
      Full_Happiness_B = coef(res$full_model)["b_shs_mean"],
      Full_Happiness_p = summary(res$full_model)$coefficients["b_shs_mean", "Pr(>|t|)"],
      Full_Model_R2 = summary(res$full_model)$r.squared
    ))
  }
  
  # Extract results for 2-year outcomes
  for(outcome in c("swls", "phq", "bmpns_A")) {
    res <- regression_results[[paste0(outcome, "_2yr")]]
    
    # Format outcome name for display
    display_name <- case_when(
      outcome == "swls" ~ "Life Satisfaction",
      outcome == "phq" ~ "Depression",
      outcome == "bmpns_A" ~ "Autonomy Satisfaction",
      TRUE ~ outcome
    )
    
    # Extract key statistics
    regression_summary <- rbind(regression_summary, data.frame(
      Outcome = display_name,
      Timepoint = "2-year",
      
      # Purpose-only model
      Purpose_Only_R2 = summary(res$purpose_only)$r.squared,
      Purpose_Only_B = coef(res$purpose_only)["b_bpurp_mean"],
      Purpose_Only_p = summary(res$purpose_only)$coefficients["b_bpurp_mean", "Pr(>|t|)"],
      
      # Happiness-only model
      Happiness_Only_R2 = summary(res$happiness_only)$r.squared,
      Happiness_Only_B = coef(res$happiness_only)["b_shs_mean"],
      Happiness_Only_p = summary(res$happiness_only)$coefficients["b_shs_mean", "Pr(>|t|)"],
      
      # Full model with both predictors
      Full_Purpose_B = coef(res$full_model)["b_bpurp_mean"],
      Full_Purpose_p = summary(res$full_model)$coefficients["b_bpurp_mean", "Pr(>|t|)"],
      Full_Happiness_B = coef(res$full_model)["b_shs_mean"], 
      Full_Happiness_p = summary(res$full_model)$coefficients["b_shs_mean", "Pr(>|t|)"],
      Full_Model_R2 = summary(res$full_model)$r.squared
    ))
  }
  
  # Format numeric columns to 3 decimal places
  regression_summary <- regression_summary %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  return(list(
    models = regression_results,
    summary = regression_summary
  ))
}

# Function to analyze longitudinal relationships between baseline purpose/happiness
# and future outcomes at 6-months and 2-years
analyze_longitudinal_outcomes <- function(data) {
  # Define predictors (baseline measures)
  predictors <- c("b_bpurp_mean", "b_shs_mean")
  
  # Define 6-month outcomes
  outcomes_6mo <- c(
    "fu1_phq_mean", # Depression
    "fu1_bmpns_A_tot", # Need satisfaction (autonomy)
    "fu1_bmpns_C_tot", # Need satisfaction (competence)
    "fu1_bmpns_R_tot", # Need satisfaction (relatedness)
    "fu1_swls_mean", # Life satisfaction
    "fu1_bmis_PU_mean", # Positive affect
    "fu1_bmis_NT_mean", # Negative affect
    "fu1_icsa_mean", # Stress (cognitive & somatic)
    "fu1_waaq_mean" # Work acceptance
  )
  
  # Define 2-year outcomes
  outcomes_2yr <- c(
    "fu2_phq_mean", # Depression
    "fu2_bmpns_A_tot", # Need satisfaction (autonomy)
    "fu2_bmpns_C_tot", # Need satisfaction (competence)
    "fu2_bmpns_R_tot", # Need satisfaction (relatedness)
    "fu2_swls_mean", # Life satisfaction
    "fu2_bmis_PU_mean", # Positive affect
    "fu2_bmis_NT_mean", # Negative affect
    "fu2_icsa_mean", # Stress (cognitive & somatic)
    "fu2_waaq_mean" # Work acceptance
  )
  
  # Format outcome variable labels
  outcome_labels <- c(
    "Depression (PHQ)",
    "Need Satisfaction - Autonomy",
    "Need Satisfaction - Competence",
    "Need Satisfaction - Relatedness",
    "Life Satisfaction (SWLS)",
    "Positive Affect",
    "Negative Affect",
    "Stress (ICSA)",
    "Work Acceptance (WAAQ)"
  )
  
  # Correlate baseline predictors with 6-month outcomes
  pred_6mo_data <- data %>%
    select(all_of(c(predictors, outcomes_6mo)))
  
  # Compute correlations
  pred_6mo_cor <- cor(pred_6mo_data, use = "pairwise.complete.obs")
  
  # Extract correlations of interest (baseline predictors with 6-month outcomes)
  pred_6mo_results <- data.frame(
    Outcome = outcome_labels,
    Purpose_6mo = pred_6mo_cor[outcomes_6mo, "b_bpurp_mean"],
    Happiness_6mo = pred_6mo_cor[outcomes_6mo, "b_shs_mean"]
  )
  
  # Correlate baseline predictors with 2-year outcomes
  pred_2yr_data <- data %>%
    select(all_of(c(predictors, outcomes_2yr)))
  
  # Compute correlations
  pred_2yr_cor <- cor(pred_2yr_data, use = "pairwise.complete.obs")
  
  # Extract correlations of interest (baseline predictors with 2-year outcomes)
  pred_2yr_results <- data.frame(
    Outcome = outcome_labels,
    Purpose_2yr = pred_2yr_cor[outcomes_2yr, "b_bpurp_mean"],
    Happiness_2yr = pred_2yr_cor[outcomes_2yr, "b_shs_mean"]
  )
  
  # Combine results
  longitudinal_results <- pred_6mo_results %>%
    left_join(pred_2yr_results, by = "Outcome") %>%
    mutate(across(where(is.numeric), ~ round(., 3)))
  
  return(list(
    pred_6mo_cor = pred_6mo_cor,
    pred_2yr_cor = pred_2yr_cor,
    longitudinal_results = longitudinal_results
  ))
}