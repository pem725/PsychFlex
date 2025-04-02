# Load required packages
library(lavaan)
library(dplyr)
library(tidyr)
library(stringr)
library(DiagrammeR)
library(knitr)
library(kableExtra)
library(ggplot2)
library(gridExtra)

# Prepare data for analysis with debug info
prepare_data <- function(data) {
  # First, create a proper person-level ID
  data_with_pid <- data %>%
    mutate(pid = row_number())
  
  # Create long format for all measurements
  data_long <- tryCatch({
    long_data <- data_with_pid %>%
      pivot_longer(
        cols = -c(id, pid),
        names_to = "name",
        values_to = "value"
      ) %>%
      # Separate the components after pivoting
      mutate(
        timepoint = str_extract(name, "^(b|fu1|fu2)"),
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
      select(-name)
    
    # Add diagnostic info to console only if needed
    if(getOption("verbose", default = FALSE)) {
      message("\nUnique timepoints after pivot:")
      print(unique(long_data$timepoint))
      message("\nUnique measures after pivot:")
      print(unique(long_data$measure))
      message("\nUnique items after pivot:")
      print(unique(long_data$item))
    }
    
    long_data <- long_data %>%
      mutate(
        timepoint = case_when(
          timepoint == "b" ~ "1",
          timepoint == "fu1" ~ "2",
          timepoint == "fu2" ~ "3"
        ),
        item_clean = case_when(
          measure == "shs" & str_detect(item, "gh_a") ~ "shs_gh_a",
          measure == "shs" & str_detect(item, "rh_a") ~ "shs_rh_a",
          measure == "shs" & str_detect(item, "ch_a") ~ "shs_ch_a",
          measure == "shs" & str_detect(item, "ch_r") ~ "shs_ch_r",
          measure == "bpurp" & str_detect(item, "^[0-9]$") ~ paste0("bpurp_", item)
        )
      )
    
    final_data <- long_data %>%
      select(pid, timepoint, item_clean, value) %>%
      pivot_wider(
        id_cols = c(pid, timepoint),
        names_from = item_clean,
        values_from = value
      ) %>%
      select(-pid) %>%
      mutate(timepoint = factor(timepoint))
    
    return(final_data)
  }, error = function(e) {
    message("Error in data preparation: ", e$message)
    return(NULL)
  })
  
  return(data_long)
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

# Improved function to extract fit measures
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

# Enhanced function for creating the measurement diagram
create_measurement_diagram <- function(model_fit, timepoint = NULL) {
  if (!requireNamespace("qgraph", quietly = TRUE)) {
    install.packages("qgraph")
  }
  library(qgraph)
  
  # Get parameter estimates
  params <- standardizedSolution(model_fit)
  
  # If timepoint is specified, filter for that timepoint
  if(!is.null(timepoint)) {
    params <- params %>% filter(group == timepoint)
  }
  
  # Define variables and their order
  manifest_vars <- c("shs_gh_a", "shs_rh_a", "shs_ch_a", "shs_ch_r",
                     "bpurp_1", "bpurp_2", "bpurp_3", "bpurp_4")
  latent_vars <- c("shs", "bpurp")
  var_names <- c(latent_vars, manifest_vars)
  n_vars <- length(var_names)
  
  # Create custom layout matrix for side-by-side latent variables with flanking indicators
  layout_matrix <- matrix(0, n_vars, 2)
  
  # Position latent variables side by side in center
  layout_matrix[1, ] <- c(-0.25, 0)  # shs latent
  layout_matrix[2, ] <- c(0.25, 0)   # bpurp latent
  
  # Position SHS indicators on the left
  layout_matrix[3:6, 1] <- rep(-0.75, 4)  # x coordinates for shs indicators
  layout_matrix[3:6, 2] <- c(0.75, 0.25, -0.25, -0.75)  # y coordinates spread vertically
  
  # Position BPURP indicators on the right
  layout_matrix[7:10, 1] <- rep(0.75, 4)  # x coordinates for bpurp indicators
  layout_matrix[7:10, 2] <- c(0.75, 0.25, -0.25, -0.75)  # y coordinates spread vertically
  
  # Create adjacency matrix
  adj_matrix <- matrix(0, n_vars, n_vars)
  edge_labels <- matrix("", n_vars, n_vars)
  
  # Clean color scheme for timepoints
  time_colors <- c("1" = "#2E8B57", "2" = "#4169E1", "3" = "#CD5C5C")
  edge_color <- if(is.null(timepoint)) "black" else time_colors[timepoint]
  
  # Fill matrices with parameter estimates
  for(i in seq_len(nrow(params))) {
    if(params$op[i] %in% c("=~", "~~")) {
      from_idx <- match(params$lhs[i], var_names)
      to_idx <- match(params$rhs[i], var_names)
      if(!is.na(from_idx) && !is.na(to_idx)) {
        # Store the estimate in the adjacency matrix
        adj_matrix[from_idx, to_idx] <- params$est.std[i]
        
        # Create edge label with rounded estimate
        edge_labels[from_idx, to_idx] <- sprintf("%.2f", params$est.std[i])
        
        # For correlations, make bidirectional
        if(params$op[i] == "~~" && params$lhs[i] != params$rhs[i]) {
          adj_matrix[to_idx, from_idx] <- params$est.std[i]
          edge_labels[to_idx, from_idx] <- edge_labels[from_idx, to_idx]
        }
      }
    }
  }
  
  # Create title based on timepoint
  title <- if(is.null(timepoint)) {
    "Standardized Estimates Across All Timepoints"
  } else {
    sprintf("Standardized Estimates for Timepoint %s", timepoint)
  }
  
  # Create the diagram
  diagram <- qgraph(adj_matrix,
                   labels = var_names,
                   edge.labels = edge_labels,
                   layout = layout_matrix,
                   groups = list(latent = 1:length(latent_vars),
                                 manifest = (length(latent_vars) + 1):n_vars),
                   directed = TRUE,
                   shape = c(rep("circle", length(latent_vars)),
                             rep("rectangle", length(manifest_vars))),
                   label.scale = FALSE,
                   edge.label.cex = 0.8,
                   label.cex = 0.9,
                   edge.width = 1.2,
                   title = title,
                   title.cex = 1.2,
                   borders = TRUE,
                   vsize = c(20, 20, rep(15, 8)),  # Larger latent variables
                   color = c("lightblue", "lightgreen", rep("white", 8)),
                   height = 8,
                   width = 8,
                   edge.color = edge_color,
                   residuals = FALSE,
                   bifactor = FALSE)
  
  return(diagram)
}

# Create a function to generate a table of model fit statistics
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

# Function to extract and format correlations between latent variables
extract_correlations <- function(model_fits) {
  if(is.null(model_fits$metric)) return(NULL)
  
  # Get standardized parameters for the metric model
  std_params <- standardizedSolution(model_fits$metric)
  
  # Extract correlations (covariances between latent variables)
  correlations <- std_params %>%
    filter(op == "~~", lhs != rhs, lhs %in% c("shs", "bpurp"), rhs %in% c("shs", "bpurp")) %>%
    mutate(
      timepoint = group,
      correlation = est.std,
      se = se,
      p_value = pvalue
    ) %>%
    select(timepoint, lhs, rhs, correlation, se, p_value) %>%
    arrange(timepoint)
  
  return(correlations)
}

# Create function for clean factor loadings table
create_loadings_table <- function(model_fits) {
  if(is.null(model_fits$metric)) return(NULL)
  
  # Get standardized parameters for the metric model
  std_params <- standardizedSolution(model_fits$metric)
  
  # Extract factor loadings
  loadings <- std_params %>%
    filter(op == "=~") %>%
    mutate(
      timepoint = group,
      factor = lhs,
      item = rhs,
      loading = est.std,
      se = se,
      p_value = pvalue
    ) %>%
    select(timepoint, factor, item, loading, se, p_value) %>%
    arrange(timepoint, factor, item)
  
  return(loadings)
}

# Main analysis function
run_measurement_invariance <- function(data, verbose = FALSE) {
  # Set verbosity option
  options(verbose = verbose)
  
  # Check if input data exists
  if(is.null(data)) {
    stop("Input data is NULL")
  }
  
  # Prepare data
  if(verbose) message("Preparing data...")
  long_data <- prepare_data(data)
  
  if(is.null(long_data)) {
    stop("Data preparation failed")
  }
  
  # Check if required variables exist
  required_vars <- c("shs_gh_a", "shs_rh_a", "shs_ch_a", "shs_ch_r",
                     "bpurp_1", "bpurp_2", "bpurp_3", "bpurp_4")
  missing_vars <- setdiff(required_vars, names(long_data))
  
  if(length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }
  
  # Initialize results lists
  model_fits <- list()
  fit_indices <- list()
  
  # Fit models
  if(verbose) message("\nFitting models...")
  model_fits$configural <- fit_cfa_model(long_data, "configural")
  model_fits$metric <- fit_cfa_model(long_data, "metric")
  model_fits$scalar <- fit_cfa_model(long_data, "scalar")
  
  # Check if any models were successfully fit
  if(all(sapply(model_fits, is.null))) {
    stop("No models could be fit successfully")
  }
  
  # Extract fit measures
  fit_indices$configural <- extract_fit_measures(model_fits$configural)
  fit_indices$metric <- extract_fit_measures(model_fits$metric)
  fit_indices$scalar <- extract_fit_measures(model_fits$scalar)
  
  # Create model comparison table
  fit_table <- create_fit_table(fit_indices)
  
  # Create model difference table
  diff_table <- create_diff_table(fit_indices)
  
  # Extract correlations
  correlations <- extract_correlations(model_fits)
  
  # Extract factor loadings
  loadings <- create_loadings_table(model_fits)
  
  # Create separate diagrams for each timepoint
  diagrams <- list()
  if(!is.null(model_fits$metric)) {
    for(t in c("1", "2", "3")) {
      diagrams[[t]] <- create_measurement_diagram(model_fits$metric, t)
    }
  }
  
  return(list(
    fits = model_fits,
    fit_table = fit_table,
    diff_table = diff_table,
    correlations = correlations,
    loadings = loadings,
    diagrams = diagrams
  ))
}

# Function to run the analysis and generate a report
run_analysis_and_report <- function(data, file_path = NULL, verbose = FALSE) {
  # Run the analysis
  results <- run_measurement_invariance(data, verbose)
  
  # Create output
  output <- list()
  
  # Add model fit table
  if(!is.null(results$fit_table)) {
    cat("\n## Model Fit Indices\n")
    print(kable(results$fit_table, format = "pipe", caption = "Model Fit Indices for Measurement Invariance Testing"))
    output$fit_table <- results$fit_table
  }
  
  # Add model differences table
  if(!is.null(results$diff_table)) {
    cat("\n## Model Comparison\n")
    print(kable(results$diff_table, format = "pipe", caption = "Model Comparison for Measurement Invariance Testing"))
    output$diff_table <- results$diff_table
  }
  
  # Add correlations table
  if(!is.null(results$correlations)) {
    cat("\n## Correlations Between Happiness and Purpose\n")
    corr_table <- results$correlations %>%
      mutate(
        Timepoint = case_when(
          timepoint == "1" ~ "Baseline",
          timepoint == "2" ~ "Follow-up 1",
          timepoint == "3" ~ "Follow-up 2"
        ),
        Correlation = sprintf("%.3f (%.3f)", correlation, se),
        `p-value` = sprintf("%.3f", p_value)
      ) %>%
      select(Timepoint, Correlation, `p-value`)
    
    print(kable(corr_table, format = "pipe", caption = "Correlations Between Happiness (SHS) and Purpose (BPURP)"))
    output$corr_table <- corr_table
  }
  
  # Display factor loadings
  if(!is.null(results$loadings)) {
    cat("\n## Factor Loadings\n")
    loadings_table <- results$loadings %>%
      mutate(
        Timepoint = case_when(
          timepoint == "1" ~ "Baseline",
          timepoint == "2" ~ "Follow-up 1",
          timepoint == "3" ~ "Follow-up 2"
        ),
        Factor = factor,
        Item = item,
        Loading = sprintf("%.3f (%.3f)", loading, se),
        `p-value` = sprintf("%.3f", p_value)
      ) %>%
      select(Timepoint, Factor, Item, Loading, `p-value`)
    
    print(kable(loadings_table, format = "pipe", caption = "Standardized Factor Loadings (with Standard Errors)"))
    output$loadings_table <- loadings_table
  }
  
  # Display diagrams
  if(!is.null(results$diagrams)) {
    cat("\n## Measurement Model Diagrams\n")
    for(t in names(results$diagrams)) {
      time_label <- case_when(
        t == "1" ~ "Baseline",
        t == "2" ~ "Follow-up 1",
        t == "3" ~ "Follow-up 2"
      )
      cat(sprintf("\n### Timepoint %s (%s)\n", t, time_label))
      print(results$diagrams[[t]])
    }
  }
  
  # Optional: Save the results to a file
  if(!is.null(file_path)) {
    save(results, file = file_path)
    cat(sprintf("\nResults saved to %s\n", file_path))
  }
  
  # Return results invisibly
  invisible(results)
}

# Example usage:
# 1. Load your data
data <- read.csv("tmpPvHitems4LLM.csv")

# 2. Run the analysis and generate the report
results <- run_analysis_and_report(data, file_path = "cfa_results.RData", verbose = TRUE)
