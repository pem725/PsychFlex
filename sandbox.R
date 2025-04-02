# Load required packages
library(lavaan)
library(dplyr)
library(tidyr)
library(stringr)
library(DiagrammeR)

# Prepare data for analysis with debug info
prepare_data <- function(data) {
  # Print input data structure
  message("Input data structure:")
  str(data)
  
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
          str_detect(name, "_r$") ~ "ch_r",
          str_detect(name, "bpurp_([0-9])$") ~ str_extract(name, "[0-9]$")
        )
      ) %>%
      select(-name)
    
    # Print diagnostics
    message("\nUnique timepoints after pivot:")
    print(unique(long_data$timepoint))
    message("\nUnique measures after pivot:")
    print(unique(long_data$measure))
    message("\nUnique items after pivot:")
    print(unique(long_data$item))
    
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
          measure == "shs" & str_detect(item, "_r") ~ "shs_ch_r",  # Modified to catch b_r and fu1_r
          measure == "bpurp" & str_detect(item, "^[0-9]$") ~ paste0("bpurp_", item)
        )
      )
    
    # Print item_clean values to verify transformation
    message("\nUnique cleaned items:")
    print(unique(long_data$item_clean))
    
    final_data <- long_data %>%
      select(pid, timepoint, item_clean, value) %>%
      pivot_wider(
        id_cols = c(pid, timepoint),
        names_from = item_clean,
        values_from = value
      ) %>%
      select(-pid) %>%
      mutate(timepoint = factor(timepoint))
    
    # Print final data structure
    message("\nFinal data structure:")
    str(final_data)
    
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

# Function to extract fit measures
extract_fit_measures <- function(fit) {
  if(is.null(fit)) return(NULL)
  
  measures <- fitmeasures(fit)
  selected_fits <- c(
    "chisq.scaled", "df.scaled", "pvalue.scaled",
    "cfi.scaled", "tli.scaled", "rmsea.scaled",
    "srmr", "bic"
  )
  
  return(measures[selected_fits])
}

# Main analysis function
run_measurement_invariance <- function(data) {
  # Check if input data exists
  if(is.null(data)) {
    stop("Input data is NULL")
  }
  
  # Prepare data
  message("Preparing data...")
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
  message("\nFitting models...")
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
  
  # Create comparison table
  comparison <- data.frame(
    Model = c("Configural", "Metric", "Scalar")
  )
  
  # Add fit measures if available
  if(!all(sapply(fit_indices, is.null))) {
    comparison$ChiSq <- sapply(fit_indices, function(x) if(!is.null(x)) round(x["chisq.scaled"], 2) else NA)
    comparison$df <- sapply(fit_indices, function(x) if(!is.null(x)) x["df.scaled"] else NA)
    comparison$CFI <- sapply(fit_indices, function(x) if(!is.null(x)) round(x["cfi.scaled"], 3) else NA)
    comparison$RMSEA <- sapply(fit_indices, function(x) if(!is.null(x)) round(x["rmsea.scaled"], 3) else NA)
    comparison$SRMR <- sapply(fit_indices, function(x) if(!is.null(x)) round(x["srmr"], 3) else NA)
    comparison$BIC <- sapply(fit_indices, function(x) if(!is.null(x)) round(x["bic"], 0) else NA)
  }
  
  # Calculate model differences
  model_diffs <- NULL
  if(!any(sapply(fit_indices, is.null))) {
    model_diffs <- data.frame(
      Comparison = c("Metric vs. Configural", "Scalar vs. Metric"),
      dCFI = c(
        fit_indices$metric["cfi.scaled"] - fit_indices$configural["cfi.scaled"],
        fit_indices$scalar["cfi.scaled"] - fit_indices$metric["cfi.scaled"]
      ),
      dRMSEA = c(
        fit_indices$metric["rmsea.scaled"] - fit_indices$configural["rmsea.scaled"],
        fit_indices$scalar["rmsea.scaled"] - fit_indices$metric["rmsea.scaled"]
      ),
      dBIC = c(
        fit_indices$metric["bic"] - fit_indices$configural["bic"],
        fit_indices$scalar["bic"] - fit_indices$metric["bic"]
      )
    )
  }
  
  # Extract standardized parameters and create visualization
  loadings <- NULL
  correlation <- NULL
  measurement_diagram <- NULL
  
  if(!is.null(model_fits$metric)) {
    std_params <- standardizedSolution(model_fits$metric)
    
    # Extract factor loadings and correlation
    loadings <- std_params %>%
      filter(op == "=~") %>%
      mutate(
        est_se = sprintf("%.3f (%.3f)", est.std, se),
        param_label = paste(lhs, op, rhs)
      )
    
    correlation <- std_params %>%
      filter(op == "~~" & lhs != rhs) %>%
      mutate(
        est_se = sprintf("%.3f (%.3f)", est.std, se),
        param_label = paste(lhs, op, rhs)
      )
  }
  
  # Create combined diagram
  diagram <- NULL
  
  if(!is.null(model_fits$metric)) {
    # Load qgraph
    if (!requireNamespace("qgraph", quietly = TRUE)) {
      install.packages("qgraph")
    }
    library(qgraph)
    
    # Get parameter estimates for each timepoint
    params_by_time <- standardizedSolution(model_fits$metric)
    
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
    edge_colors <- matrix("", n_vars, n_vars)
    
    # Color scheme for timepoints - explicitly define each timepoint's color
    time_colors <- c("1" = "darkgreen", "2" = "darkblue", "3" = "darkred")
    
    # Fill matrices with parameter estimates from all timepoints
    for(t in c("1", "2", "3")) {
      time_params <- params_by_time %>%
        filter(group == t)
      
      for(i in seq_len(nrow(time_params))) {
        if(time_params$op[i] %in% c("=~", "~~")) {
          from_idx <- match(time_params$lhs[i], var_names)
          to_idx <- match(time_params$rhs[i], var_names)
          if(!is.na(from_idx) && !is.na(to_idx)) {
            # Store the estimate in the adjacency matrix
            adj_matrix[from_idx, to_idx] <- time_params$est.std[i]
            
            # Create multi-line label with estimates from all timepoints
            current_label <- edge_labels[from_idx, to_idx]
            new_label <- sprintf("%s: %.2f", t, time_params$est.std[i])
            edge_labels[from_idx, to_idx] <- if(current_label == "") new_label else paste(current_label, new_label, sep="\n")
            
            # Store color
            edge_colors[from_idx, to_idx] <- time_colors[t]
            
            # For correlations, make bidirectional
            if(time_params$op[i] == "~~" && time_params$lhs[i] != time_params$rhs[i]) {
              adj_matrix[to_idx, from_idx] <- time_params$est.std[i]
              edge_labels[to_idx, from_idx] <- edge_labels[from_idx, to_idx]
              edge_colors[to_idx, from_idx] <- edge_colors[from_idx, to_idx]
            }
          }
        }
      }
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
                      edge.label.cex = 0.7,
                      label.cex = 0.9,
                      edge.width = 1.5,
                      title = "Standardized Estimates Across Timepoints\nT1: Green, T2: Blue, T3: Red",
                      borders = TRUE,
                      vsize = c(20, 20, rep(15, 8)),  # Larger latent variables
                      color = c("lightblue", "lightgreen", rep("white", 8)),
                      height = 10,
                      width = 10,
                      legend = FALSE,
                      edge.color = edge_colors,
                      residuals = FALSE,  # Explicitly disable residuals
                      bifactor = FALSE)   # Disable bifactor structure
  }
  
  return(list(
    fits = model_fits,
    comparison = comparison,
    model_differences = model_diffs,
    loadings = loadings,
    correlation = correlation,
    diagram = diagram
  ))
}

# Run the analysis with your data
results <- run_measurement_invariance(datPvH_shs_bpurp_items)

# Print results
cat("\nModel Comparison:\n")
print(results$comparison)

if(!is.null(results$model_differences)) {
  cat("\nModel Differences:\n")
  print(results$model_differences)
}

if(!is.null(results$loadings)) {
  cat("\nStandardized Factor Loadings (estimate (SE)):\n")
  print(results$loadings[, c("param_label", "est_se")])
}

if(!is.null(results$correlation)) {
  cat("\nFactor Correlation (estimate (SE)):\n")
  print(results$correlation[, c("param_label", "est_se")])
}

# Display the combined diagram
if(!is.null(results$diagram)) {
  cat("\nDisplaying combined measurement model diagram...\n")
  print(results$diagram)
}



# Create a simulation where we have 1000 samples of 32 observations random samples drawn from a population of 1860 people on four variables with medians of the population being 12, 11, 11, and 14.  Can you give me a simulation that returns the probability of getting a sample of 32 with median values of 18, 15, 16, and 19, respectively?
# Set the seed for reproducibility
set.seed(123)

# Define the population parameters
pop_medians <- c(12, 11, 11, 14)
n_population <- 1860

# Generate the population data
population_data <- replicate(4, rnorm(n_population, mean = pop_medians, sd = 2))

# Define the sample size and number of samples
n_sample <- 32
n_samples <- 1000

# Initialize a counter for successful samples
success_count <- 0

# Run the simulation
for(i in 1:n_samples) {
  # Generate a random sample from the population
  sample_data <- replicate(4, sample(population_data, n_sample))
  
  # Check if the sample has the desired medians
  if(all(round(apply(sample_data, 2, median), 2) == c(18, 15, 16, 19))) {
    success_count <- success_count + 1
  }
}

# Calculate the probability of getting the desired sample
success_prob <- success_count / n_samples

# Print the results
cat("Probability of getting the desired sample:", success_prob, "\n")

# Create a function to simulate the sampling process

simulate_sampling <- function(pop_medians, n_population, n_sample, n_samples) {
  # Generate the population data
  population_data <- replicate(4, rnorm(n_population, mean = pop_medians, sd = 2))
  
  # Initialize a counter for successful samples
  success_count <- 0
  
  # Run the simulation
  for(i in 1:n_samples) {
    # Generate a random sample from the population
    sample_data <- replicate(4, sample(population_data, n_sample))
    
    # Check if the sample has the desired medians
    if(all(round(apply(sample_data, 2, median), 2) == c(18, 15, 16, 19))) {
      success_count <- success_count + 1
    }
  }
  
  # Calculate the probability of getting the desired sample
  success_prob <- success_count / n_samples
  
  return(success_prob)
}

# Run the simulation with the function
set.seed(123)
success_prob <- simulate_sampling(c(12, 11, 11, 14), 1860, 32, 1000)

# Print the results
cat("Probability of getting the desired sample:", success_prob, "\n")

# Create a function to simulate the sampling process with multiple scenarios

simulate_sampling_multiple <- function(scenarios) {
  # Initialize a list to store results
  results <- list()
  
  # Run simulations for each scenario
  for(scenario in scenarios) {
    set.seed(scenario$seed)
    success_prob <- simulate_sampling(scenario$pop_medians, scenario$n_population, scenario$n_sample, scenario$n_samples)
    
    # Store the results
    results[[scenario$name]] <- success_prob
  }
  
  return(results)
}

# Define the scenarios
scenarios <- list(
  list(
    name = "Scenario 1",
    seed = 123,
    pop_medians = c(12, 11, 11, 14),
    n_population = 1860,
    n_sample = 32,
    n_samples = 1000
  ),
  list(
    name = "Scenario 2",
    seed = 456,
    pop_medians = c(10, 12, 13, 15),
    n_population = 2000,
    n_sample = 32,
    n_samples = 1000
  )
)

# Run the simulation with multiple scenarios
results_multiple <- simulate_sampling_multiple(scenarios)

# Print the results
cat("Simulation Results:\n")
print(results_multiple)
  
