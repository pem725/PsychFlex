# Load required packages
library(lavaan)
library(tidyverse)
library(psych)  # for descriptive statistics

# Read the data
data <- read.csv("tmpPvHitems4LLM.csv")

# Create composite scores for happiness at each time point
# Using the subjective happiness scale (shs) items
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

# Check for missing data
missing_summary <- data %>%
  select(b_happiness, fu1_happiness, fu2_happiness, b_purpose, fu1_purpose, fu2_purpose) %>%
  summarise(across(everything(), ~sum(is.na(.))))
print("Missing values per variable:")
print(missing_summary)

# Check descriptive statistics
desc_stats <- describe(data[, c("b_happiness", "fu1_happiness", "fu2_happiness", 
                               "b_purpose", "fu1_purpose", "fu2_purpose")], skew = F, ranges = F)[,-1]
rownames(desc_stats) <- c("Baseline Happiness", "FU1 Happiness", "FU2 Happiness",
                          "Baseline Purpose", "FU1 Purpose", "FU2 Purpose")
print(desc_stats)

# Correlation matrix
cor_matrix <- cor(data[, c("b_happiness", "fu1_happiness", "fu2_happiness", 
                          "b_purpose", "fu1_purpose", "fu2_purpose")], 
                 use = "pairwise.complete.obs")
print("Correlation matrix:")
print(cor_matrix)

# Specify the cross-lagged panel model with simplex structure
# This follows the true cross-lagged panel design with adjacent time points
clpm_model <- '
  # Auto-regressive paths for happiness (simplex structure)
  fu1_happiness ~ a1*b_happiness
  fu2_happiness ~ a2*fu1_happiness
  
  # Auto-regressive paths for purpose (simplex structure)
  fu1_purpose ~ b1*b_purpose
  fu2_purpose ~ b2*fu1_purpose
  
  # Cross-lagged paths from happiness to purpose
  fu1_purpose ~ c1*b_happiness
  fu2_purpose ~ c2*fu1_happiness
  
  # Cross-lagged paths from purpose to happiness
  fu1_happiness ~ d1*b_purpose
  fu2_happiness ~ d2*fu1_purpose
  
  # Covariances between happiness and purpose at each time point
  b_happiness ~~ e1*b_purpose
  fu1_happiness ~~ e2*fu1_purpose
  fu2_happiness ~~ e3*fu2_purpose
  
  # Indirect effects of interest
  # Purpose → Happiness → Purpose pathway (baseline to fu2)
  p_h_p_indirect := d1*a2*b2 + b1*c2
  
  # Happiness → Purpose → Happiness pathway (baseline to fu2)
  h_p_h_indirect := c1*b2*d2 + a1*d2
'

# Fit the model
clpm_fit <- sem(clpm_model, data = data, missing = "fiml")

# Summary of the model with standardized estimates
summary(clpm_fit, standardized = TRUE, fit.measures = TRUE)

# Get parameter estimates in a data frame for easier inspection
param_est <- parameterEstimates(clpm_fit, standardized = TRUE)
print("Path coefficients (standardized):")
print(param_est[param_est$op == "~", c("lhs", "op", "rhs", "label", "est", "std.all", "pvalue")])

# Output key path estimates for interpretation
cat("\nAuto-regressive paths (stability):\n")
cat("Happiness (T1→T2):", param_est[param_est$label == "a1", "est"], "p =", param_est[param_est$label == "a1", "pvalue"], "\n")
cat("Happiness (T2→T3):", param_est[param_est$label == "a2", "est"], "p =", param_est[param_est$label == "a2", "pvalue"], "\n")
cat("Purpose (T1→T2):", param_est[param_est$label == "b1", "est"], "p =", param_est[param_est$label == "b1", "pvalue"], "\n")
cat("Purpose (T2→T3):", param_est[param_est$label == "b2", "est"], "p =", param_est[param_est$label == "b2", "pvalue"], "\n")

cat("\nCross-lagged effects:\n")
cat("Happiness→Purpose (T1→T2):", param_est[param_est$label == "c1", "est"], "p =", param_est[param_est$label == "c1", "pvalue"], "\n")
cat("Happiness→Purpose (T2→T3):", param_est[param_est$label == "c2", "est"], "p =", param_est[param_est$label == "c2", "pvalue"], "\n")
cat("Purpose→Happiness (T1→T2):", param_est[param_est$label == "d1", "est"], "p =", param_est[param_est$label == "d1", "pvalue"], "\n")
cat("Purpose→Happiness (T2→T3):", param_est[param_est$label == "d2", "est"], "p =", param_est[param_est$label == "d2", "pvalue"], "\n")

cat("\nIndirect effects:\n")
cat("Purpose→Happiness→Purpose:", param_est[param_est$label == "p_h_p_indirect", "est"], "p =", param_est[param_est$label == "p_h_p_indirect", "pvalue"], "\n")
cat("Happiness→Purpose→Happiness:", param_est[param_est$label == "h_p_h_indirect", "est"], "p =", param_est[param_est$label == "h_p_h_indirect", "pvalue"], "\n")

# Test for equality of cross-lagged effects
# This tests whether the cross-effects are equal in magnitude at each time point
equal_cross_model <- '
  # Auto-regressive paths for happiness (simplex structure)
  fu1_happiness ~ a1*b_happiness
  fu2_happiness ~ a2*fu1_happiness
  
  # Auto-regressive paths for purpose (simplex structure)
  fu1_purpose ~ b1*b_purpose
  fu2_purpose ~ b2*fu1_purpose
  
  # Constrained cross-lagged paths (equal across time points)
  fu1_purpose ~ c*b_happiness
  fu2_purpose ~ c*fu1_happiness
  
  # Constrained cross-lagged paths (equal across time points)
  fu1_happiness ~ d*b_purpose
  fu2_happiness ~ d*fu1_purpose
  
  # Test for difference between cross-effects
  cross_diff := c - d
  
  # Covariances between happiness and purpose at each time point
  b_happiness ~~ b_purpose
  fu1_happiness ~~ fu1_purpose
  fu2_happiness ~~ fu2_purpose
'

equal_cross_fit <- sem(equal_cross_model, data = data, missing = "fiml")
summary(equal_cross_fit, standardized = TRUE, fit.measures = TRUE)

# Compare models to test if constraining cross-effects worsens fit
cat("\nModel comparison - testing equality of cross-lagged effects across time:\n")
anova_result <- anova(clpm_fit, equal_cross_fit)
print(anova_result)

# Alternative model testing bi-directional equality
# This tests whether happiness→purpose = purpose→happiness
bidirectional_equal_model <- '
  # Auto-regressive paths for happiness (simplex structure)
  fu1_happiness ~ a1*b_happiness
  fu2_happiness ~ a2*fu1_happiness
  
  # Auto-regressive paths for purpose (simplex structure)
  fu1_purpose ~ b1*b_purpose
  fu2_purpose ~ b2*fu1_purpose
  
  # Cross-lagged paths constrained to be equal bidirectionally at T1→T2
  fu1_purpose ~ c*b_happiness
  fu1_happiness ~ c*b_purpose
  
  # Cross-lagged paths constrained to be equal bidirectionally at T2→T3
  fu2_purpose ~ d*fu1_happiness
  fu2_happiness ~ d*fu1_purpose
  
  # Covariances between happiness and purpose at each time point
  b_happiness ~~ b_purpose
  fu1_happiness ~~ fu1_purpose
  fu2_happiness ~~ fu2_purpose
'

bidirectional_fit <- sem(bidirectional_equal_model, data = data, missing = "fiml")
summary(bidirectional_fit, standardized = TRUE, fit.measures = TRUE)

# Compare models to test if bidirectional equality constraints worsen fit
cat("\nModel comparison - testing bidirectional equality of cross-lagged effects:\n")
anova_result2 <- anova(clpm_fit, bidirectional_fit)
print(anova_result2)

# Calculate R-squared for each endogenous variable
r2_values <- inspect(clpm_fit, "r2")
cat("\nVariance explained (R-squared):\n")
print(r2_values)

# Optional: visualize the model if semPlot is available
# Uncomment these lines if you want a diagram
# if (requireNamespace("semPlot", quietly = TRUE)) {
#   library(semPlot)
#   semPaths(clpm_fit, what = "std", edge.label.cex = 0.7,
#            style = "ram", layout = "tree2",
#            intercepts = FALSE, residuals = FALSE,
#            edge.color = "black",
#            nodeLabels = c("B_Hap", "FU1_Hap", "FU2_Hap",
#                           "B_Purp", "FU1_Purp", "FU2_Purp"))
# }

