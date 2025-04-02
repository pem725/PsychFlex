# Purpose and Happiness Longitudinal Analysis Guide

This guide explains how to use the R scripts I've created to analyze the relationship between purpose (BPURP) and happiness (SHS) over time, and how they relate to other psychological variables and outcomes.

## Introduction

Building on prior work differentiating meaning and happiness (Baumeister et al., 2013), this analysis examines how purpose and happiness differ in their measurement stability over time (baseline, 6-month, and 2-year follow-ups) and their relationships with various psychological constructs. The analysis allows for:

1. Testing measurement invariance to ensure valid longitudinal comparisons
2. Examining temporal stability of purpose vs. happiness
3. Identifying differential correlates of purpose and happiness
4. Testing how each construct uniquely predicts future outcomes

## Prerequisites

The following R packages are required:

```r
# Core packages
library(lavaan)     # For CFA models
library(dplyr)      # For data manipulation
library(tidyr)      # For data reshaping
library(stringr)    # For string manipulation
library(ggplot2)    # For plotting
library(knitr)      # For table formatting
library(corrplot)   # For correlation visualization
library(psych)      # For psychometric analysis
```

## Script Components

I've created two main R script files:

1. **purpose-happiness-analysis.R**: Contains all the analysis functions
2. **purpose-happiness-example.R**: Shows example usage with your dataset

## Main Analysis Functions

The script provides several key functions that can be used separately or combined:

### 1. Measurement Invariance Testing

```r
invariance_results <- run_measurement_invariance(data)
```

This function:
- Fits configural, metric, and scalar invariance models
- Creates model fit and comparison tables
- Extracts factor loadings and correlations between constructs
- Generates separate measurement diagrams for each timepoint

### 2. Temporal Stability Analysis

```r
stability_results <- run_stability_analysis(data)
```

This function:
- Computes test-retest correlations across timepoints
- Calculates annual stability coefficients
- Creates stability visualization plots
- Allows comparison of purpose vs. happiness stability

### 3. Correlational Analysis

```r
correlate_results <- run_correlate_analysis(data)
```

This function:
- Analyzes correlations with key psychological variables
- Creates a comparative table of purpose vs. happiness correlates
- Generates a correlation heatmap
- Identifies differentiating correlates

### 4. Longitudinal Analysis

```r
longitudinal_results <- run_longitudinal_analysis(data)
```

This function:
- Examines how baseline purpose and happiness predict future outcomes
- Runs regression models to test unique predictive contributions
- Creates summary tables of predictive relationships
- Compares 6-month vs. 2-year outcomes

### 5. Comprehensive Analysis and Reporting

```r
# Run all analyses and save results
all_results <- run_purpose_happiness_analysis(data, output_dir = "results")

# Generate a comprehensive HTML report
generate_html_report(all_results, output_file = "purpose_happiness_report.html")
```

These functions run all analyses and create a formatted report with tables and visualizations.

## Step-by-Step Usage Guide

### 1. Prepare Your Data

Ensure your dataset contains all necessary variables, including:

- Baseline, 6-month, and 2-year measures of SHS and BPURP
- Related psychological variables for correlation analysis
- Outcome variables at follow-up timepoints

### 2. Run the Basic Measurement Model

First, test the measurement invariance of your constructs:

```r
# Run measurement invariance testing
invariance_results <- run_measurement_invariance(data)

# Display the results
print(invariance_results$fit_table)
print(invariance_results$diff_table)
```

This confirms that your constructs have consistent measurement properties over time.

### 3. Analyze Temporal Stability

Next, examine how stable these constructs are over time:

```r
# Compute scale scores first
data_with_scores <- compute_scale_scores(data)

# Run stability analysis
stability_results <- run_stability_analysis(data_with_scores)

# View results
print(stability_results$test_retest)
print(stability_results$stability_plots$shs_plot)
print(stability_results$stability_plots$bpurp_plot)
```

This helps you understand whether purpose is more trait-like or state-like compared to happiness.

### 4. Examine Correlational Patterns

Analyze how each construct relates to other psychological variables:

```r
# Run correlational analysis
correlate_results <- run_correlate_analysis(data_with_scores)

# View comparative correlations
print(correlate_results$purpose_happiness_correlations)
```

This identifies which variables are more strongly related to purpose vs. happiness.

### 5. Test Predictive Relationships

Finally, examine how each construct predicts future outcomes:

```r
# Run longitudinal analysis
longitudinal_results <- run_longitudinal_analysis(data_with_scores)

# View predictive relationships
print(longitudinal_results$longitudinal_correlations$longitudinal_results)
print(longitudinal_results$regression_summary)
```

This reveals the unique contributions of purpose and happiness to future well-being.

### 6. Generate a Comprehensive Report

For a complete analysis and reporting:

```r
# Run all analyses
all_results <- run_purpose_happiness_analysis(data)

# Generate HTML report
generate_html_report(all_results)
```

## Customizing the Analysis

You can modify various aspects of the analysis:

- **Visualizations**: Customize plots by adjusting the `plot_stability_patterns` function
- **Variables**: Change which correlates to include in the `analyze_correlates` function
- **Regression Models**: Add covariates to the models in `run_regression_models`
- **Report Format**: Modify the report template in `generate_html_report`

## Interpreting the Results

When interpreting your findings, consider:

1. **Measurement Invariance**: Good invariance (small dCFI, dRMSEA values) supports valid comparisons over time
2. **Stability Coefficients**: Higher annual stability suggests more trait-like constructs
3. **Correlation Patterns**: Different correlates indicate distinct psychological mechanisms
4. **Predictive Value**: Unique contributions in regression models suggest complementary roles in well-being

## Figure Generation for Publication

The script also includes functions to create publication-quality figures, such as:

```r
# Create custom stability comparison plot
stability_comparison <- ggplot(stability_results$test_retest, 
                             aes(x = Months, y = Correlation, color = Construct)) +
  geom_point(size = 3) +
  geom_line(aes(group = Construct)) +
  labs(title = "Temporal Stability of Purpose vs. Happiness",
       x = "Months between assessments",
       y = "Test-Retest Correlation") +
  theme_minimal() +
  scale_color_manual(values = c("darkgreen", "royalblue"))

# Save the figure
ggsave("stability_comparison.png", stability_comparison, width = 8, height = 6)
```

## Conclusion

This comprehensive analysis lets you thoroughly examine how purpose and happiness differ in their stability over time and their relationships with other psychological variables and outcomes. The findings can help clarify the eudaimonic-hedonic distinction in well-being research and inform interventions targeting different aspects of well-being.
