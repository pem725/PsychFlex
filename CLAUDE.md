# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a psychological research project analyzing the longitudinal relationship between purpose in life (BPURP) and happiness (SHS) using structural equation modeling, cross-lagged panel models, and measurement invariance testing. The project produces academic manuscripts for publication in psychology journals.

## Run Commands

- Run R code: `Rscript filename.R`
- Run minimal purpose-happiness analysis: `Rscript purpose-happiness-minimal.r`
- Run full analysis: `Rscript purpose-happiness-analysis.r`
- Run cross-lagged panel model: `Rscript cross-lagged-panel-model.r`
- Render Quarto document: `quarto render filename.qmd`
- Render specific formats: `quarto render filename.qmd --to pdf` or `--to html` or `--to docx`

## Data Architecture

### Data Sources
- Primary longitudinal dataset: `tmpPvHitems4LLM.csv` (processed from SPSS .sav files)
- Multiple SPSS data files (.sav) containing raw survey data from baseline, 6-month, and 2-year follow-ups
- Key measures: Subjective Happiness Scale (SHS) and Brief Purpose (BPURP) at three timepoints

### Data Structure
- Variables follow naming convention: `{timepoint}_{scale}_{item}` (e.g., `b_shs_gh_a`, `fu1_bpurp_2`)
- Timepoints: `b` (baseline), `fu1` (6-month follow-up), `fu2` (2-year follow-up) 
- Composite scores created via `compute_scale_scores()` function using `rowMeans()` with `na.rm = TRUE`

## Core Analysis Framework

### Primary Scripts
- `purpose-happiness-analysis.r`: Main analysis functions for measurement invariance, stability, correlations
- `purpose-happiness-minimal.r`: Streamlined analysis focusing on core functionality
- `cross-lagged-panel-model.r`: Implements cross-lagged panel models using lavaan
- `purpose-happiness-quantile-regression.r`: Quantile regression analyses

### Analysis Pipeline
1. **Data Preparation**: Use `compute_scale_scores()` to create composite measures
2. **Measurement Invariance**: Test configural, metric, and scalar invariance using `run_measurement_invariance()`
3. **Stability Analysis**: Compute test-retest correlations via `run_stability_analysis()`
4. **Cross-Lagged Models**: Examine bidirectional relationships over time
5. **Correlational Analysis**: Compare purpose vs happiness correlates using `run_correlate_analysis()`

### Statistical Methods
- Confirmatory Factor Analysis (CFA) using lavaan package
- Structural Equation Modeling (SEM) for longitudinal models
- Measurement invariance testing (configural → metric → scalar)
- Cross-lagged panel models to test causal precedence
- Quantile regression for distributional analyses

## Document Generation

### Quarto Documents
- Use APA Quarto extension (`apaquarto-*` formats) for academic manuscripts
- Main manuscripts: `Purpose-Happiness-JOPP.qmd`, `Paper1_measuring_purpose_happiness.qmd`
- Supplementary materials: `purpose_vs_happiness_supplement.qmd`

### Output Management
- Figures automatically generated in `*_files/figure-*` directories
- Tables stored in `output_tables/` directory as HTML and TXT files
- Results objects saved as `.RData` files for reproducibility

## Code Style Guidelines

- Function names: Use snake_case (e.g., `compute_scale_scores`)
- Variable names: Use snake_case for variables (e.g., `stability_data`)
- Indentation: 2 spaces for R scripts
- Imports: Load packages at the beginning of scripts
- Error handling: Use tryCatch for error-prone operations
- Comments: Include section headers for major code blocks
- Data structure: Leverage pipe (`%>%`) operations with dplyr/tidyr
- Code organization: Group related functions together
- Avoid hardcoded paths; use relative paths
- R version: Code is compatible with R 4.0+

## Key Dependencies

### Required R Packages
- `lavaan`: Structural equation modeling and CFA
- `dplyr`/`tidyr`: Data manipulation and reshaping  
- `ggplot2`: Visualization
- `psych`: Psychometric analysis and descriptives
- `knitr`/`kableExtra`: Table formatting
- `foreign`: Reading SPSS .sav files
- `semTools`: Additional SEM utilities
- `corrplot`: Correlation visualization

### Quarto Extensions
- `wjschne/apaquarto`: APA 7th edition formatting for academic manuscripts
- Supports PDF, HTML, DOCX, and Typst output formats

## Important File Patterns

- Analysis scripts: `*analysis*.r`, `*model*.r`
- Manuscript files: `*.qmd` (Quarto documents)
- Data files: `*.sav` (SPSS), `*.csv` (processed data)
- Results: `*.RData`, `output_tables/*`
- Bibliography: `references.bib`, `bibliography.bib`

## Research Context

This project examines whether purpose and happiness are distinct constructs by analyzing:
- Temporal stability differences between constructs
- Differential correlational patterns with psychological variables
- Bidirectional longitudinal relationships using cross-lagged panel models
- The central finding: happiness predicts future purpose more than purpose predicts happiness