# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Run Commands
- Run R code: `Rscript filename.R`
- Run minimal purpose-happiness analysis: `Rscript purpose-happiness-minimal.r`
- Run full analysis: `Rscript purpose-happiness-analysis.r`
- Render Quarto document: `quarto render filename.qmd`

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