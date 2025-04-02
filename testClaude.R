source("table-formatter.r")
data <- read.csv("tmpPvHitems4LLM.csv")
tables <- run_formatted_analysis("tmpPvHitems4LLM.csv")

# View the HTML formatted tables
tables$html_tables$stability
tables$html_tables$correlates
tables$html_tables$longitudinal_6mo
tables$html_tables$longitudinal_2yr

# Save to files for easy access
save_tables_to_files(tables, "output_tables")
