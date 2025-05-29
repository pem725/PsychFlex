# Extract specified columns from dataframe (dat)
# All column numbers have been adjusted by adding 7


library(haven)

# Read the data
dat <- read_sav("Psychological Flexibility_Full Dataset_070822.sav")

# Define column ranges and individual columns
cols_to_extract <- c(
  # TEMPORAL FOCUS - Goal Specific Hope Scale (GSHS)
  # Baseline
  447, 448:451, 449:453, 709:711,
  
  # FU1  
  1484:1489, 1780:1782, 1854:1856,
  
  # FU2
  3018:3025, 3486:3494,
  
  # Self-control (SCS)
  # Baseline
  487:496, 716:722, 723,
  
  # NEEDS - Basic Psychological Needs Scale (BMPNS)
  # Baseline
  253:279, 681:692, 738:740, 1222:1224,
  
  # FU1
  1431:1448, 1737:1754, 1755:1757, 1829:1831,
  
  # FU2
  2965:2982, 3441:3449, 3499:3510,
  
  # EMOTIONS
  # Distress intolerance (DI) - Baseline only
  437:446,
  
  # Experiential avoidance (BMEAQ)
  415:429, 634, 706, 745,
  
  # Implicit beliefs about emotions (IBAES) - Baseline only
  475:478, 635:636, 712,
  
  # PPFI (PF)
  # Baseline
  85:154, 656:675, 725:730, 1211:1213,
  
  # FU1
  1226:1273, 1858:1860,
  
  # FU2
  1913:1924, 2759:2806, 3423:3434, 3461:3472,
  
  # IDENTITY
  # Behavioral Consistency with 10 Values (VALC)
  # Baseline
  533:552, 644:653,
  
  # FU1
  1490:1509,
  
  # FU2
  3030:3049, 3521:3530,
  
  # Preferences for 10 Values (VALI)
  # Baseline
  285:304,
  
  # FU1
  1454:1473, 1760:1769, 1834:1843,
  
  # FU2
  2988:3007, 3511:3520
) -7

cols_to_extract <- c(1, cols_to_extract)  # Include the first column (ID)

# Remove duplicates and sort
cols_to_extract <- sort(unique(cols_to_extract))

# Extract the specified columns from the dataframe
extracted_dat <- dat[, cols_to_extract, drop = FALSE]

# Display summary information
cat("Total columns extracted:", ncol(extracted_dat), "\n")
cat("Column numbers extracted:", paste(head(cols_to_extract, 20), collapse = ", "), 
    ifelse(length(cols_to_extract) > 20, "...", ""), "\n")
cat("Dataframe dimensions:", nrow(extracted_dat), "rows x", ncol(extracted_dat), "columns\n")

# Optional: View first few rows and columns
head(extracted_dat[, 1:min(6, ncol(extracted_dat))])

