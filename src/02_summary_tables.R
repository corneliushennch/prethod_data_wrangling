# Header start =================================================================
# 02_summary_tables.R
#
# Author: Hennch Cornelius (cornelius.hennch@charite.de)
#
# Description: Script to create summary tables for the TK-D/DeKIZ dataset
#
# Input: imported data
# Output: .xlsx (huxtable) and/or .png tables
#
# Code written according to Hadley Wickhams "tidyverse style guide"
# Header end ===================================================================

# 1. summary statistics with missings ------------------------------------------

# vars to include
summary_vars <- colnames(tidy_data) %>% str_subset("DDT[:digit:]")

table_one <- tidy_data %>%
  filter(timepoint == "Aufnahme") %>%
  tbl_summary(by = "setting",
              include = c(summary_vars),
              missing_text = "Missing",
              statistic = list(
                DDT024 ~ "{mean} ({sd})",
                DDT025 ~ "{mean} ({sd})")
  ) %>%
  add_overall() %>%
  add_p() %>%
  bold_p() %>%
  bold_labels()

# export
table_one %>%
  as_hux_xlsx(here("output", "tables", glue("{today}_summary_table.xlsx")))

