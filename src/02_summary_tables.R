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

# 1. summary of BAS with missings ----------------------------------------------

# vars to include
summary_vars <- colnames(data_tidy) %>% str_subset("ddt[:digit:]")

table_one <- data_tidy %>%
  filter(timepoint == "aufnahme") %>%
  tbl_summary(by = "setting",
              include = c(summary_vars),
              missing_text = "Missing",
              statistic = list(
                ddt024 ~ "{mean} ({sd})",
                ddt025 ~ "{mean} ({sd})")
  ) %>%
  add_overall() %>%
  add_p() %>%
  bold_p() %>%
  bold_labels()

# export
if (save_output) {
  table_one %>%
    as_hux_xlsx(here("output", "tables", glue("{today}_summary_table.xlsx")))
}

