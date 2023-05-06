# Header start =================================================================
# 01_import.R
#
# Author: Hennch Cornelius (cornelius.hennch@charite.de)
#
# Description: Imports data for the analysis
#
# Input: Data exported from PsychEQ.
# Output: Variable key as excel readable file.
#
# Code written according to Hadley Wickhams "tidyverse style guide"
# Header end ===================================================================


# 1. import SPSS file ----------------------------------------------------------

raw_data <- haven::read_sav(here("data", "raw", "20230423_psychoEQExport.sav")) %>%
  haven::as_factor() %>%
  select(!contains("PRN"))

# 2. examine variables -------------------s--------------------------------------

# variable key for overview
var_key <- labelled::var_label(raw_data) %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "var_name", values_to = "label")

# clean variable names
raw_data_clean <- clean_names(raw_data)

# variable key for overview
var_key_clean <- labelled::var_label(raw_data) %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "var_name", values_to = "label")

if (save_output) {
  # export
  write.xlsx(var_key, here("output", "tables", "variable_key.xlsx"))

  write.xlsx(var_key_clean,
             here("output", "tables", "variable_var_key_clean.xlsx"))
}