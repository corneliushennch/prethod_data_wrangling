# Header start ==============================================================================
# 01_import.R
#
# Author: Hennch Cornelius (cornelius.hennch@charite.de)
#
# Description: Template for analysis index script that integrates multiple 
# scripts/reports
#
# Input: 
# Output:
#
# Code written according to Hadley Wickhams "tidyverse style guide"

# Packages & dependencies ------------------------------------------------------

# 1. import SPSS file ----------------------------------------------------------

raw_data <- haven::read_sav("data/raw/SPSS_PEQ_DEKIZ&TKD.sav")

# variable key for overview
var_key <- labelled::var_label(raw_data) %>% 
  as.data.frame() %>% 
  pivot_longer(everything(), names_to = "var_name", values_to = "label")

openxlsx::write.xlsx(var_key, "output/tables/variable_key.xlsx")
                     