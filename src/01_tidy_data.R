# Header start =================================================================
# 01_tidy_data.R
#
# Author: Hennch Cornelius (cornelius.hennch@charite.de)
#
# Description: Template for analysis index script that integrates multiple 
# scripts/reports
#
# Input: imported data
# Output: Tidy data frame in the environment
#
# Code written according to Hadley Wickhams "tidyverse style guide"
# Header end ===================================================================




# 1. split data into DeKIZ/TK_D ------------------------------------------------

# TK
tk_data <- raw_data %>% select(CODE, all_of(contains("TK_D"))) %>% 
  mutate(across(where(is.character), ~if_else(. == "", NA, .))) %>% 
  filter(rowSums(!is.na(select(., -CODE))) > 0)
  
# DeKIZ
dekiz_data <- raw_data %>% select(CODE, all_of(contains("DeKIZ"))) %>% 
  mutate(across(where(is.character), ~if_else(. == "", NA, .))) %>% 
  filter(rowSums(!is.na(select(., -CODE))) > 0)

# 2. checking IDs --------------------------------------------------------------

# completely empty records, that get excluded above
empty_records <- setdiff(raw_data$CODE, c(tk_data$CODE, dekiz_data$CODE))
# filter(raw_data, CODE %in% empty_records) %>% glimpse()
