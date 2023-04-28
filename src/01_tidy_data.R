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
# in a list to save code

data_split <- map(c("TK_D", "DeKIZ"), 
                  ~select(raw_data, CODE, all_of(contains(.x)))) %>% 
  map(~mutate(.x, across(where(is.character), ~if_else(. == "", NA, .)))) %>% 
  map(~filter(.x, rowSums(!is.na(select(., -CODE))) > 0)) %>% 
  map(~rename_with(.x, ~str_remove(., "_TK_D|_DeKIZ"))) %>% 
  set_names("TK_D", "DeKIZ")

# 2. checking IDs and colnames -------------------------------------------------

# completely empty records, that get excluded above
empty_records <- setdiff(raw_data$CODE, c(tk_data$CODE, dekiz_data$CODE))
# filter(raw_data, CODE %in% empty_records) %>% glimpse()

# Spelling error Katamese 
data_split[["DeKIZ"]] <- data_split[["DeKIZ"]] %>% 
  rename_with(~str_replace(.,"Katamese", "Katamnese"))

# check if all columns match
# compare_df_cols_same(data_split[[1]], data_split[[2]]) # -> complete match
compare_df <- compare_df_cols(data_split[["TK_D"]], data_split[["DeKIZ"]]) 
compare_df %>% View()


# 3. combining  ----------------------------------------------------------------
# binding the two parts back together for further processing

tidy_data <- bind_rows(data_split, .id = "setting") %>% 
  rename_with(~gsub("^([[:alnum:]]+)(Aufnahme|Verlaufsmessung|Abschlussmessung|Katamnese)", "\\1_\\2", .),
              -CODE) %>% 
  pivot_longer(cols = -c(CODE, setting), 
               names_to = c(".value", "timepoint"), 
               names_sep = "_")
  
