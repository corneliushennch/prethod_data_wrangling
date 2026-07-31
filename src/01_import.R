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

raw_data <- haven::read_sav(here("data", "raw", "psychoEQExport_2.8.2024_8.5.sav")) %>%
  haven::as_factor() %>%
  select(!contains("PRN")) %>%
  clean_names()

raw_data_26 <- haven::read_sav(here("data", "raw", "20260731_psychoEQExport.sav")) %>%
  haven::as_factor() %>%
  select(!contains("PRN")) %>%
  clean_names() %>%
  # remove BSI columns (deprecated)
  select(-starts_with("bsi"))

## 1.1 import identifiers ------------------------------------------------------


# import dekiz identifiers
dekiz_patients <- readxl::read_excel(here("data", "raw", "PEQ_Liste_DeKIZ_23.xlsx"),
                                     range = "B1:E384",
                                     col_types = c("text", "text", "text", "date")) %>%
  rename(code = `PEQ No.`)

# import tk identifiers
tk_patients <- readxl::read_excel(here("data", "raw", "PEQ_Liste_TK.xlsx"),
                                     # range = "B1:E384",
                                     col_types = c("text", "text", "text", "date"))

# align column names
names(tk_patients) <- names(dekiz_patients)

# bind_together
patient_id <- bind_rows(tk_patients, dekiz_patients)

# add labels
labelled::var_label(patient_id) <- names(patient_id)

## 1.2 import BSI-18 data ------------------------------------------------------

# bsi_data <- readxl::read_excel(here("data", "raw", "BSI18_20_24_tk_dekiz.xlsx"),
#                                guess_max = 1600) %>%
#   clean_names()

# import BSI-18 data from separate files for DeKIZ and TK_D
bsi_dekiz <- readxl::read_excel(here("data", "raw", "BSI_18_DeKIZ.xlsx"),
                               guess_max = 1600) %>%
  clean_names() %>%
  rename_all(~str_remove(., "_de_kiz"))

# import BSI-18 data for TK_D
bsi_tk <- readxl::read_excel(here("data", "raw", "BSI_18_TK_D.xlsx"),
                               guess_max = 1600) %>%
  clean_names() %>%
  rename_all(~str_remove(., "_tk_d"))

# bind together and add setting variable
bsi_data <- bind_rows(bsi_dekiz, bsi_tk, .id = "setting") %>%
  mutate(code = toupper(code),
         setting = if_else(str_detect(code, "DK"), "dekiz", "tk_d"))

# 1.3 import new bas data ------------------------------------------------------
badok <- readxl::read_excel(here("data", "processed",
                                 "missing_basisdoku_curated.xlsx"),
                            sheet = 1,
                            na = "NA",
                            guess_max = 1600)

# 1.4 import clean data (2020 - 2024) ------------------------------------------
data_20_24 <- readr::read_csv2(here("data", "processed",
                                 "20_24_prethod_data.csv"),
                            show_col_types = FALSE)


# 2. examine variable labels ---------------------------------------------------

# variable key for overview
var_key <- labelled::var_label(raw_data_26) %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "var_name", values_to = "label")

if (save_output) {
  # export
  write.xlsx(var_key, here("output", "tables", "variable_key.xlsx"))

}
