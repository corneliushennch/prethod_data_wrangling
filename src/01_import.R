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
# 2020 - 2024
raw_data_24 <- haven::read_sav(here("data", "raw", "psychoEQExport_2.8.2024_8.5.sav")) %>%
  haven::as_factor() %>%
  select(!contains("PRN")) %>%
  clean_names()

# 2024 - 2026
raw_data_26 <- haven::read_sav(here("data", "raw", "20260731_psychoEQExport.sav")) %>%
  haven::as_factor() %>%
  select(!contains("PRN")) %>%
  clean_names() %>%
  # remove deprecated BSI variables
  select(-starts_with("bsi"))

intersect(colnames(raw_data_24), colnames(raw_data_26)) %>% length()

# in raw_data_24 but not in raw_data_26 -> all there
# setdiff(colnames(raw_data_24), colnames(raw_data_26))

# in raw_data_26 but not in raw_data_24 -> most can be dropped, except b18
# setdiff(colnames(raw_data_26), colnames(raw_data_24))

# check bsi and b18 cols for data -> all called "bsi..."
# b18_24 <- raw_data_24 %>%
#   select(contains("bsi"), contains("b18"))
#
# has both bsi and b18, b18 contains the data
# b18_26 <- raw_data_26 %>%
#   select(contains("bsi"), contains("b18"))
#
# b18_data_24 <- data_2024 %>%
#   select(contains("bsi"), contains("b18"))

# just unused bsi columns columns
# b18_diff <- setdiff(colnames(b18_24), colnames(b18_26))

# rename bsi columns in raw_data_24 to b18
raw_data_24 <- raw_data_24 %>%
  rename_with(~str_replace(., "bsi", "b18"), contains("bsi"))

# select columns in raw_data_2026 that are present in raw_data_24
raw_data_26 <- raw_data_26 %>%
  select(any_of(colnames(raw_data_24)))

# check for missing cols -> all unused b18 cols
raw_data_diff <- setdiff(colnames(raw_data_24), colnames(raw_data_26))

# remove the differing columns from raw_data_24
raw_data_24 <- raw_data_24 %>%
  select(-any_of(raw_data_diff))

# check if column names are equal -> yes!
if (!identical(colnames(raw_data_24), colnames(raw_data_26))){
  stop("Column names of raw_data_2024 and raw_data_26 are not identical")
}else{
  message("Column names of raw_data_2024 and raw_data_26 are identical")
}

# merge raw data for further processing
raw_data <- bind_rows(raw_data_24, raw_data_26, .id = "import") %>%
  mutate(import = if_else(import == "1", "2024", "2026"))

# check for overlapping ID codes
overlapping_ids <- intersect(raw_data_24$code, raw_data_26$code)

# view overlapping data
overlap <- raw_data %>%
  filter(code %in% overlapping_ids)

# discard duplicates originating from 2024 data set, as they will get
# overwritten by the manually curated 2024 dataset later, if there is more
# complete data available
raw_data <- raw_data %>%
  filter(!(import == "2024" & code %in% overlapping_ids))

# check -> 33 cases from 2024 data set are removed, 33 cases from 2026 data set
# remain
# raw_data %>%
#   filter(code %in% overlapping_ids)

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

# 1.2 import clean data (2020 - 2024) ------------------------------------------
# this data was already filtered for complete cases (bdi2 score at admission and
# discharge present)
# needs to replace data in the final dataset, as it was curated manually

data_2024 <- readr::read_csv2(here("data", "processed",
                                   "2024_prethod_data.csv"),
                              show_col_types = FALSE)

## 1.3 import BSI-18 data ------------------------------------------------------

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

# 1.4 import new bas data ------------------------------------------------------
# 2020 - 2024
badok_24 <- readxl::read_excel(here("data", "processed",
                                 "2024_missing_basisdoku_curated.xlsx"),
                            sheet = 1,
                            na = "NA",
                            guess_max = 1600)

# 2024 - 2026
badok_26 <- readxl::read_excel(here("data", "processed",
                                    "2026_missing_basisdoku_weber.xlsx"),
                               sheet = 1,
                               na = "NA",
                               guess_max = 1600)

# check if column names are equal
if (!identical(colnames(badok_24), colnames(badok_26))) {
  stop("Column names of badok_24 and badok_26 are not identical")
}else{
  message("Column names of badok_24 and badok_26 are identical")
}

# check for duplicates comparing code of each data frame
badok_duplicates <- intersect(badok_24$code, badok_26$code)

# filter the duplicates out of badok_26 (those are already in badok_24 with more
# complete data)
badok_26 <- badok_26 %>% filter(!code %in% badok_duplicates)

# colnames of cols that were collapsed from numeric to categorical
collapsed_cols <- badok_24 %>%
  select(where(is.character)) %>%
  select(where(~ any(. == "> 5", na.rm = TRUE))) %>%
  colnames()

# convert collapsed_cols to character
badok_26 <- badok_26 %>%
 mutate(across(all_of(collapsed_cols), as.character))

# bind together
badok <- bind_rows(badok_24, badok_26)

# last check for duplicates
if (any(duplicated(badok$code))) {
  stop("There are duplicates in the combined badok data frame")
}else{
  message("No duplicates in the combined badok data frame")
}



# 2. examine variable labels ---------------------------------------------------

# variable key for overview
var_key <- labelled::var_label(raw_data_26) %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "var_name", values_to = "label")

if (save_output) {
  # export
  write.xlsx(var_key, here("output", "tables", "variable_key.xlsx"))

}
