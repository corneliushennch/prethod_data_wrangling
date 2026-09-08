# Header start =================================================================
# 01_tidy_data.R
#
# Author: Hennch Cornelius (cornelius.hennch@charite.de)
#
# Description: Template for analysis index script that integrates multiple
# scripts or reports
#
# Input: imported data
# Output: Tidy data frame in the environment
#
# Code written according to Hadley Wickhams "tidyverse style guide"
# Header end ===================================================================

# 1. remove completey empty rows

# raw_data_clean_filt <- raw_data_clean %>%
#   filter(rowSums(is.na(select(., -code))) < ncol(.) - 1)

# 1. tidy dataframe  -----------------------------------------------------------

## 1.1. split data into DeKIZ/TK_D ---------------------------------------------
# create a list of two data frames, one for each data source
# select the columns that contain the data source name
data_split <- map(
  c("tk_d", "de_kiz"),
  ~ select(raw_data_26, code, all_of(contains(.x))) #raw_data
) %>%
  # replace empty strings with NA
  map(~ mutate(.x, across(where(is.character), ~ if_else(. == "", NA, .)))) %>%
  # remove rows with no data
  map(~ filter(.x, rowSums(!is.na(select(., -code))) > 0)) %>%
  # remove the data source name from the column names
  map(~ rename_with(.x, ~ str_remove(., "_tk_d|_de_kiz"))) %>%
  # name the two data frames
  set_names("tk_d", "dekiz")

# check if column names are equal -> True
identical(colnames(data_split[[1]]), colnames(data_split[[2]]))

# bind together to single data frame
data_clean <- bind_rows(data_split, .id = "var_setting") %>%
  # ID in caps
  mutate(code = toupper(code)) %>%
  # exclude test rows
  filter(!str_detect(code, "TEST")) %>%
  # add setting variable (from ID col)
  mutate(setting = if_else(str_detect(code, "DK"), "dekiz", "tk_d"),
         .before = "var_setting")  %>%
  select(-var_setting)

# data_clean %>% filter(setting != var_setting) %>% glimpse()

## 1.2.pivot longer and take out timepoints from variable names ----------------
data_tidy <- data_clean %>%
  rename_with(
    ~ gsub(
      "^([[:alnum:]]+)(aufnahme|verlaufsmessung|abschlussmessung)",
      "\\1_\\2",
      .
    ),
    -c("code", "setting")
  ) %>%
  # pivot the data frame
  pivot_longer(
    cols = -c(code, setting),
    names_to = c(".value", "timepoint"),
    names_sep = "_"
  ) %>%
  # remove duplicates
  distinct(code, timepoint, .keep_all = TRUE)

# show all columns containing only NA
# data_tidy %>% select(where(~ all(is.na(.))))

# drop all columns containing only NA
data_tidy <- data_tidy %>%
  select(where(~ !all(is.na(.))))

# re-attach bas021 column with only NA values (for compatibility with 2024 data)
data_tidy <- data_tidy %>%
  add_column(bas021 = NA_real_, .after = "bas020")

# data_tidy %>% select(code, setting, timepoint, contains("bd2")) %>% view()


# 2. filter for complete cases -------------------------------------------------
# bdi sum admission and discharge n = 467

# into wide format for counting
wide <- data_tidy %>%
  select(code, timepoint, bd2sum) %>%
  pivot_wider(names_from = timepoint, values_from = bd2sum)

# ids of patients with complete cases for admission and discharge
complete_cases <- wide %>%
  filter(!is.na(aufnahme) & !is.na(abschlussmessung)) %>%
  pull(code)

# this is not necessary anymore, because we want to keep all cases for the analysis
# data_tidy <- data_tidy %>%
#   filter(code %in% complete_cases)

# 3. re-label tidy variable key ------------------------------------------------
var_key_tidy <- var_key %>%
  mutate(
    var_name =
      str_remove_all(var_name, "(aufnahme|verlaufsmessung|abschlussmessung)") %>%
      str_remove_all(., "(_tk_d|_de_kiz)") %>%
      str_remove_all("_$"),
    label =
      str_remove_all(label, "( zu| Aufnahme| Verlaufsmessung| Abschlussmessung)") %>%
      str_remove_all("(_TK_D|_DeKIZ)")
  )  %>%
  distinct(var_name, .keep_all = TRUE) %>%
  add_row(var_name = "setting", label = "setting") %>%
  add_row(var_name = "timepoint", label = "timepoint") %>%
  filter(var_name %in% colnames(data_tidy))

# setdiff(colnames(data_tidy), var_key_tidy$var_name)

# 4. relabel basisdoku timepoint -----------------------------------------------
bas_vars <- names(data_tidy) %>% str_subset("^bas")

# Shift "abschluss" values of bas* to "aufnahme" within each id
data_tidy <- data_tidy %>%
  group_by(code) %>%
  mutate(across(
    all_of(bas_vars),
    ~ ifelse(timepoint == "aufnahme", .[timepoint == "abschlussmessung"], .)
  )) %>%
  ungroup()

# relabel tidy data set
labelled::var_label(data_tidy) <- setNames(as.list(var_key_tidy$label),
                                           var_key_tidy$var_name)
# 5. update bas and ddt variables ----------------------------------------------
setdiff(colnames(badok), colnames(data_tidy))
bas_ddt_vars <- badok %>%
  select(-c(code, setting, timepoint)) %>%
  colnames()

num_cols <- c("ddt001", "ddt009", "ddt018", "ddt019", "ddt020", "ddt021",
              "ddt024", "ddt025")

# colnames of cols that were collapsed from numeric to categorical
collapsed_cols <- badok %>%
  select(where(is.character)) %>%
  select(where(~ any(. == "> 5", na.rm = TRUE))) %>%
  colnames()

data_num <- data_tidy %>%
  select(c(code, setting, timepoint, all_of(collapsed_cols))) %>%
  filter(timepoint == "aufnahme") %>%
  filter(code %in% badok$code)

# get numerical values back
badok <- badok %>%
  mutate(across(all_of(collapsed_cols), ~ ifelse(. == "> 5", data_num[[cur_column()]], .)))

# test 1 -> "> 5"
badok %>%
  select(where(is.character)) %>%
  select(where(~ any(. == "> 5", na.rm = TRUE))) %>%
  colnames()

# update data
data_tidy_updated <- data_tidy %>%
  select(-all_of(bas_ddt_vars)) %>%
  left_join(badok, by = c("setting", "code", "timepoint")) %>%
  mutate(across(all_of(num_cols), as.numeric)) %>%
  select(all_of(names(data_tidy)))


# reorder
data_tidy_updated <- select(data_tidy_updated, all_of(var_key_tidy$var_name)) %>%
  select(c(code, setting, timepoint, everything()))


# relabel
labelled::var_label(data_tidy_updated) <- setNames(as.list(var_key_tidy$label),
                                           var_key_tidy$var_name)

# check variable class
col_classes <- data_tidy_updated %>%
  map(~ class(.x)) %>% stack() %>%
  rename(var_name = "ind") %>%
  left_join(var_key_tidy, by = "var_name")

# reorder var_key
var_key_tidy <- var_key_tidy[order(match(var_key_tidy$var_name, colnames(data_tidy_updated))), ]

# 6. add 2026 data to 2024 data set --------------------------------------------
# TODO: rather replace the cases in the complete dataset with the 2024 cases
# (which have been edited manually)
## 6.1 harmonize columns -------------------------------------------------------
# check if all columns are equal
# if (!identical(colnames(data_tidy_updated), colnames(data_2024))) {
#   stop("Column names of data_tidy_updated and data_2024 are not identical")
# }else{
#   message("Column names of data_tidy_updated and data_2024 are identical")
# }

# identify missing columns (not present in data_2024 but present in data_tidy_updated)
missing_cols <- setdiff(colnames(data_tidy_updated), colnames(data_2024))

# setdiff(colnames(data_2024), colnames(data_tidy_updated))

# remove "bas007" from missing cols
missing_cols <- missing_cols[!missing_cols %in% "bas007"]

# remove missing columns from data_tidy_updated
data_tidy_updated <- data_tidy_updated %>%
  select(-all_of(missing_cols))

# add "bas007" to data_2024 with values from data_tidy_updated
data_2024 <- data_2024 %>%
  mutate(bas007 = data_tidy_updated$bas007[match(data_2024$code, data_tidy_updated$code)]) %>%
  # move after "bas006"
  relocate(bas007, .after = bas006)

# check if all columns are equal now
if (!identical(colnames(data_tidy_updated), colnames(data_2024))) {
  stop("Column names of data_tidy_updated and data_2024 are not identical after
 harmonization")
}else{
  message("Column names of data_tidy_updated and data_2024 are identical after
 harmonization")
}

# 6.2 harmonize variable classes -----------------------------------------------
# get all factor cols
factor_cols <- data_tidy_updated %>%
  select(where(is.factor)) %>%
  colnames()

# convert to factor
data_2024 <- data_2024 %>%
  mutate(across(all_of(factor_cols), as.factor))

# convert to date
data_2024 <- data_2024 %>%
  mutate(bd2dat = as.Date(bd2dat, format = "%d.%m.%y"))

# convert to numeric
numeric_cols <- c("ddt015", "ddt016", "ddt017", "ddt023")

data_tidy_updated <- data_tidy_updated %>%
  mutate(across(all_of(numeric_cols), as.numeric))

# 6.3 harmonize IDs ------------------------------------------------------------

# bind data frames together
data_tidy_2026 <- bind_rows(data_2024, data_tidy_updated, .id = "source") %>%
  mutate(source = if_else(source == "1", "2024", "2026"))

# check for overlapping ID codes
overlapping_ids <- intersect(data_tidy_updated$code, data_2024$code)

# view overlapping data
overlap <- data_tidy_2026 %>%
  filter(code %in% overlapping_ids)

# discard duplicates originating from 2026 data set
data_tidy_2026 <- data_tidy_2026 %>%
  filter(!(source == "2026" & code %in% overlapping_ids))

# recheck for duplicates -> no rows removed by distinct()
# data_tidy_2026 %>%
#   filter(timepoint == "aufnahme") %>%
#   distinct(code, .keep_all = TRUE)

# order by code and timepoint
data_tidy_2026 <- data_tidy_2026 %>%
  mutate(code = factor(code, levels = str_sort(unique(code), numeric = TRUE))) %>%
  arrange(code, timepoint)

data_tidy_2026$code %>% str_sort(numeric = TRUE)

# 9. export   ------------------------------------------------------------------

if (save_output) {
# xlsx
  write.xlsx(var_key_tidy, here("output", "tables", "variable_key_bsi_old.xlsx"))
  write.xlsx(data_tidy_2026, here("output", "tables", "data_tidy_2026_v1.xlsx"))

  # csv
  write_csv2(var_key_tidy, here("output", "tables", "variable_key_tidy.csv"))
  write_csv2(data_tidy_2026, here("output", "tables", "data_tidy_2026_v1.csv"))
}





