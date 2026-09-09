# dump.R

# 3. variable wrangling    -----------------------------------------------------
## 4.1 DDT categories     ------------------------------------------------------
# collapse DDT variable categories -> maybe later in the analysis
# -> move to different script
# ddt_vars <- c("ddt015", "ddt016", "ddt017", "ddt023")
# ddt_levels <- c("0", "1", "2", "3", "4", "5", "> 5")
#
# # data_tidy %>% select(all_of(ddt_vars)) %>% View()
#
# # collapse DDT variable categories -> maybe later in the analysis
# data_tidy <- data_tidy %>%
#   mutate(across(all_of(ddt_vars), round),
#          across(all_of(ddt_vars), ~ if_else(. > 5, "> 5", as.character(.))),
#          across(all_of(ddt_vars), ~ factor(., levels = ddt_levels))) %>%
#   # order of timepoints
#   mutate(timepoint = factor(
#     timepoint,
#     levels = c("aufnahme", "verlaufsmessung", "abschlussmessung")
#   ))

# 6. tidy and add BSI-18 data --------------------------------------------------
# shared_ids <- intersect(data_tidy$code, bsi_data$code)
# bsi_missing_cases <- setdiff(data_tidy$code, bsi_data$code)
#
# # pivot BSI data to longer format
# bsi_data_tidy <- bsi_data %>%
#   rename_with(~ gsub(
#     pattern = "^(.*?)(aufnahme|verlaufsmessung|abschlussmessung)$",
#     replacement = "\\1_\\2", .x), !contains("_")) %>%
#   pivot_longer(cols = -c(code, setting),
#                names_to = c(".value", "timepoint"),
#                names_sep = "_")
#
# # relabel BSI data
# bsi_labels <- filter(var_key_tidy, str_detect(var_name, "bsi")) %>%
#   mutate(var_name = str_replace_all(var_name, "bsi", "b18")) %>%
#   filter(var_name %in% colnames(bsi_data_tidy))
#
# labelled::var_label(bsi_data_tidy) <- setNames(as.list(bsi_labels$label),
#                                            bsi_labels$var_name)
#
# # merge new BSI data to dataset
# # old bsi columns start with "bsi", new ones with "b18"
# data_tidy <- left_join(data_tidy, bsi_data_tidy,
#                        by = c("setting", "code", "timepoint"))


# 7. update bas and ddt variables ----------------------------------------------
# setdiff(colnames(badok), colnames(data_tidy))
# bas_ddt_vars <- badok %>%
#   select(-c(code, setting, timepoint)) %>%
#   colnames()
#
# num_cols <- c("ddt001", "ddt009", "ddt018", "ddt019", "ddt020", "ddt021",
#               "ddt024", "ddt025")
#
# # colnames of cols that were collapsed from numeric to categorical
# collapsed_cols <- badok %>%
#   select(where(is.character)) %>%
#   select(where(~ any(. == "> 5", na.rm = TRUE))) %>%
#   colnames()
#
# data_num <- data_tidy %>%
#   select(c(code, setting, timepoint, all_of(collapsed_cols))) %>%
#   filter(timepoint == "aufnahme") %>%
#   filter(code %in% badok$code)
#
# # get numerical values back
# badok <- badok %>%
#   mutate(across(all_of(collapsed_cols), ~ ifelse(. == "> 5", data_num[[cur_column()]], .)))
#
# # test 1 -> "> 5"
# badok %>%
#   select(where(is.character)) %>%
#   select(where(~ any(. == "> 5", na.rm = TRUE))) %>%
#   colnames()
#
# # update data
# data_tidy_updated <- data_tidy %>%
#   select(-all_of(bas_ddt_vars)) %>%
#   left_join(badok, by = c("setting", "code", "timepoint")) %>%
#   mutate(across(all_of(num_cols), as.numeric)) %>%
#   select(all_of(names(data_tidy)))
#
#
# # rename old bsi cols in var_key
# var_key_tidy <- var_key_tidy %>%
#   mutate(var_name = str_replace_all(var_name, "bsi", "b18")) %>%
#   filter(var_name %in% names(data_tidy_updated))
#
# # reorder
# data_tidy_updated <- select(data_tidy_updated, all_of(var_key_tidy$var_name)) %>%
#   select(c(code, setting, timepoint, everything()))
#
#
# # relabel
# labelled::var_label(data_tidy_updated) <- setNames(as.list(var_key_tidy$label),
#                                            var_key_tidy$var_name)
#
# # check variable class
# col_classes <- data_tidy_updated %>%
#   map(~ class(.x)) %>% stack() %>%
#   rename(var_name = "ind") %>%
#   left_join(var_key_tidy, by = "var_name")
#
# # reorder var_key
# var_key_tidy <- var_key_tidy[order(match(var_key_tidy$var_name, colnames(data_tidy_updated))), ]

# 8. fix factors ---------------------------------------------------------------
# item_names <- data_tidy_updated %>% select(matches("\\d$")) %>% colnames()
#
# # inspect factors
#   factor_levels_list <- map(item_names, ~ {
#     # Convert the column to a factor (if not already) and get the levels
#     levels(as.factor(data_tidy_updated[[.x]]))
#   }) %>%
#     set_names(item_names)

# 6. add 2026 data to 2024 data set --------------------------------------------
# TODO: rather replace the cases in the complete dataset with the 2024 cases
# (which have been edited manually) -> done thus the following code is deprecated
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

#
