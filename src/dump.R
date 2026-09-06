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
#
