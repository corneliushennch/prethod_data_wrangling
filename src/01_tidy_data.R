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
  ~ select(raw_data, code, all_of(contains(.x)))
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
         .before = "var_setting") %>%
  select(-var_setting)

# data_clean %>% filter(id_setting != var_setting) %>% glimpse()

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


# data_tidy %>% select(code, setting, timepoint, contains("bd2")) %>% view()

# 2. check missings bd2sum -----------------------------------------------------

# data_tidy %>%
#   select(code, id_setting, timepoint, contains("bd2")) %>%
#   filter(timepoint %in% c("aufnahme","verlaufsmessung","abschlussmessung") & !is.na(bd2sum)) %>%
#   View()

# bdi sum admission = 699
# bdi sum course = 510
# bdi sum discharge = 483
data_tidy %>%
  group_by(timepoint) %>%
  summarize(na_count = sum(is.na(bd2sum))) %>%
  mutate(bd2sum_value = 727 - na_count)

# into wide format for counting
wide <- data_tidy %>%
  select(code, timepoint, bd2sum) %>%
  pivot_wider(names_from = timepoint, values_from = bd2sum)

# bdi sum all three = 385
wide %>%
  filter(!is.na(aufnahme) & !is.na(verlaufsmessung) & !is.na(abschlussmessung)) %>%
  nrow()

# bdi sum admission + discharge = 467
wide %>%
  filter(!is.na(aufnahme) & !is.na(abschlussmessung)) %>%
  nrow()

complete_cases <- wide %>%
  filter(!is.na(aufnahme) & !is.na(abschlussmessung)) %>%
  pull(code)

# 3. filter for complete cases -------------------------------------------------
# bdi sum admission and discharge n = 467

data_tidy <- data_tidy %>%
  filter(code %in% complete_cases)

# 4. variable wrangling    -----------------------------------------------------
## 4.1 DDT categories     ------------------------------------------------------
# -> move to different script
ddt_vars <- c("ddt015", "ddt016", "ddt017", "ddt023")
ddt_levels <- c("0", "1", "2", "3", "4", "5", "> 5")

# data_tidy %>% select(all_of(ddt_vars)) %>% View()

# collapse DDT variable categories
data_tidy <- data_tidy %>%
  mutate(across(all_of(ddt_vars), round),
         across(all_of(ddt_vars), ~ if_else(. > 5, "> 5", as.character(.))),
         across(all_of(ddt_vars), ~ factor(., levels = ddt_levels))) %>%
  # order of timepoints
  mutate(timepoint = factor(
    timepoint,
    levels = c("aufnahme", "verlaufsmessung", "abschlussmessung")
  ))


# 5. re-label tidy variable key ------------------------------------------------
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
  add_row(var_name = "timepoint", label = "timepoint")

# setdiff(colnames(data_tidy), var_key_tidy$var_name)

# 6. relabel basisdoku timepoint -----------------------------------------------
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

# 7. tidy and add BSI-18 data --------------------------------------------------
shared_ids <- intersect(data_tidy$code, bsi_data$code)
bsi_missing_cases <- setdiff(data_tidy$code, bsi_data$code)

# pivot BSI data to longer format
bsi_data_tidy <- bsi_data %>%
  rename_with(~ gsub(
    pattern = "^(.*?)(aufnahme|verlaufsmessung|abschlussmessung)$",
    replacement = "\\1_\\2", .x), !contains("_")) %>%
  pivot_longer(cols = -c(code, setting),
               names_to = c(".value", "timepoint"),
               names_sep = "_")

# relabel BSI data
bsi_labels <- filter(var_key_tidy, str_detect(var_name, "bsi")) %>%
  mutate(var_name = str_replace_all(var_name, "bsi", "b18")) %>%
  filter(var_name %in% colnames(bsi_data_tidy))

labelled::var_label(bsi_data_tidy) <- setNames(as.list(bsi_labels$label),
                                           bsi_labels$var_name)

# merge new BSI data to dataset
# old bsi columns start with "bsi", new ones with "b18"
data_tidy <- left_join(data_tidy, bsi_data_tidy,
                       by = c("setting", "code", "timepoint"))


# 8. update bas and ddt variables ----------------------------------------------
# setdiff(colnames(badok), colnames(data_tidy))
bas_ddt_vars <- badok %>%
  select(-c(code, setting, timepoint)) %>%
  colnames()

num_cols <- c("ddt001", "ddt009", "ddt018", "ddt019", "ddt020", "ddt021",
              "ddt024", "ddt025")

data_tidy_updated <- data_tidy %>%
  select(-all_of(bas_ddt_vars)) %>%
  left_join(badok, by = c("setting", "code", "timepoint")) %>%
  mutate(across(all_of(num_cols), as.numeric)) %>%
  select(all_of(names(data_tidy)))

# relabel
labelled::var_label(data_tidy_updated) <- setNames(as.list(var_key_tidy$label),
                                           var_key_tidy$var_name)



# 9. export   ------------------------------------------------------------------

if (save_output) {

  write.xlsx(var_key_tidy, here("output", "tables", "variable_key_tidy.xlsx"))
  write.xlsx(data_tidy_updated, here("data", "processed", "data_tidy_updated.xlsx"))

}





