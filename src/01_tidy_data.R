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
  ~ select(raw_data, code, import, all_of(contains(.x))) # raw_data
) %>%
  # replace empty strings with NA
  map(~ mutate(.x, across(where(is.character), ~ if_else(. == "", NA, .)))) %>%
  # remove rows with no data
  map(~ filter(.x, rowSums(!is.na(select(., -code, -import))) > 0)) %>%
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
    -c("code", "setting", "import")
  ) %>%
  # pivot the data frame
  pivot_longer(
    cols = -c(code, setting, import),
    names_to = c(".value", "timepoint"),
    names_sep = "_"
  )

# check duplicates -> keep for now (probably due to multiple entries for the
# same patient for different treatment periods, will get filtered out later when
# filtering for complete cases
duplicates <- data_tidy %>%
  group_by(code, timepoint) %>%
  filter(n() > 1)

# show all columns containing only NA
# data_tidy %>% select(where(~ all(is.na(.))))

# drop all columns containing only NA -> none removed
data_tidy <- data_tidy %>%
  select(where(~ !all(is.na(.))))

# re-attach bas021 column with only NA values (for compatibility with 2024 data)
# -> not necessary anymore, because we want to keep all cases for the analysis
# data_tidy <- data_tidy %>%
#   add_column(bas021 = NA_real_, .after = "bas020")

# data_tidy %>% select(code, setting, timepoint, contains("bd2")) %>% view()

# 2. filter for complete cases -------------------------------------------------
# bdi sum admission and discharge n = 467

# into wide format for counting
wide <- data_tidy %>%
  select(code, timepoint, import, bd2sum) %>%
  pivot_wider(names_from = timepoint, values_from = bd2sum)

# ids of patients with complete cases for admission and discharge
complete_cases <- wide %>%
  filter(!is.na(aufnahme) & !is.na(abschlussmessung)) %>%
  pull(code)

# this is not necessary anymore, because we want to keep all cases for the analysis
# data_tidy <- data_tidy %>%
#   filter(code %in% complete_cases)

# check for completely empty rows
no_data_rows <- data_tidy %>%
  filter(rowSums(!is.na(select(., -code, -setting, -timepoint, -import))) == 0)


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

# check
# data_tidy %>%
#   select(code, setting, timepoint, all_of(bas_vars)) %>%
#   filter(timepoint %in% c("aufnahme", "abschlussmessung")) # %>%  View()


# 5. update bas and ddt variables ----------------------------------------------
setdiff(colnames(badok), colnames(data_tidy))

# select variables that are in badok but not in data_tidy
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


# reorder columns in data_tidy_updated to match var_key_tidy order
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


# 6. replace data from manually curated 2024 dataset ---------------------------
# TODO: just replace ddt and bas columns to avoid variable coding mix-up!!!
# filtered df updated cases for data_tidy -> remove cols -> replace from data_2024

# all columns of data_2024 are present in data_tidy
setdiff(colnames(data_2024), colnames(data_tidy_updated))

# but not all columns of data_tidy are present in data_2024 -> mainly date,  status and version columns
# but also bas005 and bas007 (MINI and reason of therapy end)
col_diff <- setdiff(colnames(data_tidy_updated), colnames(data_2024))

# join missing columns (bas005 and bas007) to data_2024 from data tidy
data_2024 <- data_2024 %>%
  left_join(data_tidy_updated %>% select(setting, code, timepoint, bas005, bas007),
            by = c("setting", "code", "timepoint"))

# update coldiff after adding missing columns
col_diff <- setdiff(colnames(data_tidy_updated), colnames(data_2024))

# drop these cols from data_tidy_updated to avoid issues with the join
data_tidy_updated <- data_tidy_updated %>%
  select(-all_of(col_diff))

# replace values in all columns beginning with "bas" or "ddt" in data_tidy_updated
# with values from data_2024 for matching IDs (code) and timepoints

# update bas_ddt_vars by removing col_diff cols
bas_ddt_vars <- setdiff(bas_ddt_vars, col_diff)

# temporary df data_tidy_updated filtered by IDs present in data_2024
data_tidy_temp <- data_tidy_updated %>%
  filter(code %in% data_2024$code)  %>%
  # drop all columns beginning with "bas" or "ddt"
  select(-starts_with("bas"), -starts_with("ddt"))

# join "bas" and "ddt" columns from data_2024 to data_tidy_temp by code, setting and timepoint
data_tidy_temp <- data_tidy_temp %>%
  left_join(data_2024 %>% select(code, timepoint, setting, starts_with("bas"), starts_with("ddt")),
            by = c("code", "setting", "timepoint")) %>%
  # remove duplicates which might have been created by the join
  distinct()

# harmonize column order
data_tidy_temp <- data_tidy_temp %>%
  select(all_of(colnames(data_tidy_updated)))

# check for matching columns -> checks out!
if (!identical(colnames(data_tidy_temp), colnames(data_tidy_updated))) {
  stop("Column names of data_2024 and data_tidy are not identical")
} else {
  message("Column names of data_2024 and data_tidy are identical")
}

# check if all IDs in data_2024 are present in data_tidy
if (!all(data_tidy_temp$code %in% data_tidy_updated$code)) {
  stop("Not all IDs in data_2024 are present in data_tidy")
} else {
  message("All IDs in data_2024 are present in data_tidy")
}

# check variable class
col_classes_tidy <- data_tidy_updated %>%
  map(~ class(.x)) %>% stack() %>%
  rename(var_name = "ind" , class = "values") %>%
  left_join(var_key_tidy, by = "var_name")

col_classes_temp <- data_tidy_temp %>%
  map(~ class(.x)) %>% stack() %>%
  rename(var_name = "ind" , class = "values") %>%
  left_join(var_key_tidy, by = "var_name")

col_classes <- col_classes_tidy %>%
  left_join(col_classes_temp, by = "var_name", suffix = c("_tidy", "_temp")) %>%
  mutate(class_match = class_tidy == class_temp)

# need all to be converted to numeric in data_tidy_updated
class_mismatch <- col_classes %>%
  filter(!class_match) %>%
  select(var_name, class_tidy, class_temp)

data_tidy_temp %>%
  select(code, all_of(class_mismatch$var_name)) %>% View()

data_tidy_updated %>%
  select(code, all_of(class_mismatch$var_name)) %>% View()

# convert to numeric -> no NAs generated
data_tidy_updated <- data_tidy_updated %>%
  mutate(across(all_of(class_mismatch$var_name), as.numeric))

# drop all cases from data_tidy that are present in data_2024 -> 1404 rows removed
data_tidy_dropped <- data_tidy_updated %>%
  filter(!code %in% data_tidy_temp$code)

# bind data_2024 to data_tidy -> 1404 rows added
data_tidy_2026 <- bind_rows(data_tidy_dropped, data_tidy_temp)

# order by code and timepoint
data_tidy_2026 <- data_tidy_2026 %>%
  mutate(code = factor(code, levels = str_sort(unique(code), numeric = TRUE))) %>%
  arrange(code, timepoint)

# order by code and timepoint
data_tidy_updated <- data_tidy_updated %>%
  mutate(code = factor(code, levels = str_sort(unique(code), numeric = TRUE))) %>%
  arrange(code, timepoint)

# filter for differences -> gives no rows, so all cases in data_2024 are present in data_tidy_updated

differences <- anti_join(data_tidy_updated,
                         data_tidy_2026,
                         by = c("code", "setting", "timepoint"))

# comparison <- left_join(data_tidy_updated, data_tidy_2026, by = c("code", "setting", "timepoint"), suffix = c("_tidy", "_2026")) %>%
#   select(code, setting, timepoint, ends_with(c("_tidy", "_2026"))) %>%
#   select(code, setting, timepoint, starts_with("bas"), starts_with("ddt")) %>%
#   distinct()
#
# order_vec <- c("code", "setting", "timepoint", sort(setdiff(colnames(comparison), c("code", "setting", "timepoint"))))


## 6.3 update variable key -----------------------------------------------------

# adjust var_key_tidy to include all variables in data_tidy_2026
var_key_tidy <- var_key_tidy %>%
  filter(var_name %in% colnames(data_tidy_2026))

# relabel tidy data set
labelled::var_label(data_tidy_2026) <- setNames(as.list(var_key_tidy$label),
                                           var_key_tidy$var_name)

# 9. export   ------------------------------------------------------------------

if (save_output) {
# xlsx
  write.xlsx(var_key_tidy, here("output", "tables", "variable_key_tidy.xlsx"))
  write.xlsx(data_tidy_2026, here("output", "tables", "data_tidy_2026_v3.xlsx"))

  # csv
  write_csv2(var_key_tidy, here("output", "tables", "variable_key_tidy.csv"))
  write_csv2(data_tidy_2026, here("output", "tables", "data_tidy_2026_v3.csv"))
}

# TODO: Maybe checks and comparisons for v1 vs. v2





