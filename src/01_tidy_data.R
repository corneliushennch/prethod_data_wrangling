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
  ~ select(raw_data_clean, code, all_of(contains(.x)))
) %>%
  # replace empty strings with NA
  map(~ mutate(.x, across(where(is.character), ~ if_else(. == "", NA, .)))) %>%
  # remove rows with no data
  # map(~ filter(.x, rowSums(!is.na(select(., -code))) > 0)) %>%
  # remove the data source name from the column names
  map(~ rename_with(.x, ~ str_remove(., "_tk_d|_de_kiz"))) %>%
  # name the two data frames
  set_names("tk_d", "dekiz")

# check if colnames are equal

setdiff(colnames(data_split[[1]]), colnames(data_split[[2]]))

# 2. checking IDs and colnames -------------------------------------------------

# completely empty records, that get excluded above
empty_records <- setdiff(
  raw_data$CODE,
  c(data_split[["TK_D"]]$CODE, data_split[["DeKIZ"]]$CODE)
)

# filter(raw_data, CODE %in% empty_records) %>% glimpse()

# Spelling error Katamese
data_split[["DeKIZ"]] <- data_split[["DeKIZ"]] %>%
  rename_with(~ str_replace(., "Katamese", "Katamnese"))

# check if all columns match
# compare_df_cols_same(data_split[[1]], data_split[[2]]) # -> complete match
# compare_df <- compare_df_cols(data_split[["TK_D"]], data_split[["DeKIZ"]])
# compare_df %>% View()

# 3. combining  ----------------------------------------------------------------
# binding the two parts back together for further processing

# bind the data frames together -> labels get lost in this step
tidy_data <- bind_rows(data_split, .id = "setting") %>%
  # rename the columns
  rename_with(
    ~ gsub("^([[:alnum:]]+)(Aufnahme|Verlaufsmessung|Abschlussmessung|Katamnese)", "\\1_\\2", .),
    -CODE
  ) %>%
  # pivot the data frame
  pivot_longer(
    cols = -c(CODE, setting),
    names_to = c(".value", "timepoint"),
    names_sep = "_"
  ) %>%
  # remove duplicates
  distinct(CODE, timepoint, .keep_all = TRUE)


# 4. variable wrangling    -----------------------------------------------------
## 4.1 DDT categories    --------------------------------------------------------
ddt_vars <- c("DDT015", "DDT016", "DDT017", "DDT023")
ddt_levels <- c("0", "1", "2", "3", "4", "5", "> 5")

# collopase DDT variable categories
tidy_data <- tidy_data %>%
  mutate(
    across(all_of(ddt_vars), round),
    across(all_of(ddt_vars), ~if_else(. > 5, "> 5", as.character(.))),
    across(all_of(ddt_vars), ~factor(., levels = ddt_levels))
  ) %>%
  # order of timepoints
  mutate(timepoint = factor(timepoint, levels = c("Aufnahme", "Verlaufsmessung",
                                "Abschlussmessung", "Katamnese")))

## 4.2 small corrections -------------------------------------------------------
# years after suicide attempt -> two year numbers (2022, 2020)
# tidy_data %>%
#   filter(timepoint == "Aufnahme" & setting == "DeKIZ") %>%
#   filter(DDT024 == 2022 | DDT024 == 2020) %>%
#   mutate(test = FLZDAT - DDT024) %>%
#   select(FLZDAT, DDT024, test)

# correcting these two depending on admission date
tidy_data <- tidy_data %>%
  mutate(DDT024 = case_when(
    DDT024 == 2020 ~ 0,
    DDT024 == 2022 ~ 1,
    TRUE ~ DDT024
  ))

# 5. re-label tidy variable key ------------------------------------------------
var_key_tidy <- var_key %>%
  mutate(
    across(everything(), ~ str_replace(., "Katamese", "Katamnese")),
    across(everything(),
           ~ str_remove_all(., "(Aufnahme|Verlaufsmessung|Abschlussmessung|Katamnese)")),
    across(everything(), ~ str_remove_all(., "(_TK_D|_DeKIZ)"))
  ) %>%
  distinct(var_name, .keep_all = TRUE) %>%
  add_row(var_name = "setting", label = "Behandlungssetting")

# relabel tidy dataset
labelled::var_label(tidy_data) <- setNames(as.list(var_key_tidy$label),
                                           var_key_tidy$var_name)

# 6. filter completely empty cases ---------------------------------------------

exclude <- tidy_data %>% filter(is.na(BD2DAT) & timepoint == "Aufnahme") %>%
  pull(CODE)

tidy_data <- tidy_data %>% filter(!(CODE %in% exclude))
# tidy_data %>% select(print_date_vars) %>% glimpse()
# 7. export   ------------------------------------------------------------------

if (save_output) {
  write.xlsx(var_key_tidy, here("output", "tables", "variable_key_tidy.xlsx"))
  write.xlsx(tidy_data, here("data", "processed", "tidy_data.xlsx"))

}





