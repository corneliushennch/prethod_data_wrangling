# Header start =================================================================
# 02_summary_tables.R
#
# Author: Hennch Cornelius (cornelius.hennch@charite.de)
#
# Description: Script to create summary tables for the TK-D/DeKIZ dataset
#
# Input: imported data
# Output: .xlsx (huxtable) and/or .png tables
#
# Code written according to Hadley Wickhams "tidyverse style guide"
# Header end ===================================================================


# 1. missings summary ----------------------------------------------------------
missing_summary <- data_tidy_2026 %>%
  group_by(source, setting, timepoint) %>%
  # count NAs for each variable
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  select(-code) %>%
  select(source, setting, timepoint, ends_with("dat")) %>%
  # pivot longer
  pivot_longer(cols = -c(source, setting, timepoint),
               names_to = "variable",
               values_to = "missing_count") %>%
  pivot_wider(names_from = timepoint, values_from = missing_count) %>%
  select(source, setting, variable, aufnahme, verlaufsmessung, abschlussmessung)

# calculate total cases per setting
total_cases <- data_tidy_2026 %>% group_by(setting) %>%
  distinct(code) %>% count()

# missing percentages per setting
missing_percentages <- missing_summary %>%
  left_join(total_cases, by = "setting") %>%
  mutate(across(c(aufnahme, verlaufsmessung, abschlussmessung),
                ~ (.x / n) * 100))

data_percentages <- missing_percentages %>%
  # subtract 100 from percentages to get the percentage of available data
  mutate(across(c(aufnahme, verlaufsmessung, abschlussmessung),
                ~ 100 - .x))

# export as .xlsx with three worksheets
if (save_output) {
  # bind in a named list
  data_summaries <- lst(missing_summary, missing_percentages, data_percentages)
  # create a new workbook
  wb <- createWorkbook()
  # add worksheets and write data
  walk2(names(data_summaries), data_summaries, ~ {
    addWorksheet(wb, .x)
    writeData(wb, .x, .y)
  })

  # save workbook
  saveWorkbook(wb, here("output", "tables", glue("{today}_missing_summary.xlsx")), overwrite = TRUE)

}


# 2. summary of BAS with missings ----------------------------------------------

# vars to include
summary_vars <- colnames(data_tidy) %>% str_subset("ddt[:digit:]")

table_one <- data_tidy_updated %>%
  filter(timepoint == "aufnahme") %>%
  tbl_summary(by = "setting",
              include = all_of(summary_vars),
              missing_text = "Missing",
              statistic = list(
                ddt024 ~ "{mean} ({sd})",
                ddt025 ~ "{mean} ({sd})")
  ) %>%
  add_overall() %>%
  add_p() %>%
  bold_p() %>%
  bold_labels()

# export
if (save_output) {
  table_one %>%
    as_hux_xlsx(here("output", "tables", glue("{today}_summary_table.xlsx")))
}


