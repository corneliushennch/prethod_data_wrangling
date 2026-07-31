# Header start =================================================================
# 04_missing_badok_table.R"
#
# Author: Hennch Cornelius (cornelius.hennch@charite.de)
#
# Description: Script to create a table of missing values for manual
# completion of the basisdoku data
#
# Input: imported data
# Output: .xlsx table for manual completion
#
# Code written according to Hadley Wickhams "tidyverse style guide"
# Header end ===================================================================

# 1. prepare table for manual completion ---------------------------------------
# mainly BAS and some DDT entries
missings_table <- data_tidy %>%
  filter(timepoint == "aufnahme") %>%
  select(code, timepoint, setting, starts_with(c("bas", "ddt"))) %>%
  left_join(patient_id, by = "code") %>%
  select(code, timepoint, setting, Nachname, Vorname, Geburtsdatum, everything()) %>%
  mutate(Nachname = str_remove_all(Nachname, "Fr. |Fr.|Hr. |Hr."))

if (save_output) {

  # data frame with variable explanation
  var_key_bas <- labelled::var_label(missings_table) %>%
    enframe()

  # replace names with labels
#   names(missings_table) <- labelled::var_label(missings_table)

  # Create an empty workbook
  missing_wb <- createWorkbook()

  # Map over the sheet names vector and add worksheets to the workbook
  # sheet_list <- map(sheet_names, ~addWorksheet(wb = missing_wb, sheetName = .x))

  # Map over the missing_bas_list and sheet_list, and write data to the
  # corresponding worksheets in the workbook
  writeData(
    wb = missing_wb,
    x = missings_table %>%
      filter(setting == "tk_d") %>%
      select(-setting, -timepoint),
    sheet = addWorksheet(wb = missing_wb, sheetName = "basidoku_missings"),
    keepNA = TRUE,
    na.string = "NA")

  # add worksheet for variable key
  writeData(
    wb = missing_wb,
    x = var_key_bas,
    sheet = addWorksheet(wb = missing_wb, sheetName = "variable_key"),
    keepNA = TRUE,
    na.string = "NA")

  # Save the workbook to a specified file path
  saveWorkbook(
    missing_wb,
    here("output","tables","2026_missing_basisdoku_tk_d.xlsx"),
    overwrite = TRUE
  )

}



