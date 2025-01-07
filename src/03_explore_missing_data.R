# Header start =================================================================
# 03_explore_missing_data.R"
#
# Author: Hennch Cornelius (cornelius.hennch@charite.de)
#
# Description: Script to explore missing data in the depression dataset
#
# Input: imported data
# Output: .xlsx (huxtable) and/or .png tables
#
# Code written according to Hadley Wickhams "tidyverse style guide"
# Header end ===================================================================

# 1. add variable for missings per questionnaire -----------------------------

# Identify the items in each questionnaire
cols_by_quest <- split(names(data_tidy)[-c(1:3)],
                       substr(names(data_tidy)[-c(1:3)], 1, 3)) %>%
  map(~ c("code","timepoint", .x))

# create a list of dataframes, each dataframe contains the missing values for
# each questionnaire
missings_by_quest <- purrr::imap(cols_by_quest, ~ {
  # select the columns of the question
  df <- data_tidy %>% select(all_of(.x))
  # count the number of missing values per row -> way faster than rowwise
  df$NA_per_row <- rowSums(is.na(df))
  # calculate the percentage of missing values per row
  df <- df %>% mutate(NA_percentage = NA_per_row/(length(.x) - 2)) %>%
    # select the columns that we want to keep
    select(code, timepoint, NA_per_row, NA_percentage)
  # rename the columns
  colnames(df)[3:4] <- paste(.y, colnames(df)[3:4], sep = "_")
  # return the dataframe
  return(df)
}) %>%
  plyr::join_all(dfs = ., by = c("code", "timepoint"), match = "first") %>%
  left_join(data_tidy, ., by = c("code", "timepoint"))

# 2. summary of missings per timepoint/questionnaire ---------------------------

missings_by_quest <- missings_by_quest %>%
  mutate(across(contains("NA_perc"), ~ case_when(
    . == 1 ~ "100%",
    . >= 0.25 ~ "> 25%",
    . == 0 ~ "0%",
    . <= 0.25 ~ "< 25%") %>%
     factor(levels = c("0%","< 25%", "> 25%", "100%")),
    .names = "{.col}_fct"))

na_fct_cols <- str_subset(colnames(missings_by_quest), "_fct")

missing_summary <- map(c("dekiz", "tk_d"),
                       ~ filter(missings_by_quest, setting == .x)) %>%
  map(~ tbl_summary(.x,
    by = "timepoint",
    include = all_of(na_fct_cols)
  ))

missing_summary_combined <- tbl_merge(
  tbls = missing_summary,
  tab_spanner = c("deliz", "tk_d")
)

# 2. Missing plot --------------------------------------------------------------
## 2.1 overview missing plot from finalfit package -----------------------------
missing_plots <- map(c("dekiz", "tk_d"), ~ filter(
    .data = labelled::remove_var_label(data_tidy) %>% arrange(timepoint),
    setting == .x)) %>%
  map(~finalfit::missing_plot(.x)) %>%
  set_names(c("dekiz", "tk_d"))

## 2.2 with naniar::gg_miss_fct ordered by timepoint ---------------------------
miss_plots <- map(c("dekiz", "tk_d"), ~filter(data_tidy, setting == .x)) %>%
  map(~naniar::gg_miss_fct(.x, fct = timepoint)) %>%
  set_names(c("dekiz", "tk_d")) %>%
  imap(~ .x +
         labs(title = glue("Missing data {.y}")) +
         theme(legend.position = "none"))

leg <- get_legend(miss_plots[[1]] + theme(legend.position = "bottom"))

miss_plot_time <- plot_grid(plotlist = miss_plots, nrow = 1)
miss_plot_time <- plot_grid(miss_plot_time, leg,
                            nrow = 2, rel_heights = c(0.95, 0.05))

## 2.3 heatmap  ----------------------------------------------------------------

# wrangle data
heat_data <- missings_by_quest %>%
  select(!contains("_fct")) %>%
  pivot_longer(cols = contains("NA_percentage"),
               values_to = "NA_percentage",
               names_to = "quest") %>%
  mutate(quest = str_remove_all(quest, "_NA_percentage"),
         NA_percentage = NA_percentage * 100)

# create a list of two plots
missing_heatmap <- map(c("dekiz", "tk_d"), ~ {
  # filter the data to only include the setting
  filter(heat_data , setting == .x) %>%
    # create a plot with the x axis as timepoint, y axis as CODE, and fill as NA_percentage
    ggplot(aes(x = timepoint, y = code, fill = NA_percentage)) +
    # create a tile plot
    geom_tile(stat = "identity") +
    # colour gradient
    scale_fill_gradient(low = "darkgreen", high = "lightgrey") +
    # create a facet grid
    facet_grid(cols = vars(quest)) +
    # remove the y axis ticks and text
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = NULL, fill = "percentage \nof missings")
})

# combine plots
heat_combi <- plot_grid(
  missing_heatmap[[1]] +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
          ) +
    labs(y = "DeKIZ"),
  missing_heatmap[[2]] +
    theme(legend.position = "bottom",
          strip.text = element_blank(),
          strip.background = element_blank()) +
    labs(y = "TK-D"),
  nrow = 2
)

if (save_output) {

  # one combined miss plot for both settings
  ggsave(plot = heat_combi,
         filename = here("output", "plots",
                         glue("{today}_heat_missing_combi.pdf")),
         height = 8, width = 12)
}

# 3. missing data in quest. scores ---------------------------------------------

quest_scores <- c("FLZM00", "bsisum", "BD2SUM", "CTQO00", "BADO01",
                  "dasmit", "IE4M00", "DDTDAT", "BASDAT") %>%
  tolower()

miss_var_plots <- map(c("dekiz", "tk_d"), ~filter(data_tidy, setting == .x)) %>%
  map(~select(.x, timepoint, all_of(quest_scores))) %>%
  map(~naniar::gg_miss_var(.x, facet = timepoint))

# 4. line plot across time  ----------------------------------------------------
line_plot <- missings_by_quest %>%
  group_by(timepoint) %>%
  pivot_longer(cols = contains("_fct"), names_to = "quest", values_to = "missing") %>%
  mutate(quest = str_remove_all(quest, "_NA_percentage_fct")) %>%
  count(setting, timepoint, quest, missing, .drop = FALSE) %>%
  filter(missing == "0%") %>%
  ggplot(aes(x = timepoint, y = n, color = quest, group = quest)) +
  geom_line(stat = "identity", linewidth = 1) +
  theme_bw() +
  facet_wrap(~setting) +
  labs(y = "Number of complete questionnaires",
       x = "Timepoint",
       color = "Quest.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# print
if (save_output) {
  #two separate missing plots
  imap(missing_plots,
       ~ ggsave(plot = .x,
                filename = here("output", "plots",
                                glue("{today}_{.y}_missing.pdf")),
                height = 30, width = 10)
  )

  # one combined miss plot for both settings
  ggsave(plot = miss_plot_time,
         filename = here("output", "plots",
                         glue("{today}_missing_timepoint.pdf")),
         height = 28, width = 6)


}

# 5. prepare table for manual completion ---------------------------------------
# mainly BAS and DDT entries
missings_table <- data_tidy %>%
  filter(timepoint == "aufnahme") %>%
  select(code, timepoint, setting, starts_with(c("bas", "ddt")))

if (save_output) {

# data frame with variable explanation
var_key_bas <- labelled::var_label(missings_table) %>%
  enframe()


names(missings_table) <- labelled::var_label(missings_table)

## 5.2 basisdoku therapist   ---------------------------------------------------

# Create an empty workbook
missing_wb <- createWorkbook()

# Map over the sheet names vector and add worksheets to the workbook
# sheet_list <- map(sheet_names, ~addWorksheet(wb = missing_wb, sheetName = .x))

# Map over the missing_bas_list and sheet_list, and write data to the
# corresponding worksheets in the workbook
writeData(
    wb = missing_wb,
    x = missings_table,
    sheet = addWorksheet(wb = missing_wb, sheetName = "basidoku_missings"),
    keepNA = TRUE,
    na.string = "NA")

# Save the workbook to a specified file path
saveWorkbook(
  missing_wb,
  here("output","tables","missing_basisdoku.xlsx"),
  overwrite = TRUE
)

## 5.2 basisdoku patient   -----------------------------------------------------

# data frame with variable explanation
var_key_ddt <- labelled::var_label(missing_ddt_list[[1]]) %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "variable_name", values_to = "label")

# Create an empty workbook
missing_wb <- createWorkbook()

# Map over the sheet names vector and add worksheets to the workbook
sheet_list <- map(sheet_names, ~addWorksheet(wb = missing_wb, sheetName = .x))

# Map over the missing_bas_list and sheet_list, and write data to the
# corresponding worksheets in the workbook
map2(
  c(missing_ddt_list, list(var_key_ddt)),
  sheet_list,
  ~writeData(
    wb = missing_wb,
    x = .x,
    sheet = .y,
    keepNA = TRUE,
    na.string = "NA"
  )
)

# Save the workbook to a specified file path
saveWorkbook(
  missing_wb,
  here("output","tables","missing_basis_pat.xlsx"),
  overwrite = TRUE
)

}





