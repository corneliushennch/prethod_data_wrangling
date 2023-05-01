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

# 1. Missing plot --------------------------------------------------------------
# overview missing plot from finalfit package
missing_plots <- map(c("DeKIZ", "TK_D"), ~ filter(
    .data = labelled::remove_var_label(tidy_data) %>% arrange(timepoint),
    setting == .x)) %>%
  map(~missing_plot(.x)) %>%
  set_names(c("DeKIZ", "TK_D"))

# with naniar::gg_miss_fct ordered by timepoint
miss_plots <- map(c("DeKIZ", "TK_D"), ~filter(tidy_data, setting == .x)) %>%
  map(~naniar::gg_miss_fct(.x, fct = timepoint)) %>%
  set_names(c("DeKIZ", "TK_D")) %>%
  imap(~ .x +
         labs(title = glue("Missing data {.y}")) +
         theme(legend.position = "none"))

leg <- get_legend(miss_plots[[1]] + theme(legend.position = "bottom"))

miss_plot_time <- plot_grid(plotlist = miss_plots, nrow = 1)
miss_plot_time <- plot_grid(miss_plot_time, leg,
                            nrow = 2, rel_heights = c(0.95, 0.05))


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
         height = 30, width = 6)


}

# -> collapse number of categories for DDT015, DDT016, DDT017, DDT023







