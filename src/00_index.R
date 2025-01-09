# Header start =================================================================
# 00_index.R
#
# Author: Hennch Cornelius (cornelius.hennch@charite.de)
#
# Description: Template for analysis index script that integrates multiple
# scripts/reports for the PreTHOD project
#
# TODO replace by targets pipeline
#
# Code written according to Hadley Wickhams "tidyverse style guide"

# Packages & dependencies ------------------------------------------------------
tictoc::tic()
work_dir <- getwd()

# packages and functions
suppressPackageStartupMessages({
  library(tidyverse) # for data munging
  library(tidylog) # inline analysis feedback
  library(janitor) # for data cleaning
  library(here) # for finding files
  library(glue) # for naming files
  library(gtsummary) # for summary statistics and tables
  # library(kableExtra) # for table formatting in Markdown
  # (finalfit) # for tidy modeling and assessing tidy data
  # library(glmnet) # penalized regression modeling
  library(openxlsx) # for exporting excel files
  library(cowplot) # for arranging plots
})

# custom functions
source(here("src", "00_functions.R"))

# today's date
today <- format(Sys.Date(), "%Y%m%d")

# Header end ===================================================================

# print intermediate figures/tables/reports?
save_output <- FALSE
render_reports <- FALSE

# 1. Import and tidy -----------------------------------------------------------
# import data and export variable key for overview
source(here("src", "01_import.R"))

# preparatory data wrangling
source(here("src", "01_tidy_data.R"))

# 2. Transform -----------------------------------------------------------------
# summary tables with gtsummary
source(here("src", "02_summary_tables.R"))

# 3. Explore  ------------------------------------------------------------------
source(here("src", "03_explore_missing_data.R"))

# 4. Visualize -----------------------------------------------------------------


# 5. Communicate  --------------------------------------------------------------
# render .Rmd report files
# they can take objects from the global environment that have been created by
# the previous analysis steps

if (render_reports) {
  # powerpoint presentations
  rmarkdown::render(
    input = "markdown/01_data_overview_missings.Rmd",
    output_dir = "output/reports/presentations/",
    knit_root_dir = work_dir,
    intermediates_dir = work_dir
  )

  # pdf report
  rmarkdown::render(
    input = "markdown/report.Rmd",
    output_dir = "output/reports/",
    output_format = "pdf_document",
    knit_root_dir = work_dir,
    intermediates_dir = work_dir
  )
}

# runtime
tictoc::toc()
