# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("dataDownloader", "tidyverse", "DBI", "RSQLite", "janitor", "vegan", "ggvegan", "traitstrap", "dataDocumentation", "readxl", "broom", "glue")
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multicore")

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.

#Combine target plans
combined_plan <- c(
  download_plan,
  import_plan,
  cleaning_plan,
  transformation_plan,
  analysis_plan,
  figure_plan
  # si_figures_plan#,
  #manuscript_plan
)

