#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.
source("load_libraries.R")

targets::tar_make()

# run only one target
# targets::tar_delete(bioclim)
# targets::tar_make(bioclim)

tar_load_everything()
