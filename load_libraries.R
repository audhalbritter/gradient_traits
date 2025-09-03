# libraries

library(targets)
library(tidyverse)
library(janitor)
library(vegan)
library(ggvegan)
library(traitstrap)
library(dataDocumentation)
library(dataDownloader)
library(broom)
library(glue)
library(geodata)
library(terra)

# Use httpgd for in-IDE plotting (Cursor/VS Code) when interactive
if (interactive()) {
  if (requireNamespace("httpgd", quietly = TRUE)) {
    # Set httpgd as default device so plots render in the IDE viewer
    options(device = "httpgd")
    # Ensure a server is running early; harmless if one already exists
    try(httpgd::hgd(silent = TRUE), silent = TRUE)
  }
}