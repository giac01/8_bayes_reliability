# Wipe Environment -------------------------------------------------------------

rm(list=ls(all.names = TRUE))

gc()

# Load Packages ----------------------------------------------------------------

library(brms)
library(tidyverse)
library(future)
library(future.apply)
library(cmdstanr)

if (!requireNamespace("gbtoolbox", quietly = TRUE)) {
  devtools::install_github("giac01/gbtoolbox")
}

library(gbtoolbox)

# Load Helper Functions --------------------------------------------------------

list.files(file.path("helper_functions"), pattern = "\\.R$", full.names = TRUE) %>%
  lapply(., function(x) {source(x)})
 
# END