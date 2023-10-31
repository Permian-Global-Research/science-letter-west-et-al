# load all functions in the R/ directory
list.files("R", pattern = ".r$", full.names = TRUE) |>
  setdiff("R/load-all.r") |>
  purrr::walk(source)

# load packages used in West et al functions.
library(plyr)
library(Synth) # synthetic control package
library(MSCMT) # for a more robust optimization procedure
library(foreign)
