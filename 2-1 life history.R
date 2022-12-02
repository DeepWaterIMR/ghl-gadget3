## ---------------------------
##
## Script name:
##
## Purpose of script:
##
## Author: Mikko Vihtakari // Institute of Marine Research, Norway
## Email: mikko.vihtakari@hi.no
##
## Date Created: 2021-02-12
##
## ---------------------------

## Source the run first script

# source("0 run first.R")

## ---------------------------

## Load packages

## ---------------------------

if(reload_data) {

  ## Read initial sigmas

  init_sigma <- readr::read_csv(
    "../ghl-gadget-data/data/out/Initial ldist data.csv",
    col_types = cols())

  #############################
  # Save the required data ####

  save(init_sigma, file = file.path(base_dir, "data/Initial stock parameters.rda"))

  # rm(age_dat)

} else { ## !reload_data case
  load(file.path(base_dir, "data/Initial stock parameters.rda"))
}
