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
  ### Acquired by (from iterated fit object 2023-02-08:
  # optim_fit$stock.std %>%
  #   dplyr::filter(.data$number > 0) %>%
  #   dplyr::arrange(.data$year, .data$step, .data$stock, .data$age) %>%
  #   dplyr::group_by(.data$stock, .data$age) %>%
  #   dplyr::summarise(
  #     mean = mean(.data$mean_length, na.rm = TRUE),
  #     mean_sd = mean(.data$stddev_length, na.rm = TRUE),
  #     .groups = "drop"
  #   ) %>%
  #   rename(ms = mean_sd, ml = mean) %>%
  #   dplyr::select(age, stock, ms, ml) %>%
  #   write_csv("data/Initial ldist data.csv")

  init_sigma <- readr::read_csv("data/Initial ldist data.csv", 
                                col_types = cols())

  # init_sigma$ms <- init_sigma$ms*0.35
  # init_sigma[init_sigma$age == 1, "ms"] <- 2
  # init_sigma[init_sigma$age == 2, "ms"] <- 3


  #############################
  # Save the required data ####

  save(init_sigma, file = file.path(base_dir, "data/Initial stock parameters.rda"))

  # rm(age_dat)

} else { ## !reload_data case
  load(file.path(base_dir, "data/Initial stock parameters.rda"))
}
