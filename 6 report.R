## ---------------------------
##
## Script name:
##
## Purpose of script:
##
## Author: Mikko Vihtakari // Institute of Marine Research, Norway
## Email: mikko.vihtakari@hi.no
##
## Date Created: 2021-09-17
##
## ---------------------------

## Source the run first script

# source("0 run first.R")

## ---------------------------

## Load packages

## ---------------------------

## Source or list custom functions used within the script

## ---------------------------

## Read data

## ---------------------------

female_imm_report <- g3s_clone(female_imm, 'female_imm_report') %>%
  g3s_time(year = local(model_params$year_range),
           step = as.integer(names(model_params$timestep_fun)))

female_mat_report <- g3s_clone(female_mat, 'female_mat_report') %>%
  g3s_time(year = local(model_params$year_range),
           step = as.integer(names(model_params$timestep_fun)))

male_imm_report <- g3s_clone(male_imm, 'male_imm_report') %>%
  g3s_time(year = local(model_params$year_range),
           step = as.integer(names(model_params$timestep_fun)))

male_mat_report <- g3s_clone(male_mat, 'male_mat_report') %>%
  g3s_time(year = local(model_params$year_range),
           step = as.integer(names(model_params$timestep_fun)))

report_actions <- list(

  # Report numbers
  g3a_report_stock(
    female_imm_report, female_imm,
    gadget3:::f_substitute(~stock_ss(x),
                           list(x = as.symbol(paste0("female_imm", "__num"))))
  ),
  g3a_report_stock(
    female_mat_report, female_mat,
    gadget3:::f_substitute(~stock_ss(x),
                           list(x = as.symbol(paste0("female_mat", "__num"))))
  ),
  g3a_report_stock(
    male_imm_report, male_imm,
    gadget3:::f_substitute(~stock_ss(x),
                           list(x = as.symbol(paste0("male_imm", "__num"))))
  ),
  g3a_report_stock(
    male_mat_report, male_mat,
    gadget3:::f_substitute(~stock_ss(x),
                           list(x = as.symbol(paste0("male_mat", "__num"))))
  )
  # g3a_report_stock(male_mat_report, male_mat,
  #                  gadget3:::f_substitute(~stock_ss(x),
  #                                         list(x = as.symbol(paste0("male_mat", "__num"))))
  # ),
  # Report mean weight
  # g3a_report_stock(male_imm_report, male_imm,
  #                  gadget3:::f_substitute(~stock_ss(x),
  #                                         list(x=as.symbol(paste0("male_imm", "__wgt"))))),
  #
  # # Report biomass caught by survey
  # g3a_report_stock(male_imm_report, male_imm,
  #                  gadget3:::f_substitute(~stock_ss(x),
  #                                         list(x=as.symbol(paste0("male_imm", "__igfs"))))),
  #
  # # Report biomass caught by commercial, longline
  # g3a_report_stock(male_imm_report, male_imm,
  #                  gadget3:::f_substitute(~stock_ss(x),
  #                                         list(x=as.symbol(paste0("male_imm", "__lln"))))),
  #
  # # Bottom trawl
  # g3a_report_stock(male_imm_report, male_imm,
  #                  gadget3:::f_substitute(~stock_ss(x),
  #                                         list(x=as.symbol(paste0("male_imm", "__bmt"))))),
  #
  # # Gillnet
  # g3a_report_stock(male_imm_report, male_imm,
  #                  gadget3:::f_substitute(~stock_ss(x),
  #                                         list(x=as.symbol(paste0("male_imm", "__gil"))))),
  #
  # # Foreign
  # g3a_report_stock(male_imm_report, male_imm,
  #                  gadget3:::f_substitute(~stock_ss(x),
  #                                         list(x=as.symbol(paste0("male_imm", "__foreign"))))),
  #
  # # Recruitment numbers and weight
  # g3a_report_stock(male_imm_report, male_imm,
  #                  gadget3:::f_substitute(~stock_ss(x),
  #                                         list(x=as.symbol(paste0("male_imm", "__renewalnum"))))),
  #
  # # Report suitability (survey)
  # g3a_report_stock(male_imm_report, male_imm,
  #                  gadget3:::f_substitute(~stock_ss(x),
  #                                         list(x=as.symbol(paste0("male_imm", "__suit_igfs")))))

)
