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

if(exists("mdb")) mfdb:: mfdb_disconnect(mdb)

rm(list = ls())
source("0 run first.R")

## Model settings

reset_model <- TRUE # Change to TRUE to reset the model (delete all model files). ONLY do this if you really want to DELETE the existing model
bootstrap <- FALSE # Not implemented yet
base_dir <- "model_files" # All files and output of the currently run model will be placed in a folder with this name
mfdb_path <- "../ghl-gadget-data/data/mfdb/ghl.duckdb" # Set MDFB path here. Clone ghl-gadget-data to your computer in the same base directory than ghl-gadget for the default path to work

## Connect to the MFDB database

if(!exists("mdb")) {
  # When the ghl-gadget-data repo is made public, this should work as mfdb_path "https://github.com/DeepWaterIMR/ghl-gadget-data/raw/main/data/mfdb/ghl.duckdb"
  if(grepl("https:", mfdb_path)) {
    temp <- tempfile()
    tmp <- try(suppressWarnings(download.file(mfdb_path, temp)), silent = TRUE)

    if(class(tmp) == "try-error") {
      stop("Did not manage to find the duckdb file online. A wrong URL or a private Github repo?")
    } else {
      mdb <- mfdb::mfdb(tmp)
    }
  } else {
    # At the moment there is no way to fetch the MFDB database online. Clone ghl-gadget-data to your computer in the same base directory than ghl-gadget and then this should work as mfdb_path
    mdb <- mfdb::mfdb(mfdb_path)
  }
}

## Model and stock parameters, setting up areas, time steps, etc.

source("1 settings.R")

## Get data from MFDB

source("2-1 life history.R")
source("2-2 survey indices.R")
source("2-3 landings.R")
source("2-4 catch distribution.R")

## Setup the stocks

source("3 stocks.R")

## Add fleets and landings data

source("4 fleets.R")

## Setup likelihood components

source("5 likelihood.R")

## Formulate R based model and define initial parameters

source("6 initial parameters.R")

## Run r-based model ####

res <- model(param)


## Scratch code under ####

attributes(res)$OtherNor_fishery__catch





male_imm_report <- g3s_clone(male_imm, 'male_imm_report') %>%
  g3s_time(year = local(model_params$year_range), step = 1:4)

# Disaggregated report storage for *_mat (we store with same age/step/length as ling itself)
male_mat_report <- g3s_clone(male_mat, 'male_mat_report') %>%
  g3s_time(year = local(model_params$year_range), step = 1:4)


g3a_report_stock(male_imm_report, male_imm,
                 gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("male_imm", "__num")))))

## Old stuff under
#
#
# ## Initial optimization
#
# gadget_optimize(gd, params.in = 'params.in', params.out = 'params.init')
#
# ## Fit and make the initial plots
#
# fit <- gadget_fit(gd = gd, params.in = "params.init")
#
# # gadget_evaluate(gd, log = "thismodelaintcrashingisit.log")
#
# png("figures/ICES_plot_params_init.png", width = pagewidth*1.4, height = pagewidth, units = "mm", res = 300)
# cowplot::plot_grid(
#   plot(fit, data = 'res.by.year', type = 'catch'),
#   plot(fit, data = 'res.by.year', type = 'rec'),
#   plot(fit, data = 'res.by.year', type = 'F'),
#   plot(fit, data = 'res.by.year', type = 'total')
# )
# dev.off()
#
# ## Iterate weights and params
#
# gadget_iterative_stage_1(gd, params.in = 'params.init') %>%
#   parallel::mclapply(gadget_optimize, mc.cores = parallel::detectCores()) %>%
#   gadget_iterative_stage_2() %>%
#   gadget_optimize()
#
# gadget_evaluate(gd, params.in = 'params.init', log = "whatfhappened.txt")
#
# ## Fit the model with "final" parameters
#
# fit <- gadget_fit(gd = gd, params.in = "WGTS/params.final")
#
#
# ## END (scratch code under) ####
