## ---------------------------
##
## Script name:
##
## Purpose of script:
##
## Authors:
## Mikko Vihtakari // Institute of Marine Research, Norway
## Will Butler and Bjarki Elvarsson // Marine and Freshwater Research Institute, Iceland
## Email: mikko.vihtakari@hi.no
##
## Date Created: 2021-02-12
##
## ---------------------------

## Source the run first script

if(exists("mdb")) mfdb:: mfdb_disconnect(mdb)

rm(list = ls())

source("0 run first.R")
# source("R/stock_param_functions.R")
# source('R/g3_iterative.R')
# source('R/g3_init_guess.R')
# source('R/g3_jitter.R')
# source('R/get_gadget_plots.R')
# source('R/g3_retro.R')
# source('R/step-utils.R')

## Model settings

reset_model <- FALSE # Change to TRUE to reset the model (delete all model files). ONLY do this if you really want to DELETE the existing model
reload_data <- FALSE # Set this to true to reload data from MFDB. If FALSE and the model folders (base_dir) exist, data are retrieved from the base_dir/data folder. Automatically set to TRUE if reset_model == TRUE or !dir.exists(base_dir)
bootstrap <- FALSE # Not implemented yet
base_dir <- "model_files" # All files and output of the currently run model will be placed in a folder with this name
mfdb_path <- "../ghl-gadget-data/data/mfdb/ghl.duckdb" # Set MDFB path here. Clone ghl-gadget-data to your computer in the same base directory than ghl-gadget for the default path to work
run_iterative <- TRUE # Whether or not to run iterative reweighting
run_retro <- FALSE # Run retrospective analysis?

## Optimisation mode (param_opt_mode), options:
# (1) parameters are bounded internally (ie using the bounded function) works with 'BFGS' optim method
# (2) parameters are bounded externally so the optim function must use a box-constrained optimisation method
# (3) global search, all parameters unbounded, unconstrained optimisation can be used (eg 'BFGS')

## How should the initial abundance be setup?
## Options:
## 0 - population is initialised at equilibrium
## 1 - parameter for each age group (across stocks)
## 2 - parameter for each age group of each stock

setup_options <- list(param_opt_mode = 1,
                      initial_abund_mode = 2)

## Whether or not to bound parameters internally
setup_options$bound_params <- ifelse(setup_options$param_opt_mode == 1, TRUE, FALSE)

###########################
### Modify reload_data ####

if(reset_model | !dir.exists(base_dir)) {
  reload_data <- TRUE

  if(!dir.exists(base_dir)) {
    message(base_dir, "/data does not exist. Setting reload_data to TRUE. Data are reloaded from MFDB.")
  } else {
   message("You want to reset the model. Setting reload_data to TRUE. Data are reloaded from MFDB.")
  }
}

## Connect to the MFDB database

if(!exists("mdb") & reload_data) {
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

## Get data out of the model

# source("6 report.R")

## Formulate R based model and define initial parameters

source("7 initial parameters.R")

## Run the R-model
result <- model(tmb_param$value)
result[[1]]

# List all available reports
print(names(attributes(result)))

## Run a R-based model ####

res <- model(param)
fit <- gadget3:::g3_fit(model,tmb_param)
gadget_plots(fit, file.path(base_dir, "figures"))

## Run the TMB-based model

model_tmb <- g3_tmb_adfun(tmb_model, tmb_param)

save(model_tmb, file = file.path(base_dir, "data/TMB model.rda"), compress = "xz")

fit.opt <- optim(g3_tmb_par(tmb_param),
                 model_tmb$fn,
                 model_tmb$gr,
                 method = 'BFGS',
                 control = list(trace = 2,maxit = 1000, reltol = .Machine$double.eps^2))
### Save the parameters

write.csv(as.data.frame(fit.opt$par), file = file.path(base_dir, "data/Optimized TMB parameters.csv"))
save(fit.opt, file = file.path(base_dir, "data/Optimized TMB model.rda"), compress = "xz")

## Stupid plots

# fit <- gadget3:::g3_fit(model, params.out)
fit <- gadget3:::g3_fit(model, g3_tmb_relist(tmb_param, fit.opt$par))
save(fit, file = file.path(base_dir, "data/Fitted optimized TMB model.rda"), compress = "xz")

cowplot::plot_grid(
  plot(fit, data = 'res.by.year', type = 'F'),
  plot(fit, data = 'res.by.year', type = 'total'),
  plot(fit, data = 'res.by.year', type = 'rec'),
  plot(fit, data = 'res.by.year', type = 'catch'),
  labels = "AUTO"
)



plot(fit, data = 'res.by.year', type = 'catch')
plot(fit, data = "summary")

## Scratch code under ####

# save.image(file = "/Users/a22357/Desktop/Ghl_model_that_worked.RData")

# Debugging tricks:

## Print the environment of a g3 object:
# ls(environment(f_init_abund))
# ls(rlang::f_env(f_init_abund))
