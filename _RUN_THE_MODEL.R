## ---------------------------
##
## Script name: Run the gadget3 model for NEA Greenland halibut
##
## Purpose of script: Master script to run the model. Source the entire script or run from top to a desired line (Ctrl+Alt+B on Rstudio)
##
## Authors:
## Mikko Vihtakari // Institute of Marine Research, Norway
## Will Butler and Bjarki Elvarsson // Marine and Freshwater Research Institute, Iceland
## Email: mikko.vihtakari@hi.no
##
## ---------------------------

## Source the run first script

if(exists("mdb")) mfdb:: mfdb_disconnect(mdb)

rm(list = ls())

source("0 run first.R")

## Model settings

reset_model <- TRUE # Change to TRUE to reset the model (delete all model files). ONLY do this if you really want to DELETE the existing model
reload_data <- FALSE # Set this to true to reload data from MFDB. If FALSE and the model folders (base_dir) exist, data are retrieved from the base_dir/data folder. Automatically set to TRUE if reset_model == TRUE or !dir.exists(base_dir)
previous_model_params_as_initial <- FALSE # Whether to use parameters from fit_opt object as initial values for tmb_params. Potentially speeds up the optimization.
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

## Formulate R based model and define initial parameters

source("6 initial parameters.R")

### Turn off age data
# tmb_param <- tmb_param %>% g3_init_guess('aldist', 0, NA, NA, 0)
# tmb_param$value$cdist_sumofsquares_EggaN_aldist_female_weight <- 1

## Fit the initial parameters to the model, print the likelihood score and make plots which will be overwritten by optimized parameter plots later.

result <- model(tmb_param$value)
result[[1]]

# fit_init <- gadget3:::g3_fit(model,tmb_param)
# gadget_plots(fit_init, file.path(base_dir, "figures"))

# png(file.path(base_dir, "figures/Initial_annual_plot.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
# plot_annual(fit_init)
# dev.off()

# List all available reports
# print(names(attributes(result)))

## Compile the TMB-based model

model_tmb <- g3_tmb_adfun(tmb_model, tmb_param)

save(model_tmb, file = file.path(base_dir, "data/TMB model.rda"), compress = "xz")

## Optimize model parameters. Takes hours.

# tmb_param %>% filter(optimise, lower >= upper)

optim_param <- optim(
  par = g3_tmb_par(tmb_param),
  fn = model_tmb$fn,
  gr = model_tmb$gr,
  method = 'BFGS', # for bounnded: 'L-BFGS-B'
  # lower = g3_tmb_lower(tmb_param),
  # upper = g3_tmb_upper(tmb_param),
  control = list(trace = 2,
                 maxit = 1000,
                 reltol = .Machine$double.eps^2,
                 parscale = g3_tmb_parscale(tmb_param))
)

### Save the model parameters

write.csv(as.data.frame(optim_param$par), file = file.path(base_dir, "data/Optimized TMB parameters.csv"))
save(optim_param, file = file.path(base_dir, "data/Optimized TMB parameters.rda"), compress = "xz")

## Plots

optim_fit <- gadget3:::g3_fit(model, g3_tmb_relist(tmb_param, optim_param$par))
save(optim_fit, file = file.path(base_dir, "data/Optimized TMB model fit.rda"), compress = "xz")

gadget_plots(optim_fit, file.path(base_dir, "figures"))

tmppath <- file.path(getwd(), base_dir, "figures")
gadget_plots(optim_fit, path = tmppath, file_type = "html")
rm(tmppath)



####################################################################
## Iterative reweighting and optimization                       ####
## Running this part takes a long time (3-6 hours on a server)  ####

iter_param <- g3_iterative(
  gd = base_dir,
  wgts = "iterative_reweighting",
  r_model = model,
  tmb_model = tmb_model,
  params.in = tmb_param,
  grouping = list(si_eggan = c('log_EggaN_SI_female',
                             'log_EggaN_SI_male'),
                  si_juv = c('log_Juv_SI_1',
                             'log_Juv_SI_2',
                             'log_Juv_SI_3'),
                  otherrus = c('otherrus_ldist_f',
                               'otherrus_ldist_m'),
                  trawlrus = c('trawlrus_ldist_f',
                               'trawlrus_ldist_m')),
  use_parscale = TRUE
)

iter_fit <- gadget3:::g3_fit(model, iter_param)
save(iter_fit, file = file.path(base_dir, "data/Iterated TMB model fit.rda"), compress = "xz")

gadget_plots(iter_fit, file.path(base_dir, "figures"))

tmppath <- file.path(getwd(), base_dir, "figures")
make_html(iter_fit, path = tmppath, file_name = "model_output_figures_iter.html")
rm(tmppath)

## Save workspace

save.image(file = file.path(base_dir, "data/gadget_workspace.RData"), compress = "xz")

## Scratch code under ####

# Debugging tricks:

## Print the environment of a g3 object:
# ls(environment(f_init_abund))
# ls(rlang::f_env(f_init_abund))
