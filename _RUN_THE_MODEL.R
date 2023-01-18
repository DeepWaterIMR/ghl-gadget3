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

if(exists("mdb")) mfdb::mfdb_disconnect(mdb)

rm(list = ls())

source("0 run first.R")

## Model settings

reset_model <- TRUE # Change to TRUE to reset the model (delete all model files). ONLY do this if you really want to DELETE the existing model
reload_data <- FALSE # Set this to true to reload data from MFDB. If FALSE and the model folders (base_dir) exist, data are retrieved from the base_dir/data folder. Automatically set to TRUE if reset_model == TRUE or !dir.exists(base_dir)
previous_model_params_as_initial <- FALSE # Whether to use parameters from fit_opt object as initial values for tmb_params. Potentially speeds up the optimization.
bootstrap <- FALSE # Not implemented yet
base_dir <- "model_files" # All files and output of the currently run model will be placed in a folder with this name
mfdb_path <- "../ghl-gadget-data/data/mfdb/ghl.duckdb" # Set MDFB path here. Clone ghl-gadget-data to your computer in the same base directory than ghl-gadget for the default path to work
run_iterative <- TRUE # Whether to run iterative reweighting (takes 3-10 hours)
set_weights <- TRUE # Whether to set manual weights for likelihood components from previous iterative reweighting. The weights are defined in 6 initial parameters.R
run_retro <- FALSE # Run retrospective analysis?
force_bound_params <- TRUE # Whether parameters should be forced to their bounds. Experimental feature making it easier to control the model.

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
                      initial_abund_mode = 1)

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
source("2-3 catch distribution.R")
source("2-4 catches.R")

## Setup the stocks

source("3 stocks.R")

## Add fleets and landings data

source("4 fleets.R")

## Setup likelihood components

source("5 likelihood.R")

## Formulate R based model and define initial parameters

source("6 initial parameters.R")

### Turn off likelihood components
# tmb_param <- tmb_param %>% g3_init_guess('aldist', 0, NA, NA, 0)
# tmb_param$value$cdist_sumofsquares_EggaN_aldist_female_weight <- 1
# tmb_param <- tmb_param %>%
#   g3_init_guess('RussianSurvey_SI', 0, NA, NA, 0) %>%
#   g3_init_guess('Juv_SI_3', 0, NA, NA, 0)
#   g3_init_guess('EcoS', 0, NA, NA, 0) %>%
#   g3_init_guess('EggaS', 0, NA, NA, 0) %>%
#   g3_init_guess('aldist', 0, NA, NA, 0) %>%
#   g3_init_guess('sexdist', 0, NA, NA, 0)

## Fit the initial parameters to the model, print the likelihood score and make plots which will be overwritten by optimized parameter plots later.

# result <- model(tmb_param$value)
# result[[1]]

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

## Optimize model parameters

if(nrow(tmb_param %>% filter(optimise, lower >= upper)) > 0) warning("Parameter lower bounds higher than upper bounds. Expect trouble in optimization.")

time_optim_start <- Sys.time()
message("Optimization started ", time_optim_start)
## g3_optim is a wrapper for stats::optim. It returns the parameter
## dataframe with the optimised parameters and includes an attribute with
## a summary of the optimisation.
## The control argument is identical to control for optim with the following defaults:
## maxit = 1000, trace = 2, reltol = .Machine$double.eps^2
optim_param <- g3_optim(model = tmb_model,
                        params = tmb_param,
                        use_parscale = TRUE,
                        method = 'BFGS',
                        control = list(maxit = 2000), #,reltol = 1e-5
                        print_status = TRUE
)
time_optim_end <- Sys.time()
time_optim <- round(as.numeric(time_optim_end - time_optim_start, units = "mins"), 1)
message("Optimization finished ", time_optim_end, " after ", time_optim, " min")

## Write the times to a file
info_file <- file(file.path(base_dir, "run_times.txt"))
close(info_file)
cat(
  c("Optimization:\n",
    "   started ", as.character(time_optim_start), "\n",
    "   finished ", as.character(time_optim_end), "\n",
    "   time ", time_optim, " min", "\n\n"),
  file = file.path(base_dir, "run_times.txt"), sep = "")

### Save the model parameters

write.csv(as.data.frame(optim_param), file = file.path(base_dir, "data/Optimized TMB parameters.csv"))
save(optim_param, file = file.path(base_dir, "data/Optimized TMB parameters.rda"), compress = "xz")

## Plots

optim_fit <- g3_fit(model, optim_param)
save(optim_fit, file = file.path(base_dir, "data/Optimized TMB model fit.rda"), compress = "xz")

# gadget_plots(optim_fit, file.path(base_dir, "figures"))

tmppath <- file.path(getwd(), base_dir, "figures")
gadget_plots(optim_fit, path = tmppath, file_type = "html")
rm(tmppath)

## Copy the R scripts used to compile the model
file.copy(dir(pattern = "\\.R$"), file.path(getwd(), base_dir, "scripts"))

## Save workspace
save.image(file = file.path(base_dir, "data/gadget_workspace.RData"), compress = "xz")






#####################################################################
## Iterative reweighting and optimization                        ####
## Running this part takes a long time (3-10 hours on a server)  ####

if(run_iterative) {

  if(set_weights) {
    tmb_param[grepl('weight$', tmb_param$switch) & tmb_param$value != 0, c("value", "lower", "upper", "optimise")] <-
      data.frame(value = 1, lower = NA, upper = NA, optimise = FALSE)
  }

  time_iter_start <- Sys.time()
  message("Iteration started ", time_iter_start)

  iter_param <- g3_iterative(
    gd = base_dir,
    wgts = "iterative_reweighting",
    model = tmb_model,
    params.in = tmb_param,
    grouping = list(si_eggan = c('log_EggaN_SI_female',
                                 'log_EggaN_SI_male'),
                    si_juv = c('log_Juv_SI_1',
                               'log_Juv_SI_2')),
    use_parscale = TRUE,
    control = list(maxit = 1000),
    cv_floor = 1,
    shortcut = FALSE
  )

  time_iter_end <- Sys.time()
  time_iter <- round(as.numeric(time_iter_end - time_iter_start, units = "mins"), 1)
  message("Iteration finished ", time_iter_end, " after ", time_iter, " min")

  cat(
    c("Iteration:\n",
      "   started ", as.character(time_iter_start), "\n",
      "   finished ", as.character(time_iter_end), "\n",
      "   time ", time_iter, " min", "\n\n"),
    file = file.path(base_dir, "run_times.txt"), sep = "", append = TRUE)

  ### Save the model parameters

  write.csv(as.data.frame(iter_param), file = file.path(base_dir, "data/Iterated TMB parameters.csv"))
  save(iter_param, file = file.path(base_dir, "data/Iterated TMB parameters.rda"), compress = "xz")

  ### Plots

  iter_fit <- g3_fit(model, iter_param)
  save(iter_fit, file = file.path(base_dir, "data/Iterated TMB model fit.rda"), compress = "xz")

  # gadget_plots(iter_fit, file.path(base_dir, "figures"))

  tmppath <- file.path(getwd(), base_dir, "figures")
  make_html(iter_fit, path = tmppath, file_name = "model_output_figures_iter.html")
  rm(tmppath)
}

## Save workspace

save.image(file = file.path(base_dir, "data/gadget_workspace.RData"), compress = "xz")
message("Script finished ", Sys.time())
