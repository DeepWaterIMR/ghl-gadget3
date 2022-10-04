## ---------------------------
##
## Script name: Gadget settings
##
## Purpose of script: Set up file structure and define parameters
##
## ---------------------------

## Set up the model folders

if(reset_model & dir.exists(base_dir)) {
  if(previous_model_params_as_initial &
     file.exists("model_files/data/Optimized TMB parameters.rda")) {
    load("model_files/data/Optimized TMB parameters.rda")
    prev_param <- fit.opt$par
    names(prev_param) <- gsub("__", ".", names(prev_param))
    rm(fit.opt)
  }

  unlink(base_dir, recursive = TRUE)
  message("Resetting the model. The ", base_dir, " folder deleted and recreated.")
}

if(!dir.exists(base_dir)) {
  dir.create(file.path(base_dir, "data"), recursive = TRUE)
  dir.create(file.path(base_dir, "figures"))
  # dir.create(file.path(base_dir, "iterative_reweighting"))
}

## Model settings

model_params <- list()

model_params$year_range <- 1992:2021
model_params$timestep_fun <- mfdb::mfdb_timestep_yearly
model_params$female_stock <- c("female_imm", "female_mat")
model_params$male_stock <- c("male_imm", "male_mat")
model_params$stock_names <- c(model_params$female_stock, model_params$male_stock)
model_params$trawl_fleets <- c("TrawlNor", "TrawlRus")
model_params$other_fleets <- c("OtherNor", "OtherRus")
model_params$hist_fleets <- c("HistNor", "HistRus", "Internat")
model_params$survey_fleets <- c("EggaN")
model_params$fleet_names <- c(model_params$trawl_fleets, model_params$other_fleets, model_params$hist_fleets, model_params$survey_fleets)
model_params$species_name <- "Greenland halibut"
model_params$species_code <- "GHL"

## Model term glossary
# - m = male
# - f = female
# - M = natural mortality
# - F = fishing mortality
# - imm = immature
# - mat = mature

## Stock parameters

stock_params <- list()

stock_params$dl <- 1 # delta length i.e. length group binning

# Maximum number of length groups a stock can group within a time step (maxlengthgroupgrowth).  Default within gadget is 15:
stock_params$maxlengthgroupgrowth <-
  if(identical(model_params$timestep_fun, mfdb::mfdb_timestep_quarterly)) {
  10} else if(identical(model_params$timestep_fun,
                        mfdb::mfdb_timestep_yearly)) {20} else {15}

stock_params$male_imm$minage <- 1
stock_params$male_imm$maxage <- 10
stock_params$male_imm$minlength <- 1
stock_params$male_imm$maxlength <- 60
stock_params$male_imm$max_possible_data_length <- 60 # This parameter is used to filter maturity data to smooth maturity proportions going into likelihood.

stock_params$female_imm$minage <- 1
stock_params$female_imm$maxage <- 10
stock_params$female_imm$minlength <- 1
stock_params$female_imm$maxlength <- 120
stock_params$female_imm$max_possible_data_length <- 75 # This parameter is used to filter maturity data to smooth maturity proportions going into likelihood.

stock_params$male_mat$minage <- 1
stock_params$male_mat$maxage <- 20
stock_params$male_mat$minlength <- 1
stock_params$male_mat$maxlength <- 80
stock_params$male_mat$min_possible_data_length <- 30 # This parameter is used to filter maturity data to smooth maturity proportions going into likelihood.

stock_params$female_mat$minage <- 1
stock_params$female_mat$maxage <- 30
stock_params$female_mat$minlength <- 1
stock_params$female_mat$maxlength <- 120
stock_params$female_mat$min_possible_data_length <- 40 # This parameter is used to filter maturity data to smooth maturity proportions going into likelihood.

stock_params$minage <- min(sapply(model_params$stock_names, function(k) stock_params[[k]]$minage))
stock_params$maxage <- max(sapply(model_params$stock_names, function(k) stock_params[[k]]$maxage))
stock_params$minlength <- min(sapply(model_params$stock_names, function(k) stock_params[[k]]$minlength))
stock_params$maxlength <- max(sapply(model_params$stock_names, function(k) stock_params[[k]]$maxlength))

## Definitions

MainAreaFilter <- c(0:7, 10:18, 20:27, 30, 34:39, 50) # ICES areas 1 and 2

## Define some defaults to use with our queries below

defaults <- list(
  area = mfdb::mfdb_group("1" = MainAreaFilter),
  timestep = model_params$timestep_fun,
  year = model_params$year_range,
  species = model_params$species_code)

# Map area names to integer area numbers (in this case only "1" ==> 1, but could be anything)
areas <- structure(
  seq_along(defaults$area),
  names = names(defaults$area)
)

# Timekeeping for the model, i.e. how long we run for
time_actions <- list(
  g3a_time(start_year = min(defaults$year),
           end_year = max(defaults$year),
           defaults$timestep),
  list())

## Save the parameters

save(model_params, stock_params, MainAreaFilter, defaults, areas, time_actions, file = file.path(base_dir, "data/Model and stock parameters.rda"), compress = "xz")
