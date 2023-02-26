## ---------------------------
##
## Script name: Run the gadget3 model for NEA Greenland halibut
##
## Purpose of script: Master script to run the model. Source the entire script or run from top to a desired line (Ctrl+Alt+B on Rstudio)
##
## Authors:
## Mikko Vihtakari and Daniel Howell // Institute of Marine Research, Norway
## Will Butler and Bjarki Elvarsson // Marine and Freshwater Research Institute, Iceland
## Email: mikko.vihtakari@hi.no
##
## ---------------------------

##################################################
## Instructions to run the script in terminal ####
## You can only the model in one session / folder (computer) simultanously
## To run in terminal through screen on Eucleia, do:
## cd to the desired ghl-gadget3 folder and edit _RUN_THE_MODEL.R using Vim (vim _RUN_THE_MODEL.R)
# screen -LS gadgetrun
# /software/R-4.2.1/bin/R
# setwd("ghl-gadget3")
# source("_RUN_THE_MODEL.R", echo = TRUE)
## Ctrl-a-d # to detach (close and let the process run)
# screen -r gadgetrun # to reattach (look into the process)

## You can also automise the process above, but plotting html does not work for some reason. Switch plot_html to FALSE and produce the plots later.
# screen -LS gadgetrun bash -c '/software/R-4.2.1/bin/Rscript --verbose _RUN_THE_MODEL.R'

## Once done, you can kill inactive screen sessions (think before you do this):
# pkill screen
## Remember not to start another run in Rstudio server while the screen session is running
## Also running two screen sessions using the same ghl-gadget-data is not possible because duckdb does not support multiple connections. Kill the old screen sessions for the same folder before starting a new one.

## Alternatively you can run it using nohup (this option does not seem to work)
# cd to the desired ghl-gadget3 folder.
# nohup /software/R-4.2.1/bin/R -e 'source("_RUN_THE_MODEL.R", echo=TRUE, chdir = TRUE)' &

###################################
## Source the run first script ####

if(exists("mdb")) mfdb::mfdb_disconnect(mdb)

rm(list = ls())

source("0 run first.R")

## Model settings ####

### General options ####
reset_model <- TRUE # Change to TRUE to reset the model (delete all model files). ONLY do this if you really want to DELETE the existing model
reload_data <- FALSE # Set this to true to reload data from MFDB. If FALSE and the model folders (base_dir) exist, data are retrieved from the base_dir/data folder. Automatically set to TRUE if reset_model == TRUE or !dir.exists(base_dir)
base_dir <- "model_file20230226_explsuit_sexsldist_iter" # All files and output of the currently run model will be placed in a folder with this name
mfdb_path <- "../ghl-gadget-data/data/mfdb/ghl.duckdb" # Set MDFB path here. Clone ghl-gadget-data to your computer in the same base directory than ghl-gadget for the default path to work
plot_html <- TRUE # Whether html model summary should be plotted. In most cases you want this TRUE unless you work on a server that doesn't have pandoc installed.

### Parameter and likelihood component options ####
set_weights <- FALSE # Whether to set manual weights for likelihood components from previous iterative reweighting. The weights are defined in 6 initial parameters.R
force_bound_params <- TRUE # Whether parameters should be forced to their bounds.
use_cheat_fleet <- FALSE # Whether average EggaN maturity/stock data should be used for 1980:1990 to correct for stock proportion issues in initial population
previous_model_params_as_initial <- FALSE # Whether to use parameters optimised parameters as initial values for tmb_params. Speeds up the optimization, but also sets the model to a certain likelihood scape.

### Optimisation settings (adjust here and they'll change everywhere)
ncores <- 10 # Number of cores to use for parallel operations. The number of available computer cores is often not a limiting factor when running gadget, but memory is because the entire model is copied to RAM multiple times. Use max ncores = 10 for Eucleia, and ncores = 1 or a few for other servers.
maxit <- 3000 # Maximum number of iterations during an optimisation run.

### Run options ####
run_optim <- FALSE # Whether to run optimisation of parameters (the normal routine)
run_iterative <- TRUE # Whether to run iterative reweighting (takes 3-10 hours)
run_jitter <- FALSE # Run jittered optimisation. Repeats g3_optim multiple times with slightly different starting values.
run_retro <- FALSE # Run retrospective analysis?
run_bootstrap <- FALSE # Not implemented yet

## Optimisation mode (param_opt_mode), options:
# (1) parameters are bounded internally (ie using the bounded function) works with 'BFGS' optim method
# (2) parameters are bounded externally so the optim function must use a box-constrained optimisation method
# (3) global search, all parameters unbounded, unconstrained optimisation can be used (eg 'BFGS')

## How should the initial abundance be setup?
## Options:
## 0 - population is initialised at equilibrium
## 1 - parameter for each age group (across stocks)
## 2 - parameter for each age group of each stock
## 4 - two parameters: init_F and init.scalar. Population initiated 50/50 males/females, considers M and init_F for sexes.

setup_options <- list(param_opt_mode = 1,
                      initial_abund_mode = 4)

## Whether or not to bound parameters internally
setup_options$bound_params <- ifelse(setup_options$param_opt_mode == 1, TRUE, FALSE)

##########################
## Modify reload_data ####

if(reset_model | !dir.exists(base_dir)) {
  reload_data <- TRUE
  
  if(!dir.exists(base_dir)) {
    message(base_dir, "/data does not exist. Setting reload_data to TRUE. Data are reloaded from MFDB.")
  } else {
    message("You want to reset the model. Setting reload_data to TRUE. Data are reloaded from MFDB.")
  }
}

### Connect to the MFDB database ####

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

### Model and stock parameters, setting up areas, time steps, etc. ####

source("1 settings.R")

### Get data from MFDB ####

source("2-1 life history.R")
source("2-2 survey indices.R")
source("2-3 catch distribution.R")
source("2-4 catches.R")

### Setup the stocks ####

source("3 stocks.R")

### Add fleets and landings data ####

source("4 fleets.R")

### Setup likelihood components ####

source("5 likelihood.R")

### Formulate R based model and define initial parameters ####

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

# model_tmb <- g3_tmb_adfun(tmb_model, tmb_param)
#
# save(model_tmb, file = file.path(base_dir, "data/TMB model.rda"), compress = "xz")

## Copy the R scripts used to compile the model
file.copy(dir(pattern = "\\.R$"), file.path(getwd(), base_dir, "scripts"))


#####################################################################
## Iterative reweighting and optimization                        ####
## Running this part takes a long time (3-10 hours on a server)  ####
if(run_iterative) {
  
  ## Set component weight to 1 unless it was 0 (assuming intentional removal)
  if(set_weights) {
    tmb_param[grepl('weight$', tmb_param$switch) & tmb_param$value != 0,
              c("value", "lower", "upper", "optimise")] <-
      data.frame(value = 1, lower = NA, upper = NA, optimise = FALSE)
  }
  
  ## Component grouping
  component_grouping <- 
    list(SI_Adults = c('log_EggaN_SI_female', 'log_EggaN_SI_male', 'log_EcoS_SI', 'log_RussianS_SI'), #'log_EggaN_SI'
         SI_Juv = c('log_Juv_SI_1', 'log_Juv_SI_2'),
         TrawlNor = c('TrawlNor_ldist', 'TrawlNor_sexdist'),
         OtherNor = c('OtherNor_ldist', 'OtherNor_sexdist'), # , 'OtherNor_aldist'
         TrawlRus = c('TrawlRus_ldist', 'TrawlRus_sexdist'),
         # OtherRus = c('OtherRus_ldist', 'OtherRus_sexdist'),
         EcoS = c('EcoS_ldist', 'EcoS_sexdist'), # 'EcoS_aldist',
         EggaN = c('EggaN_aldist_female', 'EggaN_aldist_male', 'EggaN_ldist', 'EggaN_matp'),
         EggaS = c('EggaS_ldist', 'EggaS_matp') # 'EggaS_aldist', 
    )
  
  ## Remove components not listed in component grouping to avoid crashes later in g3_iterative
  tmp_test <- setNames(lapply(unname(unlist(component_grouping)), function(k) {
    list(component_found = grep(k, tmb_param[grepl('weight$', tmb_param$switch),]$switch, value = T),
         non_zero_value = grep(k, tmb_param[grepl('weight$', tmb_param$switch) & tmb_param$value != 0,]$switch, value = T)
    )
  }), unname(unlist(component_grouping)))
  
  ### Components not found (typos or removed from likelihood)
  if(any(sapply(tmp_test, function(k) length(k$component_found) == 0))) {
    missing_components <- sapply(tmp_test, function(k) length(k$component_found) == 0)
    missing_components <- missing_components[missing_components]
    warning(paste(names(missing_components), collapse = ", "), " not found from tmb_param$switch. Removed from component grouping. If a typo, correct and rerun.")
    
    component_grouping <- lapply(component_grouping, function(k) {
      k[!k %in% names(missing_components)]
    })
    
    tmp_test <- setNames(lapply(unname(unlist(component_grouping)), function(k) {
      list(component_found = grep(k, tmb_param[grepl('weight$', tmb_param$switch),]$switch, value = T),
           non_zero_value = grep(k, tmb_param[grepl('weight$', tmb_param$switch) & tmb_param$value != 0,]$switch, value = T)
      )
    }), unname(unlist(component_grouping)))
    
    ### Components with 0 weight  
  } else if (any(sapply(tmp_test, function(k) length(k$non_zero) == 0))) {
    missing_components <- sapply(tmp_test, function(k) length(k$non_zero) == 0)
    missing_components <- missing_components[missing_components]
    warning(paste(names(missing_components), collapse = ", "), " found from tmb_param$switch, but has 0 weight. Removed from component grouping. Make parameter weight != 0 if you want to keep it in g3_iterative.")
    
    component_grouping <- lapply(component_grouping, function(k) {
      k[!k %in% names(missing_components)]
    })
  }
  
  ## Run iterative
  
  time_iter_start <- Sys.time()
  message("Iteration started ", time_iter_start)
  
  iter_param <- g3_iterative(
    gd = base_dir,
    wgts = "iterative_reweighting",
    model = tmb_model,
    params.in = tmb_param,
    grouping = component_grouping,
    use_parscale = TRUE,
    control = list(maxit = maxit),
    cv_floor = 4e-4, # Gives maximum weight of 1/cv_floor for survey indices
    shortcut = FALSE,
    mc.cores = ncores
  )
  
  time_iter_end <- Sys.time()
  time_iter <- round(as.numeric(time_iter_end - time_iter_start, units = "mins"), 1)
  message("Iteration finished ", time_iter_end, " after ", time_iter, " min")
  
  if(!file.exists(file.path(base_dir, "session/run_times.txt"))) {
    info_file <- file(file.path(base_dir, "session/run_times.txt"))
    close(info_file)
  }
  
  cat(
    c("Iteration:\n",
      "   started ", as.character(time_iter_start), "\n",
      "   finished ", as.character(time_iter_end), "\n",
      "   time ", time_iter, " min", "\n\n"),
    file = file.path(base_dir, "session/run_times.txt"), sep = "", append = TRUE)
  
  ### Save the model parameters
  
  write.csv(as.data.frame(iter_param),
            file = file.path(base_dir, "iterative_reweighting/Iterated TMB parameters.csv"))
  save(iter_param,
       file = file.path(base_dir, "iterative_reweighting/Iterated TMB parameters.rda"), compress = "xz")
  
  ### Plots
  
  iter_fit <- g3_fit(model, iter_param)
  save(iter_fit,
       file = file.path(base_dir, "iterative_reweighting/Iterated TMB model fit.rda"), compress = "xz")
  
  # gadget_plots(iter_fit, file.path(base_dir, "figures"))
  
  if(plot_html) {
    tmppath <- file.path(getwd(), base_dir, "figures")
    make_html(iter_fit, path = tmppath, file_name = "model_output_figures_iter.html")
    rm(tmppath)
  }
}


#################################
## Optimize model parameters ####
if(run_optim) {
  
  dir.create(file.path(base_dir, "optim"))
  
  if(exists("iter_param")) {
    init_optim_param <- iter_param
  } else {
    init_optim_param <- tmb_param
  }
  
  if(nrow(init_optim_param %>% filter(optimise, lower >= upper)) > 0) warning("Parameter lower bounds higher than upper bounds. Expect trouble in optimization.")
  
  time_optim_start <- Sys.time()
  message("Optimization started ", time_optim_start)
  ## g3_optim is a wrapper for stats::optim. It returns the parameter
  ## dataframe with the optimised parameters and includes an attribute with
  ## a summary of the optimisation.
  ## The control argument is identical to control for optim
  optim_param <- g3_optim(model = tmb_model,
                          params = init_optim_param,
                          use_parscale = TRUE,
                          method = 'BFGS',
                          control = list(maxit = maxit #, reltol = 1e-9
                          ),
                          print_status = TRUE
  )
  time_optim_end <- Sys.time()
  time_optim <- round(as.numeric(time_optim_end - time_optim_start, units = "mins"), 1)
  message("Optimization finished ", time_optim_end, " after ", time_optim, " min")
  
  ## Write the times to a file
  if(!file.exists(file.path(base_dir, "session/run_times.txt"))) {
    info_file <- file(file.path(base_dir, "session/run_times.txt"))
    close(info_file)
  }
  
  cat(
    c("Optimization:\n",
      "   started ", as.character(time_optim_start), "\n",
      "   finished ", as.character(time_optim_end), "\n",
      "   time ", time_optim, " min", "\n",
      "   method: ", attributes(optim_param)$summary$method, "\n",
      "   convergence: ", attributes(optim_param)$summary$convergence, "\n",
      "   iterations: ", attributes(optim_param)$summary$gd_calls, "\n",
      "   score: ", round(attributes(optim_param)$summary$score, 1), "\n\n"),
    file = file.path(base_dir, "session/run_times.txt"), sep = "")
  
  ### Save the model parameters
  
  write.csv(as.data.frame(optim_param), file = file.path(base_dir, "optim/Optimized TMB parameters.csv"))
  save(optim_param, file = file.path(base_dir, "optim/Optimized TMB parameters.rda"), compress = "xz")
  
  ## Plots
  
  optim_fit <- g3_fit(model, optim_param)
  save(optim_fit, file = file.path(base_dir, "optim/Optimized TMB model fit.rda"), compress = "xz")
  
  if(plot_html) {
    tmppath <- file.path(getwd(), base_dir, "figures")
    gadget_plots(optim_fit, path = tmppath, file_type = "html")
    rm(tmppath)
  }
}

#########################################################################
## Jitter model parameters (takes 10/ncores times optimisation time) ####
if(run_jitter) {
  
  if(exists("iter_param")) {
    init_jitter_param <- iter_param
  } else if(exists("optim_param")) {
    init_jitter_param <- optim_param
  } else {
    init_jitter_param <- tmb_param
  }
  
  jitpar_out <- gadgetutils::g3_jitter(
    gd = base_dir,
    outdir = "jitter",
    model = tmb_model,
    params = init_jitter_param,
    njits = 10,
    control = list(maxit = maxit #, reltol = 1e-9
    ),
    ncores = ncores)
  
  jitpar_list <- lapply(seq_along(jitpar_out), function(i) {
    if(is.null(jitpar_out[[i]])) return(NULL)
    if(inherits(jitpar_out[[i]], "try-error")) return(NULL)
    out <- jitpar_out[[i]][jitpar_out[[i]]$optimise, c("switch", "value")]
    rownames(out) <- 1:nrow(out)
    names(out)[names(out) == "value"] <- paste0("value_run",i)
    out
  }) %>%
    purrr::discard(is.null) %>%
    purrr::reduce(dplyr::full_join, by = "switch") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      mean = mean(dplyr::c_across(dplyr::starts_with("value"))),
      sd = sd(dplyr::c_across(dplyr::starts_with("value"))),
      cv = abs(sd/mean))
  
  jitpar_list %>%
    write.g3.file(file.path(base_dir, "jitter"), 'jitter.param.comparison')
  
  p <- ggplot2::ggplot(
    data = jitpar_list,
    ggplot2::aes(.data$switch,.data$cv,label=.data$switch)) +
    ggplot2::geom_point() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "",
      y = paste0("Parameter CV over ",
                 length(grep("^value", names(jitpar_list))), " jitter runs")
    ) +
    ggplot2::theme_bw(base_size = 8)
  
  ggsave(file.path(base_dir, "figures/Jitter_parameter_CV.png"),
         plot = p, width = pagewidth, height = pagewidth*1.5, units = "mm")
  
  jitter_fit <- parallel::mclapply(seq_along(jitpar_out), function(i) {
    message(paste0(i, "/", length(jitpar_out)))
    if(is.null(jitpar_out[[i]])) return(NULL)
    if(inherits(jitpar_out[[i]], "try-error")) return(NULL)
    try(g3_fit(model = tmb_model, params = jitpar_out[[i]]))
  },
  mc.cores = ncores)
  
  jitter_fit <- Filter(
    Negate(is.null),
    lapply(jitter_fit, function(k) if(inherits(k, "try-error")) NULL else k)
  )
  
  save(jitter_fit, file = file.path(base_dir, "jitter", 'jitter_fit.rda'), compress = "xz")
  
  gadgetplots:::bind_fit_components(jitter_fit, 'score') %>%
    na.omit() %>%
    summarise(
      n_jitter = n(),
      mean = mean(nll),
      sd = sd(nll),
      cv = sd/mean) %>%
    write.g3.file(file.path(base_dir, "jitter"), 'jitter.nll.summary')
  
  ggsave(file.path(base_dir, "figures/Jitter_model_biomass.png"),
         plot = gadgetplots::plot_jitter(jitter_fit),
         width = pagewidth, height = pagewidth*0.7, units = "mm")
  
}

#########################
## Retroscpetive run ####

if(run_retro) {
  
  dir.create(file.path(base_dir, "retro"))
  
  if(exists("iter_param")) {
    init_retro_param <- iter_param
  } else if(exists("optim_param")) {
    init_retro_param <- optim_param
  } else {
    init_retro_param <- tmb_param
  }
  
  retro_model <- list()
  retro_params <- list()
  
  for(peel in 0:5){
    
    source("5 likelihood.R")
    
    if(force_bound_params) {
      retro_actions <- c(
        time_actions, stock_actions, fleet_actions, likelihood_actions,
        list(g3experiments::g3l_bounds_penalty(tmb_param))
      )
      
      retro_model[[peel]] <- g3_to_tmb(retro_actions)
    } else {
      retro_actions <- c(time_actions, stock_actions, fleet_actions, likelihood_actions)
      retro_model[[peel]] <- g3_to_tmb(retro_actions)
    }
    
    retro_params[[peel]] <- init_retro_param
    init_retro_param$value$retro_years <- peel
  }
  
  
  retro_list <- parallel::mclapply(0:5, function(peel) {
    
    # message("Running g3_optim for ", peel)
    param <- g3_optim(model = retro_model[[peel]],
                      params = retro_params[[peel]],
                      use_parscale = TRUE,
                      method = 'BFGS',
                      control = list(maxit = maxit),
                      print_status = TRUE
    )
    #message("g3_optim for ", peel, " finished. Running g3_fit")
    
    fit <- g3_fit(retro_model[[peel]], param)
    
    # message("Peel ", peel, " finished.")
    
    return(list(peel = peel, param = param, fit = fit))
  }, mc.cores = ifelse(ncores >= 5, 6, ncores)
  )
  
  ### Save the model parameters
  
  write.csv(lapply(retro_list, function(k) k$param) %>% bind_rows(), file = file.path(base_dir, "retro/Retro parameters.csv"))
  save(retro_list, file = file.path(base_dir, "retro/Retro parameters and fit.rda"), compress = "xz")
  
  # if(plot_html) {
  #   tmppath <- file.path(getwd(), base_dir, "figures")
  #   make_html(retro_fit, path = tmppath, file_name = "model_output_figures_retro.html")
  #   rm(tmppath)
  # }
  
  # Plot
  
  retro_fit <- lapply(retro_list, function(k) k$fit)
  
  p <- lapply(seq_along(retro_fit), function(i) {
    retro_fit[[i]]$res.by.year %>%
      dplyr::group_by(.data$year) %>%
      dplyr::summarise(value = sum(.data$total.biomass)/1e6) %>%
      dplyr::mutate(run = i)
  }) %>%
    dplyr::bind_rows() %>%
    ggplot2::ggplot(
      ggplot2::aes(.data$year,
                   .data$value,
                   col=as.factor(.data$run))) +
    ggplot2::geom_line() +
    ggplot2::labs(
      y = "Total model population biomass (kt)",
      x='Year',col='Years\nremoved') +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::theme_classic(base_size = 8)
  
  ggsave(file.path(base_dir, "figures/Retro_model_biomass.png"),
         plot = p, width = pagewidth, height = pagewidth*0.7, units = "mm")
  
}

## Save workspace

savehistory(file = file.path(base_dir, "session/.Rhistory"))
save.image(file = file.path(base_dir, "session/gadget_workspace.RData"), compress = "xz")
message("Script finished ", Sys.time(), ". Saved to ", base_dir)

if(exists("optim_fit") & exists("time_optim")) {
  message("Optimisation", ifelse(attributes(optim_fit)$summary$convergence, " converged.", " did not converge."), 
          " It took ", attributes(optim_fit)$summary$gd_calls, 
          " iterations and ", round(time_optim/60, 1), " hours to run.")
}

## When running Ctrl + Alt + B, run until here. The options should take care what gets evaluated.
