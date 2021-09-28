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

# Turn actions into an R function
model <- g3_to_r(c(
  stock_actions,
  fleet_actions,
  likelihood_actions,
  # report_actions,
  time_actions),
  #  trace = TRUE,
  strict = TRUE)

param <- attr(model, 'parameter_template')

param[grepl('\\.walpha$', names(param))] <- sapply(lw_constants, function(k) k[[1]])
param[grepl('\\.wbeta$', names(param))] <- sapply(lw_constants, function(k) k[[2]])
param[grepl('\\.bbin$', names(param))] <- 6
param[grepl('male\\.M\\.', names(param))] <- 0.1
param[grepl('female\\.M\\.', names(param))] <- 0.1
param[grepl('\\.rec\\.sd$', names(param))] <- 1

param[grepl('\\.init\\.sd', names(param))] <- init_sigma %>%
  slice(names(param[grepl('\\.init\\.sd', names(param))]) %>%
          gsub('.+\\.([0-9]+)','\\1',.) %>% as.numeric()) %>%
  pull(ms)

## SI's
# param[grepl('si_beta1', names(param))] <- 1
# param[grepl('si_beta2', names(param))] <- 1
# param[grepl('si_igfs_si.+weight$',names(param))] <- 1



## Old stuff

# suppressWarnings(gadget_evaluate(gd, params.out = "params.tmp"))
#
# ## update the input parameters with those from assessment Gadget runs (BESTINPUT)
# tmp <- read.gadget.parameters(sprintf('%s/params.tmp', gd)) %>%
#   init_guess('rec.[0-9]|init.[0-9]', 1e3,1,1e4,1) %>%
#   init_guess('M$', 0.1, 0.001, 1, 0) %>%
#   init_guess('init.F', 0.4, 0.01, 1, 1) %>%
#   init_guess('m.Linf', 56, 50, 200, 1) %>%
#   init_guess('f.Linf', 72, 50, 200, 1) %>%
#   init_guess('m.k', 135, 10, 1000, 1) %>% # the k's are divided by 1000 to help the H&J optimizer (sensitive to digits)
#   init_guess('f.k', 102, 10, 1000, 1) %>% # the k's are divided by 1000 to help the H&J optimizer (sensitive to digits)
#   init_guess('walpha', lw.constants$estimate[1], 1e-10, 1,0) %>%
#   init_guess('wbeta', lw.constants$estimate[2], 2, 4,0) %>%
#   init_guess('bbin',2, 0.1, 100, 1) %>%
#   init_guess('rec.scalar',15,1,50,1) %>%
#   init_guess('init.scalar',15,1,30,1) %>%
#   init_guess('recl',10,7,15,1) %>%
#   init_guess('rec.sd', 2, 0.001, 4, 1) %>%
#   init_guess('m.l50', 47, 20, 70, 1) %>%
#   init_guess('f.l50', 69, 40, 100, 1) %>%
#   init_guess('mat.alpha', 1, -9999, 9999, 1) %>%
#   init_guess('other.l50', 50, 1, 100, 1) %>%
#   init_guess('other.alpha', 0.5, 0.01, 3, 1) %>%
#   init_guess('p\\d$', 1, 0, 9999, 1) %>%
#   init_guess('L$', 1, -9999, 9999, 1)
#
# tmp %>% write.gadget.parameters(., file = sprintf('%s/params.tmp', gd))
#
# rm(tmp)
#
# ## Update the model using initial parameters
#
# gadget_evaluate(gd, params.in = "params.tmp", params.out = "params.in")
#
# unlink(file.path(gd, "params.tmp"))
#
