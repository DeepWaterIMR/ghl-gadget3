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
    trace = TRUE,
  strict = TRUE)

## Define the initial R parameters

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

# param[grepl('\\.init.\\d', names(param))] <- 1
# param[grepl('scalar', names(param))] <- 1

param[grepl('init.F', names(param))] <- 0.4

## SI's
param[grepl('si_beta', names(param))] <- 1
param[grepl('weight$',names(param))] <- 1

## Write the parameters to a csv file

write.csv(t(as.data.frame(param)), file = file.path(base_dir, "data/Initial R parameters.csv"))

# TMB model

tmb_model <- g3_to_tmb(c(
  stock_actions,
  fleet_actions,
  likelihood_actions,
  #report_actions,
  time_actions),
  #  trace = TRUE,
  strict = TRUE)

## Initial TMB model parameters

tmb_param <- attr(tmb_model, 'parameter_template')

# Copy initial guesses from R model
tmb_param$value <- I(param[rownames(tmb_param)])

# Configure lower and upper bounds
tmb_param$lower <- vapply(tmb_param$value, function (x) 0.5 * x[[1]], numeric(1))
tmb_param$upper <- vapply(tmb_param$value, function (x) 2 * x[[1]], numeric(1))
#tmb_param[grepl('^ling\\.rec\\.', rownames(tmb_param)),]$lower <- 0.0001
#tmb_param[grepl('^ling\\.rec\\.', rownames(tmb_param)),]$upper <- 1e3
# tmb_param[grepl('^ling\\.init\\.', rownames(tmb_param)),]$lower <- 0.0001
# tmb_param[grepl('^ling\\.init\\.', rownames(tmb_param)),]$upper <- 1e3
# tmb_param[grepl('^ling\\.init\\.', rownames(tmb_param)),]$lower <- 0.0001
# tmb_param[grepl('^ling\\.init\\.', rownames(tmb_param)),]$upper <- 1e3

# Disable optimisation for some parameters, to make life easier
tmb_param <-
  tmb_param %>%
  mutate(optimise = case_when(grepl('walpha',switch)~FALSE,
                              grepl('wbeta',switch)~FALSE,
                              grepl('male\\.M',switch)~FALSE,
                              grepl('init\\.sd',switch)~FALSE,
                              grepl('_weight',switch)~FALSE,
                              TRUE~TRUE))

## Write the parameters to a csv file

write.csv(tmb_param, file = file.path(base_dir, "data/Initial TMB parameters.csv"), row.names = FALSE)

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
