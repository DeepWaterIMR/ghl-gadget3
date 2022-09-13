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

## Collate actions
actions <- c(
  time_actions,
  stock_actions,
  fleet_actions,
  likelihood_actions)

# Turn actions into an R function

model <- g3_to_r(actions, trace = TRUE)

# You can edit the model code with:
#model <- edit(model)

## Define the initial R parameters

param <- attr(model, 'parameter_template')

param[grepl('_female.walpha$', names(param))] <- lw_constants %>% filter(sex == "F", term == "a") %>% pull(estimate)
param[grepl('_male.walpha$', names(param))] <- lw_constants %>% filter(sex == "M", term == "a") %>% pull(estimate)
param[grepl('_female.wbeta$', names(param))] <- lw_constants %>% filter(sex == "F", term == "b") %>% pull(estimate)
param[grepl('_male.wbeta$', names(param))] <- lw_constants %>% filter(sex == "M", term == "b") %>% pull(estimate)
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

tmb_model <- g3_to_tmb(actions)

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
  g3_init_guess('\\.rec', 1, 0.001, 1000, 1) %>%
  g3_init_guess('\\.init', 1, 0.001, 1000, 1) %>%
  g3_init_guess('recl', 12, 10, 30, 1) %>%
  g3_init_guess('rec.sd', 2, 1, 5, 1) %>%
  g3_init_guess('rec.scalar', 400, 1, 500, 1) %>%
  g3_init_guess('init.scalar', 200, 1, 300, 1) %>%
  g3_init_guess('Linf', 90, 80, 120, 1) %>%
  g3_init_guess('\\.K', 50, 40, 120, 1) %>%
  g3_init_guess('bbin', 6, 1e-08, 100, 1) %>%
  g3_init_guess('\\.alpha', 0.5, 0.01, 1, 1) %>%
  g3_init_guess('l50', 50, 40, 100, 1) %>%
  g3_init_guess('init.F', 0.4, 0.1, 0.8, 1) %>%
  g3_init_guess('\\.M', 0.15, 0.001, 1, 0) %>%
  g3_init_guess('mat_initial_alpha', 1, 0.001, 3, 1) %>%
  g3_init_guess('_female.mat_initial_a50', mat_l50 %>% filter(sex == "F") %>% pull(slope), 3, 25, 0) %>%
  g3_init_guess('_male.mat_initial_a50', mat_l50 %>% filter(sex == "M") %>% pull(slope), 3, 25, 0) %>%
  #  g3_init_guess('prop_mat0', 0.5, 0.1, 0.9, 0) %>%
  #  g3_init_guess('B0', 100, 1, 5000, 1) %>%
  g3_init_guess('mat1', log(70), log(10), log(200), 1) %>%
  # g3_init_guess('mat1', 0, 10, 200, 1) %>%
  g3_init_guess('_female.mat2', mat_l50$mean[1], 0.75*mat_l50$mean[1], 1.25*mat_l50$mean[1], 1) %>%
  g3_init_guess('_male.mat2', mat_l50$mean[2], 0.75*mat_l50$mean[2], 1.25*mat_l50$mean[2], 1) %>%
  g3_init_guess('sigma_alpha', init_sigma_coef[['alpha']], -1, 1, 0) %>%
  g3_init_guess('sigma_beta', init_sigma_coef[['beta']], 0, 2, 0) %>%
  g3_init_guess('sigma_gamma', init_sigma_coef[['gamma']], 0, 1, 0) %>%
  g3_init_guess('female.walpha', lw_constants$`F`[[1]], 1e-10, 1, 0) %>%
  g3_init_guess('female.wbeta', lw_constants$`F`[[2]], 2, 4, 0) %>%
  g3_init_guess('male.walpha', lw_constants$M[[1]], 1e-10, 1, 0) %>%
  g3_init_guess('male.wbeta', lw_constants$M[[1]], 2, 4, 0)
  # mutate(optimise = case_when(grepl('walpha',switch)~FALSE,
  #                             grepl('wbeta',switch)~FALSE,
  #                             grepl('male\\.M',switch)~FALSE,
  #                             grepl('init\\.sd',switch)~FALSE,
  #                             grepl('_weight',switch)~FALSE,
  #                             TRUE~TRUE))

## Initial sd's
if (any(grepl('\\.init\\.sd', tmb_param$switch))){

  tmb_param[grepl('female_mat\\.init\\.sd', tmb_param$switch), 'value'] <-
    init_sigma %>% filter(age %in%
                            gadget3:::stock_definition(female_mat, 'minage'):
                            gadget3:::stock_definition(female_mat, 'maxage')) %>% .$ms

  tmb_param[grepl('male_mat\\.init\\.sd', tmb_param$switch), 'value'] <-
    init.sigma %>% filter(age %in%
                            gadget3:::stock_definition(male_mat, 'minage'):
                            gadget3:::stock_definition(male_mat, 'maxage')) %>% .$ms

  tmb_param[grepl('female_imm\\.init\\.sd', tmb_param$switch), 'value'] <-
    init.sigma %>% filter(age %in%
                            gadget3:::stock_definition(female_imm, 'minage'):
                            gadget3:::stock_definition(female_imm, 'maxage')) %>% .$ms

  tmb_param[grepl('male_imm\\.init\\.sd', tmb_param$switch), 'value'] <-
    init.sigma %>% filter(age %in%
                            gadget3:::stock_definition(male_imm, 'minage'):
                            gadget3:::stock_definition(male_imm, 'maxage')) %>% .$ms

  ## Turn off optimisation
  tmb_param <-
    tmb_param %>%
    mutate(optimise = case_when(grepl('init.sd', switch) ~ FALSE,
                                grepl('.M.[\\.[0-9]', switch) ~ FALSE,
                                TRUE~optimise))
}



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
