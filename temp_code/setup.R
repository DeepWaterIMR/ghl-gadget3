library(mfdb)
library(tidyverse)
#library(Rgadget)
library(gadget3)
source('~/Documents/Gadget/gadget3/demo-ling/stock_param_functions.r')


bootstrap <- FALSE

tyr <- lubridate::year(Sys.Date())
  
year_range <- 1975:tyr

mdb<-mfdb('Iceland',db_params=list(host='mfdb.hafro.is'))

reitmapping <- 
  read.table(
        system.file("demo-data", "reitmapping.tsv", package="mfdb"),
        header=TRUE,
        as.is=TRUE)

defaults <- list(
    area = mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
    timestep = mfdb_timestep_quarterly,
    year = year_range,
    species = 'GHL')


# Map area names to integer area numbers (in this case only "1" ==> 1, but could be anything)
areas <- structure(
  seq_along(defaults$area),
  names = names(defaults$area))


# Timekeeping for the model, i.e. how long we run for
time_actions <- list(
  g3a_time(start_year = min(defaults$year),
           end_year = max(defaults$year),
           defaults$timestep),
  list())





##### Configure stocks #########################################################

source('exploratory_models/gadget/00-setup/setup-model.R')  # Generates mat_actions / imm_actions
source('exploratory_models/gadget/00-setup/setup-fleets.R')  # Generates fleet_actions

##### Configure model actions #################################################
source('exploratory_models/gadget/00-setup/setup-catchdistribution.R')
source('exploratory_models/gadget/00-setup/setup-indices.R')  # Generates fleet_actions
source('exploratory_models/gadget/00-setup/setup-likelihood.R')  # Generates likelihood_actions
#source('exploratory_models/gadget/00-setup/setup-report_actions.R')  # Generates report actions

##### Run r-based model #######################################################

# Turn actions into an R function
model <- g3_to_r(c(
  stock_actions,
  fleet_actions,
  likelihood_actions,
  #report_actions,
  time_actions),
  #  trace = TRUE, 
  strict = TRUE)

param <- attr(model, 'parameter_template')

## weight length relationship
lw.constants <- 
  mfdb_dplyr_sample(mdb) %>% 
  filter(species == local(defaults$species),
         sampling_type == 'AUT',
         !is.na(weight)) %>% 
  select(length,weight) %>% 
  collect(n=Inf) %>% 
  lm(log(weight/1e3)~log(length),.) %>% 
  broom::tidy() %>% 
  select(estimate)
## transport back to right dimension
lw.constants$estimate[1] <- exp(lw.constants$estimate[1])

## initial conditions sigma
init.sigma <- 
  mfdb_dplyr_sample(mdb) %>% 
  dplyr::filter(species == local(defaults$species),age >0,!is.na(length))  %>% 
  dplyr::select(age,length) %>% 
  dplyr::group_by(age) %>% 
  dplyr::summarise(ml=mean(length,na.rm=TRUE),ms=sd(length,na.rm=TRUE)) %>% 
  dplyr::mutate(ms = ifelse(ms == 0,1,ms)) %>%   
  dplyr::collect(n=Inf) %>% 
  dplyr::bind_rows(tibble(age = 25,ml = 100, ms = 1))


## initial guess for the maturity ogive:
mat.l50 <- 
  mfdb_dplyr_sample(mdb) %>% 
  filter(species == local(defaults$species),
         sampling_type == 'AUT',
         !is.na(maturity_stage)) %>% 
  select(length,maturity_stage) %>% 
  group_by(length,maturity_stage) %>% 
  dplyr::summarise(n=n()) %>% 
  group_by(length) %>% 
  dplyr::mutate(p=n/sum(n)) %>% 
  ungroup() %>% 
  filter(maturity_stage=='2',p>0.50,length>25) %>% 
  dplyr::summarise(l50=min(length)) %>% 
  collect(n=Inf)

# Get parameter template attached to function, fill it in
param <- attr(model, 'parameter_template')
param[grepl('\\.walpha$', names(param))] <- lw.constants$estimate[1]
param[grepl('\\.wbeta$', names(param))] <- lw.constants$estimate[2]
param[grepl('\\.bbin$', names(param))] <- 6
param[grepl('male\\.M\\.', names(param))] <- 0.15
param[grepl('\\.rec\\.sd$', names(param))] <- 1

param[grepl('\\.init\\.sd', names(param))] <- 
  init.sigma %>% 
  slice(names(param[grepl('\\.init\\.sd', names(param))]) %>% 
          gsub('.+\\.([0-9]+)','\\1',.) %>% as.numeric()) %>% 
  .$ms
## SI's
param[grepl('si_beta1', names(param))] <- 1
param[grepl('si_beta2', names(param))] <- 1
param[grepl('si_igfs_si.+weight$',names(param))] <- 1

## Old weights from gadget2
# param['cdist_sumofsquares_ldist_lln_weight'] <- 3331
# param['cdist_sumofsquares_aldist_lln_weight'] <- 2512
# param['cdist_sumofsquares_ldist_bmt_weight'] <- 1247
# param['cdist_sumofsquares_aldist_bmt_weight'] <- 1515
# param['cdist_sumofsquares_ldist_gil_weight'] <- 781
# param['cdist_sumofsquares_aldist_gil_weight'] <- 719
# param['cdist_sumofsquares_ldist_igfs_weight'] <- 6869
# param['cdist_sumofsquares_aldist_igfs_weight'] <- 11087
# param['cdist_sumofsquares_matp_igfs_weight'] <- 9
# param['adist_surveyindices_log_si_igfs_si1_weight'] <- 40
# param['adist_surveyindices_log_si_igfs_si2a_weight'] <- 8
# param['adist_surveyindices_log_si_igfs_si2b_weight'] <- 36
# param['adist_surveyindices_log_si_igfs_si3a_weight'] <- 19
# param['adist_surveyindices_log_si_igfs_si3b_weight'] <- 16
# param['adist_surveyindices_log_si_igfs_si3c_weight'] <- 13
# param['adist_surveyindices_log_si_igfs_si3d_weight'] <- 14.5



res <- model(param)

### TMB model

tmb_model <- g3_to_tmb(c(
  stock_actions,
  fleet_actions,
  likelihood_actions,
  #report_actions,
  time_actions),
  #  trace = TRUE, 
  strict = TRUE)

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
                              TRUE~TRUE))

# Compile and generate TMB ADFun (see ?TMB::MakeADFun)
ling_model_tmb <- g3_tmb_adfun(tmb_model,tmb_param)


