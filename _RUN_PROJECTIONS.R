## -----------------------------------------------------------------------------
##
## Runner to setup projections
##
## -----------------------------------------------------------------------------

source("0 run first.R")

#base_dir <- 'exploratory_models/gadget3'
#vers <- 'models/301-BNCHMK_base_simpleinit'

#outpath <- file.path(base_dir, vers, 'PROJ2')
#if (!dir.exists(outpath)) dir.create(outpath)

load("data/gadget_workspace.RData")
source("R/projection_functions.R")


base_dir <- "model_files"

outpath <- file.path(base_dir, "projections")
if(!dir.exists(outpath)) dir.create(outpath)


## Load the desired model, params and fit
# load(file = file.path(base_dir, vers, 'fit.Rdata'))
# load(file = file.path(base_dir, vers, 'tmb_model.Rdata'))
# load(file = file.path(base_dir, vers, 'WGTS/params_final.Rdata'))
# load(file = file.path(base_dir, vers, 'BOOTSTRAP/boot_fit.Rdata'))

fit <- optim_fit

## -----------------------------------------------------------------------------
## Stock and fleets
## -----------------------------------------------------------------------------

year_range <- range(fit$res.by.year$year)
end_year <- max(year_range)
start_year <- end_year + 1
num_steps <- length(unique(fit$stock.full$step))

## Recruitment will be re-sampled from this year onward
rec_start_year <- 1990 ## min(fit$res.by.year$year)

## How many years to project forward
num_project_years <- 100

## A sequence of harvest rates to test, typically try #seq(0.00, 1, by = 0.01)
harvest_rates <- seq(0.00, 1, by = 0.01)

## How many trials per harvest rate (each trial will have a unique recruitment series
## and a unique annual harvest rate sequence (if assessment error is present))
hr_trials <- 100 
recstep <- 1

## Age range for calculating fbar
age_range <- c(7,25)

#blim <- #138650858          # 125629491

## Blim in tonnes
blim <- fit$res.by.year %>% 
  filter(year == 1992, stock == "ghl_female_mat") %>% 
  pull(total.biomass) %>% 
  {round(./1e3, 2)} # round(min(fit$res.by.year$total.biomass)/1e3)

bpa <- blim * 1.4
btrigger <- bpa

## -----------------------------------------------------------------------------

# reitmapping <-
#   read.table(
#     system.file("demo-data", "reitmapping.tsv", package="mfdb"),
#     header=TRUE,
#     as.is=TRUE)
#
# defaults <- list(
#   area = mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
#   timestep = mfdb::mfdb_timestep_biannually,
#   year = year_range,
#   species = species_code)
#
# # Map area names to integer area numbers (in this case only "1" ==> 1, but could be anything)
# areas <- structure(
#   seq_along(defaults$area),
#   names = names(defaults$area))


## Setup the stocks
# female_imm <- g3_stock(
#   c(species = tolower(defaults$species), sex = 'female', 'imm'),
#   lengthgroups = seq(stock_params$female_imm$minlength,
#                      stock_params$female_imm$maxlength,
#                      stock_params$dl)) %>%
#   g3s_livesonareas(areas[c('1')]) %>%
#   g3s_age(minage = stock_params$female_imm$minage,
#           maxage = stock_params$female_imm$maxage)
#
# male_imm <- g3_stock(
#   c(species = tolower(defaults$species), sex = 'male', 'imm'),
#   lengthgroups = seq(stock_params$male_imm$minlength,
#                      stock_params$male_imm$maxlength,
#                      stock_params$dl)) %>%
#   g3s_livesonareas(areas[c('1')]) %>%
#   g3s_age(minage = stock_params$male_imm$minage,
#           maxage = stock_params$male_imm$maxage)
#
# female_mat <- g3_stock(
#   c(species = tolower(defaults$species), sex = 'female', 'mat'),
#   lengthgroups = seq(stock_params$female_mat$minlength,
#                      stock_params$female_mat$maxlength,
#                      stock_params$dl)) %>%
#   g3s_livesonareas(areas[c('1')]) %>%
#   g3s_age(minage = stock_params$female_mat$minage,
#           maxage = stock_params$female_mat$maxage)
#
# male_mat <- g3_stock(
#   c(species = tolower(defaults$species), sex = 'male', 'mat'),
#   lengthgroups = seq(stock_params$male_mat$minlength,
#                      stock_params$male_mat$maxlength,
#                      stock_params$dl)) %>%
#   g3s_livesonareas(areas[c('1')]) %>%
#   g3s_age(minage = stock_params$male_mat$minage,
#           maxage = stock_params$male_mat$maxage)
#
# ## List of stocks
# stocks <- list(female_imm,female_mat,male_imm,male_mat)

## SSB rec relationship
recage <- gadget3::g3_stock_def(male_imm, 'minage')

schedule <-
  expand.grid(year = (end_year - recage):(end_year + num_project_years),
              step = recstep)

## -----------------------------------------------------------------------------
## Use bootstrap parameters and recruitment TS'

# param_list <- c(list(base = fit$params))
rec_list <- c(list(
  base = 
    fit$stock.recruitment %>% 
    filter(recruitment > 0, year >= rec_start_year, 
           year <= max(model_params$year) - 4, step == recstep) %>% 
    group_by(year) %>% 
    summarise(recruitment = sum(recruitment))
  )) #map(filter, recruitment > 0, year >= rec_start_year, step == recstep) %>% 
  
## ------------------------------------------------------------------------------

## STOCK ACTIONS
## Warning is produced here, should check with Jamie
proj_actions0 <-
  c(attributes(tmb_model)$actions,
    proj_stock_actions2(num_project_years,
                        mat = female_mat,
                        imm_F = female_imm,
                        imm_M = male_imm))

## -----------------------------------------------------------------------------
## Setup projection fleets
## -----------------------------------------------------------------------------

proj_TrawlNor <- g3_fleet(c("TrawlNor", "proj")) %>%
  g3s_livesonareas(areas[c('1')])
proj_OtherNor <- g3_fleet(c("OtherNor", "proj")) %>%
  g3s_livesonareas(areas[c('1')])
proj_TrawlRus <- g3_fleet(c("TrawlRus", "proj")) %>%
  g3s_livesonareas(areas[c('1')])
proj_OtherRus <- g3_fleet(c("OtherRus", "proj")) %>%
  g3s_livesonareas(areas[c('1')])
proj_Internat <- g3_fleet(c("Internat", "proj")) %>%
  g3s_livesonareas(areas[c('1')])

proj_effort_scalar <-
  structure(expand.grid(
    year = start_year:(start_year + num_project_years),
    step = 1,
    area = 1,
    scalar = 1),
    area_group = list(`1` = 1))

## Setup a time-varying parameter table for harvest rates
hr_vec <-
  gadget3:::f_substitute(
    ~g3_param_table('project_hr',
                    expand.grid(cur_year = seq(end_year, end_year + py)),
                    ifmissing = 0),
    list(py = num_project_years))


exponentiate_fleets <- FALSE

## -----------------------------------------------------------------------------
## Fleet actions
## -----------------------------------------------------------------------------

if (TRUE){
  
  proj_fleet_actions <-
    
    list(
      
      proj_TrawlNor %>%
        g3a_predate_fleet(
          stocks,
          suitabilities =
            stocks %>%
            set_names(.,map(.,'name')) %>%
            map(function(x) g3_suitability_exponentiall50(
              g3_parameterized('trawlnor.alpha',
                               by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets),
              g3_parameterized('trawlnor.l50',
                               by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets))),
          catchability_f =
            g3experiments::g3a_predate_catchability_hockeyfleet(
              btrigger = g3_parameterized('btrigger'),
              hr = gadget3:::f_substitute(
                ~a*b, list(a = hr_vec, b = g3_parameterized('trawlnor_prop'))
              ),
              proportion_f = ~1,
              E = g3_timeareadata('TrawlNor_proj',
                                  proj_effort_scalar,
                                  value_field = 'scalar'),
              sum_stocks = list(female_mat)),
          run_f = ~cur_year_projection),
      
      proj_OtherNor %>%
        g3a_predate_fleet(
          stocks,
          suitabilities =
            stocks %>%
            set_names(.,map(.,'name')) %>%
            map(function(x) g3_suitability_exponentiall50(
              g3_parameterized('othernor.alpha',
                               by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets),
              g3_parameterized('othernor.l50',
                               by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets))),
          catchability_f =
            g3experiments::g3a_predate_catchability_hockeyfleet(
              btrigger = g3_parameterized('btrigger'),
              hr = gadget3:::f_substitute(
                ~a*b, list(a = hr_vec, b = g3_parameterized('othernor_prop'))),
              proportion_f = ~1,
              E = g3_timeareadata('OtherNor_proj',
                                  proj_effort_scalar,
                                  value_field = 'scalar'),
              sum_stocks = list(female_mat)),
          run_f = ~cur_year_projection),
      
      proj_TrawlRus %>%
        g3a_predate_fleet(
          stocks,
          suitabilities =
            stocks %>%
            set_names(.,map(.,'name')) %>%
            map(function(x) g3_suitability_exponentiall50(
              g3_parameterized('trawlrus.alpha',
                               by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets),
              g3_parameterized('trawlrus.l50',
                               by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets))),
          catchability_f =
            g3experiments::g3a_predate_catchability_hockeyfleet(
              btrigger = g3_parameterized('btrigger'),
              hr = gadget3:::f_substitute(
                ~a*b, list(a = hr_vec, b = g3_parameterized('trawlrus_prop'))),
              proportion_f = ~1,
              E = g3_timeareadata('TrawlRus_proj',
                                  proj_effort_scalar,
                                  value_field = 'scalar'),
              sum_stocks = list(female_mat)),
          run_f = ~cur_year_projection),
      
      proj_OtherRus %>%
        g3a_predate_fleet(
          stocks,
          suitabilities =
            stocks %>%
            set_names(.,map(.,'name')) %>%
            map(function(x) g3_suitability_exponentiall50(
              g3_parameterized('othernor.alpha',
                               by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets),
              g3_parameterized('othernor.l50',
                               by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets))),
          catchability_f =
            g3experiments::g3a_predate_catchability_hockeyfleet(
              btrigger = g3_parameterized('btrigger'),
              hr = gadget3:::f_substitute(
                ~a*b, list(a = hr_vec, b = g3_parameterized('otherrus_prop'))),
              proportion_f = ~1,
              E = g3_timeareadata('OtherRus_proj',
                                  proj_effort_scalar,
                                  value_field = 'scalar'),
              sum_stocks = list(female_mat)),
          run_f = ~cur_year_projection),
      
      proj_Internat %>%
        g3a_predate_fleet(
          stocks,
          suitabilities =
            stocks %>%
            set_names(.,map(.,'name')) %>%
            map(function(x) g3_suitability_exponentiall50(
              g3_parameterized('trawlnor.alpha',
                               by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets),
              g3_parameterized('trawlnor.l50',
                               by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets))),
          catchability_f =
            g3experiments::g3a_predate_catchability_hockeyfleet(
              btrigger = g3_parameterized('btrigger'),
              hr = gadget3:::f_substitute(
                ~a*b, list(a = hr_vec, b = g3_parameterized('internat_prop'))),
              proportion_f = ~1,
              E = g3_timeareadata('Internat_proj',
                                  proj_effort_scalar,
                                  value_field = 'scalar'),
              sum_stocks = list(female_mat)),
          run_f = ~cur_year_projection)
    )
  
}

## -------------------------------------------------------------------------------

## UPDATE ACTIONS
proj_actions <- c(proj_actions0, proj_fleet_actions)
proj_actions <- c(proj_actions, list(g3experiments::g3p_project_report(proj_actions)))

tmb_proj <- g3_to_tmb(proj_actions)
base.par.proj <- attributes(tmb_proj)$parameter_template

## -----------------------------------------------------------------------------

## Initial parameters, fill in everything except for btrigger, HRs and recruitment
## Update values
base.par.proj$value[optim_fit$params$switch] <- optim_fit$params$value

## Other columns
base.par.proj <- base.par.proj %>%
  # mutate(optimise = FALSE) %>%
  mutate(across(any_of(names(optim_fit$params)[-1:-4]),
                ~ coalesce(optim_fit$params[[cur_column()]][match(switch, optim_fit$params$switch)], .x)))

# New columns
base.par.proj$value$project_years <- num_project_years
base.par.proj$value$blim <- blim*1e3

## THE PROPORTION PER FLEET

n_catch_years <- 4

catch_props <- fit$fleet.info %>%
  dplyr::filter(amount > 1,
                year > max(model_params$year_range) - n_catch_years) %>%
  dplyr::mutate(fleet = gsub("_fishery", "", fleet)) %>%
  dplyr::group_by(fleet) %>%
  dplyr::summarise(amount = sum(amount)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(prop = amount/sum(amount))

base.par.proj$value$trawlnor_prop <- catch_props %>%
  filter(fleet == "TrawlNor") %>% pull(prop)
base.par.proj$value$othernor_prop <- catch_props %>%
  filter(fleet == "OtherNor") %>% pull(prop)
base.par.proj$value$trawlrus_prop <- catch_props %>%
  filter(fleet == "TrawlRus") %>% pull(prop)
base.par.proj$value$otherrus_prop <- catch_props %>%
  filter(fleet == "OtherRus") %>% pull(prop)
base.par.proj$value$internat_prop <- catch_props %>%
  filter(fleet == "Internat") %>% pull(prop)

## This is how Bjarki did it:
# fleet_props <-
#   fit$fleet.info %>%
#   group_by(year,fleet) %>%
#   summarise(harv.rate = mean(harv.rate,na.rm=TRUE)) %>%
#   filter(!is.infinite(harv.rate)) %>%
#   group_by(year) %>%
#   mutate(m = harv.rate/sum(harv.rate)) %>%
#   filter(year > 2016) %>%
#   group_by(fleet) %>%
#   summarise(m=mean(m)) %>%
#   {set_names(as.list(.$m),.$fleet)}

## Fill in recruitment and harvest rates to build the ad function

################################################################################
##
## NOTE, FEMALE_MAT SPAWNS INTO A DUMMY STOCK WHICH SUBSEQUENTLY AGES INTO
## FEMALE_IMM AND MALE_IMM AT A 50:50 RATIO. THE AMOUNT SPAWNED IS TAKEN 
## FROM THE BASE FIT, THEREFORE THE VALUE SHOULD BE THE TOTAL RECRUITMENT
## FOR EACH YEAR I.E. MALE.REC + FEMALE.REC BECAUSE IT WILL SUBEQUENTLY 
## BE SPLIT BETWEEN THE TWO IMMATURE STOCKS
##
################################################################################


## 2023-04-21: Things to fix: par.proj project_rec should start from 2021? (2022 for real but we set 2021 rec to 0 to avoid spikes). Project HR should start from 2022.

par.proj <- base.par.proj
par.proj <-
  par.proj %>%
  g3p_project_rec(
    recruitment = fit$stock.recruitment %>%
      filter(year >= rec_start_year,
             year <= max(model_params$year) - 4) %>% 
      group_by(year) %>% 
      summarise(recruitment = sum(recruitment)),
    method = 'bootstrap') %>%
  g3p_project_advice_error(hr_target = min(harvest_rates), advice_cv = 0) %>%
  g3_init_guess('btrigger', 1)

# r_proj <- g3_to_r(proj_actions)

################################################################################
## Checks
################################################################################

## Test to check spawning is working
# test_par <- par.proj
# test_par <- 
#   test_par %>% 
#   ## CONSTANT REC
#   # g3p_project_rec(
#   #   recruitment = fit$stock.recruitment %>%
#   #     filter(year == 2020) %>%
#   #     summarise(recruitment = sum(recruitment)), method = 'constant') %>%
#   ## VARIABLE REC
#   g3p_project_rec(
#     recruitment = fit$stock.recruitment %>%
#       filter(year >= rec_start_year,
#              year <= max(model_params$year) - 4) %>% 
#       group_by(year) %>% 
#       summarise(recruitment = sum(recruitment)), 
#     method = 'bootstrap') %>%
#   g3p_project_advice_error(hr_target = 0.1, advice_cv = 0) 
# 
# ## TEST RUN
# tmp <- attributes(r_proj(test_par$value))
# 
# ## Plot recruitment
# bind_rows(
#   fit$stock.recruitment %>% 
#     group_by(year) %>% 
#     summarise(rec = sum(recruitment), .groups = 'drop'),
#   tmp$proj_ghl_dummy__spawnednum %>% 
#     as.data.frame.table() %>% 
#     group_by(time) %>% 
#     summarise(rec = sum(Freq)) %>%
#     gadgetutils:::extract_year_step() %>% 
#     filter(year > max(fit$stock.recruitment$year %>% max()))
# ) %>% ggplot(aes(year, rec/1e6)) + geom_bar(stat = 'identity')
# 
# ## Check female and male immature
# tmp$proj_ghl_female_imm__num %>% 
#   as.data.frame.table() %>% 
#   mutate(age = gsub('age', '', age) %>% as.numeric()) %>% 
#   gadgetutils:::extract_year_step() %>% filter(year > 2020, step == 1) %>% 
#   group_by(year, age) %>% summarise(female = sum(Freq)) %>% 
#   left_join(
#     tmp$proj_ghl_male_imm__num %>% 
#       as.data.frame.table() %>% 
#       mutate(age = gsub('age', '', age) %>% as.numeric()) %>% 
#       gadgetutils:::extract_year_step() %>% filter(year > 2020, step == 1) %>% 
#       group_by(year, age) %>% summarise(male = sum(Freq)) 
#   ) %>% view


################################################################################

## Plot the model
# result <- model(par.proj$value)
# result[[1]]
# r_proj <- g3_to_r(proj_actions)
# test_fit <- gadgetutils::g3_fit(r_proj,projpar_pre[[45]])
# tmppath <- file.path(getwd(), base_dir, "figures")
# make_html(test_fit, path = tmppath, file_name = "model_output_figures_proj.html")

## So much fun...
fun_fun <- g3_tmb_adfun(tmb_proj, par.proj, type = 'Fun')
#fun_fun <- g3_tmb_adfun(tmb_proj, par.proj)

## Set up the harvest rates and trials-per-harvest-rate
hr_list <-
  expand.grid(hr = harvest_rates, trial = 1:hr_trials) %>%
  #expand.grid(hr = 0.1, trial = 1) %>%
  unite(id, remove = FALSE) %>%
  mutate(id = paste0('h', id)) %>%
  split(f = as.factor(.$id))

# ## -----------------------------------------------------------------------------
# ## Precautionary reference points (flim): no assessment error and no Btrigger
# ## -----------------------------------------------------------------------------

## Create a list of input parameters with modified annual recruitment parameters (future ones)
## annual harvest rates, and btrigger
projpar_pre <- lapply(setNames(names(hr_list), names(hr_list)), function(x){
  
  par.proj <- base.par.proj
  #par.proj$value[param_list[[hr_list[[x]]$boot]]$switch] <- param_list[[hr_list[[x]]$boot]]$value
  #par.proj$value$project_years <- num_project_years
  #par.proj$value$blim <- blim*1e3
  
  out <-
    par.proj %>%
    g3p_project_rec(recruitment = rec_list$base, method = 'bootstrap') %>%
    #g3p_project_rec(recruitment = fit$stock.recruitment %>% filter(year >= rec_start_year) %>% summarise(recruitment = mean(recruitment)), method = 'constant') %>%
    g3p_project_advice_error(hr_target = hr_list[[x]]$hr, advice_rho = 0.423, advice_cv = 0) %>%
    g3_init_guess('btrigger', 1)
  
  return(out)
  
})

## This command loops over each parameter list, runs the model and collates the output
results_pre <-
  do.call('rbind',
          parallel::mclapply(setNames(names(projpar_pre), names(projpar_pre)), function(x){
            # print(x)
            out <- runfun(fun_fun, projpar_pre[[x]]) ## NEED TO FIX STOCKS
            out$hr_target <- as.numeric(gsub('h', '', gsub('(.+)_(.+)', '\\1', x)))
            #out$boot <- as.numeric(gsub('(.+)_(.+)_(.+)', '\\2', x))
            out$trial <- as.numeric(gsub('(.+)_(.+)', '\\2', x))
            return(out)
          }, mc.cores = 30)#parallel::detectCores(logical = TRUE))
  )

save(results_pre, file = file.path(outpath, 'results_pre.Rdata'), compress = "xz")
save(projpar_pre, file = file.path(outpath, 'projpar_pre.Rdata'), compress = "xz")

# Notes
## - Calculate the risk of going below Blim
## - Interannual variation in catch


## -----------------------------------------------------------------------------
## MSY reference points (fmsy, fp0.5): assessment error and no btrigger
## -----------------------------------------------------------------------------

projpar_msy_nobtrigger <- lapply(setNames(names(hr_list), names(hr_list)), function(x){
  
  par.proj <- base.par.proj
  #par.proj$value[param_list[[hr_list[[x]]$boot]]$switch] <- param_list[[hr_list[[x]]$boot]]$value
  #par.proj$value$project_years <- num_project_years
  #par.proj$value$blim <- blim*1e3
  
  out <-
    par.proj %>%
    g3p_project_rec(recruitment = rec_list$base, method = 'bootstrap') %>%
    #g3p_project_rec(recruitment = fit$stock.recruitment %>% filter(year >= rec_start_year) %>% summarise(recruitment = mean(recruitment)), method = 'constant') %>%
    g3p_project_advice_error(hr_target = hr_list[[x]]$hr, advice_rho = 0.423, 
                             advice_cv = 0.212) %>%
    g3_init_guess('btrigger', 1)
  
  return(out)
  
})


results_msy_nobtrigger <-
  do.call('rbind',
          parallel::mclapply(setNames(names(projpar_msy_nobtrigger), names(projpar_msy_nobtrigger)), function(x){
            print(x)
            out <- runfun(fun_fun, projpar_msy_nobtrigger[[x]])
            out$hr_target <- as.numeric(gsub('h', '', gsub('(.+)_(.+)', '\\1', x)))
            #out$boot <- as.numeric(gsub('(.+)_(.+)_(.+)', '\\2', x))
            out$trial <- as.numeric(gsub('(.+)_(.+)', '\\2', x))
            return(out)
          }, mc.cores = 30)#parallel::detectCores(logical = TRUE))
  )

save(results_msy_nobtrigger, 
     file = file.path(outpath, 'results_msy_nobtrigger.Rdata'), compress = "xz")
save(projpar_msy_nobtrigger, 
     file = file.path(outpath, 'projpar_msy_nobtrigger.Rdata'), compress = "xz")

## -----------------------------------------------------------------------------
## MSY reference points (fmsy, fp0.5): assessment error and btrigger
## -----------------------------------------------------------------------------

projpar_msy <- lapply(setNames(names(hr_list), names(hr_list)), function(x){
  
  par.proj <- base.par.proj
  #par.proj$value[param_list[[hr_list[[x]]$boot]]$switch] <- param_list[[hr_list[[x]]$boot]]$value
  #par.proj$value$project_years <- num_project_years
  #par.proj$value$blim <- blim*1e3
  
  out <-
    par.proj %>%
    g3p_project_rec(recruitment = rec_list$base, method = 'bootstrap') %>%
    g3p_project_advice_error(hr_target = hr_list[[x]]$hr, advice_rho = 0.423, 
                             advice_cv = 0.212)  %>%
    g3_init_guess('btrigger', btrigger*1e3)
  
  return(out)
  
})


results_msy <-
  do.call('rbind',
          parallel::mclapply(setNames(names(projpar_msy), names(projpar_msy)), function(x){
            print(x)
            out <- runfun(fun_fun, projpar_msy[[x]])
            out$hr_target <- as.numeric(gsub('h', '', gsub('(.+)_(.+)', '\\1', x)))
            #out$boot <- as.numeric(gsub('(.+)_(.+)_(.+)', '\\2', x))
            out$trial <- as.numeric(gsub('(.+)_(.+)', '\\2', x))
            return(out)
          }, mc.cores = 30)#parallel::detectCores(logical = TRUE))
  )

save(results_msy, file = file.path(outpath, 'results_msy.Rdata'), compress = "xz")
save(projpar_msy, file = file.path(outpath, 'projpar_msy.Rdata'), compress = "xz")

## -----------------------------------------------------------------------------
