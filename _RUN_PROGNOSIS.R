## -----------------------------------------------------------------------------
##
## Runner to setup projections
##
## -----------------------------------------------------------------------------

source("0 run first.R")

load("data/gadget_workspace.RData")
source("R/projection_functions.R")

base_dir <- "short_term_projections"

outpath <- file.path(base_dir, "short_term_projections")
if(!dir.exists(outpath)) dir.create(outpath, recursive = TRUE)

## Load the desired model, params and fit
fit <- optim_fit

## -----------------------------------------------------------------------------
## Stock and fleets
## -----------------------------------------------------------------------------

year_range <- range(fit$res.by.year$year)
end_year <- max(year_range)
start_year <- end_year + 1 # This is also assessment year, assessyear parameter
num_steps <- length(unique(fit$stock.full$step))



## Recruitment will be re-sampled from this year onward
rec_start_year <- 1990 ## min(fit$res.by.year$year)

## How many years to project forward
num_project_years <- 5

## A sequence of harvest rates to test, typically try #seq(0.00, 1, by = 0.01)
harvest_rates <- seq(0.00, 1, by = 0.01)

recstep <- 1

## Blim in tonnes
blim <- optim_fit$res.by.year %>%
  filter(year == 1992, stock == "ghl_female_mat") %>%
  pull(total.biomass)

bpa <- blim * 1.4
btrigger <- bpa
hr_target <- 0.2 # HR(target)msy from the RP table

## SSB rec relationship
recage <- gadget3::g3_stock_def(male_imm, 'minage')

schedule <-
  expand.grid(year = (end_year - recage):(end_year + num_project_years),
              step = recstep)

## -----------------------------------------------------------------------------
## Use bootstrap parameters and recruitment TS'

# param_list <- c(list(base = fit$params))
recruitment <- fit$stock.recruitment %>%
    filter(recruitment > 0, year >= rec_start_year,
           year <= max(model_params$year) - 4, step == recstep) %>%
    group_by(year) %>%
    summarise(recruitment = sum(recruitment))

## ------------------------------------------------------------------------------

## STOCK ACTIONS
proj_actions0 <-
  c(attributes(tmb_model)$actions,
    proj_stock_actions2(num_project_years,
                        mat = female_mat,
                        imm_F = female_imm,
                        imm_M = male_imm))

## -----------------------------------------------------------------------------
## Setup intermediate fleets and landings
## -----------------------------------------------------------------------------

int_TrawlNor <- g3_fleet(c("TrawlNor", "int")) %>%
  g3s_livesonareas(areas[c('1')])
int_OtherNor <- g3_fleet(c("OtherNor", "int")) %>%
  g3s_livesonareas(areas[c('1')])
int_TrawlRus <- g3_fleet(c("TrawlRus", "int")) %>%
  g3s_livesonareas(areas[c('1')])
int_OtherRus <- g3_fleet(c("OtherRus", "int")) %>%
  g3s_livesonareas(areas[c('1')])
int_Internat <- g3_fleet(c("Internat", "int")) %>%
  g3s_livesonareas(areas[c('1')])

## Intermediate catches will be average of last three years
meancatch <-
  fit$fleet.info %>%
  filter(year %in% (end_year - 2):end_year, !grepl('survey$', fleet)) %>%
  group_by(fleet) %>%
  summarise(meancatch = mean(amount), .groups = 'drop') %>%
  mutate(area = 1, year = start_year, step = 1)

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

## WB comment - Effort scalar turned to zero for the intermediate year.
## This ensures the quota fleets don't catch anything in this year
proj_effort_scalar <-
  proj_effort_scalar %>%
  mutate(scalar = case_when(year == start_year ~ 0, TRUE ~ 1))

## Setup a time-varying parameter table for harvest rates
hr_vec <-
  gadget3:::f_substitute(
    ~g3_param_table('project_hr',
                    expand.grid(cur_year = seq(end_year + 1, end_year + py)),
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
              btrigger = g3_parameterized('btrigger', optimise = FALSE),
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
              btrigger = g3_parameterized('btrigger', optimise = FALSE),
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
              btrigger = g3_parameterized('btrigger', optimise = FALSE),
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
              btrigger = g3_parameterized('btrigger', optimise = FALSE),
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
              btrigger = g3_parameterized('btrigger', optimise = FALSE),
              hr = gadget3:::f_substitute(
                ~a*b, list(a = hr_vec, b = g3_parameterized('internat_prop'))),
              proportion_f = ~1,
              E = g3_timeareadata('Internat_proj',
                                  proj_effort_scalar,
                                  value_field = 'scalar'),
              sum_stocks = list(female_mat)),
          run_f = ~cur_year_projection)
    )

  int_fleet_actions <-

    list(

      int_TrawlNor %>%
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
            gadget3::g3a_predate_catchability_totalfleet(
              g3_timeareadata('TrawlNor_int',
                              meancatch %>%
                                filter(fleet == 'TrawlNor_fishery') %>%
                                select(-fleet),
                              value_field = 'meancatch')),
          run_f = gadget3:::f_substitute(~cur_year == assessyear,
                                         list(assessyear = start_year))
        ),

      int_OtherNor %>%
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
            gadget3::g3a_predate_catchability_totalfleet(
              g3_timeareadata('OtherNor_int',
                              meancatch %>%
                                filter(fleet == 'OtherNor_fishery') %>%
                                select(-fleet),
                              value_field = 'meancatch')),
          run_f = gadget3:::f_substitute(~cur_year == assessyear,
                                         list(assessyear = start_year))),

      int_TrawlRus %>%
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
            gadget3::g3a_predate_catchability_totalfleet(
              g3_timeareadata('TrawlRus_int',
                              meancatch %>%
                                filter(fleet == 'TrawlRus_fishery') %>%
                                select(-fleet),
                              value_field = 'meancatch')),
          run_f = gadget3:::f_substitute(~cur_year == assessyear,
                                         list(assessyear = start_year))),

      int_OtherRus %>%
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
            gadget3::g3a_predate_catchability_totalfleet(
              g3_timeareadata('OtherRus_int',
                              meancatch %>%
                                filter(fleet == 'OtherRus_fishery') %>%
                                select(-fleet),
                              value_field = 'meancatch')),
          run_f = gadget3:::f_substitute(~cur_year == assessyear,
                                         list(assessyear = start_year))),

      int_Internat %>%
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
            gadget3::g3a_predate_catchability_totalfleet(
              g3_timeareadata('Internat_int',
                              meancatch %>%
                                filter(fleet == 'Internat_fishery') %>%
                                select(-fleet),
                              value_field = 'meancatch')),
          run_f = gadget3:::f_substitute(~cur_year == assessyear,
                                         list(assessyear = start_year)))
    )

}

## -------------------------------------------------------------------------------

## UPDATE ACTIONS
proj_actions <- c(proj_actions0, proj_fleet_actions, int_fleet_actions)
proj_actions <- c(proj_actions,
                  list(gadget3::g3a_report_detail(proj_actions, run_f = ~TRUE)))

tmb_proj <- g3_to_tmb(proj_actions)
base.par.proj <- attributes(tmb_proj)$parameter_template

## -----------------------------------------------------------------------------

## Initial parameters
## Update values
base.par.proj$value[optim_fit$params$switch] <- optim_fit$params$value

## Other columns
base.par.proj <- base.par.proj %>%
  mutate(across(any_of(names(optim_fit$params)[-1:-4]),
                ~ coalesce(optim_fit$params[[cur_column()]][match(switch, optim_fit$params$switch)], .x)))

# New columns
base.par.proj$value$project_years <- num_project_years
base.par.proj$value$blim <- blim
base.par.proj[base.par.proj$switch=="btrigger","optimise"] <- TRUE

base.par.proj <-
  base.par.proj %>%
  g3_init_guess('project_years', value = num_project_years) %>%
  g3_init_guess('blim', value = blim) %>%
  g3_init_guess('btrigger', value = btrigger) %>%
  g3_init_guess('project_hr', value = hr_target) %>%
  g3_init_guess('project_rec', value = mean(recruitment$recruitment))

## THE PROPORTION PER FLEET

n_catch_years <- 3

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

# par.proj <- base.par.proj
# par.proj <-
#   par.proj %>%
#   g3p_project_rec(
#     recruitment = fit$stock.recruitment %>%
#       filter(year >= rec_start_year,
#              year <= max(model_params$year) - 4) %>%
#       group_by(year) %>%
#       summarise(recruitment = sum(recruitment)),
#     method = 'bootstrap') %>%
#   g3p_project_advice_error(hr_target = min(harvest_rates), advice_cv = 0) %>%
#   g3_init_guess('btrigger', 1)

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


## Quicker to use the R model for prognosis as you are only doing a few runs
r_proj <- g3_to_r(proj_actions)

test <- g3_fit(r_proj, base.par.proj)
tmppath <- file.path(getwd(), base_dir, "figures")
make_html(test, path = tmppath, file_name = "test.html", template = "nea_ghl")

## Collect the reports
res <- attributes(r_proj(base.par.proj$value))

## Check catch reports to check they are doing what we expect
catch_reports <-
  res[c(names(res)[grepl('^detail_(.+)__predby_TrawlNor$', names(res))],
        names(res)[grepl('^detail_(.+)__predby_TrawlNor_int$', names(res))],
        names(res)[grepl('^detail_(.+)__predby_TrawlNor_proj$', names(res))],
        names(res)[grepl('^detail_(.+)__predby_OtherNor$', names(res))],
        names(res)[grepl('^detail_(.+)__predby_OtherNor_int$', names(res))],
        names(res)[grepl('^detail_(.+)__predby_OtherNor_proj$', names(res))],
        names(res)[grepl('^detail_(.+)__predby_TrawlRus$', names(res))],
        names(res)[grepl('^detail_(.+)__predby_TrawlRus_int$', names(res))],
        names(res)[grepl('^detail_(.+)__predby_TrawlRus_proj$', names(res))],
        names(res)[grepl('^detail_(.+)__predby_OtherRus$', names(res))],
        names(res)[grepl('^detail_(.+)__predby_OtherRus_int$', names(res))],
        names(res)[grepl('^detail_(.+)__predby_OtherRus_proj$', names(res))],
        names(res)[grepl('^detail_(.+)__predby_Internat$', names(res))],
        names(res)[grepl('^detail_(.+)__predby_Internat_int$', names(res))],
        names(res)[grepl('^detail_(.+)__predby_Internat_proj$', names(res))])] %>%
  map(.f = function(x) as.data.frame.table(x, stringsAsFactors = FALSE)) %>%
  bind_rows(.id = 'comp') %>%
  mutate(fleet = gsub('^detail_(.+)__predby_(.+)', '\\2', comp),
         stock = gsub('^detail_(.+)__predby_(.+)', '\\1', comp),
         harvest_rate = local(hr_target),
         age = gsub('age', '', age) %>% as.numeric()) %>%
  select(time, fleet, harvest_rate, stock, length, age, catch = Freq)

## Catch by year by fleet
# progn_catch_by_fleet <-
  catch_reports %>%
  group_by(time, fleet, harvest_rate) %>%
  summarise(catch = sum(catch, na.rm = TRUE), .groups = 'drop') %>%
  gadgetutils::extract_year_step() %>%
  filter(year >= local(start_year)) %>%
  pivot_wider(names_from = fleet, values_from = catch) #%>%
#mutate(catch = comm + comm_int + comm_proj)



# ################################################################################
# ##
# ## LOW RECRUITMENT
# ##
# ## 1st Simulation - F equals 0
# ##
# ################################################################################
#
pars <-
  base.par.proj %>%
  # g3p_project_rec(recruitment = data.frame(recruitment = rec_list[[1]]),
  #                 method = 'bootstrap') %>%
  g3p_project_advice_error(hr_target = 0, advice_cv = 0)
#
out_F0 <- runfun(fun_fun, pars, cdredmat, cdredimmat)
out_F0$blim <- refPoints$blim*1e3
out_F0$baseline_rec <- rec_list$rec1
print(out_F0)
#
# ################################################################################
# ##
# ## LOW RECRUITMENT
# ##
# ## 2nd Simulation - F equals F_target
# ##
# ################################################################################
#
# pars <-
#   par.proj %>%
#   g3p_project_rec(recruitment = data.frame(recruitment = rec_list[[1]]), method = 'constant') %>%
#   g3p_project_advice_error(hr_target = hr_target, advice_cv = 0)
#
# pars$value$btrigger <- btrigger*1e3
#
# out_mgt <- runfun(fun_fun, pars, cdredmat, cdredimmat)
# out_mgt$blim <- refPoints$blim*1e3
# out_mgt$baseline_rec <- rec_list$rec1
# print(out_mgt)
#
# ################################################################################
# ##
# ## HIGH RECRUITMENT
# ##
# ## 1st Simulation - F equals 0
# ##
# ################################################################################
#
# pars <-
#   par.proj %>%
#   g3p_project_rec(recruitment = data.frame(recruitment = rec_list$rec2), method = 'constant') %>%
#   g3p_project_advice_error(hr_target = 0, advice_cv = 0)
#
# out_F0 <- runfun(fun_fun, pars, cdredmat, cdredimmat)
# out_F0$blim <- refPoints$blim*1e3
# out_F0$baseline_rec <- rec_list$rec2
# print(out_F0)
#
# ################################################################################
# ##
# ## HIGH RECRUITMENT
# ##
# ## 2nd Simulation - F equals F_target
# ##
# ################################################################################
#
# pars <-
#   par.proj %>%
#   g3p_project_rec(recruitment = data.frame(recruitment = rec_list[[2]]), method = 'constant') %>%
#   g3p_project_advice_error(hr_target = hr_target, advice_cv = 0)
#
# pars$value$btrigger <- btrigger*1e3
#
# out_mgt <- runfun(fun_fun, pars, cdredmat, cdredimmat)
# out_mgt$blim <- refPoints$blim*1e3
# out_mgt$baseline_rec <- rec_list$rec2
# print(out_mgt)
#
#
#








## Plot the model
# result <- model(par.proj$value)
# result[[1]]
# r_proj <- g3_to_r(proj_actions)
# test_fit <- gadgetutils::g3_fit(r_proj,projpar_pre[[45]])
# tmppath <- file.path(getwd(), base_dir, "figures")
# make_html(test_fit, path = tmppath, file_name = "model_output_figures_proj.html")

## So much fun...
fun_fun <- g3_tmb_adfun(tmb_proj, base.par.proj, type = 'Fun')
#fun_fun <- g3_tmb_adfun(tmb_proj, par.proj)

## Set up the harvest rates and trials-per-harvest-rate
hr_list <-
  expand.grid(hr = harvest_rates, trial = 1:hr_trials) %>%
  #expand.grid(hr = 0.1, trial = 1) %>%
  unite(id, remove = FALSE) %>%
  mutate(id = paste0('h', id)) %>%
  split(f = as.factor(.$id))


save(year_range, end_year, start_year, num_steps, rec_start_year, num_project_years,
     harvest_rates, hr_trials, recstep, blim, bpa, btrigger, hr_list, rec_list, recage,
     base.par.proj,
     file = file.path(outpath, 'projection_defintions.Rdata'), compress = "xz"
)


# ## -----------------------------------------------------------------------------
# ## Limit reference points (flim): no assessment error and no Btrigger
# ## -----------------------------------------------------------------------------

## Create a list of input parameters with modified annual recruitment parameters (future ones)
## annual harvest rates, and btrigger
projpar_pre <- lapply(setNames(names(hr_list), names(hr_list)), function(x){

  par.proj <- base.par.proj
  #par.proj$value[param_list[[hr_list[[x]]$boot]]$switch] <- param_list[[hr_list[[x]]$boot]]$value
  #par.proj$value$project_years <- num_project_years
  #par.proj$value$blim <- blim

  out <-
    par.proj %>%
    g3p_project_rec(recruitment = rec_list$base, method = 'bootstrap') %>%
    #g3p_project_rec(recruitment = fit$stock.recruitment %>% filter(year >= rec_start_year) %>% summarise(recruitment = mean(recruitment)), method = 'constant') %>%
    g3p_project_advice_error(hr_target = hr_list[[x]]$hr, advice_rho = 0, advice_cv = 0) %>%
    g3_init_guess('btrigger', value = 1, optimise = TRUE)

  return(out)

})

## This command loops over each parameter list, runs the model and collates the output
results_pre <-
  do.call('rbind',
          parallel::mclapply(setNames(names(projpar_pre), names(projpar_pre)), function(x){
            # print(x)
            out <- runfun(fun_fun, projpar_pre[[x]])
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
    g3_init_guess('btrigger', value = 1, optimise = TRUE)

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
## Precautionary reference points (fmsy, fp0.5): assessment error and btrigger
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
    g3_init_guess('btrigger', value = btrigger, optimise = TRUE)

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

r_proj <- g3_to_r(proj_actions)
msy_fit <- gadgetutils::g3_fit(r_proj,projpar_msy[[2101]])
tmppath <- file.path(getwd(), base_dir, "figures")
make_html(msy_fit, path = tmppath, file_name = "model_output_figures_proj_msy.html", template = "nea_ghl")

