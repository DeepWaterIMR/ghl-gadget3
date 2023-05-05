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

## Intermediate catches assume the catch from the previous year (untick the other option to average last three years).

expected_catch <-
  fit$fleet.info %>%
  filter(year %in% end_year, !grepl('survey$', fleet)) %>%
  group_by(fleet) %>%
  summarise(catch = mean(amount), .groups = 'drop') %>%
  mutate(area = 1, year = start_year, step = 1)
# fit$fleet.info %>%
# filter(year %in% (end_year - 2):end_year, !grepl('survey$', fleet)) %>%
# group_by(fleet) %>%
# summarise(meancatch = mean(amount), .groups = 'drop') %>%
# mutate(area = 1, year = start_year, step = 1)

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
                              expected_catch %>%
                                filter(fleet == 'TrawlNor_fishery') %>%
                                select(-fleet),
                              value_field = 'catch')),
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
                              expected_catch %>%
                                filter(fleet == 'OtherNor_fishery') %>%
                                select(-fleet),
                              value_field = 'catch')),
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
                              expected_catch %>%
                                filter(fleet == 'TrawlRus_fishery') %>%
                                select(-fleet),
                              value_field = 'catch')),
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
                              expected_catch %>%
                                filter(fleet == 'OtherRus_fishery') %>%
                                select(-fleet),
                              value_field = 'catch')),
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
                              expected_catch %>%
                                filter(fleet == 'Internat_fishery') %>%
                                select(-fleet),
                              value_field = 'catch')),
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
# base.par.proj$value$project_years <- num_project_years
# base.par.proj$value$blim <- blim
# base.par.proj[base.par.proj$switch=="btrigger","optimise"] <- TRUE

base.par.proj <-
  base.par.proj %>%
  g3_init_guess('project_years', value = num_project_years) %>%
  g3_init_guess('blim', value = blim) %>%
  g3_init_guess('btrigger', value = btrigger) %>%
  g3_init_guess('project_hr', value = hr_target) %>%
  g3experiments::g3p_project_rec(
    recruitment = data.frame(recruitment = mean(recruitment$recruitment)), method = 'constant')
# g3_init_guess('project_rec', value = 0) # /1e6

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

## Quicker to use the R model for prognosis as you are only doing a few runs
r_proj <- g3_to_r(proj_actions)

## Scenarios

proj_msy_fit <- g3_fit(r_proj, base.par.proj)
proj_hr0_fit <- g3_fit(
  r_proj, base.par.proj %>% g3_init_guess('project_hr', value = 0))
proj_hrly_fit <- g3_fit(
  r_proj,
  base.par.proj %>%
    g3_init_guess(
      'project_hr',
      value = plot_hr(optim_fit, min_catch_length = 45, return_data = TRUE) %>%
        filter(year == max(year)) %>% pull(value)))

# tmppath <- file.path(getwd(), base_dir, "figures")
# make_html(proj_hrly_fit, path = tmppath, file_name = "test.html", template = "nea_ghl")

cowplot::plot_grid(
  plot_hr(proj_hr0_fit, min_catch_length = 45) + ggtitle("HR0"),
  plot_hr(proj_msy_fit, min_catch_length = 45) + ggtitle("HRmsy"),
  plot_hr(proj_hrly_fit, min_catch_length = 45) + ggtitle("HRlastyear"),
  ncol = 1
)

rp_cols <- c("MSY" = "#6CA67A", "HRmsy" = "#82C893", "Bpa" = "#056A89", "HRpa" = "#449BCF", "Blim" = "#D44F56", "HRlim" = "#FF5F68")

bind_rows(
  plot_biomass(proj_hr0_fit, stocks = "ghl_female_mat", return_data = TRUE) %>%
    dplyr::select(year, value) %>%
    filter(year <= start_year+3) %>%
    mutate(HR = "0"),
  plot_biomass(proj_msy_fit, stocks = "ghl_female_mat", return_data = TRUE) %>%
    dplyr::select(year, value) %>%
    filter(year <= start_year+3) %>%
    mutate(HR = "MSY"),
  plot_biomass(proj_hrly_fit, stocks = "ghl_female_mat", return_data = TRUE) %>%
    dplyr::select(year, value) %>%
    filter(year <= start_year+3) %>%
    mutate(HR = as.character(end_year))
) %>%
  ggplot(ggplot2::aes(.data$year, .data$value, linetype = .data$HR)) +
  ggplot2::geom_line() +
  geom_hline(data = rp_tab %>%
               rename(rp = `Reference point`) %>%
               filter(rp %in% c("Bpa")),
             aes(yintercept = Value, color = rp)) +
  ggplot2::expand_limits(y = 0) +
  ggplot2::coord_cartesian(expand = FALSE) +
  ggplot2::scale_x_continuous(breaks = seq(1980,2030,2)) +
  scale_linetype_manual(values = c("MSY" = 1, "0" = 2, "2021" = 3)) +
  labs(x = "Year", y = "Spawning stock biomass (kt)", color =
         "Reference\npoint",
       linetype = "Harvest\nrate") +
  scale_color_manual(values = rp_cols) +
  ggplot2::theme_classic(base_size = 8) +
  theme(legend.position = "bottom")


## Basis table

basis_tab <-
  tibble(Variable =
           c(
             paste0("Harvest rate >= 45 cm (", start_year, ")"),
             paste0("Biomass >= 45 cm (", start_year, ")"),
             paste0("SSB (", start_year, ")"),
             paste0("Recruitment (", start_year, "-", start_year + 2, ")"),
             paste0("Expected catch (", start_year, ")")
           ),
         Value = c(
           plot_hr(proj_msy_fit, min_catch_length = 45, return_data = T) %>%
             filter(year == start_year) %>%
             pull(value) %>% round(., 3),
           plot_biomass(proj_msy_fit, min_catch_length = 45, return_data = TRUE) %>%
             filter(year == start_year) %>%
             pull(total.biomass) %>% {./1e3} %>% round(),
           plot_biomass(proj_msy_fit, stocks = "ghl_female_mat", return_data = TRUE) %>%
             filter(year == start_year) %>%
             pull(total.biomass) %>% {./1e3} %>% round(),
           mean(recruitment$recruitment)/1e6,
           round(sum(expected_catch$catch)/1e3)),
         Notes = c(
           paste0("Based on expected catch (", start_year, "); for >= 45 cm"),
           paste0("At 1 Januarty start_year; tonnes"),
           paste0("At 1 Januarty start_year; tonnes. Bpa = ", round(bpa/1e3)),
           paste0("Average ", paste(range(recruitment$year), collapse = "-"), " recruitment in millions. Does not influence short-term forecast"),
           paste0("Based on catch in ", end_year, "; tonnes")
         )
  )

## Values for advice tables ####

tmp <-
  bind_rows(
    full_join(
      plot_hr(proj_msy_fit, min_catch_length = 45, return_data = TRUE) %>%
        filter(year %in% start_year:(start_year+3)) %>%
        dplyr::select(-step, -area) %>%
        mutate(type = "msy", .before = 1),
      plot_biomass(proj_msy_fit, stocks = "ghl_female_mat", return_data = TRUE) %>%
        filter(year %in% start_year:(start_year+3)) %>%
        mutate(ssb = total.biomass/1e3) %>%
        dplyr::select(year, ssb),
      by = "year") %>%
      mutate(
        below_bpa = ssb < bpa/1e3,
        d_ssb = 100*(ssb/ssb[year == start_year]-1),
        d_tac = 100*(catch_biom/catch_biom[year == start_year]-1),
        d_biom = 100*(biomass/biomass[year == start_year]-1)),
    full_join(
      plot_hr(proj_hr0_fit, min_catch_length = 45, return_data = TRUE) %>%
        filter(year %in% start_year:(start_year+3)) %>%
        dplyr::select(-step, -area) %>%
        mutate(type = "0", .before = 1),
      plot_biomass(proj_hr0_fit, stocks = "ghl_female_mat", return_data = TRUE) %>%
        filter(year %in% start_year:(start_year+3)) %>%
        mutate(ssb = total.biomass/1e3) %>%
        dplyr::select(year, ssb),
      by = "year") %>%
      mutate(
        below_bpa = ssb < bpa/1e3,
        d_ssb = 100*(ssb/ssb[year == start_year]-1),
        d_tac = 100*(catch_biom/catch_biom[year == start_year]-1),
        d_biom = 100*(biomass/biomass[year == start_year]-1)),
    full_join(
      plot_hr(proj_hrly_fit, min_catch_length = 45, return_data = TRUE) %>%
        filter(year %in% start_year:(start_year+3)) %>%
        dplyr::select(-step, -area) %>%
        mutate(type = "ly", .before = 1),
      plot_biomass(proj_hrly_fit, stocks = "ghl_female_mat", return_data = TRUE) %>%
        filter(year %in% start_year:(start_year+3)) %>%
        mutate(ssb = total.biomass/1e3) %>%
        dplyr::select(year, ssb),
      by = "year") %>%
      mutate(
        below_bpa = ssb < bpa/1e3,
        d_ssb = 100*(ssb/ssb[year == start_year]-1),
        d_tac = 100*(catch_biom/catch_biom[year == start_year]-1),
        d_biom = 100*(biomass/biomass[year == start_year]-1))
  )


## Advice for the next year table ####

next_year_tab <-
  tibble(
    Basis =
      c(paste0("ICES advice basis for ", start_year + 1),
        paste0("HRmsy = ", hr_target),
        paste0("Other scenarios for ", start_year + 1),
        "HR = 0",
        paste0("Catch", start_year)
      ),
    TAC =
      c("",
        tmp %>%
          filter(type == "msy", year == start_year + 1) %>%
          pull(catch_biom) %>% {./1e3} %>% round(),
        "",
        tmp %>%
          filter(type == "0", year == start_year + 1) %>%
          pull(catch_biom) %>% {./1e3} %>% round(),
        tmp %>%
          filter(type == "ly", year == start_year + 1) %>%
          pull(catch_biom) %>% {./1e3} %>% round()
      ),
    HR =
      c("",
        tmp %>%
          filter(type == "msy", year == start_year + 1) %>%
          pull(value) %>% round(3),
        "",
        tmp %>%
          filter(type == "0", year == start_year + 1) %>%
          pull(value) %>% round(3),
        tmp %>%
          filter(type == "ly", year == start_year + 1) %>%
          pull(value) %>% round(3)
      ),
    `SSB the following year` =
      c("",
        tmp %>%
          filter(type == "msy", year == start_year + 2) %>%
          pull(ssb) %>% round(),
        "",
        tmp %>%
          filter(type == "0", year == start_year + 2) %>%
          pull(ssb) %>% round(),
        tmp %>%
          filter(type == "ly", year == start_year + 2) %>%
          pull(ssb) %>% round()
      ),
    `SSB_below_Bpa`=
      c("",
        tmp %>%
          filter(type == "msy", year == start_year + 2) %>%
          pull(below_bpa) %>% round(),
        "",
        tmp %>%
          filter(type == "0", year == start_year + 2) %>%
          pull(below_bpa) %>% round(),
        tmp %>%
          filter(type == "ly", year == start_year + 2) %>%
          pull(below_bpa) %>% round()
      ),
    `SSB change` =
      c("",
        tmp %>%
          filter(type == "msy", year == start_year + 2) %>%
          pull(d_ssb) %>% round(),
        "",
        tmp %>%
          filter(type == "0", year == start_year + 2) %>%
          pull(d_ssb) %>% round(),
        tmp %>%
          filter(type == "ly", year == start_year + 2) %>%
          pull(d_ssb) %>% round()
      ),
    `TAC change` =
      c("",
        tmp %>%
          filter(type == "msy", year == start_year + 1) %>%
          pull(d_tac) %>% round(),
        "",
        tmp %>%
          filter(type == "0", year == start_year + 1) %>%
          pull(d_tac) %>% round(),
        tmp %>%
          filter(type == "ly", year == start_year + 1) %>%
          pull(d_tac) %>% round()
      )
  )

## Advice for the year after table ####

last_year_tab <-
  tibble(
    Basis =
      c(paste0("ICES advice basis for ", start_year + 2),
        paste0("HRmsy = ", hr_target),
        paste0("Other scenarios for ", start_year + 2),
        "HR = 0",
        paste0("Catch", start_year)
      ),
    TAC =
      c("",
        tmp %>%
          filter(type == "msy", year == start_year + 2) %>%
          pull(catch_biom) %>% {./1e3} %>% round(),
        "",
        tmp %>%
          filter(type == "0", year == start_year + 2) %>%
          pull(catch_biom) %>% {./1e3} %>% round(),
        tmp %>%
          filter(type == "ly", year == start_year + 2) %>%
          pull(catch_biom) %>% {./1e3} %>% round()
      ),
    HR =
      c("",
        tmp %>%
          filter(type == "msy", year == start_year + 2) %>%
          pull(value) %>% round(3),
        "",
        tmp %>%
          filter(type == "0", year == start_year + 2) %>%
          pull(value) %>% round(3),
        tmp %>%
          filter(type == "ly", year == start_year + 2) %>%
          pull(value) %>% round(3)
      ),
    `SSB the following year` =
      c("",
        tmp %>%
          filter(type == "msy", year == start_year + 3) %>%
          pull(ssb) %>% round(),
        "",
        tmp %>%
          filter(type == "0", year == start_year + 3) %>%
          pull(ssb) %>% round(),
        tmp %>%
          filter(type == "ly", year == start_year + 3) %>%
          pull(ssb) %>% round()
      ),
    `SSB_below_Bpa`=
      c("",
        tmp %>%
          filter(type == "msy", year == start_year + 3) %>%
          pull(below_bpa) %>% round(),
        "",
        tmp %>%
          filter(type == "0", year == start_year + 3) %>%
          pull(below_bpa) %>% round(),
        tmp %>%
          filter(type == "ly", year == start_year + 3) %>%
          pull(below_bpa) %>% round()
      ),
    `SSB change` =
      c("",
        tmp %>%
          filter(type == "msy", year == start_year + 3) %>%
          pull(d_ssb) %>% round(),
        "",
        tmp %>%
          filter(type == "0", year == start_year + 3) %>%
          pull(d_ssb) %>% round(),
        tmp %>%
          filter(type == "ly", year == start_year + 3) %>%
          pull(d_ssb) %>% round()
      ),
    `TAC change` =
      c("",
        tmp %>%
          filter(type == "msy", year == start_year + 2) %>%
          pull(d_tac) %>% round(),
        "",
        tmp %>%
          filter(type == "0", year == start_year + 2) %>%
          pull(d_tac) %>% round(),
        tmp %>%
          filter(type == "ly", year == start_year + 2) %>%
          pull(d_tac) %>% round()
      )
  )

## Save ####

advice_calculations <- tmp

save(advice_calculations, basis_tab, last_year_tab, next_year_tab, file = file.path(outpath, 'short_term_projection_tables.Rdata'), compress = "xz")

save(proj_msy_fit, proj_hr0_fit, proj_hrly_fit, base.par.proj, file = file.path(outpath, 'short_term_projection_fit_objects.Rdata'), compress = "xz")
