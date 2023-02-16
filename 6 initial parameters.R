## ---------------------------
##
## Script name: Gadget initial parameters
##
## Purpose of script: Build the model and define initial parameters
##
## ---------------------------

## Collate actions
actions <- c(
  time_actions,
  stock_actions,
  # list(
  #   g3l_random_walk(
  #     'growth',
  #     substitute(log(avoid_zero(x)),
  #                list(x = g3_parameterized('ghl.K', by_year = TRUE))),
  #     weight = g3_parameterized('rnd_weight', scale = -1),
  #     sigma_f = g3_parameterized('ghl_K_sigma'))
  # ),
  fleet_actions,
  likelihood_actions)

## Actions to tmb_model

tmb_model <- g3_to_tmb(actions)

# Get the parameter template

tmb_param <- attr(tmb_model, "parameter_template")

# Init sigma hack, delete

# init_sigma <- init_sigma %>% 
#   filter(!(stock == "ghl_female_imm" & age >= 3)) %>% 
#   bind_rows(init_sigma %>% 
#               filter(stock == "ghl_female_mat") %>% 
#               mutate(stock = "ghl_female_imm"))

## Define the initial parameters

tmb_param <-
  tmb_param %>%
  g3_init_guess('\\.rec', 1e4, 0.1, 2.5e4, 1) %>%
  # g3_init_guess('\\.init', 1e4, 0.1, 1e5, 1) %>%
  g3_init_guess('recl', 14, 12, 20, 0) %>%
  g3_init_guess('rec.sd', 2, 1, 8, 0) %>%
  g3_init_guess('rec.1980', 0, 0, 100, 0) %>%
  g3_init_guess('rec.2021', 1, 0, 100, 0) %>%
  g3_init_guess('rec.scalar', 1, 1, 100, 0) %>%
  g3_init_guess('init.scalar', 35, 1, 100, 1) %>%
  g3_init_guess('_female.Linf', 90, 80, 120, 1) %>%
  g3_init_guess('_male.Linf', 60, 40, 100, 1) %>%
  g3_init_guess('\\.K', 200, 20, 500, 1) %>%
  # g3_init_guess('ghl_K_sigma', 0.2, 0, 1, 0) %>%
  # g3_init_guess('rnd_weight', 1, 1, 100, 0) %>%
  g3_init_guess('bbin', 6, 1e-08, 10, 1) %>%
  g3_init_guess('prop_mat0', 0.1, 0, 1, 1) %>%
  g3_init_guess('B0', 1e5, 1, 1e7, 1) %>%
  g3_init_guess('\\.alpha', 0.5, 0.01, 20, 1) %>%
  g3_init_guess('l50', 40, 10, 80, 1) %>%
  # g3_init_guess('ecos.survey.alpha', 0.5, 0.01, 8, 1) %>%
  g3_init_guess('ecos.survey.l50', 20, 5, 50, 1) %>%
  g3_init_guess('andersen.p0$', 0, NA, NA, 0) %>%
  g3_init_guess('andersen.p2$', 1, NA, NA, 0) %>%
  g3_init_guess('andersen.L$', stock_params$maxlength, NA, NA, 0) %>%
  g3_init_guess('\\.p1$', 0.5, 0, 1, 1) %>%
  g3_init_guess('\\.p3$', .15, 0.01, 1, 1) %>%
  g3_init_guess('\\.p4$', 35, 1e-6, 1e4, 1) %>%
  g3_init_guess('init.F', 0.4, 0.1, 0.8, 1) %>%
  # g3_init_guess('\\.M', 0.1, 0.001, 1, 0) %>%
  g3_init_guess('_female.M', 0.12, 0.001, 0.4, 0) %>%
  g3_init_guess('_male.M', 0.16, 0.001, 0.4, 0) %>%
  g3_init_guess('_female.mat_initial_alpha', 0.421, 0.001, 3, 0) %>%
  g3_init_guess('_male.mat_initial_alpha', 0.449, 0.001, 3, 0) %>%
  g3_init_guess('_female.mat_initial_a50', 14.52, 3, 25, 0) %>%
  g3_init_guess('_male.mat_initial_a50', 7.34, 3, 25, 0) %>%
  #  g3_init_guess('prop_mat0', 0.5, 0.1, 0.9, 0) %>%
  #  g3_init_guess('B0', 100, 1, 5000, 1) %>%
  g3_init_guess('mat_alpha', 70, 10, 300, 1) %>%
  g3_init_guess('_female.mat_l50',
                61.33947, 0.75*61.33947, 1.25*61.33947,
                # mat_l50$mean[1], 0.75*mat_l50$mean[1], 1.25*mat_l50$mean[1],
                1) %>%
  g3_init_guess('_male.mat_l50',
                43.62948, 0.75*43.62948, 1.25*43.62948,
                # mat_l50$mean[2], 0.75*mat_l50$mean[2], 1.25*mat_l50$mean[2],
                1) %>%
  g3_init_guess('_female.walpha',
                1.8267e-06,
                # lw_constants %>% filter(sex == "F", term == "a") %>% pull(estimate),
                1e-10, 1, 0) %>%
  g3_init_guess('_female.wbeta',
                3.4074,
                # lw_constants %>% filter(sex == "F", term == "b") %>% pull(estimate),
                2, 4, 0) %>%
  g3_init_guess('_male.walpha',
                6.1549e-06,
                # lw_constants %>% filter(sex == "M", term == "a") %>% pull(estimate),
                1e-10, 1, 0) %>%
  g3_init_guess('_male.wbeta',
                3.09421,
                # lw_constants %>% filter(sex == "M", term == "b") %>% pull(estimate),
                2, 4, 0) %>%
  g3_init_guess(pattern = '_female_mat\\.init\\.sd',
                value = init_sigma %>%
                  filter(grepl('_female_mat', stock)) %>%
                  filter(age %in%
                           stock_params$female_mat$minage:
                           stock_params$female_mat$maxage) %>%
                  pull(ms),
                lower = 0, upper = 20, optimise = FALSE) %>%
  g3_init_guess('_male_mat\\.init\\.sd',
                init_sigma %>%
                  filter(grepl('_male_mat', stock)) %>%
                  filter(age %in%
                           stock_params$male_mat$minage:
                           stock_params$male_mat$maxage) %>%
                  pull(ms), 0, 20, 0) %>%
  g3_init_guess('_female_imm\\.init\\.sd',
                init_sigma %>%
                  filter(grepl('_female_imm', stock)) %>%
                  filter(age %in%
                           stock_params$female_imm$minage:
                           stock_params$female_imm$maxage) %>%
                  pull(ms), 0, 20, 0) %>%
  g3_init_guess('_male_imm\\.init\\.sd',
                init_sigma %>%
                  filter(grepl('_male_imm', stock)) %>%
                  filter(age %in%
                           stock_params$male_imm$minage:
                           stock_params$male_imm$maxage) %>%
                  pull(ms), 0, 20, 0) %>%
  suppressWarnings()

## Add likelihood component weights (control using set_weights argument)
if(set_weights) {
  # Copy the /iterative_reweighting/weights.final as opened in Rstudio viewer under:
  tmp_weights <- read.table(textConnection(
    '                                           comp approx_weight      weight
           cdist_sumofsquares_EggaN_matp_weight  1.956147e+03  15.2974241
       cdist_sumofsquares_TrawlRus_ldist_weight  5.507159e+03 961.8353625
           cdist_sumofsquares_EcoS_ldist_weight  1.985071e+03   250
          cdist_sumofsquares_EcoS_aldist_weight  8.902087e+03   3.1857842
     cdist_sumofsquares_TrawlNor_sexdist_weight  3.411164e+02    1.5
         cdist_sumofsquares_EcoS_sexdist_weight  7.657220e+02   3
     cdist_sumofsquares_OtherNor_sexdist_weight  2.653991e+02    .7
    cdist_sumofsquares_EggaN_aldist_male_weight  4.473190e+03 430
       cdist_sumofsquares_RussianS_ldist_weight  8.910168e+02 221.0199652
      cdist_sumofsquares_OtherNor_aldist_weight  1.899254e+04  23.5569851
          cdist_sumofsquares_EggaN_ldist_weight  5.775529e+03 1000
        cdist_sumofsquares_WinterS_ldist_weight  8.141353e+02 195.4152789
  cdist_sumofsquares_EggaN_aldist_female_weight  1.366532e+04 786.3379258
     cdist_sumofsquares_TrawlRus_sexdist_weight  3.792818e+02    .6047172
       cdist_sumofsquares_OtherNor_ldist_weight  6.969325e+03  649.439644
       cdist_sumofsquares_TrawlNor_ldist_weight  3.707740e+03 619.6624068
 adist_surveyindices_log_EggaN_SI_female_weight  1.122509e+03   5
   adist_surveyindices_log_EggaN_SI_male_weight  6.397818e+02   10
        adist_surveyindices_log_Juv_SI_1_weight  2.300172e+28   30
        adist_surveyindices_log_Juv_SI_2_weight  1.901476e+28   2
         adist_surveyindices_log_EcoS_SI_weight  1.371257e+28   1.8471612
     adist_surveyindices_log_RussianS_SI_weight  4.720994e+01   4.2376414
    '
  ), header = TRUE)


  param_order <- names(tmb_param[na.omit(match(tmp_weights$comp,tmb_param$switch)), "value"])

  tmb_param[na.omit(match(tmp_weights$comp,tmb_param$switch)), "value"] <-
    tmp_weights[na.omit(match(param_order, tmp_weights$comp)), "weight"]

  rm(param_order)
}

## Add the previous optimized model parameters as initial values (control using the previous_model_params_as_initial argument)

if(exists("prev_param") & previous_model_params_as_initial) {
  param_order <- names(tmb_param[na.omit(match(names(prev_param),tmb_param$switch)), "value"])

  tmb_param[na.omit(match(names(prev_param),tmb_param$switch)), "value"] <-
    unname(prev_param[na.omit(match(param_order, names(prev_param)))])

  rm(param_order)

  # tmb_param[match(names(prev_param),tmb_param$switch), "value"] <- unname(prev_param)
}

### Force parameter bounds (experimental)

if(force_bound_params) {
  if(curl::has_internet()) {
    remotes::install_github("gadget-framework/g3experiments", upgrade = "never", quiet = TRUE)
  }

  actions <- c(actions, list(g3experiments::g3l_bounds_penalty(tmb_param)))
  tmb_model <- g3_to_tmb(actions)
}

# Turn actions into an R function

model <- g3_to_r(actions)
# You can edit the model code with:
# model <- edit(model)

### Rearrange parameters to follow the model order (fixed a bug that occurs sometimes)

tmb_param <- tmb_param[match(names(attr(model,'parameter_template')), tmb_param$switch),]

### Set initial population sd to same than recl.sd

if(!tmb_param[grep('rec.sd', tmb_param$switch),"optimise"]) {
  init_sigma[init_sigma$age == 1, "ms"] <- unname(unlist(tmb_param[grep('rec.sd', tmb_param$switch),]$value))
}

## Write the parameters to a csv file

write.csv(tmb_param, file = file.path(base_dir, "data/Initial TMB parameters.csv"), row.names = FALSE)
save(tmb_param, file = file.path(base_dir, "data/Initial TMB parameters.rda"))

## Save the R model

if(!run_retro) {
  save(model, file = file.path(base_dir, "data/R model.rda"), compress = "xz")
}
