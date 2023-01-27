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
  fleet_actions,
  likelihood_actions)

# Turn actions into an R function

model <- g3_to_r(actions)

# You can edit the model code with:
# model <- edit(model)

tmb_model <- g3_to_tmb(actions)

# Get the parameter template

tmb_param <- attr(tmb_model, "parameter_template")

## Define the initial parameters

tmb_param <-
  tmb_param %>%
  g3_init_guess('\\.rec', 1e5, 0.1, 1e6, 1) %>%
  g3_init_guess('\\.init', 1e5, 0.1, 1e6, 1) %>%
  g3_init_guess('recl', 14, 12, 20, 1) %>%
  g3_init_guess('rec.sd', 2, 1, 8, 0) %>%
  g3_init_guess('rec.1980', 0, 0, 100, 0) %>%
  g3_init_guess('rec.scalar', 10, 1, 100, 1) %>%
  g3_init_guess('init.scalar', 10, 1, 100, 1) %>%
  g3_init_guess('_female.Linf', 90, 80, 120, 1) %>%
  g3_init_guess('_male.Linf', 60, 40, 100, 1) %>%
  g3_init_guess('\\.K', 200, 20, 500, 1) %>%
  g3_init_guess('bbin', 6, 1e-08, 10, 1) %>%
  g3_init_guess('prop_mat0', 0.1, 0, 1, 1) %>%
  g3_init_guess('B0', 1e5, 1, 1e7, 1) %>%
  g3_init_guess('\\.alpha', 0.5, 0.01, 1, 1) %>%
  g3_init_guess('l50', 40, 15, 80, 1) %>%
  g3_init_guess('andersen.p0$', 0, NA, NA, 0) %>%
  g3_init_guess('andersen.p2$', 1, NA, NA, 0) %>%
  g3_init_guess('andersen.L$', stock_params$maxlength, NA, NA, 0) %>%
  g3_init_guess('\\.p1$', 0.5, 0, 1, 1) %>%
  g3_init_guess('\\.p3$', 50, 1e-6, 100, 1) %>%
  g3_init_guess('\\.p4$', 50, 1e-6, 100, 1) %>%
  g3_init_guess('init.F', 0.4, 0.1, 0.8, 1) %>%
  g3_init_guess('\\.M', 0.12, 0.001, 1, 0) %>%
  g3_init_guess('_male_imm.M', 0.24, 0.001, 1, 0) %>%
  g3_init_guess('_male_mat.M', 0.24, 0.001, 1, 0) %>%
  g3_init_guess('_female.mat_initial_alpha', 0.260, 0.001, 3, 1) %>%
  g3_init_guess('_male.mat_initial_alpha', 0.376, 0.001, 3, 1) %>%
  g3_init_guess('_female.mat_initial_a50', 12.98, 3, 25, 0) %>%
  g3_init_guess('_male.mat_initial_a50', 5.64, 3, 25, 0) %>%
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
                  filter(sex == "F") %>%
                  filter(age %in%
                           stock_params$female_mat$minage:
                           stock_params$female_mat$maxage) %>%
                  pull(ms),
                lower = 0, upper = 20, optimise = FALSE) %>%
  g3_init_guess('_male_mat\\.init\\.sd',
                init_sigma %>%
                  filter(sex == "M") %>%
                  filter(age %in%
                           stock_params$male_mat$minage:
                           stock_params$male_mat$maxage) %>%
                  pull(ms), 0, 20, 0) %>%
  g3_init_guess('_female_imm\\.init\\.sd',
                init_sigma %>%
                  filter(sex == "F") %>%
                  filter(age %in%
                           stock_params$female_imm$minage:
                           stock_params$female_imm$maxage) %>%
                  pull(ms), 0, 20, 0) %>%
  g3_init_guess('_male_imm\\.init\\.sd',
                init_sigma %>%
                  filter(sex == "M") %>%
                  filter(age %in%
                           stock_params$male_imm$minage:
                           stock_params$male_imm$maxage) %>%
                  pull(ms), 0, 20, 0) %>%
  suppressWarnings()

## Add likelihood component weights (control using set_weights argument)
if(set_weights) {
  # Copy the /iterative_reweighting/weights.final as opened in Rstudio viewer under:
  tmp_weights <- read.table(textConnection(
    '                                           comp approx_weight       weight
           cdist_sumofsquares_EggaN_matp_weight  1.956147e+03   190.551874
     cdist_sumofsquares_OtherRus_sexdist_weight  5.340361e+02     4.802193
       cdist_sumofsquares_TrawlRus_ldist_weight  5.507159e+03 10909.863753
          cdist_sumofsquares_EggaS_ldist_weight  4.640946e+03  3555.459831
           cdist_sumofsquares_EggaS_matp_weight  2.400939e+03   102.321286
           cdist_sumofsquares_EcoS_ldist_weight  1.985071e+03  2248.741029
          cdist_sumofsquares_EcoS_aldist_weight  6.293601e+03    37.718309
     cdist_sumofsquares_TrawlNor_sexdist_weight  4.182257e+02     4.757358
         cdist_sumofsquares_EcoS_sexdist_weight  7.657063e+02    82.985234
     cdist_sumofsquares_OtherNor_sexdist_weight  3.042224e+02     5.845288
    cdist_sumofsquares_EggaN_aldist_male_weight  4.432070e+03   120.219041
       cdist_sumofsquares_RussianS_ldist_weight  8.133667e+02  3036.614969
      cdist_sumofsquares_OtherNor_aldist_weight  1.899254e+04   228.943364
          cdist_sumofsquares_EggaN_ldist_weight  5.775529e+03  7724.634317
         cdist_sumofsquares_EggaS_aldist_weight  1.679424e+04    55.519817
       cdist_sumofsquares_OtherRus_ldist_weight  1.826473e+04   14445.2639
  cdist_sumofsquares_EggaN_aldist_female_weight  1.361760e+04    71.814261
     cdist_sumofsquares_TrawlRus_sexdist_weight  3.167435e+02     5.266938
       cdist_sumofsquares_OtherNor_ldist_weight  6.969325e+03 10261.845575
       cdist_sumofsquares_TrawlNor_ldist_weight  3.707740e+03  6797.157089
 adist_surveyindices_log_EggaN_SI_female_weight  1.122509e+03   272.474998
        adist_surveyindices_log_Juv_SI_1_weight  2.300172e+28         2500
        adist_surveyindices_log_Juv_SI_3_weight  1.739155e+28            0
     adist_surveyindices_log_RussianS_SI_weight  4.720994e+01    44.142828
   adist_surveyindices_log_EggaN_SI_male_weight  6.397818e+02         2500
        adist_surveyindices_log_Juv_SI_2_weight  1.901476e+28         2500
        cdist_sumofsquares_WinterS_ldist_weight  1.                   1000
  '
  ), header = TRUE)

  tmb_param[match(tmp_weights$comp,tmb_param$switch), "value"] <- tmp_weights$weight
}

## Add the previous optimized model parameters as initial values (control using the previous_model_params_as_initial argument)

if(exists("prev_param")) {
  tmb_param[match(names(prev_param),tmb_param$switch), "value"] <- unname(prev_param)
}

### Force parameter bounds (experimental)

if(force_bound_params) {
  if(curl::has_internet()) {
    remotes::install_github("gadget-framework/g3experiments", upgrade = "never", quiet = TRUE)
  }

  actions <- c(actions, list(g3experiments::g3l_bounds_penalty(tmb_param)))

  model <- g3_to_r(actions)
  tmb_model <- g3_to_tmb(actions)
}

## Write the parameters to a csv file

write.csv(tmb_param, file = file.path(base_dir, "data/Initial TMB parameters.csv"), row.names = FALSE)
save(tmb_param, file = file.path(base_dir, "data/Initial TMB parameters.rda"))
