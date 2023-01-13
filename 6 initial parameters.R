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
# rownames(tmb_param) <- 1:nrow(tmb_param)

## Define the initial parameters

tmb_param <-
  tmb_param %>%
  g3_init_guess('\\.rec', 1e5, 0.1, 1e6, 1) %>%
  g3_init_guess('\\.init', 1e5, 0.1, 1e6, 1) %>%
  g3_init_guess('recl', 14, 5, 20, 0) %>%
  g3_init_guess('rec.sd', 2, 1, 8, 0) %>%
  g3_init_guess('rec.1980', 0, 0, 100, 0) %>%
  g3_init_guess('rec.scalar', 10, 1, 100, 1) %>%
  g3_init_guess('init.scalar', 10, 1, 100, 1) %>%
  g3_init_guess('_female.Linf', 90, 80, 120, 1) %>%
  g3_init_guess('_male.Linf', 60, 40, 100, 1) %>%
  g3_init_guess('\\.K', 20, 5, 60, 1) %>%
  g3_init_guess('bbin', 6, 1e-08, 10, 1) %>%
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
  g3_init_guess('mat_alpha', 70, 10, 200, 1) %>%
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
    '                                            comp approx_weight       weight
            cdist_sumofsquares_EggaN_matp_weight  2.169076e+03     2.857687
   cdist_sumofsquares_RussianS_ldist_weight       8.133667e+02  156.6579972
      cdist_sumofsquares_OtherRus_sexdist_weight  4.800372e+02    1.8846844
        cdist_sumofsquares_TrawlRus_ldist_weight  1.948335e+04         1400
           cdist_sumofsquares_EggaS_ldist_weight  1.292751e+04 1357.8051012
            cdist_sumofsquares_EggaS_matp_weight  2.571764e+03    3.2399384
            cdist_sumofsquares_EcoS_ldist_weight  6.871618e+03 1004.9290389
           cdist_sumofsquares_EcoS_aldist_weight  9.937233e+03           20
      cdist_sumofsquares_TrawlNor_sexdist_weight  2.801587e+02    0.8506735
          cdist_sumofsquares_EcoS_sexdist_weight  8.781064e+02    3.6595778
      cdist_sumofsquares_OtherNor_sexdist_weight  3.193361e+02    1.1572215
     cdist_sumofsquares_EggaN_aldist_male_weight  8.732552e+03   21.6202662
       cdist_sumofsquares_OtherNor_aldist_weight  1.973580e+04   51.5328161
           cdist_sumofsquares_EggaN_ldist_weight  2.037149e+04 4806.0988995
          cdist_sumofsquares_EggaS_aldist_weight  3.394815e+04   84.1434913
        cdist_sumofsquares_OtherRus_ldist_weight  4.035618e+04 1104.2867422
   cdist_sumofsquares_EggaN_aldist_female_weight  2.479229e+04   21.9512872
      cdist_sumofsquares_TrawlRus_sexdist_weight  4.324776e+02            1
        cdist_sumofsquares_OtherNor_ldist_weight  2.163332e+04  898.2523937
        cdist_sumofsquares_TrawlNor_ldist_weight  1.217407e+04 1426.1293823
 adist_surveyindices_log_Russian_SI_weight        4.720994e+01           20
    adist_surveyindices_log_EggaN_SI_male_weight  6.489285e+02           30
  adist_surveyindices_log_EggaN_SI_female_weight  6.668592e+02           30
         adist_surveyindices_log_Juv_SI_1_weight  2.300172e+28           50
         adist_surveyindices_log_Juv_SI_3_weight  1.739155e+28            0
         adist_surveyindices_log_Juv_SI_2_weight  1.901476e+28           50
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
