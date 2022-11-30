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
  g3_init_guess('\\.rec', 250, 0.001, 500, 1) %>% 
  g3_init_guess('\\.init', 250, 0.001, 500, 1) %>%
  g3_init_guess('recl', 12, 5, 20, 0) %>%
  g3_init_guess('rec.sd', 2, 1, 5, 1) %>%
  g3_init_guess('rec.scalar', 50, 1, 100, 1) %>%
  g3_init_guess('init.scalar', 50, 1, 100, 1) %>%
  g3_init_guess('_female.Linf', 90, 80, 120, 1) %>%
  g3_init_guess('_male.Linf', 60, 40, 80, 1) %>%
  g3_init_guess('\\.K', 20, 5, 60, 1) %>%
  g3_init_guess('bbin', 6, 1e-08, 10, 1) %>%
  g3_init_guess('\\.alpha', 0.5, 0.01, 1, 1) %>%
  g3_init_guess('l50', 50, 40, 100, 1) %>%
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
  g3_init_guess('mat_initial_alpha', 1, 0.001, 3, 1) %>%
  g3_init_guess('_female.mat_initial_a50',
                mat_l50 %>% filter(sex == "F") %>% pull(slope), 3, 25, 0) %>%
  g3_init_guess('_male.mat_initial_a50',
                mat_l50 %>% filter(sex == "M") %>% pull(slope), 3, 25, 0) %>%
  #  g3_init_guess('prop_mat0', 0.5, 0.1, 0.9, 0) %>%
  #  g3_init_guess('B0', 100, 1, 5000, 1) %>%
  g3_init_guess('mat_alpha', 70, 10, 200, 1) %>% # these were log() before
  g3_init_guess('_female.mat_l50', mat_l50$mean[1],
                0.75*mat_l50$mean[1], 1.25*mat_l50$mean[1], 1) %>%
  g3_init_guess('_male.mat_l50', mat_l50$mean[2],
                0.75*mat_l50$mean[2], 1.25*mat_l50$mean[2], 1) %>%
  g3_init_guess('sigma_alpha', init_sigma_coef[['alpha']], -1, 1, 0) %>%
  g3_init_guess('sigma_beta', init_sigma_coef[['beta']], 0, 2, 0) %>%
  g3_init_guess('sigma_gamma', init_sigma_coef[['gamma']], 0, 1, 0) %>%
  g3_init_guess('_female.walpha',
                lw_constants %>% filter(sex == "F", term == "a") %>% pull(estimate),
                1e-10, 1, 0) %>%
  g3_init_guess('_female.wbeta',
                lw_constants %>% filter(sex == "F", term == "b") %>% pull(estimate),
                2, 4, 0) %>%
  g3_init_guess('_male.walpha',
                lw_constants %>% filter(sex == "M", term == "a") %>% pull(estimate),
                1e-10, 1, 0) %>%
  g3_init_guess('_male.wbeta',
                lw_constants %>% filter(sex == "M", term == "b") %>% pull(estimate),
                2, 4, 0) %>%
  g3_init_guess(pattern = '_female_mat\\.init\\.sd',
                value = init_sigma %>%
                  filter(age %in%
                           stock_params$female_mat$minage:
                           stock_params$female_mat$maxage) %>%
                  pull(ms),
                lower = 0, upper = 20, optimise = FALSE) %>%
  g3_init_guess('_male_mat\\.init\\.sd',
                init_sigma %>%
                  filter(age %in%
                           stock_params$male_mat$minage:
                           stock_params$male_mat$maxage) %>%
                  pull(ms), 0, 20, 0) %>%
  g3_init_guess('_female_imm\\.init\\.sd',
                init_sigma %>%
                  filter(age %in%
                           stock_params$female_imm$minage:
                           stock_params$female_imm$maxage) %>%
                  pull(ms), 0, 20, 0) %>%
  g3_init_guess('_male_imm\\.init\\.sd',
                init_sigma %>%
                  filter(age %in%
                           stock_params$male_imm$minage:
                           stock_params$male_imm$maxage) %>%
                  pull(ms), 0, 20, 0)

## Add likelihood component weights (control using set_weights argument)
if(set_weights) {
 
  tmp_weights <- read.table(textConnection( 
  '                                           switch        value
1  adist_surveyindices_log_EggaN_SI_female_weight         0.05
2    adist_surveyindices_log_EggaN_SI_male_weight         0.05
3         adist_surveyindices_log_Juv_SI_1_weight         0.05
4         adist_surveyindices_log_Juv_SI_2_weight         0.05
5         adist_surveyindices_log_Juv_SI_3_weight         0.05
6       adist_surveyindices_log_Russian_SI_weight         0.05
7   cdist_sumofsquares_EggaN_aldist_female_weight         44
8     cdist_sumofsquares_EggaN_aldist_male_weight         46
9           cdist_sumofsquares_EggaN_ldist_weight         151611
10           cdist_sumofsquares_EggaN_matp_weight         41
11          cdist_sumofsquares_other_ldist_weight         32031
12     cdist_sumofsquares_otherrus_ldist_f_weight         113386
13     cdist_sumofsquares_otherrus_ldist_m_weight         13489
14  cdist_sumofsquares_RussianSurvey_ldist_weight         4984
15       cdist_sumofsquares_trawlnor_ldist_weight         18647
16     cdist_sumofsquares_trawlrus_ldist_f_weight         24412
17     cdist_sumofsquares_trawlrus_ldist_m_weight         23729
  '
  ))
  
  tmb_param[match(tmp_weights$switch,tmb_param$switch), "value"] <- tmp_weights$value
}

## Add the previous optimized model parameters as initial values (control using the previous_model_params_as_initial argument)

if(exists("prev_param")) {
  tmb_param[match(names(prev_param),tmb_param$switch), "value"] <- unname(prev_param)
}

## Write the parameters to a csv file

write.csv(tmb_param, file = file.path(base_dir, "data/Initial TMB parameters.csv"), row.names = FALSE)
save(tmb_param, file = file.path(base_dir, "data/Initial TMB parameters.rda"))
