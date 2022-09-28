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
  g3_init_guess('\\.rec', 500, 0.001, 1000, 1) %>%
  g3_init_guess('\\.init', 500, 0.001, 1000, 1) %>%
  g3_init_guess('recl', 12, 10, 30, 1) %>%
  g3_init_guess('rec.sd', 2, 1, 5, 1) %>%
  g3_init_guess('rec.scalar', 250, 1, 500, 1) %>%
  g3_init_guess('init.scalar', 150, 1, 300, 1) %>%
  g3_init_guess('Linf', 90, 80, 120, 1) %>%
  g3_init_guess('\\.K', 50, 40, 120, 1) %>%
  g3_init_guess('bbin', 6, 1e-08, 100, 1) %>%
  g3_init_guess('\\.alpha', 0.5, 0.01, 1, 1) %>%
  g3_init_guess('l50', 50, 40, 100, 1) %>%
  g3_init_guess('init.F', 0.4, 0.1, 0.8, 1) %>%
  g3_init_guess('\\.M', 0.15, 0.001, 1, 0) %>%
  g3_init_guess('mat_initial_alpha', 1, 0.001, 3, 1) %>%
  g3_init_guess('_female.mat_initial_a50',
                mat_l50 %>% filter(sex == "F") %>% pull(slope), 3, 25, 0) %>%
  g3_init_guess('_male.mat_initial_a50',
                mat_l50 %>% filter(sex == "M") %>% pull(slope), 3, 25, 0) %>%
  #  g3_init_guess('prop_mat0', 0.5, 0.1, 0.9, 0) %>%
  #  g3_init_guess('B0', 100, 1, 5000, 1) %>%
  g3_init_guess('mat1', 70, 10, 200, 1) %>% # these were log() before
  g3_init_guess('_female.mat2', mat_l50$mean[1],
                0.75*mat_l50$mean[1], 1.25*mat_l50$mean[1], 1) %>%
  g3_init_guess('_male.mat2', mat_l50$mean[2],
                0.75*mat_l50$mean[2], 1.25*mat_l50$mean[2], 1) %>%
  g3_init_guess('sigma_alpha', init_sigma_coef[['alpha']], -1, 1, 0) %>%
  g3_init_guess('sigma_beta', init_sigma_coef[['beta']], 0, 2, 0) %>%
  g3_init_guess('sigma_gamma', init_sigma_coef[['gamma']], 0, 1, 0) %>%
  g3_init_guess('female.walpha',
                lw_constants %>% filter(sex == "F", term == "a") %>% pull(estimate),
                1e-10, 1, 0) %>%
  g3_init_guess('female.wbeta',
                lw_constants %>% filter(sex == "F", term == "b") %>% pull(estimate),
                2, 4, 0) %>%
  g3_init_guess('male.walpha',
                lw_constants %>% filter(sex == "M", term == "a") %>% pull(estimate),
                1e-10, 1, 0) %>%
  g3_init_guess('male.wbeta',
                lw_constants %>% filter(sex == "M", term == "b") %>% pull(estimate),
                2, 4, 0) %>%
  g3_init_guess(pattern = 'female_mat\\.init\\.sd',
                value = init_sigma %>%
                  filter(age %in%
                           stock_params$female_mat$minage:
                           stock_params$female_mat$maxage) %>%
                  pull(ms),
                lower = 0, upper = 20, optimise = FALSE) %>%
  g3_init_guess('male_mat\\.init\\.sd',
                init_sigma %>%
                  filter(age %in%
                           stock_params$male_mat$minage:
                           stock_params$male_mat$maxage) %>%
                  pull(ms), 0, 20, 0) %>%
  g3_init_guess('female_imm\\.init\\.sd',
                init_sigma %>%
                  filter(age %in%
                           stock_params$female_imm$minage:
                           stock_params$female_imm$maxage) %>%
                  pull(ms), 0, 20, 0) %>%
  g3_init_guess('male_imm\\.init\\.sd',
                init_sigma %>%
                  filter(age %in%
                           stock_params$male_imm$minage:
                           stock_params$male_imm$maxage) %>%
                  pull(ms), 0, 20, 0)


## Add the previous optimized model parameters as initial values (control using the previous_model_params_as_initial argument)

if(exists("prev_param")) {
  tmb_param[match(names(prev_param),tmb_param$switch), "value"] <- unname(prev_param)
}

## Write the parameters to a csv file

write.csv(tmb_param, file = file.path(base_dir, "data/Initial TMB parameters.csv"), row.names = FALSE)
save(tmb_param, file = file.path(base_dir, "data/Initial TMB parameters.rda"))

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
