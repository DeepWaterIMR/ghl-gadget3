## ---------------------------
##
## Script name: Gadget stocks
##
## Purpose of script: Set up stocks
##
## ---------------------------

## Source or list custom functions used within the script

source("R/model_actions2.R")

## ---------------------------

## Read data

## ---------------------------

#############################
## Immature stocks first ####

female_imm <- g3_stock(
  c(species = tolower(defaults$species), sex = 'female', 'imm'),
  lengthgroups = seq(stock_params$female_imm$minlength,
                     stock_params$female_imm$maxlength,
                     stock_params$dl)
) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(minage = stock_params$female_imm$minage,
          maxage = stock_params$female_imm$maxage)

male_imm <- g3_stock(
  c(species = tolower(defaults$species), sex = 'male', 'imm'),
  lengthgroups = seq(stock_params$male_imm$minlength,
                     stock_params$male_imm$maxlength,
                     stock_params$dl)
) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(minage = stock_params$male_imm$minage,
          maxage = stock_params$male_imm$maxage)

female_mat <- g3_stock(
  c(species = tolower(defaults$species), sex = 'female', 'mat'),
  lengthgroups = seq(stock_params$female_mat$minlength,
                     stock_params$female_mat$maxlength,
                     stock_params$dl)
) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(minage = stock_params$female_mat$minage,
          maxage = stock_params$female_mat$maxage)

male_mat <- g3_stock(
  c(species = tolower(defaults$species), sex = 'male', 'mat'),
  lengthgroups = seq(stock_params$male_mat$minlength,
                     stock_params$male_mat$maxlength,
                     stock_params$dl)
) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(minage = stock_params$male_mat$minage,
          maxage = stock_params$male_mat$maxage)

stocks <- list(female_imm,female_mat,male_imm,male_mat)

#############################
## Compile stock actions ####

## Immature stock actions
female_imm_actions <- model_actions2(
  imm = female_imm,
  mat = female_mat,
  mlgg = stock_params$maxlengthgroupgrowth,
  mature = FALSE,
  comp_id = c('species', 'sex'),
  rec_id = stocks,
  rec_scalar_id = stocks,
  parametric_sd = FALSE,
  init_mode = setup_options$initial_abund_mode,
  # tv_params = c('recl'),
  # by_age_params = c('m'),
  allstocks = stocks,
  exp_params = c('init.f')
  )

male_imm_actions <- model_actions2(
  imm = male_imm,
  mat = male_mat,
  mlgg = stock_params$maxlengthgroupgrowth,
  mature = FALSE,
  comp_id = c('species', 'sex'),
  rec_id = stocks,
  rec_scalar_id = stocks,
  parametric_sd = FALSE,
  init_mode = setup_options$initial_abund_mode,
  # tv_params = c('recl'),
  # by_age_params = c('m'),
  allstocks = stocks,
  exp_params = c('init.f')
  )

## Mature stock actions
female_mat_actions <- model_actions2(
  imm = female_imm,
  mat = female_mat,
  mlgg = stock_params$maxlengthgroupgrowth,
  mature = TRUE,
  comp_id = c('species', 'sex'),
  rec_id = stocks,
  rec_scalar_id = stocks,
  parametric_sd = FALSE,
  init_mode = setup_options$initial_abund_mode,
  # tv_params = c('recl'),
  # by_age_params = c('m'),
  allstocks = stocks,
  exp_params = c('init.f')
  )

male_mat_actions <- model_actions2(
  imm = male_imm,
  mat = male_mat,
  mlgg = stock_params$maxlengthgroupgrowth,
  mature = TRUE,
  comp_id = c('species', 'sex'),
  rec_id = stocks,
  rec_scalar_id = stocks,
  parametric_sd = FALSE,
  init_mode = setup_options$initial_abund_mode,
  # tv_params = c('recl'),
  # by_age_params = c('m'),
  allstocks = stocks,
  exp_params = c('init.f')
  )

## Combine stock actions
stock_actions <- c(male_imm_actions,male_mat_actions,
                   female_imm_actions,female_mat_actions)
