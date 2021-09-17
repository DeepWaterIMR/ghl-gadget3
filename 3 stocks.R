## ---------------------------
##
## Script name:
##
## Purpose of script:
##
## Author: Mikko Vihtakari // Institute of Marine Research, Norway
## Email: mikko.vihtakari@hi.no
##
## Date Created: 2021-02-12
##
## ---------------------------

## Source or list custom functions used within the script

source("R/stock_param_functions.R")

## ---------------------------

## Read data

# load("data/out/Initial stock parameters.rda")

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




# -----------------------------
# Define bounded parameters
# -----------------------------

## MODEL PARAMETERS
# List containing the parameter name, its lower and upper bounds.
# The function "g3_stock_param" is a wrapper for g3_param that inserts the species/stock name into the parameter reference
# if lower and upper are NULL, then the parameter will be unbounded e.g. ~g3_param("ling.k")
# if the lower and upper parameters are integers a reference to a bounded parameter will be created e.g. ~bounded(g3_param(ling.k), lower, upper)
# "g3_stock_param_table" is an equivalent function that creates a reference to a table of parameters e.g. g3_param_table(ling.init, minage:maxage)


model_params <- c(model_params, list(

  ## Initial conditions:
  'walpha' = list(lower = NULL, upper = NULL),
  'wbeta' = list(lower = NULL, upper = NULL),
  'init.F' = list(lower = 0.2, upper = 0.8),
  'init.sd' = list(lower = NULL, upper = NULL),
  'scalar' = list(lower = 1, upper = 100),      ## Scalar for initial abundance and recruitment (all stocks)
  'init' = list(lower = 0.001, upper = 200),
  'renew' = list(lower = 0.001, upper = 200),

  ## Renewal:
  'rec.sd' = list(lower = 1, upper = 5),
  'recl' = list(lower = -5, upper = 30),
  ## Growth:
  'Linf' = list(lower = 80, upper = 120),
  'K' = list(lower = 40, upper = 120),
  'bbin' = list(lower = 1, upper = 1000),

  ## Maturity:
  'mat1' = list(lower = NULL, upper = NULL),
  'mat2' = list(lower = 20, upper = 120),
  'mat.a' = list(lower = 0.5, upper = 2),
  'mat.a50' = list(lower = min(gadget3:::stock_definition(female_imm, 'minage'),
                               gadget3:::stock_definition(female_mat, 'minage')),
                   upper = max(gadget3:::stock_definition(female_imm, 'maxage'),
                               gadget3:::stock_definition(female_mat, 'maxage'))),

  ## Mortality
  'M' = list(lower = NULL, upper = NULL),

  ## Age bounds as symbols
  imm_minage = as.symbol(paste0(female_imm$name, "__minage")),
  imm_maxage = as.symbol(paste0(female_imm$name, "__maxage")),
  mat_minage = as.symbol(paste0(female_mat$name, "__minage")),
  mat_maxage = as.symbol(paste0(female_mat$name, "__maxage"))

))

# INITIAL ABUNDANCE

f_init_abund <-
  ~bounded(g3_param("ghl_female.scalar"), 1, 100) * bounded(
    if (cur_time == 0)
      g3_param_table("ghl_female.init", expand.grid(
        age = seq(
          min(ghl_female_imm__minage, ghl_female_mat__minage),
          max(ghl_female_imm__maxage, ghl_female_mat__maxage))))
    else
      g3_param_table("ghl_female.renew", expand.grid(
        cur_year = seq(start_year, end_year)))
    , 0.001, 200)

m_init_abund <-
  ~bounded(g3_param("ghl_male.scalar"), 1, 100) * bounded(
    if (cur_time == 0)
      g3_param_table("ghl_male.init", expand.grid(
        age = seq(
          min(ghl_male_imm__minage, ghl_male_mat__minage),
          max(ghl_male_imm__maxage, ghl_male_mat__maxage))))
    else
      g3_param_table("ghl_male.renew", expand.grid(
        cur_year = seq(start_year, end_year)))
    , 0.001, 200)

## Ensure that old fish are not immature
# a50 is bounded

prop_m_age <- ~ 1/(1 + exp(-mat.a*(age - a50)))
f_prop_m_age <-
  gadget3:::f_substitute(
    prop_m_age,
    list('a50' = bounded_param(female_imm,id=c('species','sex'), "mat.a50", model_params),
         'mat.a' = bounded_param(female_imm,id=c('species','sex'), "mat.a", model_params)))

m_prop_m_age <-
  gadget3:::f_substitute(
    prop_m_age,
    list('a50' = bounded_param(male_imm,id=c('species','sex'), "mat.a50", model_params),
         'mat.a' = bounded_param(male_imm,id=c('species','sex'), "mat.a", model_params)))

## mean length is estimated based on a Von B relationship used for immature and mature

mean_l <- ~Linf * (1 - exp(-1 * K * (age - (1 + log(1 - recl/Linf)/K))))
m_mean_l <-
  gadget3:::f_substitute(
    mean_l,
    list('recl' = bounded_param(male_imm,id=c('species','sex'), "recl", model_params),
         'Linf' = bounded_param(male_imm,id=c('species','sex'), "Linf", model_params),
         'K' = gadget3:::f_substitute(~0.001 * K,
                                      list(K = bounded_param(male_imm,id=c('species','sex'),
                                                             "K", model_params)))))

f_mean_l <-
  gadget3:::f_substitute(
    mean_l,
    list('recl' = bounded_param(female_imm,id=c('species','sex'), "recl", model_params),
         'Linf' = bounded_param(female_imm,id=c('species','sex'), "Linf", model_params),
         'K' = gadget3:::f_substitute(~0.001 * K,
                                      list(K = bounded_param(male_imm,id=c('species','sex'),
                                                             "K", model_params)))))


## setup stock actions

female_imm_actions <- list(

  g3a_initialconditions_normalparam(
    female_imm,
    # NB: area & age factor together (gadget2 just multiplied them)
    # initial abundance at age is 1e4 * q
    factor_f =
      gadget3:::f_substitute(
        ~init_abund * exp(-1 * (M + init.F) * (age - ghl_female_imm__minage)) * (1 - prop_m_age),
        c(model_params,
          init_abund = f_init_abund,
          prop_m_age = f_prop_m_age,
          init.F = bounded_param(female_imm,id=c('species','sex'), "init.F", model_params),
          M = ~g3_param_table("ghl_female.M",
                              data.frame(age = seq(ghl_female_imm__minage,
                                                   ghl_female_mat__maxage))))),
    mean_f = f_mean_l,
    stddev_f = bounded_table(female_imm, "init.sd", model_params),
    alpha_f = ~g3_param("female.walpha"),
    beta_f = ~g3_param("female.wbeta")),

  g3a_renewal_normalparam(
    female_imm,
    factor_f = f_init_abund,
    mean_f = f_mean_l,
    stddev_f = bounded_param(female_imm,id=c('species','sex'), "rec.sd", model_params),
    alpha_f = ~g3_param("female.walpha"),
    beta_f = ~g3_param("female.wbeta"),
    run_f = ~cur_step == 1 && age == 1 && cur_time > 0),

  g3a_growmature(
    female_imm,
    impl_f = g3a_grow_impl_bbinom(
      g3a_grow_lengthvbsimple(
        bounded_param(female_imm,id=c('species','sex'), "Linf", model_params),
        gadget3:::f_substitute(~0.001 * K,
                               list(
                                 K = bounded_param(female_imm,id=c('species','sex'), "K", model_params))
        )),
      g3a_grow_weightsimple(~g3_param("female.walpha"), ~g3_param("female.wbeta")),
      beta_f = bounded_param(female_imm,id=c('species','sex'), "bbin", model_params),
      maxlengthgroupgrowth = stock_params$maxlengthgroupgrowth),
    maturity_f = g3a_mature_continuous(
      alpha = gadget3:::f_substitute(~(0.001 * exp(mat1)),
                                     list(mat1 = bounded_param(female_imm,id=c('species','sex'), "mat1", model_params))),
      l50 = bounded_param(female_imm,id=c('species','sex'), "mat2", model_params)),
    output_stocks = list(female_mat)),

  g3a_naturalmortality(female_imm,
                       g3a_naturalmortality_exp(~g3_param_table("ghl_female.M",
                                                                data.frame(age = seq(ghl_female_imm__minage,
                                                                                     ghl_female_mat__maxage))))),
  g3a_age(female_imm,
          output_stocks = list(female_mat)),
  list())

male_imm_actions <- list(

  g3a_initialconditions_normalparam(
    male_imm,
    factor_f =
      gadget3:::f_substitute(
        ~init_abund * exp(-1 * (M + init.F) * (age - ghl_male_imm__minage)) * (1 - prop_m_age),
        c(model_params,
          init_abund = m_init_abund,
          prop_m_age = m_prop_m_age,
          init.F = bounded_param(male_imm,id=c('species','sex'), "init.F", model_params),
          M = ~g3_param_table("ghl_male.M",
                              data.frame(age = seq(ghl_male_imm__minage,
                                                   ghl_male_mat__maxage))))),
    mean_f = m_mean_l,
    stddev_f = bounded_table(male_imm, "init.sd", model_params),
    alpha_f = ~g3_param("male.walpha"),
    beta_f = ~g3_param("male.wbeta")),

  g3a_renewal_normalparam(male_imm,
                          factor_f = m_init_abund,
                          mean_f = m_mean_l,
                          stddev_f = bounded_param(male_imm,id=c('species','sex'), "rec.sd", model_params),
                          alpha_f = ~g3_param("male.walpha"),
                          beta_f = ~g3_param("male.wbeta"),
                          run_f = ~cur_step == 1 && age == 1 && cur_time > 0),

  g3a_growmature(
    male_imm,
    impl_f = g3a_grow_impl_bbinom(
      g3a_grow_lengthvbsimple(
        bounded_param(male_imm,id=c('species','sex'), "Linf", model_params),
        gadget3:::f_substitute(~0.001 * K,
                               list(
                                 K = bounded_param(male_imm,id=c('species','sex'), "K", model_params))
        )),
      g3a_grow_weightsimple(~g3_param("male.walpha"), ~g3_param("male.wbeta")),
      beta_f = bounded_param(male_imm,id=c('species','sex'), "bbin", model_params),
      maxlengthgroupgrowth = stock_params$maxlengthgroupgrowth),
    maturity_f = g3a_mature_continuous(
      alpha = gadget3:::f_substitute(~(0.001 * exp(mat1)),
                                     list(mat1 = bounded_param(male_imm,id=c('species','sex'), "mat1", model_params))),
      l50 = bounded_param(male_imm,id=c('species','sex'), "mat2", model_params)),
    output_stocks = list(male_mat)),

  g3a_naturalmortality(male_imm,
                       g3a_naturalmortality_exp(~g3_param_table("ghl_male.M",
                                                                data.frame(age = seq(ghl_male_imm__minage,
                                                                                     ghl_male_mat__maxage))))),

  g3a_age(male_imm,
          output_stocks = list(male_mat)),
  list())

## MATURE STOCKS
female_mat_actions <- list(
  g3a_initialconditions_normalparam(
    female_mat,
    # NB: area & age factor together (gadget2 just multiplied them)
    # initial abundance at age is 1e4 * q
    factor_f =
      gadget3:::f_substitute(
        ~init_abund * exp(-1 * (M + init.F) * (age - ghl_female_mat__minage)) * prop_m_age,
        c(model_params,
          init_abund = f_init_abund,
          prop_m_age = f_prop_m_age,
          init.F = bounded_param(female_mat,id=c('species','sex'), "init.F", model_params),
          M =~g3_param_table("ghl_female.M",
                             data.frame(age = seq(ghl_female_imm__minage,
                                                  ghl_female_mat__maxage))))),
    mean_f = f_mean_l,
    stddev_f = bounded_table(female_mat, "init.sd", model_params),
    alpha_f = ~g3_param("female.walpha"),
    beta_f = ~g3_param("female.wbeta")),

  g3a_growmature(female_mat,
                 impl_f = g3a_grow_impl_bbinom(
                   g3a_grow_lengthvbsimple(bounded_param(female_mat,id=c('species','sex'), "Linf", model_params),
                                           gadget3:::f_substitute(~0.001 * K,
                                                                  list(K = bounded_param(female_mat,id=c('species','sex'), "K", model_params)))),
                   g3a_grow_weightsimple(~g3_param("female.walpha"), ~g3_param("female.wbeta")),
                   beta_f = bounded_param(female_mat,id=c('species','sex'), "bbin", model_params),
                   maxlengthgroupgrowth = stock_params$maxlengthgroupgrowth)),

  g3a_naturalmortality(
    female_mat,
    g3a_naturalmortality_exp(~g3_param_table("ghl_female.M",
                                             data.frame(age = seq(ghl_female_imm__minage,
                                                                  ghl_female_mat__maxage))))),


  g3a_age(female_mat),
  list())


male_mat_actions <- list(
  g3a_initialconditions_normalparam(
    male_mat,
    # NB: area & age factor together (gadget2 just multiplied them)
    # initial abundance at age is 1e4 * q
    factor_f =
      gadget3:::f_substitute(
        ~init_abund * exp(-1 * (M + init.F) * (age - ghl_male_mat__minage)) * prop_m_age,
        c(model_params,
          init_abund = m_init_abund,
          prop_m_age = f_prop_m_age,
          init.F = bounded_param(male_mat,id=c('species','sex'), "init.F", model_params),
          M = ~g3_param_table("ghl_male.M",
                              data.frame(age = seq(ghl_male_imm__minage,
                                                   ghl_male_mat__maxage))))),
    mean_f = f_mean_l,
    stddev_f = bounded_table(male_mat, "init.sd", model_params),
    alpha_f = ~g3_param("male.walpha"),
    beta_f = ~g3_param("male.wbeta")),

  g3a_growmature(
    male_mat,
    impl_f = g3a_grow_impl_bbinom(
      g3a_grow_lengthvbsimple(
        bounded_param(male_mat,id=c('species','sex'), "Linf", model_params),
        gadget3:::f_substitute(~0.001 * K,
                               list(K = bounded_param(male_mat,id=c('species','sex'), "K", model_params)))),
      g3a_grow_weightsimple(~g3_param("male.walpha"), ~g3_param("male.wbeta")),
      beta_f = bounded_param(male_mat,id=c('species','sex'), "bbin", model_params),
      maxlengthgroupgrowth = stock_params$maxlengthgroupgrowth)),

  g3a_naturalmortality(male_mat,
                       g3a_naturalmortality_exp(~g3_param_table("ghl_male.M",
                                                                data.frame(age = seq(ghl_male_imm__minage,
                                                                                     ghl_male_mat__maxage))))),


  g3a_age(male_mat),
  list())


stock_actions <- c(male_imm_actions,male_mat_actions,female_imm_actions,female_mat_actions)
stocks <- list(female_imm,female_mat,male_imm,male_mat)


