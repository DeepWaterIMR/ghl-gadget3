## ---------------------------
##
## Script name: Gadget likelihood
##
## Purpose of script: Set up Gadget likelihood files
##
## ---------------------------

nll_breakdown <- TRUE  # Turn to TRUE to get per-step nll
lik_report <- TRUE

likelihood_actions <- list(
  g3l_understocking(stocks,
                    nll_breakdown = nll_breakdown,
                    weight = 1e6),

  ## Length distributions

  g3l_catchdistribution(
    'trawl_ldist',
    TrawlNor_ldist %>% mutate(area = 1), # This hack due to a bug in g3/mfdb
    fleets = list(TrawlNor, TrawlRus),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'other_ldist',
    TrawlNor_ldist %>% mutate(area = 1), # This hack due to a bug in g3/mfdb
    fleets = list(OtherNor, OtherRus, Internat),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'EggaN_ldist',
    EggaN_ldist %>% mutate(area = 1), # This hack due to a bug in g3/mfdb
    fleets = list(EggaN),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  ## Length-age distributions

  g3l_catchdistribution(
    'EggaN_aldist',
    EggaN_aldist_female %>% mutate(area = 1), # This hack due to a bug in g3/mfdb
    fleets = list(EggaN),
    stocks = list(female_imm, female_mat),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'EggaN_aldist',
    EggaN_aldist_male %>% mutate(area = 1), # This hack due to a bug in g3/mfdb
    fleets = list(EggaN),
    stocks = list(male_imm, male_mat),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  ## Maturity proportions

  # g3l_catchdistribution(
  #   'EggaN_mat',
  #   EggaN_mat %>%
  #     mutate(area = 1) %>%
  #     rename(stock = maturity_stage) %>%
  #     mutate(stock = paste0("ghl_", stock)),
  #   fleets = list(EggaN),
  #   stocks = stocks,
  #   g3l_distribution_sumofsquares(),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),
  # Error in if (all(diff(ag) == 1)) { :
  # missing value where TRUE/FALSE needed

  ## Survey indices

  g3l_abundancedistribution(
    'EggaN_si_female',
    EggaN_biomass_female %>%
      mutate(area = 1) %>% # This hack due to a bug in g3/mfdb
      rename("weight" = "total_weight"), # A bug here too
    fleets = list(),
    stocks = list(female_imm, female_mat),
    g3l_distribution_surveyindices_log(
      alpha = g3_stock_param(stocks[[1]],id ='species', "si_alpha_female"),
      beta = g3_stock_param(stocks[[1]],id ='species', "si_beta_female")),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_abundancedistribution(
    'EggaN_si_male',
    EggaN_biomass_male %>%
      mutate(area = 1) %>% # This hack due to a bug in g3/mfdb
      rename("weight" = "total_weight"), # A bug here too
    fleets = list(),
    stocks = list(male_imm, male_mat),
    g3l_distribution_surveyindices_log(
      alpha = g3_stock_param(stocks[[1]],id ='species', "si_alpha_male"),
      beta = g3_stock_param(stocks[[1]],id ='species', "si_beta_male")),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  list()

)
