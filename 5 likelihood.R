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
    TrawlNor_ldist,
    fleets = list(TrawlNor),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'other_ldist',
    OtherNor_ldist,
    fleets = list(OtherNor, OtherRus, Internat),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'EggaN_ldist',
    EggaN_ldist,
    fleets = list(EggaN),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  ## Length-age distributions

  g3l_catchdistribution(
    'EggaN_aldist_female',
    EggaN_aldist_female,
    fleets = list(EggaN),
    stocks = list(female_imm, female_mat),
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'EggaN_aldist_male',
    EggaN_aldist_male,
    fleets = list(EggaN),
    stocks = list(male_imm, male_mat),
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  ## Maturity proportions

  g3l_catchdistribution(
    'EggaN_matp',
    EggaN_mat %>%
      rename(stock = maturity_stage) %>%
      mutate(stock = paste0("ghl_", stock)),
    fleets = list(EggaN),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  ## Survey indices

  g3l_abundancedistribution(
    'EggaN_si_female',
    EggaN_biomass_female %>%
      rename("weight" = "total_weight"), # A bug here
    fleets = list(),
    stocks = list(female_imm, female_mat),
    area_group = c(all = 1),
    g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_abundancedistribution(
    'EggaN_si_male',
    EggaN_biomass_male %>%
      rename("weight" = "total_weight"), # A bug here
    fleets = list(),
    stocks = list(male_imm, male_mat),
    area_group = c(all = 1),
    g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  list()

)
