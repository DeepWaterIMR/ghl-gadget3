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
    'trawlnor_ldist',
    TrawlNor_ldist,
    fleets = list(TrawlNor),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'trawlrus_ldist_f',
    TrawlRus_ldist_female,
    fleets = list(TrawlRus),
    stocks = list(female_imm, female_mat),
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'trawlrus_ldist_m',
    TrawlRus_ldist_male,
    fleets = list(TrawlRus),
    stocks = list(male_imm, male_mat),
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'other_ldist',
    OtherNor_ldist,
    fleets = list(OtherNor, Internat),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'otherrus_ldist_f',
    OtherRus_ldist_female,
    fleets = list(OtherRus),
    stocks = list(female_imm, female_mat),
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'otherrus_ldist_m',
    OtherRus_ldist_male,
    fleets = list(OtherRus),
    stocks = list(male_imm, male_mat),
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

  g3l_catchdistribution(
    'RussianSurvey_ldist',
    RussianSurvey_ldist,
    fleets = list(RussianSurvey),
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
    g3l_distribution_sumofsquares(over = c('area','length','sex')),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'EggaN_aldist_male',
    EggaN_aldist_male,
    fleets = list(EggaN),
    stocks = list(male_imm, male_mat),
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(over = c('area','length','sex')),
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
    g3l_distribution_sumofsquares(over = c('area','length','sex')),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  ## Survey indices

  g3l_abundancedistribution(
    'EggaN_SI_female',
    if("total_weight" %in% colnames(EggaN_SI_female)) {
      EggaN_SI_female %>% rename("weight" = "total_weight")
    } else {
      EggaN_SI_female
    },
    fleets = list(),
    stocks = list(female_imm, female_mat),
    area_group = c(all = 1),
    g3l_distribution_surveyindices_log(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_abundancedistribution(
    'EggaN_SI_male',
    if("total_weight" %in% colnames(EggaN_SI_male)) {
      EggaN_SI_male %>% rename("weight" = "total_weight")
    } else {
      EggaN_SI_male
    },
    fleets = list(),
    stocks = list(male_imm, male_mat),
    area_group = c(all = 1),
    g3l_distribution_surveyindices_log(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_abundancedistribution(
    'Juv_SI_1',
    Juv_SI_1,
    fleets = list(),
    stocks = list(male_imm, female_imm),
    area_group = c(all = 1),
    g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_abundancedistribution(
    'Juv_SI_2',
    Juv_SI_2,
    fleets = list(),
    stocks = list(male_imm, female_imm),
    area_group = c(all = 1),
    g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_abundancedistribution(
    'Juv_SI_3',
    Juv_SI_3,
    fleets = list(),
    stocks = list(male_imm, female_imm),
    area_group = c(all = 1),
    g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_abundancedistribution(
    'Russian_SI',
    Russian_SI %>%
      rename("weight" = "total_weight"), # A bug here
    fleets = list(),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_surveyindices_log(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  list()

)
