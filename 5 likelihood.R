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

  ## Length distributions ####

  g3l_catchdistribution(
    'TrawlNor_ldist',
    TrawlNor_ldist,
    fleets = list(TrawlNor),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  # g3l_catchdistribution(
  #   'TrawlNor_ldist_male',
  #   TrawlNor_ldist_male,
  #   fleets = list(TrawlNor),
  #   stocks = list(male_imm, male_mat),
  #   area_group = c(all = 1),
  #   g3l_distribution_sumofsquares(),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),
  #
  # g3l_catchdistribution(
  #   'TrawlNor_ldist_female',
  #   TrawlNor_ldist_female,
  #   fleets = list(TrawlNor),
  #   stocks = list(female_imm, female_mat),
  #   area_group = c(all = 1),
  #   g3l_distribution_sumofsquares(),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),

  g3l_catchdistribution(
    'TrawlRus_ldist',
    TrawlRus_ldist,
    fleets = list(TrawlRus),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  # g3l_catchdistribution(
  #   'TrawlRus_ldist_female',
  #   TrawlRus_ldist_female,
  #   fleets = list(TrawlRus),
  #   stocks = c(female_imm, female_mat),
  #   area_group = c(all = 1),
  #   g3l_distribution_sumofsquares(),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),
  #
  # g3l_catchdistribution(
  #   'TrawlRus_ldist_male',
  #   TrawlRus_ldist_male,
  #   fleets = list(TrawlRus),
  #   stocks = c(male_imm, male_mat),
  #   area_group = c(all = 1),
  #   g3l_distribution_sumofsquares(),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),

  g3l_catchdistribution(
    'OtherNor_ldist',
    OtherNor_ldist,
    fleets = list(OtherNor),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  # g3l_catchdistribution(
  #   'OtherRus_ldist',
  #   OtherRus_ldist,
  #   fleets = list(OtherRus),
  #   stocks = stocks,
  #   area_group = c(all = 1),
  #   g3l_distribution_sumofsquares(),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),

  g3l_catchdistribution(
    'EggaN_ldist',
    EggaN_ldist,
    fleets = list(EggaN),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  # g3l_catchdistribution(
  #   'EggaS_ldist',
  #   EggaS_ldist,
  #   fleets = list(EggaS),
  #   stocks = stocks,
  #   area_group = c(all = 1),
  #   g3l_distribution_sumofsquares(),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),

  g3l_catchdistribution(
    'EcoS_ldist',
    EcoS_ldist,
    fleets = list(EcoS),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'WinterS_ldist',
    WinterS_ldist,
    fleets = list(WinterS),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'RussianS_ldist',
    RussianS_ldist,
    fleets = list(RussianS),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  ## Length-age distributions ####

  g3l_catchdistribution(
    'OtherNor_aldist',
    OtherNor_aldist %>% rename(stock_re = sex),
    fleets = list(OtherNor),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(over = c('area', 'sex', 'length')),
    nll_breakdown = nll_breakdown,
    report = lik_report),

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

  # g3l_catchdistribution(
  #   'EggaS_aldist',
  #   EggaS_aldist %>% rename(stock_re = sex),
  #   fleets = list(EggaS),
  #   stocks = stocks,
  #   area_group = c(all = 1),
  #   g3l_distribution_sumofsquares(over = c('area', 'sex','length')),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),

  g3l_catchdistribution(
    'EcoS_aldist',
    EcoS_aldist %>% rename(stock_re = sex),
    fleets = list(EcoS),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(over = c('area', 'sex', 'length')),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  ## Sex distributions ####

  g3l_catchdistribution(
    'TrawlNor_sexdist',
    TrawlNor_sexratio %>% rename(stock_re = sex),
    fleets = list(TrawlNor),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(over = c('area', 'length')),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'OtherNor_sexdist',
    OtherNor_sexratio %>% rename(stock_re = sex),
    fleets = list(OtherNor),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(over = c('area', 'length')),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    'TrawlRus_sexdist',
    TrawlRus_sexratio %>% rename(stock_re = sex),
    fleets = list(TrawlRus),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(over = c('area', 'length')),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  # g3l_catchdistribution(
  #   'OtherRus_sexdist',
  #   OtherRus_sexratio %>% rename(stock_re = sex),
  #   fleets = list(OtherRus),
  #   stocks = stocks,
  #   area_group = c(all = 1),
  #   g3l_distribution_sumofsquares(over = c('area', 'length')),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),

  # g3l_catchdistribution(
  #   'EggaN_sexdist',
  #   EggaN_sexratio %>% rename(stock_re = sex),
  #   fleets = list(EggaN),
  #   stocks = stocks,
  #   area_group = c(all = 1),
  #   g3l_distribution_sumofsquares(over = c('area', 'length')),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),
  #
  # g3l_catchdistribution(
  #   'EggaS_sexdist',
  #   EggaS_sexratio %>% rename(stock_re = sex),
  #   fleets = list(EggaS),
  #   stocks = stocks,
  #   area_group = c(all = 1),
  #   g3l_distribution_sumofsquares(over = c('area', 'length')),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),

  g3l_catchdistribution(
    'EcoS_sexdist',
    EcoS_sexratio %>% rename(stock_re = sex),
    fleets = list(EcoS),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(over = c('area', 'length')),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  ## Maturity proportions ####

  g3l_catchdistribution(
    'EggaN_matp',
    EggaN_mat %>%
      rename(stock = maturity_stage) %>%
      mutate(stock = paste0("ghl_", stock)),
    fleets = list(EggaN),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_sumofsquares(over = c('area','length')),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  # g3l_catchdistribution(
  #   'EggaN_matp_females',
  #   EggaN_mat %>%
  #     filter(maturity_stage %in% c("female_imm", "female_mat")) %>%
  #     rename(stock = maturity_stage) %>%
  #     mutate(stock = paste0("ghl_", stock)),
  #   fleets = list(EggaN),
  #   stocks = list(female_imm, female_mat),
  #   area_group = c(all = 1),
  #   g3l_distribution_sumofsquares(over = c('area','length')),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),
  #
  # g3l_catchdistribution(
  #   'EggaN_matp_males',
  #   EggaN_mat %>%
  #     filter(maturity_stage %in% c("male_imm", "male_mat")) %>%
  #     rename(stock = maturity_stage) %>%
  #     mutate(stock = paste0("ghl_", stock)),
  #   fleets = list(EggaN),
  #   stocks = list(male_imm, male_mat),
  #   area_group = c(all = 1),
  #   g3l_distribution_sumofsquares(over = c('area','length')),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),

  # g3l_catchdistribution(
  #   'EggaS_matp',
  #   EggaS_mat %>%
  #     rename(stock = maturity_stage) %>%
  #     mutate(stock = paste0("ghl_", stock)),
  #   fleets = list(EggaS),
  #   stocks = stocks,
  #   area_group = c(all = 1),
  #   g3l_distribution_sumofsquares(over = c('area','length')),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),

  if(use_cheat_fleet) {
    g3l_catchdistribution(
      'Cheat_matp',
      Cheat_mat %>%
        rename(stock = maturity_stage) %>%
        mutate(stock = paste0("ghl_", stock)),
      fleets = list(Cheat),
      stocks = stocks,
      area_group = c(all = 1),
      g3l_distribution_sumofsquares(),
      nll_breakdown = nll_breakdown,
      report = lik_report)},

  ## Survey indices ####

  g3l_abundancedistribution(
    'EggaN_SI_female',
    if("total_weight" %in% colnames(EggaN_SI_female)) {
      EggaN_SI_female %>%
        dplyr::filter(year <= max(model_params$year_range) - model_params$peel) %>%
        rename("weight" = "total_weight")
    } else {
      EggaN_SI_female %>%
        dplyr::filter(year <= max(model_params$year_range) - model_params$peel)
    },
    fleets = list(),
    stocks = list(female_imm, female_mat),
    area_group = c(all = 1),
    g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_abundancedistribution(
    'EggaN_SI_male',
    if("total_weight" %in% colnames(EggaN_SI_male)) {
      EggaN_SI_male %>%
        dplyr::filter(year <= max(model_params$year_range) - model_params$peel) %>%
        rename("weight" = "total_weight")
    } else {
      EggaN_SI_male %>%
        dplyr::filter(year <= max(model_params$year_range) - model_params$peel)
    },
    fleets = list(),
    stocks = list(male_imm, male_mat),
    area_group = c(all = 1),
    g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  # g3l_abundancedistribution(
  #   'EggaN_SI',
  #   if("total_weight" %in% colnames(EggaN_SI)) {
  #     EggaN_SI %>%
  #       dplyr::filter(year <= max(model_params$year_range) - model_params$peel) %>%
  #       rename("weight" = "total_weight")
  #   } else {
  #     EggaN_SI %>%
  #       dplyr::filter(year <= max(model_params$year_range) - model_params$peel)
  #   },
  #   fleets = list(),
  #   stocks = stocks,
  #   area_group = c(all = 1),
  #   g3l_distribution_surveyindices_log(beta = 1),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),

  g3l_abundancedistribution(
    'EcoS_SI',
    EcoS_SI %>%
      dplyr::filter(year <= max(model_params$year_range) - model_params$peel) %>%
      rename("weight" = "total_weight"), # A bug here,
    fleets = list(),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  # Suitability tied to the SI
  # g3l_catchdistribution(
  #   'EcoS_SI',
  #   EcoS_SI %>%
  #     rename("weight" = "total_weight"), # A bug here,
  #   fleets = list(EcoS),
  #   stocks = stocks,
  #   area_group = c(all = 1),
  #   g3l_distribution_surveyindices_log(beta = 1),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),

  g3l_abundancedistribution(
    'Juv_SI_1',
    Juv_SI_1 %>%
      dplyr::filter(year <= max(model_params$year_range) - model_params$peel),
    fleets = list(),
    stocks = list(male_imm, female_imm),
    area_group = c(all = 1),
    g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_abundancedistribution(
    'Juv_SI_2',
    Juv_SI_2 %>%
      dplyr::filter(year <= max(model_params$year_range) - model_params$peel),
    fleets = list(),
    stocks = list(male_imm, female_imm),
    area_group = c(all = 1),
    g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  # g3l_abundancedistribution(
  #   'Juv_SI_3',
  #   Juv_SI_3,
  #   fleets = list(),
  #   stocks = list(male_imm, female_imm),
  #   area_group = c(all = 1),
  #   g3l_distribution_surveyindices_log(),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),

  # g3l_abundancedistribution(
  #   'WinterS_SI',
  #   WinterS_SI,
  #   fleets = list(),
  #   stocks = stocks,
  #   area_group = c(all = 1),
  #   g3l_distribution_surveyindices_log(beta = 1),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),

  g3l_abundancedistribution(
    'RussianS_SI',
    Russian_SI %>%
      dplyr::filter(year <= max(model_params$year_range) - model_params$peel) %>%
      rename("weight" = "total_weight"), # A bug here
    fleets = list(),
    stocks = stocks,
    area_group = c(all = 1),
    g3l_distribution_surveyindices_log(beta = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  # g3l_abundancedistribution(
  #   'Rus_CPUE_SI',
  #   Rus_CPUE_SI %>%
  #     dplyr::filter(year <= max(model_params$year_range) - model_params$peel),
  #   fleets = list(),
  #   stocks = stocks,
  #   g3l_distribution_surveyindices_log(beta = 1),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),

  list()

)
