##
## Setup likelihood 
##
## -----------------------------------------------------------------------------

## weird inconsistencies in Gadget

nll_breakdown <- TRUE  # Turn to TRUE to get per-step nll
lik_report <- TRUE

likelihood_actions <- list(
  g3l_understocking(stocks, nll_breakdown = nll_breakdown, weight = 1e6),
  
  g3l_catchdistribution(
    'ldist_lln_is',
    ldist.lln.is[[1]] ,
    fleets = list(lln_is),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_catchdistribution(
    'ldist_lln_gl',
    ldist.lln.gl[[1]] ,
    fleets = list(lln_is),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_catchdistribution(
    'ldist_bmt_is',
    (ldist.bmt.is[[1]]),
    fleets = list(bmt_is),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,  
    report = lik_report),
  
  g3l_catchdistribution(
    'aldist_bmt_is',
    (aldist.bmt.is[[1]]),
    fleets = list(bmt_is),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_catchdistribution(
    'ldist_bmt_gl',
    (ldist.bmt.gl[[1]]),
    fleets = list(bmt_is),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,  
    report = lik_report),
  
  
  g3l_catchdistribution(
    'ldist_gil_is',
    (ldist.gil.is[[1]]),
    fleets = list(gil_is),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,    
    report = lik_report),
  
  g3l_catchdistribution(
    'ldist_aut_is',
    (ldist.aut.is[[1]]),
    fleets = list(aut_is),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_catchdistribution(
    'aldist_aut_is',
    (aldist.aut.is[[1]]),
    fleets = list(aut_is),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,  
    report = lik_report),
  
  g3l_catchdistribution(
    'matp_aut_is',
    (matp.aut.is %>%
       rename(stock = maturity_stage) %>%
       mutate(stock = recode(as.factor(stock), 
                             femimm = "ghl_female_imm", 
                             malimm = "ghl_male_imm", 
                             malmat = "ghl_male_mat",
                             femmat = "ghl_female_mat"))),
    fleets = list(aut_is),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,     
    report = lik_report),
  
  g3l_abundancedistribution(
    'si_aut_si1_is',
    (aut.SI1[[1]]),
    fleets = list(),
    stocks = stocks,
    g3l_distribution_surveyindices_log(alpha = g3_stock_param(stocks[[1]],id ='species', "si_alpha1"),
                                       beta = g3_stock_param(stocks[[1]],id ='species', "si_beta1")),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_abundancedistribution(
    'si_aut_si2_is',
    (aut.SI2[[1]]),
    fleets = list(),
    stocks = stocks,
    g3l_distribution_surveyindices_log(alpha = g3_stock_param(stocks[[1]],id ='species', "si_alpha2"),
                                       beta = g3_stock_param(stocks[[1]],id ='species', "si_beta2")),
    nll_breakdown = nll_breakdown,     
    report = lik_report),
  
  g3l_abundancedistribution(
    'si_aut_si3',
    (aut.SI3[[1]]),
    fleets = list(),
    stocks = stocks,
    g3l_distribution_surveyindices_log(alpha = g3_stock_param(stocks[[1]],id ='species', "si_alpha3"),
                                       beta = 1),
    nll_breakdown = nll_breakdown,     
    report = lik_report),
  
  g3l_abundancedistribution(
    'si_aut_si4',
    (aut.SI4[[1]]),
    fleets = list(),
    stocks = stocks,
    g3l_distribution_surveyindices_log(alpha = g3_stock_param(stocks[[1]],id ='species', "si_alpha4"),
                                       beta = 1),
    nll_breakdown = nll_breakdown,     
    report = lik_report),
  
  g3l_abundancedistribution(
    'si_aut_si5',
    (aut.SI5[[1]]),
    fleets = list(),
    stocks = stocks,
    g3l_distribution_surveyindices_log(alpha = g3_stock_param(stocks[[1]],id ='species', "si_alpha5"),
                                       beta = 1),
    nll_breakdown = nll_breakdown,    
    report = lik_report),
  
  g3l_abundancedistribution(
    'si_aut_6',
    (aut.SI6[[1]]),
    fleets = list(),
    stocks = stocks,
    g3l_distribution_surveyindices_log(alpha = g3_stock_param(stocks[[1]],id ='species', "si_alpha6"),
                                       beta = 1),
    nll_breakdown = nll_breakdown,     
    report = lik_report),
  
  g3l_abundancedistribution(
    'si_aut_si7',
    (aut.SI7[[1]]),
    fleets = list(),
    stocks = stocks,
    g3l_distribution_surveyindices_log(alpha = g3_stock_param(stocks[[1]],id ='species', "si_alpha7"),
                                       beta = 1),
    nll_breakdown = nll_breakdown,     
    report = lik_report),
  
  list()
)
