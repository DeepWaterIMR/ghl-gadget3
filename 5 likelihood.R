## ---------------------------
##
## Script name: Set up Gadget likelihood files
##
## Purpose of script:
##
## Author: Mikko Vihtakari // Institute of Marine Research, Norway
## Email: mikko.vihtakari@hi.no
##
## Date Created: 2021-02-12
##
## ---------------------------

## Source the run first script

# source("0 run first.R")

## ---------------------------

## Load packages

## ---------------------------

## Source or list custom functions used within the script

## ---------------------------

## Read data

## ---------------------------

nll_breakdown <- TRUE  # Turn to TRUE to get per-step nll
lik_report <- TRUE

likelihood_actions <- list(
  g3l_understocking(stocks, nll_breakdown = nll_breakdown, weight = 1e6),

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

  g3l_catchdistribution(
    'EggaN_mat',
    EggaN_mat %>%
      mutate(area = 1) %>%
      rename(stock = maturity_stage) %>%
      mutate(stock = paste0("ghl_", stock)),
    fleets = list(EggaN),
    stocks = stocks,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),

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

## Old stuff ####
#
# ## Penalty functions
#
# lik <- gadgetlikelihood('likelihood',gd,missingOkay = TRUE) %>%
#   gadget_update("penalty",
#                 name = "bounds",
#                 weight = "0.5",
#                 data = data.frame(
#                   switch = c("default"),
#                   power = c(2),
#                   upperW=10000,
#                   lowerW=10000,
#                   stringsAsFactors = FALSE)) %>%
#   gadget_update("understocking",
#                 name = "understocking",
#                 weight = "100")
#
# ## Catch distribution
#
# lik <- lik %>%
#   gadget_update("catchdistribution",
#                 name = "TrawlNor.ldist",
#                 weight = 1,
#                 aggregationlevel = 1,
#                 data = TrawlNor.ldist,
#                 fleetnames = "TrawlNor",
#                 stocknames = model_params$stock_names) %>%
#   gadget_update("catchdistribution",
#                 name = "OtherNor.ldist",
#                 weight = 1,
#                 aggregationlevel = 1,
#                 data = OtherNor.ldist,
#                 fleetnames = "OtherNor",
#                 stocknames = model_params$stock_names) # %>%
#   # gadget_update("catchdistribution",
#   #               name = "TrawlRus.ldist",
#   #               weight = 1,
#   #               aggregationlevel = 1,
#   #               data = TrawlNor.ldist,
#   #               fleetnames = "TrawlRus",
#   #               stocknames = model_params$stock_names) %>%
#   # gadget_update("catchdistribution",
#   #               name = "OtherRus.ldist",
#   #               weight = 1,
#   #               aggregationlevel = 1,
#   #               data = OtherNor.ldist,
#   #               fleetnames = "OtherRus",
#   #               stocknames = model_params$stock_names) %>%
#   # gadget_update("catchdistribution",
#   #               name = "Internat.ldist",
#   #               weight = 1,
#   #               aggregationlevel = 1,
#   #               data = OtherNor.ldist,
#   #               fleetnames = "Internat",
#   #               stocknames = model_params$stock_names) %>%
#   # gadget_update("catchdistribution",
#   #               name = "NoSlope.ldist",
#   #               weight = 1,
#   #               aggregationlevel = 1,
#   #               data = NoSlope.ldist,
#   #               fleetnames = "NoSlope",
#   #               stocknames = model_params$stock_names)
#
# png(paste(gd, "Figures/TrawlNor_ldist.png", sep = "/"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
# print(plot.ldist(TrawlNor.ldist))
# dev.off()
#
# png(paste(gd, "Figures/OtherNor_ldist.png", sep = "/"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
# print(plot.ldist(OtherNor.ldist))
# dev.off()
#
# png(paste(gd, "Figures/NoSlope_ldist.png", sep = "/"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
# print(plot.ldist(NoSlope.ldist))
# dev.off()
#
# ## Stock distribution
#
# lik <- lik %>%
#   gadget_update("stockdistribution",
#               name = "NoSlope.mat",
#               weight = 1,
#               data = NoSlope.mat,
#               fleetnames = c("NoSlope"),
#               stocknames = model_params$stock_names)
#
# # Add plot here
#
# ## Survey indices
#
# lik <- lik %>%
#   gadget_update("surveyindices",
#                 name = "NoSlope.f",
#                 weight = 1,
#                 data = NoSlope.biomass.f %>%
#                   rename("number" = "total_weight"),
#                 sitype = "lengths",
#                 biomass = 1,
#                 fittype = 'fixedslopeloglinearfit',
#                 slope = 1,
#                 stocknames = model_params$female_stock) %>%
#   gadget_update("surveyindices",
#                 name = "NoSlope.m",
#                 weight = 1,
#                 data = NoSlope.biomass.m %>%
#                   rename("number" = "total_weight"),
#                 sitype = "lengths",
#                 biomass = 1,
#                 fittype = 'fixedslopeloglinearfit',
#                 slope = 1,
#                 stocknames = model_params$male_stock)
#
#
# ## Write the files
#
# lik %>% write.gadget.file(gd)
#
#
#
#   # gadget_update("surveyindices",
#   #               name = "NoSlope.f",
#   #               weight = 1,
#   #               data = NoSlope.biomass.f,
#   #               sitype = "lengths",
#   #               biomass = 1,
#   #               #fittype = 'loglinearfit',
#   #               fittype = 'fixedslopeloglinearfit',
#   #               slope=1,
#   #               stocknames = c("imm_f", "mat_f")) %>%
#   # gadget_update("surveyindices",
#   #               name = "si.20-25",
#   #               weight = 1,
#   #               data = igfs.SI2a[[1]],
#   #               fittype = 'loglinearfit',
#   #               #fittype = 'fixedslopeloglinearfit',
#   #               #slope=1,
#   #               stocknames = stock_names) %>%
#   # gadget_update("surveyindices",
#   #               name = "si.25-30",
#   #               weight = 1,
#   #               data = igfs.SI2b[[1]],
#   #               #fittype = 'loglinearfit',
#   #               fittype = 'fixedslopeloglinearfit',
#   #               slope=1,
#   #               stocknames = stock_names) %>%
#   # gadget_update("surveyindices",
#   #               name = "si.30-35",
#   #               weight = 1,
#   #               data = igfs.SI3a[[1]],
#   #               fittype = 'fixedslopeloglinearfit',
#   #               slope=1,
#   #               stocknames = stock_names) %>%
#   # gadget_update("surveyindices",
#   #               name = "si.35-40",
#   #               weight = 1,
#   #               data = igfs.SI3b[[1]],
#   #               fittype = 'fixedslopeloglinearfit',
#   #               slope=1,
#   #               stocknames = stock_names) %>%
#   # gadget_update("surveyindices",
#   #               name = "si.40-45",
#   #               weight = 1,
#   #               data = igfs.SI3c[[1]],
#   #               fittype = 'fixedslopeloglinearfit',
#   #               slope=1,
#   #               stocknames = stock_names) %>%
#   # gadget_update("surveyindices",
#   #               name = "si.45-60",
#   #               weight = 1,
#   #               data = igfs.SI3d[[1]],
#   #               fittype = 'fixedslopeloglinearfit',
#   #               slope=1,
#   #               stocknames = stock_names) %>%
#
#
