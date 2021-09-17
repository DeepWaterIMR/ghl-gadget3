############ Configure fleets ##################################################

## Survey(s)
aut_is <-
  g3_fleet(c(country = 'iceland','aut')) %>%
  g3s_livesonareas(areas[c('1')])

aut_gl <-
  g3_fleet(c(country = 'greenland','aut')) %>%
  g3s_livesonareas(areas[c('1')]) ## 

aut_fo <-
  g3_fleet(c(country = 'faroes','aut')) %>%
  g3s_livesonareas(areas[c('1')])

## Commercial
lln_is <-
  g3_fleet(c(country = 'iceland','lln')) %>%
  g3s_livesonareas(areas[c('1')])

bmt_is <-
  g3_fleet(c(country = 'iceland','bmt')) %>%
  g3s_livesonareas(areas[c('1')])

gil_is <-
  g3_fleet(c(country = 'iceland','gil')) %>%
  g3s_livesonareas(areas[c('1')])


lln_fo <-
  g3_fleet(c(country = 'faroes','lln')) %>%
  g3s_livesonareas(areas[c('1')])

bmt_fo <-
  g3_fleet(c(country = 'faroes','bmt')) %>%
  g3s_livesonareas(areas[c('1')])
# 
# gil_fo <-
#   g3_fleet(c(country = 'iceland','gil')) %>%
#   g3s_livesonareas(areas[c('1')])


lln_gl <-
  g3_fleet(c(country = 'greenland','lln')) %>%
  g3s_livesonareas(areas[c('1')])

bmt_gl <-
  g3_fleet(c(country = 'greenland','bmt')) %>%
  g3s_livesonareas(areas[c('1')])


## Collect catches by fleet:
lln_is_landings <- 
  mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('HLN','LLN'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))


bmt_is_landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('BMT','NPT','DSE','PSE','PGT','SHT'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))


gil_is_landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear='GIL',
  sampling_type = 'LND',
  species = defaults$species),
  defaults))


lln_gl_landings <- 
  mfdb_sample_totalweight(mdb, NULL, c(list(
    gear=c('HLN','LLN'),
    sampling_type = 'LOG',
    data_source = 'greenland_halibut_logbooks',
    species = defaults$species),
    within(defaults,{ area = mfdb_group(`1` = 1101)}))) ## only from Greenland


bmt_gl_landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('BMT','NPT','DSE','PSE','PGT','SHT'),
  sampling_type = 'LOG',
  data_source = 'greenland_halibut_logbooks',
  species = defaults$species),
  within(defaults,{ area = mfdb_group(`1` = 1101)}))) ## only from Greenland
  

lln_fo_landings <- 
  mfdb_sample_totalweight(mdb, NULL, c(list(
    gear=c('HLN','LLN'),
    sampling_type = 'LOG',
    data_source = 'greenland_halibut_logbooks',
    species = defaults$species),
    within(defaults,{ area = mfdb_group(`1` = c(1143,1142,1141,1151))}))) ## only from Faroes
    


bmt_fo_landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('BMT','NPT','DSE','PSE','PGT','SHT'),
  sampling_type = 'LOG',
  data_source = 'greenland_halibut_logbooks',
  species = defaults$species),
  within(defaults,{ area = mfdb_group(`1` = c(1143,1142,1141,1151))}))) ## only from Faroes



aut_is_landings <- 
  structure(data.frame(year=defaults$year,step=2,area=1,total_weight=1),
            area_group = mfdb_group(`1` = 1))


aut_gl_landings <- 
  structure(data.frame(year=defaults$year,step=2,area=1,total_weight=1),
            area_group = mfdb_group(`1` = 1))

aut_fo_landings <- 
  structure(data.frame(year=defaults$year,step=2,area=1,total_weight=1),
            area_group = mfdb_group(`1` = 1))




## Bounded parameters for fleet suitabilities
fleet_bounds <- list(
  
  'lln.l50' = list(lower = 40, upper = 100),
  'lln.alpha' = list(lower = 0.01, upper = 1),
  
  'bmt.l50' = list(lower = 40, upper = 100),
  'bmt.alpha' = list(lower = 0.01, upper = 1),
  
  'gil.l50' = list(lower = 40, upper = 100),
  'gil.alpha' = list(lower = 0.01, upper = 1),
  
  'aut.l50' = list(lower = 20, upper = 50),
  'aut.alpha' = list(lower = 0.01, upper = 1)
  
)

## create fleet actions
fleet_actions <-
  list(
    lln_is %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(bounded_param(x,id = 'species','lln.alpha',fleet_bounds),
                                                                        bounded_param(x,id = 'species','lln.l50',fleet_bounds))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('lln_is_landings', lln_is_landings[[1]] %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    bmt_is %>%
      g3a_predate_fleet(stocks,
                        stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(bounded_param(x,id = 'species','bmt.alpha',fleet_bounds),
                                                                        bounded_param(x,id = 'species','bmt.l50',fleet_bounds))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('bmt_is_landings', bmt_is_landings[[1]] %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    lln_gl %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(bounded_param(x,id = 'species','lln.alpha',fleet_bounds),
                                                                        bounded_param(x,id = 'species','lln.l50',fleet_bounds))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('lln_gl_landings', lln_gl_landings[[1]] %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    bmt_gl %>%
      g3a_predate_fleet(stocks,
                        stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(bounded_param(x,id = 'species','bmt.alpha',fleet_bounds),
                                                                        bounded_param(x,id = 'species','bmt.l50',fleet_bounds))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('bmt_gl_landings', bmt_gl_landings[[1]] %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    lln_fo %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(bounded_param(x,id = 'species','lln.alpha',fleet_bounds),
                                                                        bounded_param(x,id = 'species','lln.l50',fleet_bounds))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('lln_fo_landings', lln_fo_landings[[1]] %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    bmt_fo %>%
      g3a_predate_fleet(stocks,
                        stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(bounded_param(x,id = 'species','bmt.alpha',fleet_bounds),
                                                                        bounded_param(x,id = 'species','bmt.l50',fleet_bounds))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('bmt_fo_landings', bmt_fo_landings[[1]] %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    gil_is %>%
      g3a_predate_fleet(stocks,
                        stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(bounded_param(x,id = 'species','gil.alpha',fleet_bounds),
                                                                        bounded_param(x,id = 'species','gil.l50',fleet_bounds))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('gil_landings', gil_is_landings[[1]] %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    aut_is %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(bounded_param(x,id = 'species',param_name = 'aut.alpha',fleet_bounds),
                                                                        bounded_param(x,id = 'species',param_name = 'aut.l50',fleet_bounds))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('aut_is_landings', aut_is_landings %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    aut_gl %>%
      g3a_predate_fleet(stocks,
                        suitabilities = 
                          stocks %>% 
                          set_names(.,map(.,'name')) %>% 
                          map(function(x) g3_suitability_exponentiall50(bounded_param(x,id = 'species',param_name = 'aut.alpha',fleet_bounds),
                                                                        bounded_param(x,id = 'species',param_name = 'aut.l50',fleet_bounds))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('aut_gl_landings', aut_gl_landings %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year)))))
    )
