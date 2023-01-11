## ---------------------------
##
## Script name: Gadget fleets
##
## Purpose of script: Set up fleets
##
## ---------------------------
## Read data

if(!exists("TrawlNor_catches")) source("2-3 landings.R")

# load("data/out/Landings to Gadget.rda")

## Source functions

# source("R/figure_functions.R")

## ---------------------------

## Surveys

EggaN <- g3_fleet(c("EggaN", "survey")) %>%
  g3s_livesonareas(areas[c('1')])

EggaS <- g3_fleet(c("EggaS", "survey")) %>%
  g3s_livesonareas(areas[c('1')])

EcoS <- g3_fleet(c("EcoS", "survey")) %>%
  g3s_livesonareas(areas[c('1')])

RussianSurvey <- g3_fleet(c("RussianSurvey", "survey")) %>%
  g3s_livesonareas(areas[c('1')])

## Commercial

TrawlNor <- g3_fleet(c("TrawlNor", "fishery")) %>%
  g3s_livesonareas(areas[c('1')])

OtherNor <- g3_fleet(c("OtherNor", "fishery")) %>%
  g3s_livesonareas(areas[c('1')])

# if(nrow(HistNor_catches) > 0) {
#   HistNor <- g3_fleet(c("HistNor", "fishery")) %>%
#     g3s_livesonareas(areas[c('1')])
# }

TrawlRus <- g3_fleet(c("TrawlRus", "fishery")) %>%
  g3s_livesonareas(areas[c('1')])

OtherRus <- g3_fleet(c("OtherRus", "fishery")) %>%
  g3s_livesonareas(areas[c('1')])

# if(nrow(HistRus_catches) > 0) {
#   HistRus <- g3_fleet(c("HistRus", "fishery")) %>%
#     g3s_livesonareas(areas[c('1')])
# }

Internat <- g3_fleet(c("Internat", "fishery")) %>%
  g3s_livesonareas(areas[c('1')])

## Create fleet actions

exponentiate_fleets <- FALSE

fleet_actions <-
  list(
    TrawlNor %>%
      g3a_predate_fleet(
        stocks,
        suitabilities =
          stocks %>%
          set_names(.,map(.,'name')) %>%
          map(function(x)
            # g3_suitability_andersen(
            #   g3_parameterized('andersen.p0', by_stock = 'species'),
            #   p1 = g3_parameterized('trawlnor.p1', by_stock = 'species'),
            #   p2 = g3_parameterized('andersen.p2', by_stock = 'species'),
            #   p3 = g3_parameterized('trawlnor.p3', by_stock = 'species'),
            #   p4 = g3_parameterized('trawlnor.p4', by_stock = 'species'),
            #   p5 = g3_parameterized('andersen.L', by_stock = 'species')
            # )
            g3_suitability_exponentiall50(
              g3_parameterized('trawlnor.alpha', by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets),
              g3_parameterized('trawlnor.l50', by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets)
              )
          ),
        catchability_f =
          g3a_predate_catchability_totalfleet(
            g3_timeareadata('TrawlNor_catches',
                            TrawlNor_catches %>%
                              mutate(area = 1, # Check this hack out
                                     step = as.numeric(step),
                                     year = as.numeric(year))
            )
          )
      ),
    OtherNor %>%
      g3a_predate_fleet(
        stocks,
        suitabilities =
          stocks %>%
          set_names(.,map(.,'name')) %>%
          map(function(x)
            g3_suitability_andersen(
              g3_parameterized('andersen.p0', by_stock = c('species', 'sex')),
              p1 = g3_parameterized('other.p1', by_stock = c('species', 'sex')),
              p2 = g3_parameterized('andersen.p2', by_stock = c('species', 'sex')),
              p3 = g3_parameterized('other.p3', by_stock = c('species', 'sex')),
              p4 = g3_parameterized('other.p4', by_stock = c('species', 'sex')),
              p5 = g3_parameterized('andersen.L', by_stock = c('species', 'sex'))
            )
            # g3_suitability_exponentiall50(
            #   g3_parameterized('other.alpha', by_stock = 'species',
            #                    exponentiate = exponentiate_fleets),
            #   g3_parameterized('other.l50', by_stock = 'species',
            #                    exponentiate = exponentiate_fleets)
            #   )
          ),
        catchability_f =
          g3a_predate_catchability_totalfleet(
            g3_timeareadata('OtherNor_catches',
                            OtherNor_catches %>%
                              mutate(area = 1, # Check this hack out
                                     step = as.numeric(step),
                                     year = as.numeric(year))
            )
          )
      ),
    TrawlRus %>%
      g3a_predate_fleet(
        stocks,
        suitabilities =
          stocks %>%
          set_names(.,map(.,'name')) %>%
          map(function(x)
          #   g3_suitability_andersen(
          #     g3_parameterized('andersen.p0', by_stock = 'species'),
          #     p1 = g3_parameterized('trawlrus.p1', by_stock = 'species'),
          #     p2 = g3_parameterized('andersen.p2', by_stock = 'species'),
          #     p3 = g3_parameterized('trawlrus.p3', by_stock = 'species'),
          #     p4 = g3_parameterized('trawlrus.p4', by_stock = 'species'),
          #     p5 = g3_parameterized('andersen.L', by_stock = 'species')
          #   )
            g3_suitability_exponentiall50(
              g3_parameterized('trawlrus.alpha', by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets),
              g3_parameterized('trawlrus.l50', by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets)
            )
          ),
        catchability_f =
          g3a_predate_catchability_totalfleet(
            g3_timeareadata('TrawlRus_catches',
                            TrawlRus_catches %>%
                              mutate(area = 1, # Check this hack out
                                     step = as.numeric(step),
                                     year = as.numeric(year))
            )
          )
      ),
    OtherRus %>%
      g3a_predate_fleet(
        stocks,
        suitabilities =
          stocks %>%
          set_names(.,map(.,'name')) %>%
          map(function(x)
            g3_suitability_andersen(
              g3_parameterized('andersen.p0', by_stock = c('species', 'sex')),
              p1 = g3_parameterized('otherrus.p1', by_stock = c('species', 'sex')),
              p2 = g3_parameterized('andersen.p2', by_stock = c('species', 'sex')),
              p3 = g3_parameterized('otherrus.p3', by_stock = c('species', 'sex')),
              p4 = g3_parameterized('otherrus.p4', by_stock = c('species', 'sex')),
              p5 = g3_parameterized('andersen.L', by_stock = c('species', 'sex'))
            )
            # g3_suitability_exponentiall50(
            #   g3_parameterized('other.alpha', by_stock = 'species',
            #                    exponentiate = exponentiate_fleets),
            #   g3_parameterized('other.l50', by_stock = 'species',
            #                    exponentiate = exponentiate_fleets)
            # )
          ),
        catchability_f =
          g3a_predate_catchability_totalfleet(
            g3_timeareadata('OtherRus_catches',
                            OtherRus_catches %>%
                              mutate(area = 1, # Check this hack out
                                     step = as.numeric(step),
                                     year = as.numeric(year))
            )
          )
      ),
    Internat %>%
      g3a_predate_fleet(
        stocks,
        suitabilities =
          stocks %>%
          set_names(.,map(.,'name')) %>%
          map(function(x)
            g3_suitability_andersen(
              g3_parameterized('andersen.p0', by_stock = c('species', 'sex')),
              p1 = g3_parameterized('other.p1', by_stock = c('species', 'sex')),
              p2 = g3_parameterized('andersen.p2', by_stock = c('species', 'sex')),
              p3 = g3_parameterized('other.p3', by_stock = c('species', 'sex')),
              p4 = g3_parameterized('other.p4', by_stock = c('species', 'sex')),
              p5 = g3_parameterized('andersen.L', by_stock = c('species', 'sex'))
            )
            # g3_suitability_exponentiall50(
            #   g3_parameterized('other.alpha', by_stock = 'species',
            #                    exponentiate = exponentiate_fleets),
            #   g3_parameterized('other.l50', by_stock = 'species',
            #                    exponentiate = exponentiate_fleets)
            # )
          ),
        catchability_f =
          g3a_predate_catchability_totalfleet(
            g3_timeareadata('Internat_catches',
                            Internat_catches %>%
                              mutate(area = 1, # Check this hack out
                                     step = as.numeric(step),
                                     year = as.numeric(year))
            )
          )
      ),
    EggaN %>%
      g3a_predate_fleet(
        stocks,
        suitabilities =
          stocks %>%
          set_names(.,map(.,'name')) %>%
          map(function(x)
            g3_suitability_exponentiall50(
              g3_parameterized('eggan.survey.alpha', by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets),
              g3_parameterized('eggan.survey.l50', by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets)
            )
          ),
        catchability_f =
          g3a_predate_catchability_totalfleet(
            g3_timeareadata('EggaN_catches',
                            EggaN_catches %>%
                              mutate(area = 1, # Check this hack out
                                     step = as.numeric(step),
                                     year = as.numeric(year))
            )
          )
      ),
    EggaS %>%
      g3a_predate_fleet(
        stocks,
        suitabilities =
          stocks %>%
          set_names(.,map(.,'name')) %>%
          map(function(x)
            g3_suitability_exponentiall50(
              g3_parameterized('eggan.survey.alpha', by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets),
              g3_parameterized('eggan.survey.l50', by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets)
            )
          ),
        catchability_f =
          g3a_predate_catchability_totalfleet(
            g3_timeareadata('EggaS_catches',
                            EggaS_catches %>%
                              mutate(area = 1, # Check this hack out
                                     step = as.numeric(step),
                                     year = as.numeric(year))
            )
          )
      ),
    EcoS %>%
      g3a_predate_fleet(
        stocks,
        suitabilities =
          stocks %>%
          set_names(.,map(.,'name')) %>%
          map(function(x)
            g3_suitability_exponentiall50(
              g3_parameterized('eco.survey.alpha', by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets),
              g3_parameterized('eco.survey.l50', by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets)
            )
          ),
        catchability_f =
          g3a_predate_catchability_totalfleet(
            g3_timeareadata('EcoS_catches',
                            EcoS_catches %>%
                              mutate(area = 1, # Check this hack out
                                     step = as.numeric(step),
                                     year = as.numeric(year))
            )
          )
      ),
    RussianSurvey %>%
      g3a_predate_fleet(
        stocks,
        suitabilities =
          stocks %>%
          set_names(.,map(.,'name')) %>%
          map(function(x)
            g3_suitability_exponentiall50(
              g3_parameterized('rus.survey.alpha', by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets),
              g3_parameterized('rus.survey.l50', by_stock = c('species', 'sex'),
                               exponentiate = exponentiate_fleets)
            )
          ),
        catchability_f =
          g3a_predate_catchability_totalfleet(
            g3_timeareadata('RussianSurvey_catches',
                            RussianSurvey_catches %>%
                              mutate(area = 1, # Check this hack out
                                     step = as.numeric(step),
                                     year = as.numeric(year))
            )
          )
      )
  )

