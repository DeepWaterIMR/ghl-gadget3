## ---------------------------
##
## Script name: Gadget fleets
##
## Purpose of script: Set up fleets
##
## ---------------------------
## Read data

if(!exists("TrawlNor_landings")) source("2-3 landings.R")

# load("data/out/Landings to Gadget.rda")

## Source functions

# source("R/figure_functions.R")

## ---------------------------

## Surveys

EggaN <- g3_fleet(c("EggaN", "survey")) %>%
  g3s_livesonareas(areas[c('1')])

## Commercial

TrawlNor <- g3_fleet(c("TrawlNor", "fishery")) %>%
  g3s_livesonareas(areas[c('1')])

OtherNor <- g3_fleet(c("OtherNor", "fishery")) %>%
  g3s_livesonareas(areas[c('1')])

if(nrow(HistNor_landings) > 0) {
  HistNor <- g3_fleet(c("HistNor", "fishery")) %>%
    g3s_livesonareas(areas[c('1')])
}

TrawlRus <- g3_fleet(c("TrawlRus", "fishery")) %>%
  g3s_livesonareas(areas[c('1')])

OtherRus <- g3_fleet(c("OtherRus", "fishery")) %>%
  g3s_livesonareas(areas[c('1')])

if(nrow(HistRus_landings) > 0) {
  HistRus <- g3_fleet(c("HistRus", "fishery")) %>%
    g3s_livesonareas(areas[c('1')])
}

Internat <- g3_fleet(c("Internat", "fishery")) %>%
  g3s_livesonareas(areas[c('1')])

## Create fleet actions

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
            #   p1 = g3_parameterized('trawl.p1', by_stock = 'species'),
            #   p2 = g3_parameterized('andersen.p2', by_stock = 'species'),
            #   p3 = g3_parameterized('trawl.p3', by_stock = 'species'),
            #   p4 = g3_parameterized('trawl.p4', by_stock = 'species'),
            #   p5 = g3_parameterized('andersen.L', by_stock = 'species')
            # )
            g3_suitability_exponentiall50(
              g3_parameterized('trawl.alpha', by_stock = 'species', exponentiate = TRUE),
              g3_parameterized('trawl.l50', by_stock = 'species', exponentiate = TRUE)
              )
          ),
        catchability_f =
          g3a_predate_catchability_totalfleet(
            g3_timeareadata('TrawlNor_landings',
                            TrawlNor_landings %>%
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
            # g3_suitability_andersen(
            #   g3_parameterized('andersen.p0', by_stock = 'species'),
            #   p1 = g3_parameterized('other.p1', by_stock = 'species'),
            #   p2 = g3_parameterized('andersen.p2', by_stock = 'species'),
            #   p3 = g3_parameterized('other.p3', by_stock = 'species'),
            #   p4 = g3_parameterized('other.p4', by_stock = 'species'),
            #   p5 = g3_parameterized('andersen.L', by_stock = 'species')
            # )
            g3_suitability_exponentiall50(
              g3_parameterized('other.alpha', by_stock = 'species', exponentiate = TRUE),
              g3_parameterized('other.l50', by_stock = 'species', exponentiate = TRUE)
              )
          ),
        catchability_f =
          g3a_predate_catchability_totalfleet(
            g3_timeareadata('OtherNor_landings',
                            OtherNor_landings %>%
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
            # g3_suitability_andersen(
            #   g3_parameterized('andersen.p0', by_stock = 'species'),
            #   p1 = g3_parameterized('trawl.p1', by_stock = 'species'),
            #   p2 = g3_parameterized('andersen.p2', by_stock = 'species'),
            #   p3 = g3_parameterized('trawl.p3', by_stock = 'species'),
            #   p4 = g3_parameterized('trawl.p4', by_stock = 'species'),
            #   p5 = g3_parameterized('andersen.L', by_stock = 'species')
            # )
            g3_suitability_exponentiall50(
              g3_parameterized('trawl.alpha', by_stock = 'species', exponentiate = TRUE),
              g3_parameterized('trawl.l50', by_stock = 'species', exponentiate = TRUE)
            )
          ),
        catchability_f =
          g3a_predate_catchability_totalfleet(
            g3_timeareadata('TrawlRus_landings',
                            TrawlRus_landings %>%
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
            # g3_suitability_andersen(
            #   g3_parameterized('andersen.p0', by_stock = 'species'),
            #   p1 = g3_parameterized('other.p1', by_stock = 'species'),
            #   p2 = g3_parameterized('andersen.p2', by_stock = 'species'),
            #   p3 = g3_parameterized('other.p3', by_stock = 'species'),
            #   p4 = g3_parameterized('other.p4', by_stock = 'species'),
            #   p5 = g3_parameterized('andersen.L', by_stock = 'species')
            # )
            g3_suitability_exponentiall50(
              g3_parameterized('other.alpha', by_stock = 'species', exponentiate = TRUE),
              g3_parameterized('other.l50', by_stock = 'species', exponentiate = TRUE)
            )
          ),
        catchability_f =
          g3a_predate_catchability_totalfleet(
            g3_timeareadata('OtherRus_landings',
                            OtherRus_landings %>%
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
            # g3_suitability_andersen(
            #   g3_parameterized('andersen.p0', by_stock = 'species'),
            #   p1 = g3_parameterized('other.p1', by_stock = 'species'),
            #   p2 = g3_parameterized('andersen.p2', by_stock = 'species'),
            #   p3 = g3_parameterized('other.p3', by_stock = 'species'),
            #   p4 = g3_parameterized('other.p4', by_stock = 'species'),
            #   p5 = g3_parameterized('andersen.L', by_stock = 'species')
            # )
            g3_suitability_exponentiall50(
              g3_parameterized('other.alpha', by_stock = 'species', exponentiate = TRUE),
              g3_parameterized('other.l50', by_stock = 'species', exponentiate = TRUE)
            )
          ),
        catchability_f =
          g3a_predate_catchability_totalfleet(
            g3_timeareadata('Internat_landings',
                            Internat_landings %>%
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
              g3_parameterized('survey.alpha', by_stock = 'species',
                               exponentiate = TRUE),
              g3_parameterized('survey.l50', by_stock = 'species', exponentiate = TRUE)
            )
          ),
        catchability_f =
          g3a_predate_catchability_totalfleet(
            g3_timeareadata('EggaN_landings',
                            EggaN_landings %>%
                              mutate(area = 1, # Check this hack out
                                     step = as.numeric(step),
                                     year = as.numeric(year))
            )
          )
      )
    # Add the historic fleets here when you'll need them
  )

