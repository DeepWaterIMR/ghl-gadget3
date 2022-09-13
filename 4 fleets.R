## ---------------------------
##
## Script name: Set up fleets
##
## Purpose of script:
##
## Author: Mikko Vihtakari // Institute of Marine Research, Norway
## Email: mikko.vihtakari@hi.no
##
## Date Created: 2021-02-12
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


## Bounded parameters for fleet suitabilities
# fleet_bounds <- list(
#
#   'trawl.l50' = list(lower = 40, upper = 100),
#   'trawl.alpha' = list(lower = 0.01, upper = 1),
#
#   'other.l50' = list(lower = 40, upper = 100),
#   'other.alpha' = list(lower = 0.01, upper = 1),
#
#   'survey.l50' = list(lower = 40, upper = 100),
#   'survey.alpha' = list(lower = 0.01, upper = 1)
#
# )

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
            g3_suitability_exponentiall50(
              g3_stock_param(x, id = 'species', 'trawl.alpha',
                             setup_options$bound_params),
              g3_stock_param(x, id = 'species', 'trawl.l50',
                             setup_options$bound_params)
              # bounded_param(x, id = 'species','trawl.alpha',fleet_bounds),
              # bounded_param(x, id = 'species','trawl.l50',fleet_bounds)
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
            g3_suitability_exponentiall50(
              g3_stock_param(x, id = 'species', 'other.alpha',
                             setup_options$bound_params),
              g3_stock_param(x, id = 'species', 'other.l50',
                             setup_options$bound_params)
              # bounded_param(x, id = 'species','other.alpha',fleet_bounds),
              # bounded_param(x, id = 'species','other.l50',fleet_bounds)
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
            g3_suitability_exponentiall50(
              g3_stock_param(x, id = 'species', 'trawl.alpha',
                             setup_options$bound_params),
              g3_stock_param(x, id = 'species', 'trawl.l50',
                             setup_options$bound_params)
              # bounded_param(x, id = 'species','trawl.alpha',fleet_bounds),
              # bounded_param(x, id = 'species','trawl.l50',fleet_bounds)
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
            g3_suitability_exponentiall50(
              g3_stock_param(x, id = 'species', 'other.alpha',
                             setup_options$bound_params),
              g3_stock_param(x, id = 'species', 'other.l50',
                             setup_options$bound_params)
              # bounded_param(x, id = 'species','other.alpha',fleet_bounds),
              # bounded_param(x, id = 'species','other.l50',fleet_bounds)
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
            g3_suitability_exponentiall50(
              g3_stock_param(x, id = 'species', 'other.alpha',
                             setup_options$bound_params),
              g3_stock_param(x, id = 'species', 'other.l50',
                             setup_options$bound_params)
              # bounded_param(x, id = 'species','other.alpha',fleet_bounds),
              # bounded_param(x, id = 'species','other.l50',fleet_bounds)
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
              g3_stock_param(x, id = 'species', 'survey.alpha',
                             setup_options$bound_params),
              g3_stock_param(x, id = 'species', 'survey.l50',
                             setup_options$bound_params)
              # bounded_param(x, id = 'species','survey.alpha',fleet_bounds),
              # bounded_param(x, id = 'species','survey.l50',fleet_bounds)
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


## Old stuff under ####

# Write the model files
#
# fleets <- gadgetfleet('Modelfiles/fleet', gd, missingOkay = TRUE)
#
# ## TrawlNor (Norwegian trawl and seine fleet) ####
# # Note: the selectivity has not been thought through
#
# fig.file.path <- paste(gd, "Figures/TrawlNor_landings.png", sep = "/")
#
# if(nrow(TrawlNor) > 0) {
#   fleets <- fleets %>% gadget_update(
#     component = 'totalfleet',
#     name = 'TrawlNor',
#     suitability =
#       paste0('\n',
#              paste(c('imm_f','imm_m', "mat_f", "mat_m"),
#                    'function','andersenfleet',
#                    0, #p0
#                    to.gadget.formulae(quote(log(180/trawl.p1.lmode))), #p1
#                    1, #p2
#                    '#trawl.p3', #p3
#                    '#trawl.p4',
#                    stock_params$maxlength, #L
#                    collapse='\n')),
#     data = TrawlNor)
#
#   png(fig.file.path, width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
#   print(plot.landings(TrawlNor))
#   dev.off()
# }
#
#
#
# ## OtherNor (Norwegian longline and gillnet fleet) ####
# # Note: the selectivity has not been thought through
#
# fig.file.path <- paste(gd, "Figures/OtherNor_landings.png", sep = "/")
#
# if(nrow(OtherNor) > 0) {
#   fleets <- fleets %>% gadget_update(
#     'totalfleet',
#     name = 'OtherNor',
#     suitability =
#       paste0('\n',
#              paste(c("imm_f", "imm_m", "mat_f", "mat_m"),
#                    'function','exponentiall50',
#                    '#other.alpha','#other.l50',
#                    collapse='\n')),
#     data = OtherNor)
#
#   png(fig.file.path, width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
#   print(plot.landings(OtherNor))
#   dev.off()
# }
#
# ## TrawlRus (Russian trawl fleet) ####
# # Note: the selectivity has not been thought through
#
# fig.file.path <- paste(gd, "Figures/TrawlRus_landings.png", sep = "/")
#
# if(nrow(TrawlRus) > 0) {
#   fleets <- fleets %>% gadget_update(
#     'totalfleet',
#     name = 'TrawlRus',
#     suitability =
#       paste0('\n',
#              paste(c('imm_f','imm_m', "mat_f", "mat_m"),
#                    'function','andersenfleet',
#                    0, #p0
#                    to.gadget.formulae(quote(log(180/trawl.p1.lmode))), #p1
#                    1, #p2
#                    '#trawl.p3', #p3
#                    '#trawl.p4',
#                    stock_params$maxlength, #L
#                    collapse='\n')),
#     data = TrawlRus)
#
#   png(fig.file.path, width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
#   print(plot.landings(TrawlRus))
#   dev.off()
# }
#
# ## OtherRus (Russian other than trawl fleet) ####
# # Note: the selectivity has not been thought through
#
# fig.file.path <- paste(gd, "Figures/OtherRus_landings.png", sep = "/")
#
# if(nrow(OtherRus) > 0) {
#   fleets <- fleets %>% gadget_update(
#     'totalfleet',
#     name = 'OtherRus',
#     suitability =
#       paste0('\n',
#              paste(c('imm_f','imm_m', "mat_f", "mat_m"),
#                    'function','exponentiall50',
#                    '#other.alpha','#other.l50',
#                    collapse='\n')),
#     data = OtherRus)
#
#   png(fig.file.path, width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
#   print(plot.landings(OtherRus))
#   dev.off()
# }
#
# ## HistNor (Norwegian historic landings) ####
# # Note: the selectivity has not been thought through
#
# fig.file.path <- paste(gd, "Figures/HistNor_landings.png", sep = "/")
#
# if(nrow(HistNor) > 0) {
#   fleets <- fleets %>% gadget_update(
#     'totalfleet',
#     name = 'HistNor',
#     suitability =
#       paste0('\n',
#              paste(c('imm_f','imm_m', "mat_f", "mat_m"),
#                    'function','exponentiall50',
#                    '#other.alpha','#other.l50',
#                    collapse='\n')),
#     data = HistNor)
#
#   png(fig.file.path, width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
#   print(plot.landings(HistNor))
#   dev.off()
# } else if(file.exists(fig.file.path)){
#   file.remove(fig.file.path)
# }
#
# ## HistRus (Russian historic landings) ####
# # Note: the selectivity has not been thought through
#
# fig.file.path <- paste(gd, "Figures/HistRus_landings.png", sep = "/")
#
# if(nrow(HistRus) > 0) {
#   fleets <- fleets %>% gadget_update(
#     'totalfleet',
#     name = 'HistRus',
#     suitability =
#       paste0('\n',
#              paste(c('imm_f','imm_m', "mat_f", "mat_m"),
#                    'function','exponentiall50',
#                    '#other.alpha','#other.l50',
#                    collapse='\n')),
#     data = HistRus)
#
#   png(fig.file.path, width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
#   print(plot.landings(HistRus))
#   dev.off()
# } else if(file.exists(fig.file.path)){
#   file.remove(fig.file.path)
# }
#
# ## HistInt (International landings) ####
# # Note: the selectivity has not been thought through
#
# fig.file.path <- paste(gd, "Figures/Internat_landings.png", sep = "/")
#
# if(nrow(HistInt) > 0) {
#   fleets <- fleets %>% gadget_update(
#     'totalfleet',
#     name = 'Internat',
#     suitability =
#       paste0('\n',
#              paste(c('imm_f','imm_m', "mat_f", "mat_m"),
#                    'function','exponentiall50',
#                    '#other.alpha','#other.l50',
#                    collapse='\n')),
#     data = HistInt)
#
#   png(fig.file.path, width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
#   print(plot.landings(HistInt))
#   dev.off()
# } else if(file.exists(fig.file.path)){
#   file.remove(fig.file.path)
# }
#
# ## NoSlope (EggaN survey index dummy landings) ####
# # Note: the selectivity has not been thought through
#
# fig.file.path <- paste(gd, "Figures/NoSlope_landings.png", sep = "/")
#
# if(nrow(NoSlope) > 0) {
#   fleets <- fleets %>% gadget_update(
#     'totalfleet',
#     name = 'NoSlope',
#     suitability =
#       paste0('\n',
#              paste(c('imm_f','imm_m', "mat_f", "mat_m"),
#                    'function','andersenfleet',
#                    0, #p0
#                    to.gadget.formulae(quote(log(180/noslope.p1.lmode))), #p1
#                    1, #p2
#                    '#noslope.p3', #p3
#                    '#noslope.p4',
#                    stock_params$maxlength, #L
#                    collapse='\n')),
#     data = NoSlope)
#
#   png(fig.file.path, width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
#   print(plot.landings(NoSlope))
#   dev.off()
# } else if(file.exists(fig.file.path)){
#   file.remove(fig.file.path)
# }
#
#
# # Write to files
#
# fleets %>% write.gadget.file(gd)
#
# rm(fleets)
#
# ## Model parameters
# # - #gil.alpha = the alpha parameter for the ExponentialL50 Suitability Function of gill net fleets. Assumed same among fleets.
# # - #gil.l50 = the l50 parameter for the ExponentialL50 Suitability Function of gill net fleets. Assumed same among fleets.
