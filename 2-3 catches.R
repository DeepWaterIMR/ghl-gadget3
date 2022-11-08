## ---------------------------
##
## Script name: Catches
##
## Purpose of script: Load catch data from MFDB
##
## ---------------------------

if(reload_data) {

  ## Source or list custom functions used within the script

  source("R/figure_functions.R")

  ## ---------------------------

  ## Read data

  ## ---------------------------

  ### TrawlNor

  if(min(model_params$year_range) < 1977) {
    stop("Add splitting historic Norwegian catches to gear. See the Catch data document")
  } else {
    TrawlNor_catches <- mfdb_sample_totalweight(
      mdb = mdb, cols = NULL,
      params = list(gear = c("Trawls", "Seines"),
                    sampling_type = "LND",
                    year = model_params$year_range,
                    institute = "NOR",
                    timestep = model_params$timestep_fun
      )
    )[[1]]
  }

  png(file.path(base_dir, "figures/TrawlNor_catches.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(plot.catches(TrawlNor_catches))
  dev.off()

  ### OtherNor

  if(min(model_params$year_range) < 1977) {
    stop("Add splitting historic Norwegian catches to gear. See the Catch data document")
  } else {
    OtherNor_catches <- mfdb_sample_totalweight(
      mdb = mdb, cols = NULL,
      params =
        list(gear =
               grep("Trawls|Seines|Historical",
                    tbl(mdb$db,"gear") %>% select(name) %>% pull(),
                    invert = TRUE, value = TRUE),
             sampling_type = "LND",
             year = model_params$year_range,
             institute = "NOR",
             timestep = model_params$timestep_fun
        )
    )[[1]]
  }

  png(file.path(base_dir, "figures/OtherNor_catches.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(plot.catches(OtherNor_catches))
  dev.off()

  ### TrawlRus

  if(min(model_params$year_range) < 1991) {

    TrawlRus <- mfdb_sample_totalweight(
      mdb = mdb, cols = NULL,
      params = list(data_source = "TRW-catches-RUS",
                    sampling_type = "LND",
                    year = model_params$year_range,
                    institute = "RUS",
                    timestep = model_params$timestep_fun
      )
    )[[1]]

    OtherRus <- mfdb_sample_totalweight(
      mdb = mdb, cols = NULL,
      params =
        list(data_source = "OTH-catches-RUS",
             sampling_type = "LND",
             year = model_params$year_range,
             institute = "RUS",
             timestep = model_params$timestep_fun
        )
    )[[1]]

    ## Split factor
    split_fac <- full_join(
      TrawlRus %>% rename("TrawlRus" = "total_weight"),
      OtherRus %>% rename("OtherRus" = "total_weight"),
      by = c("year", "step", "area")) %>%
      filter(year < 2001, year > 1990) %>%
      mutate(prop = OtherRus/(TrawlRus+OtherRus)) %>%
      pull(prop) %>%
      mean()

    ### HistRus

    HistRus <- mfdb_sample_totalweight(
      mdb = mdb, cols = NULL,
      params = list(data_source = "HIST-catches-RUS",
                    year = model_params$year_range,
                    timestep = model_params$timestep_fun
      )
    )[[1]] %>%
      filter(year < 1991) %>%
      mutate(TrawlRus = (1-split_fac)*total_weight,
             OtherRus = split_fac*total_weight) %>%
      dplyr::select(year, step, area, TrawlRus, OtherRus) %>%
      na.omit()

    ## TrawlRus

    TrawlRus_catches <- bind_rows(
      HistRus %>% dplyr::select(-OtherRus) %>% rename("total_weight" = "TrawlRus"),
      TrawlRus %>% filter(year > 1990)
    )

    ## OtherRus

    OtherRus_catches <- bind_rows(
      HistRus %>% dplyr::select(-TrawlRus) %>% rename("total_weight" = "OtherRus"),
      OtherRus %>% filter(year > 1990)
    )

    rm(HistRus, TrawlRus, OtherRus, split_fac)

  } else {

    TrawlRus_catches <- mfdb_sample_totalweight(
      mdb = mdb, cols = NULL,
      params = list(data_source = "TRW-catches-RUS",
                    sampling_type = "LND",
                    year = model_params$year_range,
                    institute = "RUS",
                    timestep = model_params$timestep_fun
      )
    )[[1]]

    ### OtherRus

    OtherRus_catches <- mfdb_sample_totalweight(
      mdb = mdb, cols = NULL,
      params =
        list(data_source = "OTH-catches-RUS",
             sampling_type = "LND",
             year = model_params$year_range,
             institute = "RUS",
             timestep = model_params$timestep_fun
        )
    )[[1]]
  }


  png(file.path(base_dir, "figures/TrawlRus_catches.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(plot.catches(TrawlRus_catches))
  dev.off()

  png(file.path(base_dir, "figures/OtherRus_catches.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(plot.catches(OtherRus_catches))
  dev.off()

  ### International catches

  Internat_catches <- mfdb_sample_totalweight(
    mdb = mdb, cols = NULL,
    params = list(data_source = "HIST-catches-INT",
                  year = model_params$year_range,
                  timestep = model_params$timestep_fun
    )
  )[[1]]

  png(file.path(base_dir, "figures/Internat_catches.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(plot.catches(Internat_catches))
  dev.off()

  ## Survey dummy catches

  if(!exists("EggaN_SI_biomass_female")) source("2-2 survey indices.R")

  EggaN_catches <- structure(
    data.frame(
      year = unique(EggaN_SI_biomass_female$year), step = 1, area = 1, total_weight = 1),
    area_group = mfdb_group(`1` = 1))

  png(file.path(base_dir, "figures/EggaN_catches.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(plot.catches(EggaN_catches))
  dev.off()

  if(!exists("Russian_SI")) source("2-2 survey indices.R")

  RussianSurvey_catches <- structure(
    data.frame(
      year = unique(Russian_SI$year), step = 1, area = 1, total_weight = 1),
    area_group = mfdb_group(`1` = 1))

  png(file.path(base_dir, "figures/RussianSurvey_catches.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(plot.catches(RussianSurvey_catches))
  dev.off()

  ## All catches

  png(file.path(base_dir, "figures/All_catches.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(plot.catches(list("TrawlNor" = TrawlNor_catches,
                    "OtherNor" = OtherNor_catches,
                    "TrawlRus" = TrawlRus_catches,
                    "OtherRus" = OtherRus_catches,
                    "Internat" = Internat_catches)) +
    # ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE)) +
    ggplot2::theme(legend.position = "bottom"))
  dev.off()

  # Save

  save(TrawlNor_catches, OtherNor_catches, TrawlRus_catches, OtherRus_catches, Internat_catches, EggaN_catches, RussianSurvey_catches, file = file.path(base_dir, "data/Catches to Gadget.rda"))


  # !reload_data case
} else {
  load(file.path(base_dir, "data/Catches to Gadget.rda"))
}
