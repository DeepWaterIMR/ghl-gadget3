## ---------------------------
##
## Script name:
##
## Purpose of script:
##
## Author: Mikko Vihtakari // Institute of Marine Research, Norway
## Email: mikko.vihtakari@hi.no
##
## Date Created: 2021-03-17
##
## ---------------------------

## Source the run first script

## ---------------------------

## Load packages

## ---------------------------

if(reload_data) {
  
  ## Source or list custom functions used within the script
  
  source("R/figure_functions.R")
  
  ## ---------------------------
  
  ## Read data
  
  ## ---------------------------
  
  ### HistNor
  
  HistNor_landings <- mfdb_sample_totalweight(
    mdb = mdb, cols = NULL,
    params = list(data_source = "HIST-landings-NOR",
                  year = model_params$year_range,
                  timestep = model_params$timestep_fun
    )
  )[[1]]
  
  png(file.path(base_dir, "figures/HistNor_landings.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(plot.landings(HistNor_landings))
  dev.off()
  
  ### TrawlNor
  
  TrawlNor_landings <- mfdb_sample_totalweight(
    mdb = mdb, cols = NULL,
    params = list(gear = c("Trawls", "Seines"),
                  sampling_type = "LND",
                  year = model_params$year_range,
                  institute = "NOR",
                  timestep = model_params$timestep_fun
    )
  )[[1]]
  
  png(file.path(base_dir, "figures/TrawlNor_landings.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(plot.landings(TrawlNor_landings))
  dev.off()
  
  ### OtherNor
  
  OtherNor_landings <- mfdb_sample_totalweight(
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
  
  png(file.path(base_dir, "figures/OtherNor_landings.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(plot.landings(OtherNor_landings))
  dev.off()
  
  ### TrawlRus
  
  TrawlRus_landings <- mfdb_sample_totalweight(
    mdb = mdb, cols = NULL,
    params = list(data_source = "TRW-landings-RUS",
                  sampling_type = "LND",
                  year = model_params$year_range,
                  institute = "RUS",
                  timestep = model_params$timestep_fun
    )
  )[[1]]
  
  ### OtherRus
  
  OtherRus_landings <- mfdb_sample_totalweight(
    mdb = mdb, cols = NULL,
    params =
      list(data_source = "GIL-landings-RUS",
           sampling_type = "LND",
           year = model_params$year_range,
           institute = "RUS",
           timestep = model_params$timestep_fun
      )
  )[[1]]
  
  ### HistRus
  
  HistRus_landings <- mfdb_sample_totalweight(
    mdb = mdb, cols = NULL,
    params = list(data_source = "HIST-landings-RUS",
                  year = model_params$year_range,
                  timestep = model_params$timestep_fun
    )
  )[[1]]
  
  ## Split HistRus to trawl and other fleets using mean ratio of these in the data
  
  if(nrow(HistRus_landings) > 0) {
    rus_split_factor <- 
      full_join(TrawlRus_landings %>% rename(trawlrus = total_weight),
                OtherRus_landings %>% rename(otherrus = total_weight),
                by = c("year", "step", "area")) %>% 
      mutate(prop = otherrus/trawlrus) %>% 
      pull(prop) %>% 
      mean(., na.rm = TRUE)
    
    TrawlRus_landings <- HistRus_landings %>% 
      filter(year <=  1991) %>% 
      mutate(total_weight = total_weight * (1- rus_split_factor)) %>% 
      bind_rows(
        TrawlRus_landings %>% 
          filter(!is.na(total_weight))
      )
    
    OtherRus_landings <- HistRus_landings %>% 
      filter(year <=  1991) %>% 
      mutate(total_weight = total_weight * rus_split_factor) %>% 
      bind_rows(
        OtherRus_landings
      )
    
    rm(rus_split_factor, HistRus_landings)
    
  }
  
  png(file.path(base_dir, "figures/TrawlRus_landings.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(plot.landings(TrawlRus_landings))
  dev.off()
  
  png(file.path(base_dir, "figures/OtherRus_landings.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(plot.landings(OtherRus_landings))
  dev.off()
  
  
  # png(file.path(base_dir, "figures/HistRus_landings.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  # print(plot.landings(HistRus_landings))
  # dev.off()
  
  ### International landings
  
  Internat_landings <- mfdb_sample_totalweight(
    mdb = mdb, cols = NULL,
    params = list(data_source = "HIST-landings-INT",
                  year = model_params$year_range,
                  timestep = model_params$timestep_fun
    )
  )[[1]]
  
  png(file.path(base_dir, "figures/Internat_landings.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(plot.landings(Internat_landings))
  dev.off()
  
  ## Survey dummy landings
  
  if(!exists("EggaN_SI_biomass_female")) source("2-2 survey indices.R")
  
  EggaN_landings <- structure(
    data.frame(
      year = unique(EggaN_SI_biomass_female$year), step = 1, area = 1, total_weight = 1),
    area_group = mfdb_group(`1` = 1))
  
  png(file.path(base_dir, "figures/EggaN_landings.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(plot.landings(EggaN_landings))
  dev.off()
  
  # Save
  
  save(HistNor_landings, TrawlNor_landings, OtherNor_landings, TrawlRus_landings, OtherRus_landings, Internat_landings, EggaN_landings, file = file.path(base_dir, "data/Landings to Gadget.rda")) #  HistRus_landings, 
  
  
  # !reload_data case
} else {
  load(file.path(base_dir, "data/Landings to Gadget.rda"))
}
