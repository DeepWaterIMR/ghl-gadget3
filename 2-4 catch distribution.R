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

# source("0 run first.R")

## ---------------------------

## Load packages

## ---------------------------

# Connect to the database

## Source or list custom functions used within the script

## ---------------------------

## Read data

## ---------------------------

if(reload_data) {
  
  ###############
  ## Landings ###
  
  ### TrawlNor
  
  ## A problem for ldist data allocation to sexes: most of the data do not contain sex information! Therefore, not done in the current model. Must be solved in another way.
  # mfdb_dplyr_sample(mdb) %>% filter(data_source == "ldist-landings-NOR") %>% group_by(sex) %>% count() %>% collect() %>% mutate(pr = 100*(n/sum(n, na.rm = TRUE)))
  
  TrawlNor_ldist <- mfdb_sample_count(
    mdb,
    cols = c("age", "length"),
    params =
      list(data_source = "ldist-landings-NOR",
           gear = c("BottomTrawls", "PelagicTrawls", "OtherTrawls", "Seines", "DanishSeines"),
           year = model_params$year_range,
           timestep = model_params$timestep_fun,
           age = mfdb_interval(
             "all", c(stock_params$minage, stock_params$maxage),
             open_ended = c("upper","lower")
           ),
           length = mfdb_interval(
             "len",
             seq(stock_params$minlength, stock_params$maxlength,
                 by = stock_params$dl),
             open_ended = c("upper","lower")
           )
      )
  )[[1]] %>%
    filter(!year %in% c(1993, 1995, 1998))
  ## For quarterly
  # filter(!(year == 2000 & step == 1)) %>% # Only 1 fish
  # filter(!(year == 1993 & step == 2)) %>% ## Weird spike
  # filter(!(year == 1996 & step == 4)) %>% ## Weird spike
  # filter(!(year == 1997 & step == 4)) %>%
  # filter(!(year == 1998)) %>% ## Few data points
  # filter(!(year == 1999 & step == 1)) %>%
  # filter(!(year == 1999 & step == 3)) %>%
  # filter(!(year == 2001 & step == 4)) %>%
  # filter(!(year == 2005 & step == 1)) %>%
  # filter(!(year == 2009 & step == 3))
  
  test <- TrawlNor_ldist %>%
    group_by(year, step) %>%
    summarise(n = sum(number)) %>%
    filter(n < 20)
  
  if(nrow(test) > 0) {
    warning("Following year-step in TrawlNor_ldist contain < 10 fish: ", paste(paste(test$year, test$step, sep = "-"), collapse = ", "))
  } else {
    rm(test)
  }
  
  png(file.path(base_dir, "figures/TrawlNor_ldist.png"), width = pagewidth, height = pagewidth*1.5, units = "mm", res = 300)
  print(plot.ldist(TrawlNor_ldist))
  dev.off()
  
  AllNorLandings_aldist <- mfdb_sample_count(
    mdb,
    cols = c("age", "length"),
    params =
      list(data_source = "ldist-landings-NOR",
           year = model_params$year_range,
           timestep = model_params$timestep_fun,
           age = mfdb_interval(
             "age", stock_params$minage:stock_params$maxage,
             open_ended = c("upper")
           ),
           length = mfdb_interval(
             "len",
             seq(stock_params$minlength, stock_params$maxlength,
                 by = 5),
             open_ended = c("upper","lower")
           )
      )
  )[[1]]
  
  png(file.path(base_dir, "figures/AllNorLandings_aldist.png"), width = pagewidth, height = pagewidth*2, units = "mm", res = 300)
  print(plot.adist(AllNorLandings_aldist))
  dev.off()
  
  ### OtherNor
  
  # mfdb_dplyr_sample(mdb) %>% group_by(gear) %>% count() %>% collect()
  
  OtherNor_ldist <- mfdb_sample_count(
    mdb,
    cols = c("age", "length"),
    params =
      list(data_source = "ldist-landings-NOR",
           gear = c("Lines", "Gillnets"),
           year = model_params$year_range,
           timestep = model_params$timestep_fun,
           age = mfdb_interval(
             "all", c(stock_params$minage, stock_params$maxage),
             open_ended = c("upper","lower")
           ),
           length = mfdb_interval(
             "len",
             seq(stock_params$minlength, stock_params$maxlength,
                 by = stock_params$dl),
             open_ended = c("upper","lower")
           )
      )
  )[[1]] # %>%
  # filter(!(year == 1995 & step == 3))
  
  test <- OtherNor_ldist %>%
    group_by(year, step) %>%
    summarise(n = sum(number)) %>%
    filter(n < 50)
  
  if(nrow(test) > 0) {
    warning("Following year-step in OtherNor_ldist contain < 10 fish: ", paste(paste(test$year, test$step, sep = "-"), collapse = ", "))
  } else {
    rm(test)
  }
  
  png(file.path(base_dir, "figures/OtherNor_ldist.png"), width = pagewidth, height = pagewidth*1.5, units = "mm", res = 300)
  print(plot.ldist(OtherNor_ldist))
  dev.off()
  
  #
  # OtherNor_adist <- mfdb_sample_count(
  #   mdb,
  #   cols = c("age", "length"),
  #   params =
  #     list(data_source = "ldist-landings-NOR",
  #          gear = c("Lines", "Gillnets"),
  #          year = model_params$year_range,
  #          timestep = model_params$timestep_fun,
  #          age = mfdb_interval(
  #            "age", stock_params$minage:stock_params$maxage,
  #            open_ended = c("upper")
  #          ),
  #          length = mfdb_interval(
  #            "len",
  #            seq(stock_params$minlength, stock_params$maxlength,
  #                by = stock_params$dl),
  #            open_ended = c("upper","lower")
  #          )
  #     )
  # )[[1]]
  #
  # png(file.path(base_dir, "figures/OtherNor_adist.png"), width = pagewidth, height = pagewidth*2, units = "mm", res = 300)
  # print(plot.adist(OtherNor_adist))
  # dev.off()
  
  ## Russian trawl catches
  
  ### Females (combine eventually with males?)
  
  tmp <- readRDS("../ghl-gadget-data/data/out/Russian trawl ldist female from gadget2.rds")
  
  if(identical(model_params$timestep_fun, mfdb::mfdb_timestep_yearly)) {
    tmp$step <- 1
  }
  
  tmp2 <- attributes(tmp)$length
  
  tmp <- tmp %>% 
    group_by(year, step, area, age, length) %>% 
    summarise(number = sum(number)) %>% 
    ungroup()
  
  attributes(tmp)$length <- tmp2
  attributes(tmp)$step <- attributes(TrawlNor_ldist)$step
  attributes(tmp)$area <- attributes(TrawlNor_ldist)$area
  attributes(tmp)$age <- attributes(TrawlNor_ldist)$age
  
  TrawlRus_ldist_female <- tmp %>% 
    filter(!year %in% c(1991, 1993))
  
  png(file.path(base_dir, "figures/TrawlRus_ldist_female.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.ldist(TrawlRus_ldist_female, free_y = T))
  dev.off()
  
  # test <- TrawlRus_ldist_female %>%
  #   group_by(year, step) %>%
  #   summarise(n = sum(number)) %>%
  #   filter(n < 50)
  
  ### Males (combine eventually with females?)
  
  tmp <- readRDS("../ghl-gadget-data/data/out/Russian trawl ldist male from gadget2.rds")
  
  if(identical(model_params$timestep_fun, mfdb::mfdb_timestep_yearly)) {
    tmp$step <- 1
  }
  
  tmp2 <- attributes(tmp)$length
  
  tmp <- tmp %>% 
    group_by(year, step, area, age, length) %>% 
    summarise(number = sum(number)) %>% 
    ungroup()
  
  attributes(tmp)$length <- tmp2
  attributes(tmp)$step <- attributes(TrawlNor_ldist)$step
  attributes(tmp)$area <- attributes(TrawlNor_ldist)$area
  attributes(tmp)$age <- attributes(TrawlNor_ldist)$age
  
  TrawlRus_ldist_male <- tmp %>% 
    filter(!year %in% c(1991, 1993))
  
  png(file.path(base_dir, "figures/TrawlRus_ldist_male.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.ldist(TrawlRus_ldist_male, free_y = T))
  dev.off()
  
  # test <- TrawlRus_ldist_male %>%
  #   group_by(year, step) %>%
  #   summarise(n = sum(number)) %>%
  #   filter(n < 50)
  
  ## Russian other catches
  
  ### Females (combine eventually with males?)
  
  tmp <- readRDS("../ghl-gadget-data/data/out/Russian other ldist female from gadget2.rds")
  
  if(identical(model_params$timestep_fun, mfdb::mfdb_timestep_yearly)) {
    tmp$step <- 1
  }
  
  tmp2 <- attributes(tmp)$length
  
  tmp <- tmp %>% 
    group_by(year, step, area, age, length) %>% 
    summarise(number = sum(number)) %>% 
    ungroup()
  
  attributes(tmp)$length <- tmp2
  attributes(tmp)$step <- attributes(TrawlNor_ldist)$step
  attributes(tmp)$area <- attributes(TrawlNor_ldist)$area
  attributes(tmp)$age <- attributes(TrawlNor_ldist)$age
  
  # plot.ldist(tmp, free_y = T)
  OtherRus_ldist_female <- tmp %>% 
    filter(!year %in% c(1995, 2005))
  
  png(file.path(base_dir, "figures/OtherRus_ldist_female.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.ldist(OtherRus_ldist_female, free_y = T))
  dev.off()
  
  ### Males (combine eventually with males?)
  
  tmp <- readRDS("../ghl-gadget-data/data/out/Russian other ldist male from gadget2.rds")
  
  if(identical(model_params$timestep_fun, mfdb::mfdb_timestep_yearly)) {
    tmp$step <- 1
  }
  
  tmp2 <- attributes(tmp)$length
  
  tmp <- tmp %>% 
    group_by(year, step, area, age, length) %>% 
    summarise(number = sum(number)) %>% 
    ungroup()
  
  attributes(tmp)$length <- tmp2
  attributes(tmp)$step <- attributes(TrawlNor_ldist)$step
  attributes(tmp)$area <- attributes(TrawlNor_ldist)$area
  attributes(tmp)$age <- attributes(TrawlNor_ldist)$age
  
  # plot.ldist(tmp, free_y = T)
  OtherRus_ldist_male <- tmp %>% 
    filter(!year %in% c(1995, 2005))
  
  png(file.path(base_dir, "figures/OtherRus_ldist_male.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.ldist(OtherRus_ldist_male, free_y = T))
  dev.off()
  
  rm(tmp, tmp2)
  
  ##############
  ## Surveys ###
  
  ### EggaN (Norwegian slope survey)
  
  # mfdb_dplyr_sample(mdb) %>% filter(sampling_type == "ENS") %>% group_by(gear) %>% count() %>% collect()
  # mfdb_dplyr_sample(mdb) %>% filter(data_source == "EggaN-index") %>% collect() %>% dplyr::select(where(~sum(!is.na(.x)) > 0))
  
  EggaN_ldist <- 
    mfdb_sample_count(
      mdb,
      cols = c("age", "length"),
      params =
        list(data_source = "EggaN-index",
             year = model_params$year_range[model_params$year_range >= 1996],
             timestep = model_params$timestep_fun,
             age = mfdb_interval(
               "all", c(stock_params$minage, stock_params$maxage),
               open_ended = c("upper","lower")
             ),
             length = mfdb_interval(
               "len",
               seq(stock_params$minlength, stock_params$maxlength,
                   by = stock_params$dl),
               open_ended = c("upper","lower")
             )
        )
    )[[1]] # StoX estimates
  
  #   mfdb_sample_count(
  #   mdb,
  #   cols = c("age", "length"),
  #   params =
  #     list(sampling_type = "ENS",
  #          gear = "BottomTrawls",
  #          year = model_params$year_range[model_params$year_range >= 1996],
  #          timestep = model_params$timestep_fun,
  #          age = mfdb_interval(
  #            "all", c(stock_params$minage, stock_params$maxage),
  #            open_ended = c("upper","lower")
  #          ),
  #          length = mfdb_interval(
  #            "len",
  #            seq(stock_params$minlength, stock_params$maxlength,
  #                by = stock_params$dl),
  #            open_ended = c("upper","lower")
  #          )
  #     )
  # )[[1]] # The Biotic data
  
  png(file.path(base_dir, "figures/EggaN_ldist.png"), width = pagewidth, height = pagewidth*1.5, units = "mm", res = 300)
  print(plot.ldist(EggaN_ldist))
  dev.off()
  
  EggaN_aldist_female <- mfdb_sample_count(
    mdb,
    cols = c("age", "length"),
    params =
      list(sampling_type = "ENS",
           gear = "BottomTrawls",
           year = model_params$year_range[model_params$year_range >= 1996],
           sex = "F",
           timestep = model_params$timestep_fun,
           age = mfdb_interval(
             "age", stock_params$minage:stock_params$maxage,
             open_ended = c("upper")
           ),
           length = mfdb_interval(
             "len",
             seq(stock_params$minlength, stock_params$maxlength,
                 by = 5),
             open_ended = c("upper","lower")
           )
      )
  )[[1]]
  
  png(file.path(base_dir, "figures/EggaN_aldist_female.png"), width = pagewidth, height = pagewidth*2, units = "mm", res = 300)
  print(plot.adist(EggaN_aldist_female))
  dev.off()
  
  EggaN_aldist_male <- mfdb_sample_count(
    mdb,
    cols = c("age", "length"),
    params =
      list(sampling_type = "ENS",
           gear = "BottomTrawls",
           year = model_params$year_range[model_params$year_range >= 1996],
           sex = "M",
           timestep = model_params$timestep_fun,
           age = mfdb_interval(
             "age", stock_params$minage:stock_params$maxage,
             open_ended = c("upper")
           ),
           length = mfdb_interval(
             "len",
             seq(stock_params$minlength, stock_params$maxlength,
                 by = 5),
             open_ended = c("upper","lower")
           )
      )
  )[[1]]
  
  png(file.path(base_dir, "figures/EggaN_aldist_male.png"), width = pagewidth, height = pagewidth*2, units = "mm", res = 300)
  print(plot.adist(EggaN_aldist_male))
  dev.off()
  
  ## Russian survey
  
  RussianSurvey_ldist <- readRDS("../ghl-gadget-data/data/out/Russian survey ldist from gadget2.rds")
  
  if(identical(model_params$timestep_fun, mfdb::mfdb_timestep_yearly)) {
    RussianSurvey_ldist$step <- 1
  }
  
  tmp <- attributes(RussianSurvey_ldist)$length
  
  RussianSurvey_ldist <- RussianSurvey_ldist %>% 
    group_by(year, step, area, age, length) %>% 
    summarise(number = sum(number))
  
  attributes(RussianSurvey_ldist)$length <- tmp
  attributes(RussianSurvey_ldist)$step <- attributes(EggaN_ldist)$step
  attributes(RussianSurvey_ldist)$area <- attributes(EggaN_ldist)$area
  attributes(RussianSurvey_ldist)$age <- attributes(EggaN_ldist)$age
  
  rm(tmp)
  
  png(file.path(base_dir, "figures/RussianSurvey_ldist.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.ldist(RussianSurvey_ldist))
  dev.off()
  
  ############################
  ## Maturity proportions ####
  
  EggaN_mat <- mfdb_concatenate_results(
    mfdb_sample_count(
      mdb,
      c('maturity_stage','age','length'),
      list(
        length =
          mfdb_interval('len',
                        seq(stock_params$minlength, stock_params$maxlength,
                            by = 5*stock_params$dl),
                        open_ended = c('lower','upper')),
        maturity_stage = mfdb_group(male_imm = 1:2, male_mat = 3:5),
        sex = "M",
        data_source = "ldist-surveys-NOR",
        sampling_type = "ENS",
        timestep = model_params$timestep_fun,
        year = model_params$year_range[model_params$year_range >= 1996]
      ))[[1]],
    
    mfdb_sample_count(
      mdb,
      c('maturity_stage','age','length'),
      list(
        length =
          mfdb_interval('len',
                        seq(stock_params$minlength, stock_params$maxlength,
                            by = 5*stock_params$dl),
                        open_ended = c('lower','upper')),
        maturity_stage = mfdb_group(female_imm = 1:2, female_mat = 3:5),
        sex = "F",
        data_source = "ldist-surveys-NOR",
        sampling_type = "ENS",
        timestep = model_params$timestep_fun,
        year = model_params$year_range[model_params$year_range >= 1996]
      ))[[1]]
  )
  
  nremoved <- sum(EggaN_mat$number) -
    EggaN_mat %>%
    mutate(len = as.numeric(gsub("len", "", length))) %>%
    filter(
      (grepl("^female_mat", maturity_stage) &
         len > stock_params$female_mat$min_possible_data_length) |
        (grepl("^female_imm", maturity_stage) &
           len <stock_params$female_imm$max_possible_data_length) |
        (grepl("^male_mat", maturity_stage) &
           len > stock_params$male_mat$min_possible_data_length) |
        (grepl("^male_imm", maturity_stage) &
           len < stock_params$male_imm$max_possible_data_length)
    ) %>% pull(number) %>% sum()
  
  if(nremoved > 0) {
    message("Removed ", nremoved, " observations from EggaN maturity proportions to smooth the data going into likelihood")
  }
  
  rm(nremoved)
  
  EggaN_mat <- EggaN_mat %>%
    mutate(len = as.numeric(gsub("len", "", length))) %>%
    filter(
      (grepl("^female_mat", maturity_stage) &
         len > stock_params$female_mat$min_possible_data_length) |
        (grepl("^female_imm", maturity_stage) &
           len <stock_params$female_imm$max_possible_data_length) |
        (grepl("^male_mat", maturity_stage) &
           len > stock_params$male_mat$min_possible_data_length) |
        (grepl("^male_imm", maturity_stage) &
           len < stock_params$male_imm$max_possible_data_length)
    ) %>%
    dplyr::select(-len)
  
  attributes(EggaN_mat)$age$all <- stock_params$minage:stock_params$maxage
  
  png(file.path(base_dir, "figures/Maturity_data.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.mat(EggaN_mat))
  dev.off()
  
  png(file.path(base_dir, "figures/Maturity_data_proportions.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.matp(EggaN_mat))
  dev.off()
  
  png(file.path(base_dir, "figures/Maturity_age_ldist_length_comparison.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(compare_mat_ldist(EggaN_ldist, EggaN_mat, rbind(EggaN_aldist_female %>% mutate(sex = "F"), EggaN_aldist_male %>% mutate(sex = "M"))))
  dev.off()
  
  # Save
  
  save(TrawlNor_ldist, AllNorLandings_aldist, OtherNor_ldist, EggaN_ldist, EggaN_aldist_female, EggaN_aldist_male, RussianSurvey_ldist, TrawlRus_ldist_male, TrawlRus_ldist_female, OtherRus_ldist_male, OtherRus_ldist_female, EggaN_mat, file = file.path(base_dir, "data/Catch distributions to Gadget.rda"))
  
  ## !reload_data case
} else {
  load(file.path(base_dir, "data/Catch distributions to Gadget.rda"))
}
