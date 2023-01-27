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

if(reload_data) {

  # source("R/gadget_data_functions.R")
  source("R/figure_functions.R")
  source("R/clean_data_functions.R")

  # Data to by-pass MFDB

  nor_survey_ldist <- readRDS("../ghl-gadget-data/data/out/Length data for surveys.rds")
  nor_survey_aldist <- readRDS("../ghl-gadget-data/data/out/Age length data for surveys.rds")
  nor_catch_ldist <- readRDS("../ghl-gadget-data/data/out/Length data for catches.rds")

  ##############
  # Catches ####

  ## TrawlNor ####

  # Note that "ShrimpTrawls" add small fish to length distributions. These are currently not included but they may be included in landings

  ## Direct way without mfdb
  # TrawlNor_ldist <- g3_data(
  # nor_catch_ldist %>%
  #   filter(gear %in% c("BottomTrawls", "PelagicTrawls", "Seines", "DanishSeines")),
  #   params =
  #     list(year = model_params$year_range,
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
  # )

  TrawlNor_ldist <- mfdb::mfdb_sample_count(
    mdb,
    cols = c("length"),
    params =
      list(data_source = "ldist-catches-NOR",
           gear = c("BottomTrawls", "PelagicTrawls", "OtherTrawls", "Seines", "DanishSeines"),
           year = model_params$year_range,
           timestep = model_params$timestep_fun,
           length = mfdb_interval(
             "len",
             seq(stock_params$minlength, stock_params$maxlength,
                 by = 2*stock_params$dl),
             open_ended = c("upper","lower")
           )
      )
  )[[1]] %>%
    filter(!year %in% c(1991, 1993, 1995, 1996, 1998))
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
  print(plot.ldist(TrawlNor_ldist, type = "ggridges"))
  dev.off()

  #### Sex ratio

  TrawlNor_sexratio <- mfdb_sample_count(
    mdb,
    c('sex', 'length'),
    list(
      data_source = "ldist-catches-NOR",
      gear = c("BottomTrawls", "ShrimpTrawls", "PelagicTrawls", "OtherTrawls", "Seines", "DanishSeines"),
      length =
        mfdb_interval(
          'len',
          seq(41, stock_params$male_mat$max_possible_data_length+6,
              by = 5*stock_params$dl),
          open_ended = c('lower','upper')),
      sex = mfdb_group(female = 'F', male = 'M'),
      timestep = model_params$timestep_fun,
      year = model_params$year_range
    ))[[1]] %>%
    filter(!year %in% c(1988, 1992, 1994, 1995, 1998, 1999, 2000, 2001, 2002, 2005, 2011))

  TrawlNor_sexratio <- clean_sexratio_data(TrawlNor_sexratio)

  png(file.path(base_dir, "figures/TrawlNor_sexratio.png"), width = pagewidth, height = pagewidth*1.5, units = "mm", res = 300)
  print(plot.sexr(TrawlNor_sexratio))
  dev.off()

  # AllNorCatches_aldist <- mfdb_sample_count(
  #   mdb,
  #   cols = c("age", "length"),
  #   params =
  #     list(data_source = "ldist-catches-NOR",
  #          year = model_params$year_range,
  #          timestep = model_params$timestep_fun,
  #          age = mfdb_interval(
  #            "age", stock_params$minage:stock_params$maxage,
  #            open_ended = c("upper")
  #          ),
  #          length = mfdb_interval(
  #            "len",
  #            seq(stock_params$minlength, stock_params$maxlength,
  #                by = 5),
  #            open_ended = c("upper","lower")
  #          )
  #     )
  # )[[1]]
  #
  # png(file.path(base_dir, "figures/AllNorCatches_aldist.png"), width = pagewidth, height = pagewidth*2, units = "mm", res = 300)
  # print(plot.aldist(AllNorCatches_aldist))
  # dev.off()

  ## OtherNor ####

  # mfdb_dplyr_sample(mdb) %>% group_by(gear) %>% count() %>% collect()

  ## Direct way without MFDB
  # OtherNor_ldist <- g3_data(
  #   nor_catch_ldist %>%
  #     filter(gear %in% c("Longlines", "Gillnets")),
  #   params =
  #     list(
  #       year = model_params$year_range,
  #       timestep = model_params$timestep_fun,
  #       age = mfdb_interval(
  #         "all", c(stock_params$minage, stock_params$maxage),
  #         open_ended = c("upper","lower")
  #       ),
  #       length = mfdb_interval(
  #         "len",
  #         seq(stock_params$minlength, stock_params$maxlength,
  #             by = stock_params$dl),
  #         open_ended = c("upper","lower")
  #       )
  #     )
  # )

  OtherNor_ldist <- mfdb_sample_count(
    mdb,
    cols = c("length"),
    params =
      list(data_source = "ldist-catches-NOR",
           gear = c("Lines", "Gillnets"),
           year = model_params$year_range,
           timestep = model_params$timestep_fun,
           length = mfdb_interval(
             "len",
             seq(stock_params$minlength, stock_params$maxlength,
                 by = 2*stock_params$dl),
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
  print(plot.ldist(OtherNor_ldist, type = "ggridges"))
  dev.off()

  #### Sex ratio

  OtherNor_sexratio <- mfdb_sample_count(
    mdb,
    c('sex', 'length'),
    list(
      data_source = "ldist-catches-NOR",
      gear = c("Lines", "Gillnets"),
      length =
        mfdb_interval(
          'len',
          seq(41, stock_params$male_mat$max_possible_data_length+6,
              by = 5*stock_params$dl),
          open_ended = c('lower','upper')),
      sex = mfdb_group(female = 'F', male = 'M'),
      timestep = model_params$timestep_fun,
      year = model_params$year_range
    ))[[1]] %>% filter(!year %in% c(1985,1991,1994,2011,2012)) # add 1992,1995?

  OtherNor_sexratio <- clean_sexratio_data(OtherNor_sexratio)

  png(file.path(base_dir, "figures/OtherNor_sexratio.png"), width = pagewidth, height = pagewidth*1.5, units = "mm", res = 300)
  print(plot.sexr(OtherNor_sexratio))
  dev.off()

  OtherNor_aldist <- g3_data(
    nor_catch_ldist %>% filter(
      gear %in% c("Longlines", "Gillnets"),
      !is.na(age),
      grepl("new", readingtype),
      !is.na(sex)),
    params =
      list(
        year = model_params$year_range,
        timestep = model_params$timestep_fun,
        age = mfdb_interval(
          "age", stock_params$minage:stock_params$maxage,
          open_ended = c("upper")
        ),
        length = mfdb_interval(
          "len",
          seq(41, stock_params$male_mat$max_possible_data_length+6, by = 5),
          open_ended = c("upper","lower")
        ),
        sex = mfdb_group(female = 'F', male = 'M')
      ),
    verbose = FALSE) %>% filter(!year %in% c(2019))

  # tmp <- mfdb_sample_count(
  #   mdb,
  #   cols = c("age", "length"),
  #   params =
  #     list(data_source = "ldist-catches-NOR",
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
  png(file.path(base_dir, "figures/OtherNor_aldist.png"), width = pagewidth, height = pagewidth*2, units = "mm", res = 300)
  p1 <- OtherNor_aldist %>% filter(sex == "female") %>% plot.aldist(., facet_age = FALSE, ncol = 1, scales = "free_y") + ggtitle("Females") + theme(legend.position = "bottom")
  p2 <- OtherNor_aldist %>% filter(sex == "male") %>% plot.aldist(., facet_age = FALSE, ncol = 1, scales = "free_y") + ggtitle("Males") + theme(legend.position = "none")
  cowplot::plot_grid(cowplot::plot_grid(p1 + theme(legend.position = "none"), p2), cowplot::get_legend(p1), ncol = 1, rel_heights = c(10,1)) %>% print
  dev.off()

  rm(p1, p2)

  ## Russian trawl catches ####

  tmp_f <- readRDS("../ghl-gadget-data/data/out/Russian trawl ldist female from gadget2.rds")
  tmp_m <- readRDS("../ghl-gadget-data/data/out/Russian trawl ldist male from gadget2.rds")

  if(identical(model_params$timestep_fun, mfdb::mfdb_timestep_yearly)) {
    tmp_f$step <- 1
    tmp_m$step <- 1
  }

  split_fun <- function(x) {
    mean(
      c(as.numeric(sapply(strsplit(x, "-"), "[", 2)),
        as.numeric(sapply(strsplit(x, "-"), "[", 3)))
    )
  }

  tmp <- bind_rows(
    tmp_f %>%
      mutate(
        sex = "female",
        length = sapply(length, split_fun)),
    tmp_m %>%
      mutate(
        sex = "male",
        length = sapply(length, split_fun))
  ) %>%
    mutate(age = as.integer(gsub("[^0-9.-]", "", age)))

  TrawlRus_ldist <- gadgetutils::g3_data(
    tmp,
    params =
      list(
        year = model_params$year_range,
        timestep = model_params$timestep_fun,
        length = mfdb_interval(
          "len",
          seq(stock_params$minlength, stock_params$maxlength,
              by = 2*stock_params$dl),
          open_ended = c("upper","lower")
        )
      ),
    method = "number",
    column_names = c(year = "year", step = "step", area = "area"),
    verbose = FALSE) %>%
    filter(!year %in% c(1991, 1993))

  png(file.path(base_dir, "figures/TrawlRus_ldist.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.ldist(TrawlRus_ldist, type = "ggridges"))
  dev.off()

  ### Sex ratio

  # tmp <- tmp %>% filter(length >= tmp %>% filter(number>0) %>% pull(length) %>% min())

  TrawlRus_sexratio <- g3_data(
    tmp,
    params =
      list(
        length =
          mfdb_interval(
            'len',
            seq(41, stock_params$male_mat$max_possible_data_length+6,
                by = 5*stock_params$dl),
            open_ended = c('lower','upper')),
        sex = mfdb_group(female = 'female', male = 'male'),
        timestep = model_params$timestep_fun,
        year = model_params$year_range
      ),
    method = "number",
    column_names = c(year = "year", step = "step", area = "area"),
    verbose = FALSE
  )

  TrawlRus_sexratio <- clean_sexratio_data(TrawlRus_sexratio)

  png(file.path(base_dir, "figures/TrawlRus_sexratio.png"), width = pagewidth, height = pagewidth*1.5, units = "mm", res = 300)
  print(plot.sexr(TrawlRus_sexratio))
  dev.off()

  ## Russian other catches ####

  tmp_f <- readRDS("../ghl-gadget-data/data/out/Russian other ldist female from gadget2.rds")
  tmp_m <- readRDS("../ghl-gadget-data/data/out/Russian other ldist male from gadget2.rds")

  if(identical(model_params$timestep_fun, mfdb::mfdb_timestep_yearly)) {
    tmp_f$step <- 1
    tmp_m$step <- 1
  }

  split_fun <- function(x) {
    mean(
      c(as.numeric(sapply(strsplit(x, "-"), "[", 2)),
        as.numeric(sapply(strsplit(x, "-"), "[", 3)))
    )
  }

  tmp <- bind_rows(
    tmp_f %>%
      mutate(
        sex = "female",
        length = sapply(length, split_fun)),
    tmp_m %>%
      mutate(
        sex = "male",
        length = sapply(length, split_fun))
  ) %>%
    mutate(age = as.integer(gsub("[^0-9.-]", "", age)))

  OtherRus_ldist <- gadgetutils::g3_data(
    tmp,
    params =
      list(
        year = model_params$year_range,
        timestep = model_params$timestep_fun,
        length =
          mfdb_interval(
            "len",
            seq(stock_params$minlength, stock_params$maxlength,
                by = 2*stock_params$dl),
            open_ended = c("upper","lower")
          )
      ),
    method = "number",
    column_names = c(year = "year", step = "step", area = "area"),
    verbose = FALSE) %>%
    filter(!year %in% c(1995, 2005))

  png(file.path(base_dir, "figures/OtherRus_ldist.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.ldist(OtherRus_ldist, type = "ggridges"))
  dev.off()

  ### Sex ratio
  OtherRus_sexratio <- g3_data(
    tmp,
    params =
      list(
        length =
          mfdb_interval(
            'len',
            seq(41, stock_params$male_mat$max_possible_data_length+6,
                by = 5*stock_params$dl),
            open_ended = c('lower','upper')),
        sex = mfdb_group(female = 'female', male = 'male'),
        timestep = model_params$timestep_fun,
        year = model_params$year_range
      ),
    method = "number",
    column_names = c(year = "year", step = "step", area = "area"),
    verbose = FALSE
  ) %>%
    filter(!year %in% c(1995, 2005))

  OtherRus_sexratio <- clean_sexratio_data(OtherRus_sexratio)

  png(file.path(base_dir, "figures/OtherRus_sexratio.png"), width = pagewidth, height = pagewidth*1.5, units = "mm", res = 300)
  print(plot.sexr(OtherRus_sexratio))
  dev.off()

  ##############
  # Surveys ####

  #############
  ## EggaN ####

  # mfdb_dplyr_sample(mdb) %>% filter(sampling_type == "ENS") %>% group_by(gear) %>% count() %>% collect()
  # mfdb_dplyr_sample(mdb) %>% filter(data_source == "EggaN-index") %>% collect() %>% dplyr::select(where(~sum(!is.na(.x)) > 0))

  # EggaN_ldist <-
  #   mfdb_sample_count(
  #     mdb,
  #     cols = c("age", "length"),
  #     params =
  #       list(data_source = "EggaN-index-abundance",
  #            year = model_params$year_range[model_params$year_range >= 1996],
  #            timestep = model_params$timestep_fun,
  #            age = mfdb_interval(
  #              "all", c(stock_params$minage, stock_params$maxage),
  #              open_ended = c("upper","lower")
  #            ),
  #            length = mfdb_interval(
  #              "len",
  #              seq(stock_params$minlength, stock_params$maxlength,
  #                  by = stock_params$dl),
  #              open_ended = c("upper","lower")
  #            )
  #       )
  #   )[[1]] # StoX estimates

  EggaN_ldist <- mfdb_sample_count(
    mdb,
    cols = c("length"),
    params =
      list(sampling_type = "ENS",
           gear = "BottomTrawls",
           year = model_params$year_range,
           timestep = model_params$timestep_fun,
           length = mfdb_interval(
             "len",
             seq(stock_params$minlength, stock_params$maxlength,
                 by = 2*stock_params$dl),
             open_ended = c("upper","lower")
           )
      )
  )[[1]] # The Biotic data

  png(file.path(base_dir, "figures/EggaN_ldist.png"), width = pagewidth, height = pagewidth*1.5, units = "mm", res = 300)
  print(plot.ldist(EggaN_ldist, type = "ggridges"))
  dev.off()

  EggaN_aldist_female <- g3_data(
    nor_survey_aldist %>%
      filter(
        sampling_type == "ENS",
        sex == "F",
        !is.na(age),
        readingtype %in% c("new_other_reader", "new_qualified_reader")
      ),
    params =
      list(
        timestep = model_params$timestep_fun,
        age = mfdb_interval(
          "age", stock_params$minage:stock_params$maxage,
          open_ended = c("upper")
        ),
        length = mfdb_interval(
          "len",
          seq(31, 87, by = 5),
          open_ended = c("upper","lower")
        )
      ),
    method = "est_n",
    verbose = FALSE
  ) %>% rename("number" = "est_n") %>% filter(year != 2007)

  png(file.path(base_dir, "figures/EggaN_aldist_female.png"), width = pagewidth, height = pagewidth*2, units = "mm", res = 300)
  print(plot.aldist(EggaN_aldist_female, scales = "free_y"))
  dev.off()

  png(file.path(base_dir, "figures/EggaN_adist_female.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.adist(EggaN_aldist_female, scales = "free_y", ncol = 1))
  dev.off()

  EggaN_aldist_male <- g3_data(
    nor_survey_aldist %>%
      filter(
        sampling_type == "ENS",
        sex == "M",
        !is.na(age),
        readingtype %in% c("new_other_reader", "new_qualified_reader")
      ),
    params =
      list(
        timestep = model_params$timestep_fun,
        age = mfdb_interval(
          "age", stock_params$minage:stock_params$maxage,
          open_ended = c("upper")
        ),
        length = mfdb_interval(
          "len",
          seq(31,66, by = 5),
          open_ended = c("upper","lower")
        )
      ),
    method = "est_n",
    verbose = FALSE
  ) %>% rename("number" = "est_n")


  png(file.path(base_dir, "figures/EggaN_aldist_male.png"), width = pagewidth, height = pagewidth*2, units = "mm", res = 300)
  print(plot.aldist(EggaN_aldist_male))
  dev.off()

  png(file.path(base_dir, "figures/EggaN_adist_male.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.adist(EggaN_aldist_male, scales = "free_y", ncol = 1))
  dev.off()

  ## Sex ratio

  EggaN_sexratio <- mfdb_sample_count(
    mdb,
    c('sex','length'),
    list(
      sampling_type = "ENS",
      gear = "BottomTrawls",
      length =
        mfdb_interval(
          'len', seq(31, stock_params$male_mat$max_possible_data_length+6,
                     by = 5*stock_params$dl),
          open_ended = c('lower','upper')),
      sex = mfdb_group(female = 'F', male = 'M'),
      timestep = model_params$timestep_fun,
      year = model_params$year_range
    ))[[1]]

  EggaN_sexratio <- clean_sexratio_data(EggaN_sexratio)

  png(file.path(base_dir, "figures/EggaN_sexratio.png"), width = pagewidth, height = pagewidth*1.5, units = "mm", res = 300)
  print(plot.sexr(EggaN_sexratio))
  dev.off()

  #############
  ## EggaS ####

  ## Non-mfdb way
  # EggaS_ldist <- g3_data(
  #   nor_survey_ldist %>%
  #     filter(
  #       sampling_type == "ESS",
  #       gear == "BottomTrawls"
  #     ),
  #   params =
  #     list(
  #       year = model_params$year_range,
  #       timestep = model_params$timestep_fun,
  #       age = mfdb_interval(
  #         "all", c(stock_params$minage, stock_params$maxage),
  #         open_ended = c("upper","lower")
  #       ),
  #       length = mfdb_interval(
  #         "len",
  #         seq(stock_params$minlength, stock_params$maxlength,
  #             by = stock_params$dl),
  #         open_ended = c("upper","lower")
  #       )
  #     )
  # )

  EggaS_ldist <- mfdb_sample_count(
    mdb,
    cols = c("length"),
    params =
      list(sampling_type = "ESS",
           gear = "BottomTrawls",
           year = model_params$year_range,
           timestep = model_params$timestep_fun,
           length = mfdb_interval(
             "len",
             seq(stock_params$minlength, stock_params$maxlength,
                 by = 2*stock_params$dl),
             open_ended = c("upper","lower")
           )
      )
  )[[1]]

  png(file.path(base_dir, "figures/EggaS_ldist.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.ldist(EggaS_ldist, type = "ggridges"))
  dev.off()

  ## Age-length

  EggaS_aldist <- g3_data(
    nor_survey_aldist %>%
      filter(
        sampling_type == "ESS",
        !is.na(age),
        readingtype %in% c("new_other_reader", "new_qualified_reader")
      ),
    params =
      list(
        timestep = model_params$timestep_fun,
        age = mfdb_interval(
          "age", stock_params$minage:stock_params$maxage,
          open_ended = c("upper")
        ),
        length = mfdb_interval(
          "len",
          seq(31, 87, by = 5),
          open_ended = c("upper","lower")
        ),
        sex = mfdb_group(female = 'F', male = 'M')
      ),
    method = "est_n",
    verbose = FALSE
  ) %>% rename("number" = "est_n")

  png(file.path(base_dir, "figures/EggaS_aldist.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  p1 <- EggaS_aldist %>% filter(sex == "female") %>% plot.aldist(., facet_age = FALSE, ncol = 1, scales = "free_y") + ggtitle("Females") + theme(legend.position = "bottom")
  p2 <- EggaS_aldist %>% filter(sex == "male") %>% plot.aldist(., facet_age = FALSE, ncol = 1, scales = "free_y") + ggtitle("Males") + theme(legend.position = "none")
  cowplot::plot_grid(cowplot::plot_grid(p1 + theme(legend.position = "none"), p2), cowplot::get_legend(p1), ncol = 1, rel_heights = c(10,1)) %>% print
  dev.off()

  rm(p1, p2)

  ## Sex ratio

  ## Non-mfdb way
  # EggaS_sexratio <- g3_data(
  #   nor_survey_ldist %>%
  #     filter(
  #       sampling_type == "ESS",
  #       gear == "BottomTrawls",
  #       !is.na(sex)
  #     ),
  #   list(
  #     age = mfdb_interval(
  #       "all", c(stock_params$minage, stock_params$maxage),
  #       open_ended = c("upper","lower")
  #     ),
  #     length =
  #       mfdb_interval('len',
  #                     seq(stock_params$minlength, stock_params$maxlength,
  #                         by = 5*stock_params$dl),
  #                     open_ended = c('lower','upper')),
  #     sex = mfdb_group(female = 'F', male = 'M'),
  #     timestep = model_params$timestep_fun,
  #     year = model_params$year_range
  #   ))

  EggaS_sexratio <- mfdb_sample_count(
    mdb,
    c('sex','length'),
    list(
      sampling_type = "ESS",
      gear = "BottomTrawls",
      length =
        mfdb_interval(
          'len',
          seq(41, stock_params$male_mat$max_possible_data_length+6,
              by = 5*stock_params$dl),
          open_ended = c('lower','upper')),
      sex = mfdb_group(female = 'F', male = 'M'),
      timestep = model_params$timestep_fun,
      year = model_params$year_range
    ))[[1]]

  EggaS_sexratio <- clean_sexratio_data(EggaS_sexratio)

  png(file.path(base_dir, "figures/EggaS_sexratio.png"), width = pagewidth, height = pagewidth*1.5, units = "mm", res = 300)
  print(plot.sexr(EggaS_sexratio))
  dev.off()

  ########################
  ## Ecosystem survey ####

  EcoS_ldist <- mfdb_sample_count(
    mdb,
    cols = c("length"),
    params =
      list(sampling_type = "ECS",
           gear = "ShrimpTrawls",
           year = model_params$year_range,
           timestep = model_params$timestep_fun,
           length = mfdb_interval(
             "len",
             seq(stock_params$minlength, stock_params$maxlength,
                 by = 2*stock_params$dl),
             open_ended = c("upper","lower")
           )
      )
  )[[1]]

  png(file.path(base_dir, "figures/EcoS_ldist.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.ldist(EcoS_ldist, type = "ggridges"))
  dev.off()

  ## Age-length

  EcoS_aldist <- g3_data(
    nor_survey_aldist %>%
      filter(
        sampling_type == "ECS",
        !is.na(age),
        # grepl("new", readingtype)
        readingtype %in% c("new_other_reader", "new_qualified_reader")
      ),
    params =
      list(
        timestep = model_params$timestep_fun,
        age = mfdb_interval(
          "age", stock_params$minage:stock_params$maxage,
          open_ended = c("upper")
        ),
        length = mfdb_interval(
          "len", seq(11, 76, by = 5),
          open_ended = c("upper","lower")
        ),
        sex = mfdb_group(female = 'F', male = 'M')
      ),
    method = "est_n",
    verbose = FALSE
  ) %>%
    rename("number" = "est_n") %>%
    filter(!year %in% c(2013, 2014, 2015, 2016, 2017, 2019))

  png(file.path(base_dir, "figures/EcoS_aldist.png"), width = pagewidth, height = pagewidth*1.5, units = "mm", res = 300)
  p1 <- EcoS_aldist %>% filter(sex == "female") %>% plot.aldist(., facet_age = FALSE, ncol = 1, scales = "free_y") + ggtitle("Females") + theme(legend.position = "bottom")
  p2 <- EcoS_aldist %>% filter(sex == "male") %>% plot.aldist(., facet_age = FALSE, ncol = 1, scales = "free_y") + ggtitle("Males") + theme(legend.position = "none")
  cowplot::plot_grid(cowplot::plot_grid(p1 + theme(legend.position = "none"), p2), cowplot::get_legend(p1), ncol = 1, rel_heights = c(10,1)) %>% print
  dev.off()

  rm(p1, p2)

  ## Sex ratio

  EcoS_sexratio <- mfdb_sample_count(
    mdb,
    c('sex', 'length'),
    list(
      sampling_type = "ECS",
      gear = "ShrimpTrawls",
      length =
        mfdb_interval(
          'len',
          seq(21, stock_params$male_mat$max_possible_data_length+6,
              by = 5*stock_params$dl),
          open_ended = c('lower','upper')),
      sex = mfdb_group(female = 'F', male = 'M'),
      timestep = model_params$timestep_fun,
      year = model_params$year_range
    ))[[1]]


  EcoS_sexratio <- clean_sexratio_data(EcoS_sexratio)

  png(file.path(base_dir, "figures/EcoS_sexratio.png"), width = pagewidth, height = pagewidth*1.5, units = "mm", res = 300)
  print(plot.sexr(EcoS_sexratio))
  dev.off()

  #####################
  ## Winter survey ####

  WinterS_ldist <- mfdb_sample_count(
    mdb,
    cols = c("length"),
    params =
      list(sampling_type = "WNS",
           gear = "ShrimpTrawls",
           population = c("H100-300","H300-400","I100-300","I300-400","I400-500",
                          "J100-300","J300-400","L100-300","L300-400"),
           year = model_params$year_range[!model_params$year_range %in% "1980"],
           timestep = model_params$timestep_fun,
           length = mfdb_interval(
             "len",
             seq(stock_params$minlength, stock_params$maxlength,
                 by = 5*stock_params$dl),
             open_ended = c("upper","lower")
           )
      )
  )[[1]]

  png(file.path(base_dir, "figures/WinterS_ldist.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.ldist(WinterS_ldist, type = "ggridges"))
  dev.off()

  ######################
  ## Russian survey ####

  RussianS_ldist <- readRDS("../ghl-gadget-data/data/out/Russian survey ldist from gadget2.rds")

  if(identical(model_params$timestep_fun, mfdb::mfdb_timestep_yearly)) {
    RussianS_ldist$step <- 1
  }

  tmp <- attributes(RussianS_ldist)$length

  RussianS_ldist <- RussianS_ldist %>%
    dplyr::select(-age) %>%
    group_by(year, step, area, length) %>%
    summarise(number = sum(number))

  attributes(RussianS_ldist)$length <- tmp
  attributes(RussianS_ldist)$step <- attributes(EggaN_ldist)$step
  attributes(RussianS_ldist)$area <- attributes(EggaN_ldist)$area
  attributes(RussianS_ldist)$age <- attributes(EggaN_ldist)$age

  rm(tmp)

  png(file.path(base_dir, "figures/RussianS_ldist.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.ldist(RussianS_ldist))
  dev.off()

  ############################
  ## Maturity proportions ####

  ## EggaN

  EggaN_mat <- mfdb_concatenate_results(
    mfdb_sample_count(
      mdb,
      c('maturity_stage','length'),
      list(
        length =
          mfdb_interval(
            'len', seq(31, 91, by = 5*stock_params$dl),
            open_ended = c('lower','upper')),
        maturity_stage = mfdb_group(male_imm = 1:2, male_mat = 3:5),
        sex = "M",
        data_source = "ldist-surveys-NOR",
        sampling_type = "ENS",
        timestep = model_params$timestep_fun,
        year = model_params$year_range
      ))[[1]],

    mfdb_sample_count(
      mdb,
      c('maturity_stage','length'),
      list(
        length =
          mfdb_interval(
            'len', seq(31, 91, by = 5*stock_params$dl),
            open_ended = c('lower','upper')),
        maturity_stage = mfdb_group(female_imm = 1:2, female_mat = 3:5),
        sex = "F",
        data_source = "ldist-surveys-NOR",
        sampling_type = "ENS",
        timestep = model_params$timestep_fun,
        year = model_params$year_range
      ))[[1]]
  ) %>% filter(!year %in% 1999)

  EggaN_mat <- clean_mat_data(EggaN_mat)

  # nremoved <- sum(EggaN_mat$number) -
  #   EggaN_mat %>%
  #   mutate(len = as.numeric(gsub("len", "", length))) %>%
  #   filter(
  #     (grepl("^female_mat", maturity_stage) &
  #        len > stock_params$female_mat$min_possible_data_length) |
  #       (grepl("^female_imm", maturity_stage) &
  #          len <stock_params$female_imm$max_possible_data_length) |
  #       (grepl("^male_mat", maturity_stage) &
  #          len > stock_params$male_mat$min_possible_data_length) |
  #       (grepl("^male_imm", maturity_stage) &
  #          len < stock_params$male_imm$max_possible_data_length)
  #   ) %>% pull(number) %>% sum()

  # if(nremoved > 0) {
  #   message("Removed ", nremoved, " observations from EggaN maturity proportions to smooth the data going into likelihood")
  # }
  #
  # rm(nremoved)
  #
  # EggaN_mat <- EggaN_mat %>%
  #   mutate(len = as.numeric(gsub("len", "", length))) %>%
  #   filter(
  #     (grepl("^female_mat", maturity_stage) &
  #        len > stock_params$female_mat$min_possible_data_length) |
  #       (grepl("^female_imm", maturity_stage) &
  #          len < stock_params$female_imm$max_possible_data_length) |
  #       (grepl("^male_mat", maturity_stage) &
  #          len > stock_params$male_mat$min_possible_data_length) |
  #       (grepl("^male_imm", maturity_stage) &
  #          len < stock_params$male_imm$max_possible_data_length)
  #   )
  #
  # if(!is.na(stock_params$force_even_stock_distribution_length)) {
  #
  #   EggaN_mat <- EggaN_mat %>%
  #     filter(len > stock_params$force_even_stock_distribution_length) %>%
  #     bind_rows(
  #       tibble(year = unique(EggaN_mat$year),
  #              step = unique(EggaN_mat$step),
  #              area = unique(EggaN_mat$area)) %>%
  #         tidyr::expand(nesting(year, step, area),
  #                       maturity_stage = c("female_imm", "male_imm")) %>%
  #         mutate(age = unique(EggaN_mat$age)) %>%
  #         tidyr::expand(
  #           nesting(year, step, area, maturity_stage),
  #           length =
  #             names(
  #               mfdb_interval(
  #                 'len', seq(31, stock_params$male_mat$max_possible_data_length+6,
  #                            by = 5*stock_params$dl),
  #                 open_ended = c('lower','upper')))) %>%
  #         mutate(number = 1,
  #                len = as.numeric(gsub("len", "", length)))
  #     ) %>%
  #     arrange(year, step, area, maturity_stage, len)
  # }
  #
  # EggaN_mat <- EggaN_mat %>% dplyr::select(-len)


  png(file.path(base_dir, "figures/EggaN_maturity_data.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.mat(EggaN_mat))
  dev.off()

  png(file.path(base_dir, "figures/EggaN_maturity_data_proportions.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.matp(EggaN_mat))
  dev.off()

  png(file.path(base_dir, "figures/Maturity_age_ldist_length_comparison.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(compare_mat_ldist(EggaN_ldist, EggaN_mat, rbind(EggaN_aldist_female %>% mutate(sex = "F"), EggaN_aldist_male %>% mutate(sex = "M"))))
  dev.off()

  ## EggaS

  EggaS_mat <- mfdb_concatenate_results(
    mfdb_sample_count(
      mdb,
      c('maturity_stage','length'),
      list(
        length =
          mfdb_interval(
            'len', seq(31, 91, by = 5*stock_params$dl),
            open_ended = c('lower','upper')),
        maturity_stage = mfdb_group(male_imm = 1:2, male_mat = 3:5),
        sex = "M",
        data_source = "ldist-surveys-NOR",
        sampling_type = "ESS",
        timestep = model_params$timestep_fun,
        year = model_params$year_range
      ))[[1]],

    mfdb_sample_count(
      mdb,
      c('maturity_stage','length'),
      list(
        length =
          mfdb_interval(
            'len', seq(31, 91, by = 5*stock_params$dl),
            open_ended = c('lower','upper')),
        maturity_stage = mfdb_group(female_imm = 1:2, female_mat = 3:5),
        sex = "F",
        data_source = "ldist-surveys-NOR",
        sampling_type = "ESS",
        timestep = model_params$timestep_fun,
        year = model_params$year_range
      ))[[1]]
  )

  EggaS_mat <- clean_mat_data(EggaS_mat) %>% filter(!year %in% 2016)

  # nremoved <- sum(EggaS_mat$number) -
  #   EggaS_mat %>%
  #   mutate(len = as.numeric(gsub("len", "", length))) %>%
  #   filter(
  #     (grepl("^female_mat", maturity_stage) &
  #        len > stock_params$female_mat$min_possible_data_length) |
  #       (grepl("^female_imm", maturity_stage) &
  #          len <stock_params$female_imm$max_possible_data_length) |
  #       (grepl("^male_mat", maturity_stage) &
  #          len > stock_params$male_mat$min_possible_data_length) |
  #       (grepl("^male_imm", maturity_stage) &
  #          len < stock_params$male_imm$max_possible_data_length)
  #   ) %>% pull(number) %>% sum()
  #
  # if(nremoved > 0) {
  #   message("Removed ", nremoved, " observations from EggaS maturity proportions to smooth the data going into likelihood")
  # }
  #
  # rm(nremoved)
  #
  # EggaS_mat <- EggaS_mat %>%
  #   mutate(len = as.numeric(gsub("len", "", length))) %>%
  #   filter(
  #     (grepl("^female_mat", maturity_stage) &
  #        len > stock_params$female_mat$min_possible_data_length) |
  #       (grepl("^female_imm", maturity_stage) &
  #          len < stock_params$female_imm$max_possible_data_length) |
  #       (grepl("^male_mat", maturity_stage) &
  #          len > stock_params$male_mat$min_possible_data_length) |
  #       (grepl("^male_imm", maturity_stage) &
  #          len < stock_params$male_imm$max_possible_data_length)
  #   )
  #
  # if(!is.na(stock_params$force_even_stock_distribution_length)) {
  #
  #   EggaS_mat <- EggaS_mat %>%
  #     filter(len > stock_params$force_even_stock_distribution_length) %>%
  #     bind_rows(
  #       tibble(year = unique(EggaS_mat$year),
  #              step = unique(EggaS_mat$step),
  #              area = unique(EggaS_mat$area)) %>%
  #         tidyr::expand(nesting(year, step, area),
  #                       maturity_stage = c("female_imm", "male_imm")) %>%
  #         mutate(age = unique(EggaS_mat$age)) %>%
  #         tidyr::expand(
  #           nesting(year, step, area, maturity_stage, age),
  #           length =
  #             names(
  #               mfdb_interval(
  #                 'len',
  #                 seq(stock_params$minlength,
  #                     stock_params$force_even_stock_distribution_length,
  #                     by = 5*stock_params$dl)))) %>%
  #         mutate(number = 1,
  #                len = as.numeric(gsub("len", "", length)))
  #     ) %>%
  #     arrange(year, step, area, maturity_stage, age, len)
  # }
  #
  # EggaS_mat <- EggaS_mat %>% dplyr::select(-len) %>% filter(year != "2016")
  #
  # attributes(EggaS_mat)$age$all <- stock_params$minage:stock_params$maxage

  png(file.path(base_dir, "figures/EggaS_maturity_data.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.mat(EggaS_mat))
  dev.off()

  png(file.path(base_dir, "figures/EggaS_maturity_data_proportions.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(plot.matp(EggaS_mat))
  dev.off()

  ###########
  # Save ####

  save(TrawlNor_ldist, TrawlNor_sexratio, OtherNor_ldist, OtherNor_sexratio, OtherNor_aldist, EggaN_ldist, EggaN_aldist_female, EggaN_aldist_male, EggaN_mat, EggaS_ldist, EggaS_aldist, EggaS_sexratio, EggaS_mat, EcoS_ldist, EcoS_aldist, EcoS_sexratio, RussianS_ldist, TrawlRus_ldist, TrawlRus_sexratio, OtherRus_ldist, OtherRus_sexratio, file = file.path(base_dir, "data/Catch distributions to Gadget.rda"))

  ## !reload_data case
} else {
  load(file.path(base_dir, "data/Catch distributions to Gadget.rda"))
}
