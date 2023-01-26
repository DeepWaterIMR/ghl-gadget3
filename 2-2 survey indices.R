## ---------------------------
##
## Script name:
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

if(reload_data) {

  ## Norwegian Slope Survey in Autumn (EggaNor) ####

  EggaN_SI_as_biomass_index <- TRUE # Switch to shift between abundance and biomass indices for EggaN

  if(EggaN_SI_as_biomass_index) {

    EggaN_SI_female <- mfdb_sample_totalweight(
      mdb = mdb, cols = c("length"),
      params = list(
        data_source = "EggaN-index-biomass",
        population = c("C500-700", "C700-1000", "D500-700", "D700-1000",
                       "E500-700", "E700-1000", "F500-700", "F700-1000"),
        sex = "F",
        length = mfdb_interval(
          "all", c(28, stock_params$maxlength),
          open_ended = c("upper")),
        # Remove 94&95 data from the SI:
        year = model_params$year_range[model_params$year_range >= 1996],
        timestep = model_params$timestep_fun
      )
    )[[1]]

    EggaN_SI_male <- mfdb_sample_totalweight(
      mdb = mdb, cols = c("length"),
      params = list(
        data_source = "EggaN-index-biomass",
        population = c("C500-700", "C700-1000", "D500-700", "D700-1000",
                       "E500-700", "E700-1000", "F500-700", "F700-1000"),
        sex = "M",
        length = mfdb_interval(
          "all", c(28, stock_params$maxlength),
          open_ended = c("upper")),
        # Remove 94&95 data from the SI:
        year = model_params$year_range[model_params$year_range >= 1996],
        timestep = model_params$timestep_fun
      )
    )[[1]]

  } else {

    EggaN_SI_female <- mfdb_sample_count(
      mdb = mdb, cols = c("length"),
      params = list(
        data_source = "EggaN-index-abundance",
        population = c("C500-700", "C700-1000", "D500-700", "D700-1000",
                       "E500-700", "E700-1000", "F500-700", "F700-1000"),
        sex = "F",
        length = mfdb_interval(
          "all", c(28, stock_params$maxlength),
          open_ended = c("upper")),
        # Remove 94&95 data from the SI:
        year = model_params$year_range[model_params$year_range >= 1996],
        timestep = model_params$timestep_fun
      )
    )[[1]]

    EggaN_SI_male <- mfdb_sample_count(
      mdb = mdb, cols = c("length"),
      params = list(
        data_source = "EggaN-index-abundance",
        population = c("C500-700", "C700-1000", "D500-700", "D700-1000",
                       "E500-700", "E700-1000", "F500-700", "F700-1000"),
        sex = "M",
        length = mfdb_interval(
          "all", c(28, stock_params$maxlength),
          open_ended = c("upper")),
        # Remove 94&95 data from the SI:
        year = model_params$year_range[model_params$year_range >= 1996],
        timestep = model_params$timestep_fun
      )
    )[[1]]

  }


  p <- bind_rows(EggaN_SI_female %>% mutate(sex = "F"),
                 EggaN_SI_male %>% mutate(sex = "M")) %>%
    mutate(value = if(EggaN_SI_as_biomass_index) total_weight else number) %>%
    ggplot(., aes(x = year, y = value/1e6, fill = sex)) +
    geom_area(position = position_stack(reverse = TRUE)) +
    scale_x_continuous("Year", expand = c(0, 0), breaks = seq(1900, 2030, 2)) +
    scale_y_continuous(
      if(EggaN_SI_as_biomass_index) "Survey index biomass (kt)" else {
        "Survey index abundance (millions)"
      },
      expand = c(0, 0)) +
    labs(fill = "Sex") +
    theme(legend.position = "bottom")

  png(file.path(base_dir, "figures/EggaN_index.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(p)
  dev.off()

  # Barents Sea Ecosystem Survey (BESS) index excluding the juvenile indices below ####

  EcoS_SI_as_biomass_index <- TRUE # Switch to shift between abundance and biomass indices for EggaN

  if(EcoS_SI_as_biomass_index) {
    EcoS_SI <- mfdb_sample_totalweight(
      mdb = mdb, cols = c("length"),
      params = list(
        data_source = "BESS-index-biomass",
        population = grep("^O|^X",
                          tbl(mdb$db, "population") %>% select(name) %>% pull(),
                          invert = TRUE, value = TRUE),
        length = mfdb_interval(
          "all", c(28, stock_params$maxlength),
          open_ended = c("upper")
        ),
        timestep = model_params$timestep_fun,
        year = model_params$year_range[model_params$year_range != "2014"])
    )[[1]]
  } else {
    EcoS_SI <- mfdb_sample_count(
      mdb = mdb, cols = c("length"),
      params = list(
        data_source = "BESS-index-abundance",
        population = grep("^O|^X",
                          tbl(mdb$db, "population") %>% select(name) %>% pull(),
                          invert = TRUE, value = TRUE),
        length = mfdb_interval(
          "all", c(28, stock_params$maxlength),
          open_ended = c("upper")
        ),
        timestep = model_params$timestep_fun,
        year = model_params$year_range[model_params$year_range != "2014"])
    )[[1]]
  }

  p <- ggplot(
    EcoS_SI %>%
      mutate(value = if(EcoS_SI_as_biomass_index) total_weight else number),
    aes(x = year, y = value/1e6)
  ) +
    geom_col() +
    labs(
      y = ifelse(EcoS_SI_as_biomass_index,
                 "Survey index biomass (kt)",
                 "Survey index abundance (millions)"),
      x = "Year") +
    scale_x_continuous(expand = c(0, 0), breaks = seq(1900, 2030, 2)) +
    scale_y_continuous(expand = c(0, 0))

  png(file.path(base_dir, "figures/EcoS_index.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(p)
  dev.off()

  ## Juvenile indices from Barents Sea Ecosystem Survey data (BESS) ####

  Juv_SI_1 <- mfdb_sample_count(
    mdb = mdb, cols = c("length"),
    params = list(
      data_source = "BESS-index-abundance",
      population = grep("^O|^X",
                        tbl(mdb$db, "population") %>% select(name) %>% pull(),
                        invert = TRUE, value = TRUE),
      length = mfdb_interval("len", c(9,17), open_ended = c("lower")),
      timestep = model_params$timestep_fun,
      year = model_params$year_range[model_params$year_range != "2014"])
  )[[1]]

  Juv_SI_2 <- mfdb_sample_count(
    mdb = mdb, cols = c("length"),
    params = list(
      data_source = "BESS-index-abundance",
      population = grep("^O|^X",
                        tbl(mdb$db, "population") %>% select(name) %>% pull(),
                        invert = TRUE, value = TRUE),
      length = mfdb_interval("len", c(18,27)),
      timestep = model_params$timestep_fun,
      year = model_params$year_range[model_params$year_range != "2014"])
  )[[1]]

  Juv_SI_3 <- mfdb_sample_count(
    mdb = mdb, cols = c("length"),
    params = list(
      data_source = "BESS-index-abundance",
      population = grep("^O|^X",
                        tbl(mdb$db, "population") %>% select(name) %>% pull(),
                        invert = TRUE, value = TRUE),
      length = mfdb_interval("len", c(28,35)),
      timestep = model_params$timestep_fun,
      year = model_params$year_range[model_params$year_range != "2014"])
  )[[1]]


  p <- bind_rows(Juv_SI_1 %>% mutate(length = "Juv_SI_1 (10-17 cm)"),
                 Juv_SI_2 %>% mutate(length = "Juv_SI_2 (18-27 cm)"),
                 Juv_SI_3 %>% mutate(length = "Juv_SI_3 (28-35 cm)")) %>%
    ggplot(., aes(x = year, y = number/1e6)) +
    geom_col() +
    facet_wrap(~length, scales = "free_y", ncol = 1) +
    labs(y = "Abundance (millions)", x = "Year") +
    scale_x_continuous(expand = c(0, 0), breaks = seq(1900, 2030, 2)) +
    scale_y_continuous(expand = c(0, 0)) # +
  # geom_segment(ggplot2::aes(x = year-0.5, xend = year+.5,
  #                           y=Inf, yend = number/1e6),
  #              lty=2, col='gray', inherit.aes = FALSE) +
  # coord_cartesian(clip = "off")#+
  # theme(legend.position='none',panel.spacing = ggplot2::unit(0,'cm'),
  #       plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
  #       strip.background = ggplot2::element_blank(),
  #       strip.text.x = ggplot2::element_blank())

  png(file.path(base_dir, "figures/Juvenile_index.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(p)
  dev.off()

  ## Winter survey index ####

  # WinterS_SI <- read.csv('../ghl-gadget-data/data/out/Winter survey index.csv') %>%
  #   dplyr::select(startyear, bm) %>%
  #   rename("year" = "startyear", "weight" = "bm") %>%
  #   add_g3_attributes(
  #     params = list(
  #       year = model_params$year_range,
  #       step = model_params$timestep_fun,
  #       length = mfdb_interval(
  #         "all", c(40, stock_params$maxlength),
  #         open_ended = c("upper"))
  #     )
  #   )
  # 
  # p <- ggplot(WinterS_SI, aes(x = year, y = weight)) +
  #   geom_col() +
  #   labs(y = "Survey index biomass (1000 t)", x = "Year") +
  #   scale_x_continuous(expand = c(0, 0), breaks = seq(1900, 2030, 2)) +
  #   scale_y_continuous(expand = c(0, 0))
  # 
  # ggsave(filename = file.path(base_dir, "figures/WinterS_index.png"),
  #        plot = print(p), width = pagewidth, height = pagewidth*0.7,
  #        units = "mm", bg = "white")
  # 
  # rm(p)

  ## Russian survey index ####

  Russian_SI <- read.csv('../ghl-gadget-data/data/out/Russian survey index from gadget2.csv')

  if(identical(model_params$timestep_fun, mfdb::mfdb_timestep_yearly)) {
    Russian_SI$step <- 1
  }

  Russian_SI <- Russian_SI %>% filter(year %in% model_params$year_range)

  attributes(Russian_SI)$step <- attributes(EggaN_SI_female)$step
  attributes(Russian_SI)$area <- attributes(EggaN_SI_female)$area
  attributes(Russian_SI)$year <- stats::setNames(
    lapply(unique(Russian_SI$year),
           function(k) k), lapply(unique(Russian_SI$year), function(k) k))
  tmp <- attributes(mfdb_sample_totalweight(
    mdb = mdb, cols = c("length"),
    params = list(
      data_source = "EggaN-index-biomass",
      length = mfdb_interval(
        "all", c(40, stock_params$maxlength),
        open_ended = c("upper"))
    )
  )[[1]])$length
  names(tmp) <- unique(Russian_SI$length)

  attributes(Russian_SI)$length <- tmp

  p <- ggplot(Russian_SI, aes(x = year, y = total_weight/1e6)) +
    geom_col() +
    labs(y = "Survey index biomass (1000 t)", x = "Year") +
    scale_x_continuous(expand = c(0, 0), breaks = seq(1900, 2030, 2)) +
    scale_y_continuous(expand = c(0, 0))

  ggsave(filename = file.path(base_dir, "figures/Russian_index.png"),
         plot = print(p), width = pagewidth, height = pagewidth*0.7,
         units = "mm", bg = "white")

  rm(p)

  ## Save ####

  save(EggaN_SI_female, EggaN_SI_male, Juv_SI_1, Juv_SI_2, Juv_SI_3, EcoS_SI,
       Russian_SI, file = file.path(base_dir, "data/Survey indices to Gadget.rda"))


  ### Plot all ####

  p <- dplyr::bind_rows(
    EggaN_SI_female %>%
      mutate(value = if(EggaN_SI_as_biomass_index) total_weight else number) %>%
      dplyr::mutate(index = "EggaN_SI_female",
                    p = value/max(value)) %>%
      dplyr::select(year, step, area, length, index, p),
    EggaN_SI_male %>%
      mutate(value = if(EggaN_SI_as_biomass_index) total_weight else number) %>%
      dplyr::mutate(index = "EggaN_SI_male",
                    p = value/max(value)) %>%
      dplyr::select(year, step, area, length, index, p),
    Juv_SI_1 %>%
      dplyr::mutate(index = "Juv_SI_1",
                    p = number/max(number)) %>%
      dplyr::select(-number),
    Juv_SI_2 %>%
      dplyr::mutate(index = "Juv_SI_2",
                    p = number/max(number)) %>%
      dplyr::select(-number),
    Juv_SI_3 %>%
      dplyr::mutate(index = "Juv_SI_3",
                    p = number/max(number)) %>%
      dplyr::select(-number),
    EcoS_SI %>%
      mutate(value = if(EcoS_SI_as_biomass_index) total_weight else number) %>%
      dplyr::mutate(index = "EcoS_SI",
                    p = value/max(value)) %>%
      dplyr::select(year, step, area, length, index, p),
    Russian_SI %>%
      dplyr::mutate(step = as.character(step),
                    index = "Russian_SI",
                    p = total_weight/max(total_weight)) %>%
      dplyr::select(-total_weight)
  ) %>%
    ggplot(aes(x = year, y = p, color = index)) +
    geom_line() +
    labs(x = "Year", y = "Stadardized index", color = "Survey index") +
    scale_x_continuous(expand = c(0, 0), breaks = seq(1900, 2030, 2)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position = "bottom")

  ggsave(
    filename = file.path(base_dir, "figures/Survey_index_comparison.png"),
    plot = print(p), width = pagewidth, height = pagewidth*0.7,  units = "mm",
    bg = "white")

  ## !reload_data case
} else {
  load(file.path(base_dir, "data/Survey indices to Gadget.rda"))
}
