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

  ## Norwegian Slope Survey in Autumn (EggaNor)

  EggaN_SI_biomass_female <- mfdb_sample_totalweight(
    mdb = mdb, cols = c("length"),
    params = list(data_source = "EggaN-index",
                  sex = "F",
                  length = mfdb_interval(
                    "all", c(stock_params$minlength, stock_params$maxlength),
                    open_ended = c("upper","lower")),
                  # Remove 94&95 data from the SI:
                  year = model_params$year_range[model_params$year_range >= 1996],
                  timestep = model_params$timestep_fun
    )
  )[[1]]

  EggaN_SI_biomass_male <- mfdb_sample_totalweight(
    mdb = mdb, cols = c("length"),
    params = list(data_source = "EggaN-index",
                  sex = "M",
                  length = mfdb_interval(
                    "all", c(stock_params$minlength, stock_params$maxlength),
                    open_ended = c("upper","lower")),
                  # Remove 94&95 data from the SI:
                  year = model_params$year_range[model_params$year_range >= 1996],
                  timestep = model_params$timestep_fun
    )
  )[[1]]

  p <- bind_rows(EggaN_SI_biomass_female %>% mutate(sex = "F"),
                 EggaN_SI_biomass_male %>% mutate(sex = "M")) %>%
    ggplot(., aes(x = year, y = total_weight/1e6, fill = sex)) +
    geom_area(position = position_stack(reverse = TRUE)) +
    scale_x_continuous("Year", expand = c(0, 0), breaks = seq(1900, 2030, 2)) +
    scale_y_continuous("Survey index biomass (1000 t)", expand = c(0, 0)) +
    labs(fill = "Sex") +
    theme(legend.position = "bottom")

  png(file.path(base_dir, "figures/EggaN_index.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(p)
  dev.off()

  ## Juvenile indices from Barents Sea Ecosystem Survey data (BESS)

  Juv_SI_1 <- mfdb_sample_count(
    mdb = mdb, cols = c("length"),
    params = list(data_source = "BESS-index",
                  length = mfdb_interval("len", c(10,17)),
                  timestep = model_params$timestep_fun,
                  year = model_params$year_range[model_params$year_range != "2014"])
  )[[1]]


  Juv_SI_2 <- mfdb_sample_count(
    mdb = mdb, cols = c("length"),
    params = list(data_source = "BESS-index",
                  length = mfdb_interval("len", c(18,27)),
                  timestep = model_params$timestep_fun,
                  year = model_params$year_range[model_params$year_range != "2014"])
  )[[1]]

  Juv_SI_3 <- mfdb_sample_count(
    mdb = mdb, cols = c("length"),
    params = list(data_source = "BESS-index",
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
    labs(y = "Abundance\n(millions)", x = "Year") +
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

  ## Save

  save(EggaN_SI_biomass_female, EggaN_SI_biomass_male, file = file.path(base_dir, "data/Survey indices to Gadget.rda"))

  rm(p)


  ## !reload_data case
} else {
  load(file.path(base_dir, "data/Survey indices to Gadget.rda"))
}
