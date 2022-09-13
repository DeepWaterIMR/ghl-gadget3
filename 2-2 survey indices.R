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

  ## Norwegian Slope survey in autumn (EggaNor)

  EggaN_biomass_female <- mfdb_sample_totalweight(
    mdb = mdb, cols = c("length"),
    params = list(data_source = "NoSlope-index",
                  sex = "F",
                  length = mfdb_interval(
                    "all", c(stock_params$minlength, stock_params$maxlength),
                    open_ended = c("upper","lower")),
                  # Remove 94&95 data from the SI:
                  year = model_params$year_range[model_params$year_range >= 1996],
                  timestep = model_params$timestep_fun
    )
  )[[1]]

  EggaN_biomass_male <- mfdb_sample_totalweight(
    mdb = mdb, cols = c("length"),
    params = list(data_source = "NoSlope-index",
                  sex = "M",
                  length = mfdb_interval(
                    "all", c(stock_params$minlength, stock_params$maxlength),
                    open_ended = c("upper","lower")),
                  # Remove 94&95 data from the SI:
                  year = model_params$year_range[model_params$year_range >= 1996],
                  timestep = model_params$timestep_fun
    )
  )[[1]]

  p <- bind_rows(EggaN_biomass_female %>% mutate(sex = "F"),
                 EggaN_biomass_male %>% mutate(sex = "M")) %>%
    ggplot(., aes(x = year, y = total_weight/1e6, fill = sex)) +
    geom_area(position = position_stack(reverse = TRUE)) +
    scale_x_continuous("Year", expand = c(0, 0), breaks = seq(1900, 2030, 2)) +
    scale_y_continuous("Survey index biomass (1000 t)", expand = c(0, 0)) +
    labs(fill = "Sex") +
    theme(legend.position = "bottom")

  png(file.path(base_dir, "figures/EggaN_index.png"), width = pagewidth, height = pagewidth*0.7, units = "mm", res = 300)
  print(p)
  dev.off()

  save(EggaN_biomass_female, EggaN_biomass_male, file = file.path(base_dir, "data/Survey indices to Gadget.rda"))

  rm(p)


  ## !reload_data case
} else {
  load(file.path(base_dir, "data/Survey indices to Gadget.rda"))
}
