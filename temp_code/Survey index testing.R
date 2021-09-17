## ---------------------------
##
## Script name:
##
## Purpose of script: Running this script requires access to BioticExplorerServer
##
## Author: Mikko Vihtakari // Institute of Marine Research, Norway
## Email: mikko.vihtakari@hi.no
##
## Date Created: 2021-06-17
##
## ---------------------------

## Source the run first script

source("0 run first.R")

## ---------------------------

## Load packages

packages <- c("MonetDB.R", "sf")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

rm(packages)

## ---------------------------

## Source or list custom functions used within the script

## ---------------------------

## Load data

strataPolys <- sf::st_as_sf(RstoxBase::DefineStratumPolygon(FileName = "/Users/a22357/ownCloud/GadgetGhl/STOX/project/EggaNor_2019 - 3.0.11/output/baseline/DefineStratumPolygon/StratumPolygon.geojson")) %>%
  mutate(area = st_area(.) %>% units::set_units(nautical_mile^2))

## ---------------------------

## Connect to the database

con_db <- DBI::dbConnect(MonetDB.R::MonetDB(), host="localhost", dbname="bioticexplorer", user="monetdb", password="monetdb")

## Create the data objects

stnall <- dplyr::tbl(con_db, "stnall")

## Cruise series filtering

csList <- BioticExplorerServer::cruiseSeries %>%
  select(cruiseseriescode, name) %>%
  unique() %>% arrange(cruiseseriescode)

selCS <- csList[grepl("winter|ecosystem cruise in autumn|continental", csList$name, ignore.case = TRUE),]
# selCS <- csList[grepl("continental.*autumn", csList$name, ignore.case = TRUE),]
csFilt <- selCS$cruiseseriescode

## Filter

filtExp <- paste(sapply(csFilt, function(k) {paste0("cruiseseriescode %like% '", k, ",%' | cruiseseriescode %like% '%,", k, "' | cruiseseriescode %like% '%,", k, ",%'", " | cruiseseriescode %in% c('", k,"')")}), collapse = " | ")

y <- stnall %>%
  filter(
    !!!rlang::parse_exprs(filtExp),
    area %in% MainAreaFilter
  ) %>%
  collect() %>%
  select(where(~!all(is.na(.x))))

###############################
# Ghl biomass from surveys ####

ghl_survey_data <- y %>% filter(gearcategory == "Bottom trawls",
                    gearcondition %in% 1:2,
                    samplequality %in% 1,
                    !is.na(distance)) %>%
  group_by(startyear, cruise, cruiseseriescode, stationstartdate, bottomdepthstart, gear, gearname, serialnumber, longitudestart, latitudestart, distance) %>%
  summarise(biomass = sum(catchweight[commonname == "blåkveite"], na.rm = TRUE)) %>%
  mutate(cpue = biomass/(distance*(80/1852)),
         cruiseseriescode = recode(cruiseseriescode, `5` = "Winter", `6` = "Ecosystem", `16` = "EggaN", `25` = "EggaS")) %>%
  rename("cruiseseries" = "cruiseseriescode")

save(ghl_survey_data, file = "data/out/Survey data.rda")

## Scrap code, move

ghl_survey_data %>% filter(cruiseseries == "EggaN") %>%
  ggplot(., aes(x = bottomdepthstart, y = latitudestart, z = cpue)) +
  stat_summary_hex(bins = 20, fun = "mean") +
  scale_fill_distiller(name = expression(paste("Mean\nCPUE (kg ", nmi^-2, ")")), palette = "Spectral", na.value = "white", oob = scales::squish) +
  scale_y_continuous(breaks = seq(60,80, 2))

ghl_survey_data %>% filter(cruiseseries == "EggaS") %>%
  ggplot(., aes(x = bottomdepthstart, y = latitudestart, z = cpue)) +
  stat_summary_hex(bins = 10, fun = "mean") +
  scale_fill_distiller(name = expression(paste("Mean\nCPUE (kg ", nmi^-2, ")")), palette = "Spectral", na.value = "white", oob = scales::squish) +
  scale_y_continuous(breaks = seq(60,80, 2))



##############################
# Survey index comparison ####

out <- y %>% filter(gearcategory == "Bottom trawls",
                    gearcondition %in% 1:2,
                    samplequality %in% 1,
                    !is.na(distance)) %>%
  group_by(startyear, serialnumber, longitudestart, latitudestart, distance) %>%
  summarise(biomass = sum(catchweight[commonname == "blåkveite"], na.rm = TRUE)) %>%
  mutate(cpue = biomass/(distance*(80/1852))) %>%
  st_as_sf(coords = c("longitudestart", "latitudestart"), crs = 4326) %>%
  st_join(strataPolys[, "area"]) %>%
  filter(!is.na(area)) %>%
  st_set_geometry(NULL) %>%
  group_by(startyear, area) %>%
  mutate(area = as.numeric(area)) %>%
  summarise(mean = mean(cpue), sd = sd(cpue), n = n()) %>%
  mutate(index = area*mean/1e6) %>%
  group_by(startyear) %>%
  summarise(value = sum(index, na.rm = TRUE)) %>%
  mutate(index = "BioticExplorerServer\n& tidyverse") %>%
  rename(year = startyear)

load("data/out/Survey index data old.rda")

official <- rbind(NoSlope.biomass.f, NoSlope.biomass.m) %>%
  group_by(year) %>%
  summarise(value = sum(number)) %>%
  mutate(index = "Official (current AFWG model)")

load("data/out/Survey index data new.rda")

mfdb_dt <- bind_rows(lapply(EggaN, function(k) {
  k$station_length %>% group_by(year) %>%
    summarise(value = sum(Biomass, na.rm = TRUE)/1e9) %>%
    add_column(index = "Stox (station\nbased for MFDB)")
}))

stox <- bind_rows(lapply(EggaN, function(k) {
  k$bs_biomass_year %>% group_by(year) %>%
    summarise(value = sum(Biomass_sum_mean, na.rm = TRUE)/1e9, ci.min = sum(`Biomass_sum_5%`, na.rm = TRUE)/1e9, ci.max = sum(`Biomass_sum_95%`, na.rm = TRUE)/1e9) %>%
    add_column(index = "Stox (bootstrapped)")
}))

out <- bind_rows(out, official, mfdb_dt, stox)


png("figures/Survey index comparison EggaN.png", width = pagewidth_in, height = pagewidth_in*0.7, units = "in", res = 300)
ggplot(out, aes(x = year, y = value, color = index)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1990, 2020, 5), limits = c(1994, 2020),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Year", y = "Biomass (1000 tons)", color = "Index") +
  # expand_limits(y = 0) +
  coord_cartesian(ylim = c(40, 110)) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.margin = unit(c(5.5,12,5.5,5.5), "pt"))
dev.off()

png("figures/Survey index comparison EggaN for presentation.png", width = pagewidth_in, height = pagewidth_in*0.7, units = "in", res = 300)
ggplot() +
  geom_ribbon(data = stox,
              aes(x = year, ymin = ci.min, ymax = ci.max), color = NA, fill = "grey80", alpha = 0.5) +
  geom_line(data = out %>% filter(index %in% c("BioticExplorerServer\n& tidyverse", "Stox (bootstrapped)")),
          aes(x = year, y = value, linetype = index)) +
  scale_x_continuous(breaks = seq(1990, 2020, 5), limits = c(1994, 2020),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Year", y = "Biomass (1000 tons)", color = "Index") +
  # expand_limits(y = 0) +
  coord_cartesian(ylim = c(40, 95)) +
  scale_linetype("Method", labels = c("Simple calculus", "StoX")) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom",
        plot.margin = unit(c(5.5,12,5.5,5.5), "pt"))
dev.off()
