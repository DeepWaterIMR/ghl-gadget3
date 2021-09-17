## ---------------------------
##
## Script name:
##
## Purpose of script:
##
## Author: Mikko Vihtakari // Institute of Marine Research, Norway
## Email: mikko.vihtakari@hi.no
##
## Date Created: 2021-03-02
##
## ---------------------------

## Source the run first script

source("0 run first.R")

## ---------------------------

## Load packages

library(RstoxFramework)
library(RstoxUtils)

## ---------------------------

## Source or list custom functions used within the script

## ---------------------------

## Read data

## ---------------------------


## Definitions

MainAreaFilter <- c(0:7, 10:18, 20:27, 30, 34:39, 50) # ICES areas 1 and 2

## Plot

load("data/out/Landings data old.rda")
load("data/out/Landings data new.rda")

## ####

tmp <- landings %>%
  filter(nation == "NOR" & (gear_cat %in% c(1,5) | gear_id == 61) & year > 1991) %>%
  group_by(year) %>%
  summarise(TrawlNor = sum(mass)/1e6)

tmp2 <- landings %>%
  filter(nation == "NOR" & !(gear_cat %in% c(1,5) | gear_id == 61) & year > 1991) %>%
  group_by(year) %>%
  summarise(OtherNor = sum(mass)/1e6)

tmp <- merge(tmp, tmp2, all = TRUE)

tmp2 <- data.frame(year = hist_land$Year, HistNor = hist_land$Norway/1e3) %>% filter(year <= 1991)

tmp <- merge(tmp, tmp2, all = TRUE)

tmp2 <- afwg_gadget_landings %>%
  filter(fleet == "trawl_ru") %>%
  group_by(year) %>%
  summarize(TrawlRus = sum(value)/1e6)

tmp <- merge(tmp, tmp2, all = TRUE)

tmp2 <- afwg_gadget_landings %>%
  filter(fleet == "gil_ru") %>%
  group_by(year) %>%
  summarize(OtherRus = sum(value)/1e6)

tmp <- merge(tmp, tmp2, all = TRUE)

tmp2 <- data.frame(year = hist_land$Year, HistRus = hist_land$Russia/1e3) %>% filter(year <= 1991)

tmp <- merge(tmp, tmp2, all = TRUE)

tmp2 <- data.frame(year = hist_land$Year, Internat = hist_land$Others/1e3)

sugg_land <- merge(tmp, tmp2, all = TRUE) %>% rename("Year" = "year")

ggplot(data = reshape2::melt(sugg_land, id = 1) %>% replace_na(list(value = 0)),
       aes(x = Year, y = value, fill = variable)
) +
  # geom_col() +
  geom_area(position = position_stack(reverse = TRUE)) +
  scale_x_continuous("Year", expand = c(0, 0), n.breaks = 20) +
  scale_y_continuous("Landings (1000 t)", expand = c(0, 0)) +
  scale_fill_manual("Fleet", values = cols) +
  theme(legend.position = "bottom")

## ####

afwg_gadget_landings %>%
  filter(fleet %in% c("trawl_ru", "gil_ru")) %>%
  group_by(year, fleet) %>%
  summarize(value = sum(value)/1e6) %>%
  tidyr::pivot_wider(names_from = fleet, values_from = value) %>%
  mutate(ratio = 100*trawl_ru/(gil_ru + trawl_ru)) %>%
  ggplot(., aes(x = year, y = ratio)) +
  geom_path(color = cols[3]) +
  scale_y_continuous("Percentage of trawl catches of Russian landings") +
  scale_x_continuous("Year", n.breaks = 10)


## ####




tmp <- landings %>%
  filter(!nation %in% c("NOR", "RUS")) %>%
  group_by(year) %>%
  summarise(Database = sum(mass)/1e6)

tmp2 <- data.frame(year = hist_land$Year, Historical = hist_land$Others/1e3)

tmp <- merge(tmp, tmp2, all = TRUE)
tmp <- tmp[rowSums(tmp[-1], na.rm = TRUE) > 0,]

ggplot(reshape2::melt(tmp[names(tmp) != "diff"], id = 1), aes(x = year, y = value, color = variable)) +
  geom_path() +
  scale_y_continuous("Catch (t)") +
  scale_x_continuous("Year", limits = range(tmp$year)) +
  labs(color = "Source") +
  theme_bw() +
  theme(legend.position = "top")

tmp %>%
  na.omit() %>%
  filter(abs(diff) > 0.01) %>%
  mutate(diff = round(diff*1000, 0), New = round(New, 3), Old = round(Old, 3)) %>%
  rename("Year" = "year", "Difference (t)" = "diff") %>%
  knitr::kable(caption = 'The sum of annual Norwegian longline and gillnet landings acquired using the new approach differing more than by 10 t from gil_no landings used in the current assessment model. The year 2019 was not available in the model.') %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "condensed"))

landings %>%
  filter(nation == "NOR") %>%
  mutate(gear_category =
           recode(gear_id, !!! setNames(RstoxUtils::FDIRcodes$gearCodes$gearCategory, RstoxUtils::FDIRcodes$gearCodes$idGear))
  ) %>%
  mutate(gear_category = recode(gear_category, "Skytevaapen" = "Annet", "Ruser" = "Annet")) %>%
  mutate_at(vars(gear_category), factor, levels = c("Traal", "Noter", "Kroker", "Garn", "Annet")) %>%
  mutate_at(vars(year), factor) %>%
  group_by(gear_category, year, .drop = FALSE) %>%
  summarise(sum = sum(mass, na.rm = T)/1e6) %>%
  mutate_at(vars(year), function(x) as.integer(as.character(x))) %>%
  ggplot(., aes(x = year, y = sum, fill = gear_category)) +
  geom_area(position = position_stack(reverse = TRUE)) +
  scale_x_continuous("Year", expand = c(0, 0), n.breaks = 10) +
  scale_y_continuous("Landings (1000 t)", expand = c(0, 0)) +
  labs(fill = "Gear category") +
  theme(legend.position = "bottom")


landings %>%
  filter(nation == "NOR", year %in% (max(landings$year)-2):max(landings$year)) %>%
  mutate(gear_category =
           recode(gear_id, !!! setNames(FDIRcodes$gearCodes$gearCategory, FDIRcodes$gearCodes$idGear))
         ) %>%
  group_by(gear_category) %>%
  summarise(sum = sum(mass)) %>%
  mutate(pr = 100*sum/sum(sum)) %>%
  ggplot(., aes(x = gear_category, y = pr)) +
  geom_col() +
  labs(y = "Percentage of total Norwegian landings", x = "Gear category") +
  theme_classic()

landings %>%
  filter(nation == "NOR") %>%
  mutate(gear_category =
           recode(gear_id, !!! setNames(FDIRcodes$gearCodes$gearCategory, FDIRcodes$gearCodes$idGear))
  ) %>%
  group_by(gear_category, year) %>%
  summarise(sum = sum(mass)/1e6) %>%
  ggplot(., aes(x = year, y = sum, fill = gear_category)) +
  geom_area(position = position_stack(reverse = TRUE)) +
  scale_x_continuous("Year", expand = c(0, 0), n.breaks = 20) +
  scale_y_continuous("Landings (1000 t)", expand = c(0, 0)) +
  labs(fill = "Gear category") +
  theme_classic() +
  theme(legend.position = "bottom")


  geom_col() +
  labs(y = "Percentage of total Norwegian landings", x = "Gear category") +
  theme_bw()

  landings %>%
    filter(nation == "NOR") %>%
    mutate(gear_category =
             recode(gear_id,
                    !!!setNames(FDIRcodes$gearCodes$gearCategory,
                                FDIRcodes$gearCodes$idGear)
             ),
           gear_name =
             recode(gear_id,
                    !!!setNames(FDIRcodes$gearCodes$gearName,
                                FDIRcodes$gearCodes$idGear)
             )
    ) %>%
    filter(gear_category %in% c("Traal", "Noter")) %>%
    group_by(gear_name) %>%
    summarise(sum = sum(mass)/1e6) %>%
    mutate(pr = round(100*sum/sum(sum), 1)) %>%
    arrange(-sum) %>%
    rename("Gear name" = "gear_name", "Summed catch (1000 t)" = "sum", "Percentage of total" = "pr") %>%
    knitr::kable(caption = 'Summed catches by gear name in the Norwegian trawl and seine fleet.') %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "condensed"))

landings %>%
  group_by(nation) %>%
  summarise(sum = sum(mass)) %>%
  mutate(pr = round(100*sum/sum(sum), 1)) %>%
  arrange(-pr)


  mutate(gear_category =
           recode(gear_id, !!! setNames(FDIRcodes$gearCodes$gearCategory, FDIRcodes$gearCodes$idGear)),
         gear_name =
           recode(gear_id, !!! setNames(FDIRcodes$gearCodes$gearName, FDIRcodes$gearCodes$idGear))
  ) %>%
  group_by(gear_category)

tmp <- landings %>%
  filter(nation == "RUS") %>%
  group_by(year) %>%
  summarise(Database = sum(mass)/1e6)

tmp2 <- afwg_gadget_landings %>%
  filter(fleet %in% c("trawl_ru", "gil_ru")) %>%
  group_by(year) %>%
  summarize(Old = sum(value)/1e6)

tmp2 <- tmp2[!names(tmp2) %in% c("area", "fleet", "value")]

tmp3 <- na.omit(data.frame(year = hist_land$Year, Historical = hist_land$Russia/1e3))
tmp3 <- tmp3[tmp3$year >= min(tmp$year),]

tmp <- merge(tmp, tmp2, all = TRUE)
tmp <- merge(tmp, tmp3, all = TRUE)

ggplot(reshape2::melt(tmp[names(tmp) != "diff"], id = 1), aes(x = year, y = value, color = variable)) +
  geom_path() +
  scale_y_continuous("Catch (t)") +
  scale_x_continuous("Year", limits = range(tmp$year)) +
  labs(color = "Source") +
  theme_bw() +
  theme(legend.position = "top")


## Trawl, other nations

tmp <- landings %>% filter(!nation %in% c("NOR", "RUS") & gear_cat == 5) %>% group_by(year) %>% summarise(new = sum(mass)/1e6)


## Trawl

tmp <- landings %>% filter(nation == "NOR" & gear_cat == 5) %>% group_by(year) %>% summarise(new = sum(mass)/1e6)
tmp2 <- afwg_gadget_landings %>% filter(fleet == "trawl_no") %>% mutate(old = value/1e6)
tmp2 <- tmp2[!names(tmp2) %in% c("area", "fleet", "value")]

# tmp2 <- trawl_no %>% group_by(year) %>% summarise(old = sum(value)/1e3)

tmp <- merge(tmp, tmp2, all = TRUE)
tmp$diff <- tmp$old - tmp$new

p1 <- ggplot(reshape2::melt(tmp[-4], id = 1), aes(x = year, y = value, color = variable)) +
  geom_path() +
  scale_y_continuous("Catch (t)") +
  scale_x_continuous("Year") +
  theme_bw() +
  theme(legend.position = "top")

p2 <- ggplot(na.omit(tmp), aes(x = year, y = diff)) +
  geom_col() +
  scale_y_continuous("Difference (old - new; 1000 t)") +
  scale_x_continuous("Year") +
  theme_bw()

cowplot::plot_grid(p1, p2, ncol = 1)

## Norwegian

tmp <- landings %>% filter(nation == "NOR") %>% group_by(year) %>% summarise(New = sum(mass)/1e6)
tmp2 <- afwg_gadget_landings %>% filter(fleet %in% c("trawl_no", "gil_no")) %>% group_by(year) %>% summarise(Old = sum(value)/1e6)
tmp3 <- data.frame(year = hist_land$Year, Historical = hist_land$Norway/1e3)

tmp <- merge(tmp, tmp2, all = TRUE)
tmp <- merge(tmp, tmp3, all = TRUE)

p1 <- ggplot(reshape2::melt(tmp, id = 1), aes(x = year, y = value, color = variable)) +
  geom_path() +
  scale_y_continuous("Catch (t)") +
  scale_x_continuous("Year") +
  scale_color_discrete("Source") +
  theme_bw() +
  theme(legend.position = "top")

tmp <- reshape2::melt(tmp, id = c("year", "Historical"))
tmp$diff <- tmp$value - tmp$Historical
tmp <- tmp[!is.na(tmp$diff),]

p2 <- ggplot(tmp, aes(x = year, y = diff, color = variable)) +
  geom_path() +
  scale_y_continuous("Difference (variable - Historical; 1000 t)") +
  scale_x_continuous("Year") +
  theme_bw()

cowplot::plot_grid(p1, p2, ncol = 1)

## "Gillnet"

tmp <- landings %>% filter(nation == "RUS" & (gear_cat %in% c(1,5) | gear_id == 61)) %>% group_by(year) %>% summarise(New = sum(mass)/1e6)
tmp2 <- afwg_gadget_landings %>% filter(fleet == "gil_no") %>% mutate(Old = value/1e6)
tmp2 <- tmp2[!names(tmp2) %in% c("area", "fleet", "value")]

tmp <- merge(tmp, tmp2, all = TRUE)
tmp$diff <- tmp$Old - tmp$New

p1 <- ggplot(reshape2::melt(tmp[names(tmp) != "diff"], id = 1), aes(x = year, y = value, color = variable)) +
  geom_path() +
  scale_y_continuous("Catch (t)") +
  scale_x_continuous("Year", limits = range(tmp$year)) +
  labs(color = "Source") +
  theme_bw() +
  theme(legend.position = "top")

p2 <- ggplot(tmp[!is.na(tmp$diff),], aes(x = year, y = diff)) +
  geom_col() +
  scale_y_continuous("Difference (Old - New; 1000 t)") +
  scale_x_continuous("Year", limits = range(tmp$year)) +
  theme_bw()

cowplot::plot_grid(p1, p2, ncol = 1)





tmp2 <- gil_no %>% group_by(year) %>% summarise(total = sum(value)/1e3)

ggplot() +
  geom_path(data = tmp, aes(x = year, y = total)) +
  geom_path(data = tmp2, aes(x = year, y = total), color = "red") +
  ylab("Total catch ICES areas 1 and 2 (1000 tons)")

## Total
tmp <- landings %>% filter(nation == "NOR") %>% group_by(year) %>% summarise(total = sum(mass)/1e6)

ggplot() +
  geom_path(data = tmp, aes(x = year, y = total)) +
  geom_path(data = hist_land, aes(x = Year, y = Norway/1e3), color = "red") +
  ylab("Total catch ICES areas 1 and 2 (1000 tons)")


ggplot(tmp, aes(x = year, y = total)) + geom_col() + ylab("Total catch ICES areas 1 and 2 (1000 tons)")

## Gear_cat

## Nation
tmp <- landings
levels(tmp$nation)[!levels(tmp$nation) %in% c("NOR", "RUS")] <- "Other"
levels(tmp$nation)[levels(tmp$nation) == c("NOR")] <- "Norway"
levels(tmp$nation)[levels(tmp$nation) == c("RUS")] <- "Russia"

tmp <- tmp %>% group_by(year, nation) %>% summarise(total = sum(mass))

ggplot(tmp, aes(x = year, y = total, fill = nation)) + geom_col() + ylab("Total catch ICES areas 1 and 2 (1000 tons)")

landings %>% group_by(gear_cat) %>% summarise(total = sum(mass)/1e6)

##

tmp <- readxl::read_excel("data/in/landings/AFWG_2020_tables.xlsx", "8.7")

colindex <- list(start = which(tmp[1,] == "Year"), end = which(tmp[1,] == "Total"))

out <- lapply(1:2, function(i) {
  dt <- tmp[-1, select.element(colindex, i)[1]:select.element(colindex, i)[2]]
  names(dt) <- as.character(tmp[1, select.element(colindex, i)[1]:select.element(colindex, i)[2]])
  dt
})

out <- do.call(rbind, out)
out <- out[apply(out[2:5], 1, function(x) sum(is.na(x))) != 4,]
out$Year <- as.integer(gsub("\\*", "", out$Year))
out[2:5] <- lapply(out[2:5], as.numeric)

out$diff <- out$Total - rowSums(out[2:4], na.rm = TRUE)

out$Total <- rowSums(out[2:4], na.rm = TRUE)

hist_land <- out

rm(out, tmp)

ggplot(data = reshape2::melt(hist_land[-5], id = 1),
       aes(x = Year, y = value, fill = variable)
) +
  geom_area(position = position_stack(reverse = TRUE)) +
  scale_x_continuous("Year", expand = c(0, 0), n.breaks = 20) +
  scale_y_continuous("Landings (tonns)", expand = c(0, 0)) +
  scale_fill_manual("Country",
                    values = c("Norway" = "#4472C4", "Russia" = "#FFC000", "Others" = "#A5A5A5")
  ) +
  theme_classic() +
  theme(legend.position = "bottom")


dt2 <- tmp[-1, select.element(colindex, 2)[1]:select.element(colindex, 2)[2]]
names(dt2) <- as.character(tmp[1, select.element(colindex, 2)[1]:select.element(colindex, 2)[2]])


tmp <- try(mfdb())
tmp <- unlist(strsplit(tmp[1], "\\n"))
tmp <- trimws(gsub("\\*", "", tmp[grepl("\\*", tmp)]))

if(!"ghl" %in% tmp) {
mdb <- mfdb("ghl") # Setting up the database
# mfdb("ghl", destroy_schema = TRUE)
}





DBI::dbConnect(RPostgres::Postgres(dbname = "mf"))
con<-dbConnect(RPostgres::Postgres())

## Landings taxonomy

gear_code_conversion_table<-
  data.frame(IMRcode=3000:3199,gear="BMT",description="Bottom trawl") %>%
  bind_rows(data.frame(gearcode=3200:3299,gear="SHR",description="Shrimp trawl")) %>%
  bind_rows(data.frame(gearcode=3410:3412,gear="SPT",description="Semipelagic trawl")) %>%
  bind_rows(data.frame(gearcode=3500:3599,gear="PEL",description="Pelagic trawl")) %>%
  bind_rows(data.frame(gearcode=3600:3699,gear="DSN",description="Danish seine")) %>%
  bind_rows(data.frame(gearcode=3700:3799,gear="SEN",description="Seine")) %>%
  bind_rows(data.frame(gearcode=4000:4199,gear="GLN",description="Gillnets")) %>%
  bind_rows(data.frame(gearcode=4300:4399,gear="FNT",description="Fyke nets/Ruser")) %>%
  bind_rows(data.frame(gearcode=5100:5199,gear="LIN",description="Lines")) %>%
  bind_rows(data.frame(gearcode=5200:5299,gear="HLN",description="Handlines")) %>%
  bind_rows(data.frame(gearcode=5300:5399,gear="PTS",description="Pots/Teiner")) %>%
  bind_rows(data.frame(gearcode=c(3400:3409,3413:3499,4200:4299,4400:5099,6000:6699),gear="UNS",description="Unspesified"))




tmp <- RstoxUtils::FDIRcodes$gearCodes %>%
  rename("gear_id" = "idGear", "description" = "gearName")

%>%

  mfdb_empty_taxonomy(mdb,"gear")

RstoxUtils::FDIRcodes$gearCodes %>%
  rename("gear_id" = "idGear", "description" = "gearName", "t_group" = "gearCategory") %>%
  mutate(name = gear_id) %>%
  mfdb_import_gear_taxonomy(mdb,.)


gear_code_conversion_table %>%
  select(-gearcode,name=gear) %>%
  distinct() %>%
  mutate(t_group=NA) %>%
  mfdb_import_gear_taxonomy(mdb,.)

tbl(mdb$db,"gear")  # View






x$main_area <- factor(as.numeric(as.character(x$main_area)), levels = sort(unique(as.numeric(as.character(x$main_area)))))

x <- x[x$main_area %in% mainAreas,]

kveiteLandingsXML <- x

bla <- ReadBiotic("/Users/a22357/Desktop/EggaN2019.xml")
da <- StoxBiotic(bla)


ga <- LengthDistribution(da)
ma <- ga[ga$SpeciesCategory %in% "blåkveite/172930/127144/Reinhardtius hippoglossoides",]
