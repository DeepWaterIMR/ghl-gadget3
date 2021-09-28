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

###############
## Landings ###

# if(exists("mdb")) { # Replace this with a possibility to load data from base_dir

### TrawlNor

## A problem for ldist data allocation to sexes: most of the data do not contain sex information! Therefore, not done in the current model. Must be solved in another way.
# mfdb_dplyr_sample(mdb) %>% filter(data_source == "ldist-landings-NOR") %>% group_by(sex) %>% count() %>% mutate(pr = 100*(n/sum(n, na.rm = TRUE))) %>% collect()

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
)[[1]]


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
               by = stock_params$dl),
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
)[[1]]

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


##############
## Surveys ###

### NoSlope (EggaN)

# mfdb_dplyr_sample(mdb) %>% filter(sampling_type == "ENS") %>% group_by(gear) %>% count() %>% collect()

EggaN_ldist <- mfdb_sample_count(
  mdb,
  cols = c("age", "length"),
  params =
    list(sampling_type = "ENS",
         gear = "BottomTrawls",
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
)[[1]]

png(file.path(base_dir, "figures/EggaN_ldist.png"), width = pagewidth, height = pagewidth*1.5, units = "mm", res = 300)
print(plot.ldist(EggaN_ldist))
dev.off()

EggaN_aldist_female <- mfdb_sample_count(
  mdb,
  cols = c("age", "length"),
  params =
    list(sampling_type = "ENS",
         gear = "BottomTrawls",
         year = model_params$year_range,
         sex = "F",
         timestep = model_params$timestep_fun,
         age = mfdb_interval(
           "age", stock_params$minage:stock_params$maxage,
           open_ended = c("upper")
         ),
         length = mfdb_interval(
           "len",
           seq(stock_params$minlength, stock_params$maxlength,
               by = stock_params$dl),
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
         year = model_params$year_range,
         sex = "M",
         timestep = model_params$timestep_fun,
         age = mfdb_interval(
           "age", stock_params$minage:stock_params$maxage,
           open_ended = c("upper")
         ),
         length = mfdb_interval(
           "len",
           seq(stock_params$minlength, stock_params$maxlength,
               by = stock_params$dl),
           open_ended = c("upper","lower")
         )
    )
)[[1]]

png(file.path(base_dir, "figures/EggaN_aldist_male.png"), width = pagewidth, height = pagewidth*2, units = "mm", res = 300)
print(plot.adist(EggaN_aldist_male))
dev.off()

############################
## Maturity proportions ####

png(file.path(base_dir, "figures/Maturity_ogive.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
print(plot.maturity('sampling_type == "ENS"'))
dev.off()

EggaN_mat <- mfdb_concatenate_results(
  mfdb_sample_count(
    mdb,
    c('maturity_stage','age','length'),
    list(
      length =
        mfdb_interval('len',
                      seq(stock_params$minlength, stock_params$maxlength,
                          by = 2*stock_params$dl),
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
    c('maturity_stage','age','length'),
    list(
      length =
        mfdb_interval('len',
                      seq(stock_params$minlength, stock_params$maxlength,
                          by = 2*stock_params$dl),
                      open_ended = c('lower','upper')),
      maturity_stage = mfdb_group(female_imm = 1:2, female_mat = 3:5),
      sex = "F",
      data_source = "ldist-surveys-NOR",
      sampling_type = "ENS",
      timestep = model_params$timestep_fun,
      year = model_params$year_range
    ))[[1]]
)

png(file.path(base_dir, "figures/Maturity_data.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
print(plot.mat(EggaN_mat))
dev.off()

png(file.path(base_dir, "figures/Maturity_data_proportions.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
print(plot.matp(EggaN_mat))
dev.off()


# Save

save(TrawlNor_ldist, AllNorLandings_aldist, OtherNor_ldist, EggaN_ldist, EggaN_aldist_female, EggaN_aldist_male, EggaN_mat, file = file.path(base_dir, "data/Catch distributions to Gadget.rda"))
# } else {
#   load(file.path(base_dir, "data/Survey indices to Gadget.rda"))
# }
