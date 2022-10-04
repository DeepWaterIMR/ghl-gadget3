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

  ## Source or list custom functions used within the script

  source("R/figure_functions.R")

  ## ---------------------------

  ## ---------------------------

  ##################################
  ## Weight length relationship ####

  lw.dat <- mfdb_dplyr_sample(mdb) %>%
    filter(data_source == "ldist-surveys-NOR", # only survey lengths
           !is.na(weight)) %>%
    select(length, weight, sex) %>%
    collect()

  # Randomly allocate sex for fish with missing sex information and under 25 cm.
  lw.dat[is.na(lw.dat$sex) & lw.dat$length <= 25, "sex"] <-
    sample(c("F", "M"),
           nrow(lw.dat[is.na(lw.dat$sex) & lw.dat$length <= 25,]),
           replace = TRUE)

  lw.dat <- lw.dat %>% filter(!is.na(sex))

  invisible(capture.output({
    lw <- ggFishPlots::plot_lw(lw.dat, split.by.sex = TRUE, use.nls = TRUE)}))

  lw_constants <- lw$params

  ## Control plot ####

  png(file.path(base_dir, "figures/Length_weight_relationship.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(lw$plot)
  dev.off()

  rm(lw.dat, lw)

  ########################################################
  ## Initial conditions sigma (of mean length by age) ####

  age_dat <- mfdb_dplyr_sample(mdb) %>%
    dplyr::filter(data_source == "ldist-surveys-NOR",
                  !is.na(age)) %>%
    dplyr::select(age, sex, maturity_stage, length) %>%
    dplyr::collect() %>%
    mutate(maturity = as.integer(maturity_stage >= 3)) %>%
    dplyr::select(-maturity_stage)

  ## Growth curve

  growth <- ggFishPlots::plot_growth(age_dat, split.by.sex = TRUE)

  png(file.path(base_dir, "figures/Growth_curve.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  print(growth$plot)
  dev.off()

  von_b_params <- growth$params

  rm(growth)

  ## Sigmas (these need likely be split to sex)

  mean_lengths <- age_dat %>%
    dplyr::group_by(age) %>%
    dplyr::summarise(ml = mean(length, na.rm = TRUE), ms = sd(length, na.rm = TRUE), n = n())

  mod_sigma <- lm(ms ~ age, data = mean_lengths[mean_lengths$n > 5,])
  mod_mean <- fishmethods::growth(size = age_dat$length, age = age_dat$age, Sinf = max(age_dat$length), K = 0.1, t0 = 0, graph = FALSE)$vout

  init_sigma <- tibble(
    age = stock_params$minage:stock_params$maxage,
    ms = predict(mod_sigma, data.frame(age = stock_params$minage:stock_params$maxage)),
    ml = predict(mod_mean, data.frame(age = stock_params$minage:stock_params$maxage))
  )

  init_sigma_coef <-
    init_sigma %>%
    filter(age > 3 & age < 20) %>%
    lm(I(ms/ml)~I(1/age) + age, data = .) %>%
    coefficients() %>%
    setNames(c('alpha', 'beta', 'gamma'))

  ## Control plots

  tmp <- init_sigma %>%
    rename("ms_mod" = "ms", "ml_mod" = "ml") %>%
    full_join(mean_lengths, by = "age")

  tmp2 <- purrr::pmap_df(tmp, ~ tibble(length = stock_params$minlength:stock_params$maxlength, age = ..1, density = dnorm(length, ..4, ..5)))
  tmp[is.na(tmp$ml),"ml"] <- tmp[is.na(tmp$ml),"ml_mod"]

  tmp3 <- purrr::pmap_df(tmp, ~ tibble(length = stock_params$minlength:stock_params$maxlength, age = ..1, density = dnorm(length, ..3, ..2)))

  png(file.path(base_dir, "figures/Initial_sigma.png"), width = pagewidth*1.5, height = pagewidth, units = "mm", res = 300)
  p <- ggplot() +
    facet_wrap(~age, scales = "free_y") +
    geom_density(data = age_dat, aes(x = length, color = "Data", fill = "Data")) +
    geom_line(data = tmp2, aes(x = length, y = density, color = "Mean")) +
    geom_line(data = tmp3, aes(x = length, y = density, color = "Modeled")) +
    scale_fill_manual("Fill", values = "grey") +
    scale_color_manual("Color", values = c("black", "red", "blue")) +
    labs(x = "Length (cm)", y = "Density")

  suppressWarnings(print(p))
  dev.off()
  # Density distribution of lengths for each age group in data (grey) together with normal distributions using mean values (red) and linearly modeled standard deviations (blue). Modeled standard deviations were further used in the Gadget model to set initial stock age-length distributions.

  png(file.path(base_dir, "figures/Length_at_age.png"), width = pagewidth*1.5, height = pagewidth, units = "mm", res = 300)
  p <- ggplot() +
    geom_vline(data = tmp, aes(xintercept = ml), color = "grey", size = 2) +
    geom_errorbarh(data = tmp, aes(xmin = ml - ms_mod, xmax = ml + ms_mod, y = 0), color = "grey", size = 2) +
    geom_freqpoly(data = age_dat, aes(x = length)) +
    facet_wrap(~age)  +
    labs(x = "Length (cm)", y = "Count")
  suppressWarnings(suppressMessages(print(p)))
  dev.off()

  # Split initial sigma by sex?

  ###############################
  ## Initial maturity ogives ####

  # Should we use all survey data ('data_source == "ldist-surveys-NOR"') or only EggaN ('sampling_type == "ENS"') for maturity estimation? Applies also to 2-4 catchdistribution

  mat_dat <- mfdb_dplyr_sample(mdb) %>%
    filter(!is.na(length), !is.na(sex), !is.na(maturity_stage)) %>%
    filter(data_source == "ldist-surveys-NOR") %>%
    select(year, month, areacell, age, sex, maturity_stage, length) %>%
    collect() %>%
    mutate(maturity = as.integer(maturity_stage >= 3))

  mat_l50 <- ggFishPlots::plot_maturity(mat_dat, split.by.sex = TRUE)

  png(file.path(base_dir, "figures/Maturity_ogive_by_length.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  suppressMessages(print(mat_l50$plot))
  dev.off()

  mat_l50 <- mat_l50$params

  mat_a50 <- ggFishPlots::plot_maturity(
    age_dat, length = "age", split.by.sex = T, xlab = "Age",
    length.bin.width = 1, length.unit = "years")

  png(file.path(base_dir, "figures/Maturity_ogive_by_age.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)
  suppressMessages(print(mat_a50$plot))
  dev.off()

  mat_a50 <- mat_a50$params

  #############################
  # Save the required data ####

  save(lw_constants, init_sigma, mat_l50, mat_a50, von_b_params, file = file.path(base_dir, "data/Initial stock parameters.rda"))

  rm(age_dat, tmp, tmp2, tmp3, p)

} else { ## !reload_data case
  load(file.path(base_dir, "data/Initial stock parameters.rda"))
}
