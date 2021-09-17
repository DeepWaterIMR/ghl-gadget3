## ---------------------------
##
## Script name: NOTE: THIS FILE IS NOT FINISHED YET
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

lw.dat[is.na(lw.dat$sex) & lw.dat$length <= 25, "sex"] <- sample(c("F", "M"), nrow(lw.dat[is.na(lw.dat$sex) & lw.dat$length <= 25,]), replace = TRUE) # Randomly allocate sex for fish with missing sex information and under 25 cm.

lw.dat <- lw.dat %>% filter(!is.na(sex))

lw_constants <- lw.dat %>%
  split(.$sex) %>%
  purrr::map(~nls(I(weight)~a*length^b,., start = list(a =1e-6,b=3))) %>%
  purrr::map(broom::tidy) %>%
  purrr::map(pull, estimate)

lw_constants <- lapply(lw_constants, function(k) {names(k) <- c("a", "b"); k})

#
#   lm(log(weight)~log(length),.) %>%
#   broom::tidy() %>%
#   pull(estimate)
#
# ## Transport back to right dimension
# names(lw_constants) <- c("a", "b")
# lw_constants[1] <- exp(lw_constants[1])

## Control plot ####

png(file.path(base_dir, "figures/Length_weight_relationship.png"), width = pagewidth, height = pagewidth, units = "mm", res = 300)

tmp <- bind_rows(tibble(length = 0:max(lw.dat[lw.dat$sex == "F", "length"]),
                        weight = lw_constants[[1]][[1]]*length^lw_constants[[1]][[2]],
                        sex = "F"),
                 tibble(length = 0:max(lw.dat[lw.dat$sex == "M", "length"]),
                        weight = lw_constants[[2]][[1]]*length^lw_constants[[2]][[2]],
                        sex = "M")
)

tmp2 <- bind_rows(lapply(seq_along(lw_constants), function(i) {
  out <- data.frame(matrix(lw_constants[[i]], ncol = 2))
  names(out) <- names(lw_constants[[i]])
  out$sex <- names(lw_constants[i])
  out
}))

p1 <- ggplot() +
  geom_point(data = lw.dat, aes(x = length, y = weight, color = sex), shape = 21, alpha = 0.4, size = 0.5) +
  facet_wrap(~sex) +
  annotate("line", x = tmp[tmp$sex == "F",]$length, y = tmp[tmp$sex == "F",]$weight, color = "tomato4") +
  annotate("line", x = tmp[tmp$sex == "M",]$length, y = tmp[tmp$sex == "M",]$weight, color = "dodgerblue4") +
  geom_text(data = tmp2,
            aes(x = 0, y = Inf,
                label = paste("a (kg) =", round(a, 9),
                              "\na (g) =", round(a*1000, 6),
                              "\nb =", round(b, 3))),
            vjust = 1, hjust = 0) +
  scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
  labs(x = "Length (cm)", y = "Weight (kg)") +
  theme(legend.position = "none")

p2 <- ggplot() +
  geom_point(data = lw.dat, aes(x = log(length), y = log(weight), color = sex), shape = 21, alpha = 0.4, size = 0.5) +
  facet_wrap(~sex) +
  annotate("line", x = log(tmp[tmp$sex == "F",]$length), y = log(tmp[tmp$sex == "F",]$weight), color = "tomato4") +
  annotate("line", x = log(tmp[tmp$sex == "M",]$length), y = log(tmp[tmp$sex == "M",]$weight), color = "dodgerblue4") +
  scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
  theme(legend.position = "none")

print(cowplot::plot_grid(p1, p2, labels = "auto", ncol = 1))
dev.off()

rm(lw.dat, tmp, tmp2, p1, p2)

########################################################
## Initial conditions sigma (of mean length by age) ####

age_dat <- mfdb_dplyr_sample(mdb) %>%
  dplyr::filter(data_source == "ldist-surveys-NOR",
                !is.na(age)) %>%
  dplyr::select(age,length) %>%
  dplyr::collect()

mean_lengths <- age_dat %>%
  dplyr::group_by(age) %>%
  dplyr::summarise(ml = mean(length, na.rm = TRUE), ms = sd(length, na.rm = TRUE), n = n())

mod_sigma <- lm(ms ~ age, data = mean_lengths[mean_lengths$n > 5,])
mod_mean <- fishmethods::growth(size = age_dat$length, age = age_dat$age, Sinf = max(age_dat$length), K = 0.1, t0 = 0, graph = FALSE)$vout

init_sigma <- tibble(
  age = stock_params$minage:stock_params$maxage,
  ms = predict(mod_sigma, data.frame(age = stock_params$minage:stock_params$maxage))
)

## Control plots

tmp <- init_sigma %>% rename("ms_mod" = "ms") %>% full_join(mean_lengths, by = "age") %>%
  mutate(ml_mod = predict(mod_mean, data.frame(age = min(age):max(age))))
tmp2 <- purrr::pmap_df(tmp, ~ tibble(length = stock_params$minlength:stock_params$maxlength, age = ..1, density = dnorm(length, ..3, ..4)))
tmp[is.na(tmp$ml),"ml"] <- tmp[is.na(tmp$ml),"ml_mod"]

tmp3 <- purrr::pmap_df(tmp, ~ tibble(length = stock_params$minlength:stock_params$maxlength, age = ..1, density = dnorm(length, ..3, ..2)))

png(file.path(base_dir, "figures/Initial sigma.png"), width = pagewidth*1.5, height = pagewidth, units = "mm", res = 300)
  p <- ggplot() +
    geom_density(data = age_dat, aes(x = length, color = "Data", fill = "Data")) +
    geom_line(data = tmp2, aes(x = length, y = density, color = "Mean")) +
    geom_line(data = tmp3, aes(x = length, y = density, color = "Modeled")) +
    scale_fill_manual("Fill", values = "grey") +
    scale_color_manual("Color", values = c("black", "red", "blue")) +
    facet_wrap(~age, scales = "free_y") +
    labs(x = "Length (cm)", y = "Density")

  suppressWarnings(print(p))
dev.off()
# Density distribution of lengths for each age group in data (grey) together with normal distributions using mean values (red) and linearly modeled standard deviations (blue). Modeled standard deviations were further used in the Gadget model to set initial stock age-length distributions.

png(file.path(base_dir, "figures/Length at age.png"), width = pagewidth*1.5, height = pagewidth, units = "mm", res = 300)
p <- ggplot() +
  geom_vline(data = tmp, aes(xintercept = ml), color = "grey", size = 2) +
  geom_errorbarh(data = tmp, aes(xmin = ml - ms_mod, xmax = ml + ms_mod, y = 0), color = "grey", size = 2) +
  geom_freqpoly(data = age_dat, aes(x = length)) +
  facet_wrap(~age)  +
  labs(x = "Length (cm)", y = "Count")
suppressMessages(print(p))
dev.off()

rm(age_dat, tmp, tmp2, tmp3, p)

# Split initial sigma by sex?

##############################
## Initial maturity ogive ####

mat_l50 <- plot.maturity(filter.exp = 'data_source == "ldist-surveys-NOR"', plot = FALSE)

#############################
# Save the required data ####

save(lw_constants, init_sigma, mat_l50, file = file.path(base_dir, "data/Initial stock parameters.rda"))
