## ---------------------------
##
## Script name: Projection figures and tables
##
## Purpose of script: Make figures and reference point table from projection results
##
## ---------------------------
## 
## Custom functions

### Quantile function
quantile_df <- function(x, scale = 1, probs = c(0.05,0.25, 0.5, 0.75, 0.95)) {
  tibble(
    value = quantile(x/scale, probs, na.rm = TRUE),
    prob = probs
  )
}

### Calculate fbar from a fit object
fbar_func <- function(fit, fbar_ages){
  fit$stock.prey %>% 
    filter(age %in% fbar_ages) %>% 
    group_by(year,age) %>% 
    summarise(c=sum(number_consumed), 
              n=sum(number[step==1]),
              catch_mass = sum(biomass_consumed)) %>% 
    mutate(f=-log(1-c/n)) %>% 
    group_by(year) %>% 
    summarise(fbar = mean(f, na.rm = TRUE), 
              catch_n = sum(c, na.rm = TRUE),
              catch_mass = sum(catch_mass, na.rm = TRUE))
}
# ggplot(fbar_func(fit), aes(x = year, y = fbar)) + geom_path()

## Read data

## Variables and definitions

f_round <- 3
rp_cols <- c("Fmsy" = "#449BCF", "MSY" = "#056A89", "Fpa" = "#D696C8", "Blim" = "#82C893", "Bpa" = "#FF5F68")

# Reference point calculus

## Precautionary reference points
res_pre <- 
  results_pre %>% 
  filter(year > (max(year) -50), step == 1, hr_target <= 0.3, stock == 'ghl_female_mat') %>% 
  group_by(hr_target) %>% 
  reframe(f = round(mean(fbar), digits = f_round),
          quantile_df(biomass, 1e3)) 

### Flim
Elim <- 
  res_pre %>% 
  filter(prob == 0.5, value > local(blim/1e3)) %>% 
  filter(hr_target == max(hr_target)) %>% 
  rename(hr_lim = hr_target, flim = f)

## MSY reference points

## Yield curve no btrigger
yield_dat <- 
  results_msy_nobtrigger %>% 
  filter(year > (max(year) -50), hr_target <= 0.3) %>% 
  group_by(hr_target, year, trial) %>% 
  summarise(y = sum(catch), 
            ssb = mean(biomass[stock == 'ghl_female_mat']),
            f = mean(fbar[step == 1])) %>% 
  group_by(hr_target) %>% 
  reframe(f = round(mean(f), digits = f_round),
          ssb = mean(ssb)/1e3,
          quantile_df(y, 1e3)) 
#  ggplot(yield_dat %>% filter(prob == 0.5), aes(x = ssb, y = value)) + geom_point()

## Median yield
Emsy <- yield_dat %>% filter(prob == 0.5) %>% filter(value == max(value))

Emsy_range <- yield_dat %>% 
  filter(prob == 0.5) %>% 
  filter(value > 0.95*max(value)) %>% 
  filter(hr_target == min(hr_target) | hr_target == max(hr_target))

## SSB quantiles from yield_dat
ssb_dat <- 
  results_msy %>% 
  filter(year > (max(year) -50), step==1, hr_target <= 0.3) %>% 
  group_by(year, hr_target, trial) %>% 
  summarise(ssb = mean(biomass[stock == 'ghl_female_mat']), 
            f = mean(fbar[step == 1])) %>% 
  group_by(hr_target) %>% 
  reframe(f = round(mean(f), digits = f_round),
          quantile_df(ssb, 1e3)) 
# ggplot(ssb_dat %>% filter(prob == 0.5), aes(x = f, y = value)) + geom_point()

Epa <- 
  ssb_dat %>% 
  filter(prob == 0.05) %>% 
  filter(value > blim/1e3) %>% 
  filter(hr_target == max(hr_target))

if(nrow(Epa) == 0) {
  Epa <- tibble(hr_target = 0, f = 0, value = 0, prob = 0.05)
}

Pbref <- 
  results_msy %>% 
  filter(year > (max(year) -50), step==1, hr_target <= 0.3) %>% 
  group_by(year, hr_target, trial) %>% 
  summarise(ssb = mean(biomass[stock == 'ghl_female_mat']), 
            f = mean(fbar[step == 1])) %>% 
  group_by(hr_target) %>% 
  summarise(f = round(mean(f), digits = f_round),
            pbpa = mean(ssb < bpa * 1e3),
            pblim = mean(ssb < blim * 1e3))

##############
## Tables ####

rp_tab <- tibble::tibble(
  `Reference point` = 
    c("Blim", "Bpa", "Btrigger", "MSY", "Flim", "Fmsy", "Fpa", "HRlim", "HRmsy", "HRpa"),
  Value = 
    c(blim, bpa, btrigger, Emsy$value, Elim$flim, Emsy$f, Epa$f, Elim$hr_lim, Emsy$hr_target, Epa$hr_target),
  Basis = c("Lowest modelled mature female substock biomass",
            "Blim x 1.4",
            "Bpa",
            "Maximum sustainable yield",
            "F leading to P(SSB < Blim) = 0.5",
            "F leading to MSY",
            "F, when ICES AR is applied, leading to P(SSB > Blim) = 0.05",
            "Harvest rate (HR) leading to P(SSB < Blim) = 0.5",
            "HR leading to MSY",
            "HR, when ICES AR is applied, leading to P(SSB > Blim) = 0.05"
  )
)


###############
## Figures ####

x_axis1 <- tibble(RP = c("Fmsy", "Fpa"), 
                  value = c(Emsy$f, Epa$f))
x_axis2 <- tibble(RP = c("Fmsy"), 
                  type = c("min", "max"),
                  value = c(Emsy_range$f)) %>% 
  pivot_wider(names_from = type, values_from = value)

y_axis1 <- tibble(RP = c("MSY"), value = c(Emsy$value))
y_axis2 <- tibble(RP = c("Blim", "Bpa"), value = c(blim, bpa))

p1 <- yield_dat %>% 
  filter(prob==0.5) %>% 
  ggplot(aes(f, value)) + 
  geom_ribbon(data = yield_dat %>%
                mutate(value = value) %>% 
                filter(prob %in% c(0.05,0.5,0.95)) %>% 
                pivot_wider(names_from = prob,values_from = value),
              aes(y=0.5,ymin = `0.05`, ymax = `0.95`),
              alpha = 0.3) +
  geom_ribbon(data = yield_dat %>%
                mutate(value = value) %>% 
                filter(prob %in% c(0.25,0.5,0.75)) %>% 
                pivot_wider(names_from = prob,values_from = value),
              aes(y=0.5,ymin = `0.25`, ymax = `0.75`),
              alpha = 0.3) +
  geom_rect(data = x_axis2,
            aes(xmin = min, xmax = max, ymin = -Inf, ymax = Inf, fill = RP),
            alpha = 0.3, color = NA, inherit.aes = FALSE) + 
  geom_vline(data = x_axis1,
             aes(xintercept = value, color = RP)) +
  geom_hline(data = y_axis1, 
             aes(yintercept = value, color = RP)) +
  geom_line() +
  geom_text(
    data = fbar_func(fit, fbar_ages = min(age_range):max(age_range)) %>% 
      mutate(value = catch_mass/1e6) %>% 
      rename("f" = "fbar"),
    aes(label = substr(year, 3,4)),
    size = FS(8)) +
  coord_cartesian(expand = FALSE) +
  expand_limits(x = c(-0.01, 0.4), y = c(250)) +
  scale_color_manual(values = rp_cols) +
  scale_fill_manual(values = rp_cols, guide = "none") +
  labs(y="Catch (kt)", x = 'F', color = "Reference\npoint") +
  theme(legend.position = c(0.9,0.8))


p2 <- ssb_dat %>% 
  filter(prob==0.5) %>% 
  ggplot(aes(f, value)) + 
  geom_ribbon(data = ssb_dat %>%
                mutate(value = value) %>% 
                filter(prob %in% c(0.05,0.5,0.95)) %>% 
                pivot_wider(names_from = prob,values_from = value),
              aes(y=0.5,ymin = `0.05`, ymax = `0.95`),
              alpha = 0.5) +
  geom_ribbon(data = ssb_dat %>%
                mutate(value = value) %>%  
                filter(prob %in% c(0.25,0.5,0.75)) %>% 
                pivot_wider(names_from = prob,values_from = value),
              aes(y=0.5,ymin = `0.25`, ymax = `0.75`),
              alpha = 0.5) +
  geom_rect(data = x_axis2,
            aes(xmin = min, xmax = max, ymin = 0, ymax = Inf, fill = RP),
            alpha = 0.3, color = NA, inherit.aes = FALSE) + 
  geom_vline(data = x_axis1,
             aes(xintercept = value, color = RP)) +
  geom_hline(data = y_axis2, 
             aes(yintercept = value/1e3, color = RP)) +
  geom_line() +
  geom_text(
    data = fit$res.by.year %>%
      filter(stock == 'ghl_female_mat') %>%
      dplyr::mutate(value = total.biomass/1e6, f = F) %>% 
      dplyr::select(year, value, f),
    aes(label = substr(year, 3,4)),
    size = FS(8)) +
  scale_y_continuous(trans = "log1p", breaks = c(0,1,5,10,25,50,100,1000,2500,5000)) +
  scale_color_manual(values = rp_cols) +
  scale_fill_manual(values = rp_cols, guide = "none") +
  labs(y="SSB (kt)", x = 'F', color = "Reference\npoint") +
  theme(legend.position = c(0.9,0.8))

tmp <- tibble(
  ssb = seq(0,600,by=10), 
  rec = pmin(1,(ssb)/(blim*1e3))*
    fit$res.by.year %>% 
    filter(year >= 1985) %>% 
    pull(recruitment) %>% 
    mean(.,na.rm = T)
)

p3 <- results_msy_nobtrigger %>% 
  filter(year > (max(year) -50), step==1, hr_target == Emsy$hr_target) %>%
  group_by(hr_target, year, trial) %>% 
  summarise(y = sum(catch), 
            ssb = mean(biomass[stock == 'ghl_female_mat']),
            f = mean(fbar[step == 1]),
            rec = sum(rec, na.rm = TRUE)) %>% 
  ggplot(aes(ssb/1e3, rec/1e3))  + 
  geom_point(alpha = 0.1) + 
  labs(y='Recruitment (millions)', x="SSB (kt)") + 
  geom_vline(xintercept = blim/1e3, col = 'red') + 
  geom_line(data = tmp,
            aes(x = ssb, y = rec),
            col = 'blue') 

p4 <- Pbref %>% 
  rename(Bpa = pbpa, Blim = pblim) %>% 
  pivot_longer(names_to = 'rp', cols = c('Bpa', 'Blim')) %>% 
  ggplot(aes(f,value)) + 
  geom_line(aes(col = rp)) +
  geom_hline(yintercept = 0.05) +
  labs(y = 'Prob < Ref. point', x = 'F', col = "Reference\npoint") + 
  scale_color_manual(values = rp_cols) +
  theme(legend.position = c(0.8,0.3))


ggsave(plot = print(cowplot::plot_grid(p1,p2,p3,p4)), 
       filename = file.path(base_dir, "figures/Simulation_plots.png"), 
       units = 'in', width = pagewidth_in, height = pagewidth_in)
