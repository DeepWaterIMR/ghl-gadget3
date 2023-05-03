## ---------------------------
##
## Script name: Projection figures and tables
##
## Purpose of script: Make figures and reference point table from projection results
##
## ---------------------------
##
## Run first

source("0 run first.R")

## Custom functions

### Quantile function
quantile_df <- function(x, scale = 1, probs = c(0.05,0.25, 0.5, 0.75, 0.95)) {
  tibble(
    value = quantile(x/scale, probs, na.rm = TRUE),
    prob = probs
  )
}

## Read data

if(!exists("base_dir")) base_dir <- "projections"
outpath <- file.path(base_dir, "projections")

if(!exists("optim_fit")) load("data/Optimized TMB model fit.rda")
fit <- optim_fit

if(!exists("results_pre")) load(file.path(outpath, 'results_pre.Rdata'))
if(!exists("results_msy_nobtrigger")) load(file.path(outpath, 'results_msy_nobtrigger.Rdata'))
if(!exists("results_msy")) load(file.path(outpath, 'results_msy.Rdata'))
if(!exists("blim")) load(file.path(outpath, 'projection_defintions.Rdata'))

## Variables and definitions

f_round <- 3
rp_cols <- c("MSY" = "#6CA67A", "HRmsy" = "#82C893", "Bpa" = "#056A89", "HRpa" = "#449BCF", "Blim" = "#D44F56", "HRlim" = "#FF5F68")

###############################
# Reference point calculus ####
# All weights turned to kilotons and abundances to millions

## Limit reference point: no assessment error and no Btrigger (HR(>45cm) is called f)
lim_dat <-
  results_pre %>%
  filter(year > (max(year) -50)) %>%
  group_by(hr_target) %>%
  reframe(f = round(mean(hr), digits = f_round),
          quantile_df(ssb_that_spawns, 1e6))

### HRlim (called f)
HRlim <-
  lim_dat %>%
  filter(prob == 0.5, value > local(blim/1e6)) %>%
  # slice(which.min(abs(value-blim/1e6))) %>%
  filter(hr_target == max(hr_target))

##  Limit reference point: assessment error and btrigger (HR(>45cm) is called f)
pre_dat <-
  results_msy %>%
  filter(year > (max(year) -50)) %>%
  group_by(hr_target) %>%
  reframe(f = round(mean(hr), digits = f_round),
          quantile_df(ssb_that_spawns, 1e6))

## HRpa the Icelandic way (called f)
HRpa <-
  pre_dat %>%
  filter(prob == 0.05) %>%
  filter(value > blim/1e6) %>%
  filter(hr_target == max(hr_target))

### HRpa the ICES way (unfinished)
# results_msy %>%
#   filter(year > (max(year) - 50)) %>%
#   mutate(pass = ssb_that_spawns > blim) %>%
#   group_by(hr_target) %>%
#   summarise(
#     HRpa = mean(hr),
#     prop = sum(pass)/length(pass),
#     value = mean(ssb_that_spawns/1e6)) %>%
#   ungroup() %>%
#   slice(which.min(abs(0.95-prop)))
### end

## MSY reference points

## Yield curve no btrigger (HR(>45cm) is called f)
yield_dat <-
  results_msy_nobtrigger %>%
  filter(year > (max(year) -50)) %>%
  group_by(hr_target) %>%
  reframe(f = mean(hr),
          ssb = mean(ssb_that_spawns)/1e6,
          quantile_df(catch, 1e6))

## Median yield (MSY and HRmsy)
HRmsy <- yield_dat %>%
  filter(prob == 0.5) %>%
  filter(value == max(value))

## Uncertainty for HRmsy (does not work for MSY)
HRmsy_range <- yield_dat %>%
  filter(prob == 0.5) %>%
  filter(value > 0.95*max(value)) %>%
  filter(hr_target == min(hr_target) | hr_target == max(hr_target))

##############
## Tables ####

rp_tab <- tibble::tibble(
  `Reference point` =
    c("Blim", "Bpa", "Btrigger", "MSY", "HR(bar)lim", "HR(bar)msy", "HR(bar)pa",
      "HR(target)lim", "HR(target)msy", "HR(target)pa"),
  Value =
    c(blim/1e6, bpa/1e6, btrigger/1e6, HRmsy$value, HRlim$f, HRmsy$f, HRpa$f,
      HRlim$hr_target, HRmsy$hr_target, HRpa$hr_target),
  Basis = c("Lowest modelled mature female substock biomass",
            "Blim x 1.4",
            "Bpa",
            "Maximum sustainable yield",
            "HR(>=45cm) leading to P(SSB < Blim) = 0.5",
            "HR(>=45cm) leading to MSY",
            "HR(>=45cm), when ICES AR is applied, leading to P(SSB > Blim) = 0.05",
            "HR(target) leading to P(SSB < Blim) = 0.5",
            "HR(target) leading to MSY",
            "HR(target), when ICES AR is applied, leading to P(SSB > Blim) = 0.05"
  )
)

save(rp_tab, file = file.path(base_dir, "projections/reference_point_table.rda"))

###############
## Figures ####

## Simulation plot ####

x_axis1 <- tibble(RP = c("HRmsy", "HRpa", "HRlim"),
                  value = c(HRmsy$f, HRpa$f, HRlim$f))
x_axis2 <- tibble(RP = c("HRmsy"),
                  type = c("min", "max"),
                  value = c(HRmsy_range$f)) %>%
  pivot_wider(names_from = type, values_from = value)

y_axis1 <- tibble(RP = c("MSY"), value = c(HRmsy$value))
y_axis2 <- tibble(RP = c("Blim", "Bpa"), value = c(blim/1e6, bpa/1e6))

leg_p <- ggplot() +
  geom_vline(data = x_axis1, aes(xintercept = value, color = RP)) +
  geom_hline(data = y_axis1, aes(yintercept = value, color = RP)) +
  geom_hline(data = y_axis2, aes(yintercept = value, color = RP)) +
  scale_color_manual("Reference\npoint", values = rp_cols, breaks = names(rp_cols)) +
  theme(legend.position = "bottom")

### Yield plot

p1 <- yield_dat %>%
  filter(prob==0.5) %>%
  ggplot(aes(f, value)) +
  geom_rect(data = x_axis2,
            aes(xmin = min, xmax = max, ymin = -Inf, ymax = Inf, fill = RP),
            alpha = 0.3, color = NA, inherit.aes = FALSE) +
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
  geom_vline(data = x_axis1,
             aes(xintercept = value, color = RP)) +
  geom_hline(data = y_axis1,
             aes(yintercept = value, color = RP)) +
  geom_line() +
  geom_text(
    data =
      plot_hr(optim_fit, min_catch_length = 45, return_data = TRUE) %>%
      rename("f" = "value") %>%
      mutate(value = catch_biom/1e6),
    aes(label = substr(year, 3,4)),
    size = FS(8)) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  scale_x_continuous(expand = expansion(mult = c(0, .04)), n.breaks = 10) +
  scale_color_manual(values = rp_cols) +
  scale_fill_manual(values = rp_cols, guide = "none") +
  labs(y="Catch (kt)", x = 'Harvest rate (\u2265 45 cm)', color = "Reference\npoint") +
  theme(legend.position = "none")

### SSB vs harvest rate

p2 <- pre_dat %>%
  filter(prob==0.5) %>%
  ggplot(aes(f, value)) +
  geom_rect(data = x_axis2,
            aes(xmin = min, xmax = max, ymin = 0, ymax = Inf, fill = RP),
            alpha = 0.3, color = NA, inherit.aes = FALSE) +
  geom_ribbon(data = pre_dat %>%
                mutate(value = value) %>%
                filter(prob %in% c(0.05,0.5,0.95)) %>%
                pivot_wider(names_from = prob,values_from = value),
              aes(y=0.5,ymin = `0.05`, ymax = `0.95`),
              alpha = 0.5) +
  geom_ribbon(data = pre_dat %>%
                mutate(value = value) %>%
                filter(prob %in% c(0.25,0.5,0.75)) %>%
                pivot_wider(names_from = prob,values_from = value),
              aes(y=0.5,ymin = `0.25`, ymax = `0.75`),
              alpha = 0.5) +
  geom_vline(data = x_axis1,
             aes(xintercept = value, color = RP)) +
  geom_hline(data = y_axis2,
             aes(yintercept = value, color = RP)) +
  geom_line() +
  geom_text(
    data =
      full_join(
        plot_hr(optim_fit, min_catch_length = 45, return_data = TRUE) %>%
          dplyr::select(year, value) %>%
          rename(f = value),
        plot_biomass(optim_fit, stocks = "ghl_female_mat", return_data = TRUE) %>%
          mutate(value = total.biomass/1e6) %>%
          dplyr::select(year, value),
        by = "year"
      ),
    aes(label = substr(year, 3,4)),
    size = FS(8)) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  scale_x_continuous(expand = expansion(mult = c(0, .04)), n.breaks = 8) +
  scale_color_manual(values = rp_cols) +
  scale_fill_manual(values = rp_cols, guide = "none") +
  labs(y="SSB (kt)", x = 'Harvest rate (\u2265 45 cm)', color = "Reference\npoint") +
  theme(legend.position = "none")

### Stock - recruit plot

tmp1 <- results_msy_nobtrigger %>% # For testing that RPs are correct: results_pre
  filter(year > (max(year) -50), hr_target == HRmsy$hr_target) %>%
  dplyr::select(rec, ssb_that_spawns) %>%
  rename(ssb = ssb_that_spawns)

tmp2 <- tibble(
  ssb = seq(0,round(max(tmp1$ssb)),by=1e5),
  rec = pmin(1,ssb/blim)*
    optim_fit$res.by.year %>%
    filter(year >= rec_start_year) %>%
    group_by(year) %>%
    summarise(recruitment = sum(recruitment, na.rm = TRUE)) %>%
    pull(recruitment) %>%
    mean(.,na.rm = T)
)

p3 <- tmp1 %>%
  ggplot(aes(ssb/1e6, rec/1e6))  +
  geom_point(alpha = 0.1) +
  labs(y = 'Recruitment (millions)',
       x = "Spawning stock biomass (kt)",
       color = "Reference\npoint") +
  geom_vline(
    data = y_axis2,
    aes(xintercept = value, color = RP)) +
  scale_color_manual(values = rp_cols) +
  geom_line(data = tmp2, col = 'white', linewidth = 1.5) +
  geom_line(data = tmp2, col = 'blue') +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  scale_x_continuous(expand = expansion(mult = c(0, .04)), n.breaks = 8) +
  theme(legend.position = "none")

# Probability of going below reference point plot

Pbref <-
  results_msy %>%
  filter(year > (max(year) -50)) %>%
  group_by(hr_target) %>%
  summarise(f = mean(hr),
            pbpa = mean(ssb_that_spawns < bpa),
            pblim = mean(ssb_that_spawns < blim))

p4 <- Pbref %>%
  rename(Bpa = pbpa, Blim = pblim) %>%
  pivot_longer(names_to = 'rp', cols = c('Bpa', 'Blim')) %>%
  ggplot(aes(f,value)) +
  geom_hline(yintercept = 0.05, color = "grey") +
  geom_vline(data = x_axis1,
             aes(xintercept = value, color = RP), alpha = 0.5) +
  geom_line(aes(col = rp)) +
  labs(y = 'Prob < Ref. point', x = 'Harvest rate (\u2265 45 cm)',
       col = "Reference\npoint") +
  coord_cartesian(xlim = c(0.1, 0.25)) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  scale_x_continuous(expand = expansion(mult = c(0, .05)), n.breaks = 10) +
  scale_color_manual(values = rp_cols) +
  theme(legend.position = "none")

ggsave(
  plot =
    print(
      cowplot::plot_grid(
        cowplot::plot_grid(p1,p2,p3,p4, labels = "AUTO"),
        cowplot::get_legend(leg_p),
        ncol = 1, rel_heights = c(9,1)
      )),
  filename = file.path(base_dir, "figures/Simulation_plots.png"),
  units = 'in', width = pagewidth_in, height = pagewidth_in,
  bg = "white")

## Histogram of draws fit ####
# fithist <-
#   fit$stock.recruitment %>%
#   mutate(rec = recruitment/1e6) %>%
#   filter(year >= rec_start_year) %>%
#   pull(rec) %>% hist(breaks = seq(0, 200, by = 25))
#
# simhist <-
#   results_msy_nobtrigger %>%
#   filter(year > 2025, hr_target == 0.06) %>%
#   mutate(rec = rec/1e6) %>%
#   pull(rec) %>% hist(breaks = seq(0, 500, by = 25))
#
# recounts <- data.frame(Type = 'Base_fit', c = fithist$counts/sum(fithist$counts), mids = fithist$mids) %>%
#   bind_rows(
#     data.frame(Type = 'Simulation', c = simhist$counts/sum(simhist$counts), mids = simhist$mids)
#   ) %>%
#   ggplot(aes(mids, c, fill = Type)) +
#   geom_bar(stat = 'identity', position = 'dodge') +
#   labs(x = "Recruitment (millions)", y = "Proportion")
#
# ggsave(plot = recounts,
#        filename = file.path(base_dir, "figures/Simulation_recruitment_counts.png"),
#        units = 'in', width = pagewidth_in, height = pagewidth_in*0.6)

