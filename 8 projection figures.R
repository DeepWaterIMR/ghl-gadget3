## ---------------------------
##
## Script name: Projection figures and tables
##
## Purpose of script: Make figures and reference point table from projection results
##
## ---------------------------
## Read data



results_msy %>%
  #filter(year > 2070) %>%
  group_by(year, hr_target, trial) %>%
  summarise(c = sum(catch), ssb = mean(biomass[stock == 'ghl_female_mat']), fbar = max(fbar)) %>%
  group_by(hr_target) %>%
  summarise(yield = median(c), ssb = median(ssb), fbar = median(fbar)) %>%
  ggplot(aes(fbar, yield)) + geom_point()


results_msy %>%
  filter(year > 2050) %>%
  group_by(year, hr_target, trial) %>% 
  summarise(catch = sum(catch)/1e3, ssb = mean(biomass[stock == 'ghl_female_mat']), fbar = max(fbar)) %>%
  ggplot(aes(fbar, catch)) + geom_point()



## Quantile function
quantile_df <- function(x, scale = 1, probs = c(0.05,0.25, 0.5, 0.75, 0.95)) {
  tibble(
    value = quantile(x/scale, probs, na.rm = TRUE),
    prob = probs
  )
}


## Variables
f_round <- 3

## Yield curve no btrigger
yield_dat <- 
  results_msy_nobtrigger %>% # With btrigger: results_msy
  filter(year > (max(year) -50), hr_target <= 0.3) %>% 
  group_by(hr_target, year, trial) %>% 
  summarise(y = sum(catch), 
            ssb = mean(biomass[stock == 'ghl_female_mat']),
            f = mean(fbar[step == 1])) %>% 
  group_by(hr_target) %>% 
  reframe(f = round(mean(f), digits = f_round),
          ssb = mean(ssb),
          quantile_df(y, 1e3)) 

## Median yield
Emsy <- yield_dat %>% filter(prob == 0.5) %>% filter(value == max(value))

Emsy_range <- yield_dat %>% 
  filter(prob == 0.5) %>% 
  filter(value > 0.95*max(value)) %>% 
  filter(hr_target == min(hr_target) | hr_target == max(hr_target))


ssb_dat <- 
  results_msy %>% 
  filter(year > (max(year) -50), step==1, hr_target <= 0.3) %>% 
  group_by(year, hr_target, boot, trial) %>% 
  summarise(ssb = mean(ssb), 
            f = mean(fbar[step == 1])) %>% 
  group_by(hr_target) %>% 
  reframe(f = round(mean(f), digits = f_round),
          quantile_df(ssb, 1e3)) 

Epa <- 
  ssb_dat %>% 
  filter(prob == 0.05) %>% 
  filter(value > blim) %>% 
  filter(hr_target == max(hr_target))


Pbref <- 
  results_msy %>% 
  filter(year > (max(year) -50), step==1, hr_target <= 0.3) %>% 
  group_by(year, hr_target, boot, trial) %>% 
  summarise(ssb = mean(ssb), 
            f = mean(fbar[step == 1])) %>% 
  group_by(hr_target) %>% 
  summarise(f = round(mean(f), digits = f_round),
            pbpa = mean(ssb < bpa * 1e3),
            pblim = mean(ssb < blim * 1e3))


# fit$res.by.year %>% 
#   left_join(fbar_func(fit))

fbar_func <- function(fit, age_range = 3:50){
  fit$stock.prey %>% 
    filter(age %in% age_range) %>% 
    group_by(year,age) %>% 
    summarise(c=sum(number_consumed), 
              n=sum(number[step==1])) %>% 
    mutate(f=-log(1-c/n)) %>% 
    group_by(year) %>% 
    summarise(fbar = mean(f))
}


tibble::tibble(
  `Reference point` = c("Blim", "Bpa", "Btrigger", "MSY", "Flim", "Fmsy", "Fpa", "HRlim", 
                        "HRmsy", "HRpa"),
  Value = c(blim, bpa, btrigger, Emsy$value, Elim$flim, Emsy$f, Epa$f, Elim$hr_lim, Emsy$hr_target, Epa$hr_target),
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







proj_plot <- 
  cowplot::plot_grid(
    
    ggplot() + 
      geom_ribbon(data = yield_dat %>%
                    filter(prob %in% c(0.05,0.5,0.95)) %>% 
                    pivot_wider(names_from = prob,values_from = value),
                  aes(x = f, y=0.5, ymin = `0.05`, ymax = `0.95`),
                  alpha = 0.5) +
      geom_ribbon(data = yield_dat %>%
                    filter(prob %in% c(0.25,0.5,0.75)) %>% 
                    pivot_wider(names_from = prob,values_from = value),
                  aes(y=0.5,ymin = `0.25`, ymax = `0.75`),
                  alpha = 0.5) +
      geom_line(data = yield_dat %>% 
                  filter(prob==0.5),
                aes(f, value/1e3)) + 
      geom_vline(xintercept = Emsy$f) +
      geom_vline(xintercept = Emsy_range$f, lty=2)+
      geom_hline(yintercept = Emsy$value/1e3) + 
      geom_vline(xintercept = Epa$f,col='red') +
      # geom_point(data=fit$res.by.year %>% 
      #              filter(stock == 'cdred_mat') %>% 
      #              select(value=catch,f=F),
      #            aes(y=value/1e3)) +
      labs(y="Catch ('000 tons)", x = 'F'),
    
    ssb_dat %>% 
      filter(prob==0.5) %>% 
      ggplot(aes(f, value/1e3)) + 
      geom_ribbon(data = ssb_dat %>%
                    mutate(value = value/1e3) %>% 
                    filter(prob %in% c(0.05,0.5,0.95)) %>% 
                    pivot_wider(names_from = prob,values_from = value),
                  aes(y=0.5,ymin = `0.05`, ymax = `0.95`),
                  alpha = 0.5) +
      geom_ribbon(data = ssb_dat %>%
                    mutate(value = value/1e3) %>%  
                    filter(prob %in% c(0.25,0.5,0.75)) %>% 
                    pivot_wider(names_from = prob,values_from = value),
                  aes(y=0.5,ymin = `0.25`, ymax = `0.75`),
                  alpha = 0.5) +
      geom_line() + 
      geom_vline(xintercept = Emsy$f) +
      geom_vline(xintercept = Emsy_range$f,lty=2)+
      geom_vline(xintercept = Epa$f,col='red') +
      geom_hline(yintercept = blim/1e3, col='red') + 
      geom_hline(yintercept = bpa/1e3, lty = 2) + 
      # geom_point(data=fit$res.by.year %>% 
      #              filter(stock == 'cdred_mat') %>% 
      #              select(value=total.biomass,f=F),
      #            aes(y=value/1e3)) +
      labs(y="SSB ('000 tons)", x = 'F') ,
    
    results_msy_nobtrigger %>% 
      filter(year > (max(year) -50), step==1, hr_target == 0.06) %>%
      #filter(step==1, hr_target == 0.06) %>%
      # mutate(ssb = round(ssb/1e3, 0), rec = round(rec/1e3, 0)) %>% 
      # select(ssb, rec) %>% 
      # distinct() %>% 
      # ggplot(aes(ssb, rec/1e3)) + 
      ggplot(aes(ssb/1e6, rec/1e6))  + 
      geom_point(alpha = 0.1) + 
      labs(y='Recruitment (millions)', x="SSB ('000 tons)") + 
      geom_vline(xintercept = blim/1e3, col = 'red') + 
      geom_line(data = tibble(ssb=seq(0,600,by=10)*1e6, rec = pmin(1,(ssb)/(blim*1e3))*
                                fit$res.by.year %>% 
                                filter(year >= 1985) %>% 
                                pull(recruitment) %>% 
                                mean(.,na.rm = T)),
                col = 'white',linewidth = 1.5) + 
      geom_line(data = tibble(ssb=seq(0,600,by=10)*1e6,rec = pmin(1,(ssb)/(blim*1e3))*
                                fit$res.by.year %>% 
                                filter(year >= 1985) %>% 
                                pull(recruitment) %>% 
                                mean(.,na.rm = T)),
                col = 'blue'),
    
    Pbref %>% 
      rename(Bpa = pbpa, Blim = pblim) %>% 
      pivot_longer(names_to = 'rp', cols = c('Bpa', 'Blim')) %>% 
      ggplot(aes(f,value)) + 
      geom_line(aes(col = rp)) +
      geom_hline(yintercept = 0.05) +
      labs(y = 'Prob < Ref. point', x = 'F', col = 'Ref. point') + 
      theme(legend.position = c(0.8,0.3)),
    
    ncol = 2
    
  )





# results_msy %>%
#   filter(year > 2170) %>%
#   group_by(year, hr_target, boot, trial) %>%
#   summarise(c = sum(catch), ssb = mean(ssb[step == 1]), fbar = median(fbar[step == 1])) %>%
#   group_by(hr_target) %>%
#   summarise(yield = median(c), ssb = median(ssb), fbar = median(fbar)) %>%
#   ggplot(aes(fbar, yield)) + geom_point()

# results_msy_nobtrigger %>%
#   filter(year > 2170) %>%
#   group_by(year, hr_target, boot, trial) %>%
#   summarise(c = sum(catch), ssb = mean(ssb[step == 1]), fbar = median(fbar[step == 1])) %>%
#   group_by(hr_target) %>%
#   summarise(yield = median(c), ssb = median(ssb), fbar = median(fbar)) %>%
#   ggplot(aes(fbar, yield)) + geom_point()


