par.proj <- base.par.proj
out <-
  par.proj %>%
  g3p_project_rec(recruitment = rec_list$base, method = 'constant') %>%
  g3p_project_advice_error(hr_target = 0.3, advice_rho = 0, advice_cv = 0) %>%
  g3_init_guess('btrigger', value = 1, optimise = TRUE)

tmp <- fun_fun$report(g3_tmb_par(out))

## SSB
qq <- (tmp$proj_ghl_female_mat__num * tmp$proj_ghl_female_mat__wgt) %>% 
  as.data.frame.table(stringsAsFactors = F) %>% 
  mutate(time = as.numeric(time) + 1) %>% 
  group_by(time) %>% 
  summarise(ssb = sum(Freq, na.rm = T)) %>% 
  left_join(
    tmp$proj_ghl_dummy__spawnednum %>% 
      as.data.frame.table(stringsAsFactors = F) %>%
      mutate(time = as.numeric(time)) %>% 
      group_by(time) %>% 
      summarise(rec = sum(Freq)) %>% 
      mutate(time = time)
  ) %>% 
  left_join(
    tmp$proj_ghl_female_imm__num %>% 
      as.data.frame.table(stringsAsFactors = F) %>%
      filter(age == 'age2') %>% 
      mutate(time = as.numeric(time)) %>% 
      group_by(time) %>% 
      summarise(recF = sum(Freq)) %>% 
      mutate(time = time)
  ) %>% 
  left_join(
    tmp$proj_ghl_male_imm__num %>% 
      as.data.frame.table(stringsAsFactors = F) %>%
      filter(age == 'age2') %>% 
      mutate(time = as.numeric(time)) %>% 
      group_by(time) %>% 
      summarise(recM = sum(Freq)) %>% 
      mutate(time = time)
  ) %>% 
  mutate(recFM = recF + recM) %>%  ## Should equal column æ rec
  left_join(
    (tmp$proj_ghl_female_mat__spawningnum * tmp$proj_ghl_female_mat__wgt) %>% 
      as.data.frame.table(stringsAsFactors = F) %>%
      mutate(time = as.numeric(time)) %>% 
      group_by(time) %>% 
      summarise(ssb_that_spawns = sum(Freq, na.rm = TRUE)) %>% 
      mutate(time = time)
    )

qq %>% filter(time %in% c(45:55))

##############################################################################
## Internal gadget functions
##############################################################################

## Logspace add function
lsa <- function(a,b){ pmax(a, b) + log1p(exp(pmin(a,b) - pmax(a, b))) }
hs <- function(r0, s, blim){ r0*lsa(-1000 * s / blim, -1000) / -1000  }
hse <- function(r0, s, blim){ r0*pmin(s/blim, 1)}
## Hosckey stock formula
#r = f_substitute(~r0 * logspace_add(-1000 * s / blim, -1000) / -1000, list(
#  r0 = r0,
#  blim = blim)))

## So

qq %>% mutate(hockey_SSB = hs(49248729, .data$ssb, blim),
              hockey_equiv_SSB = hse(49248729, .data$ssb, blim),
              hockey_SSB_that_spawns = hs(49248729, .data$ssb_that_spawns, blim),
              hockey_equiv_SSB_that_spawns = hse(49248729, .data$ssb_that_spawns, blim),
              blim = blim
         ) %>% 
  filter(time %in% 45:75) %>% 
  view()

qq %>% ggplot(aes(time, ssb)) + geom_line()
qq %>% ggplot(aes(time, rec)) + geom_line() #+ geom_abline(rec2, col = "red")
qq %>% ggplot(aes(ssb, rec)) + geom_point() + geom_vline(xintercept = local(blim))
qq %>% ggplot(aes(ssb_that_spawns, rec)) + geom_point() + geom_vline(xintercept = local(blim))
