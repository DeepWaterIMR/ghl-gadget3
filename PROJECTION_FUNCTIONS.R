## -----------------------------------------------------------------------------
##
## Projection functions
## 
## -----------------------------------------------------------------------------

proj_stock_actions2 <- function(num_project_years,
                               mat,
                               imm_F,
                               imm_M,
                               comp_id = c('species', 'sex')){

  if(gadget3::g3_stock_def(imm_F, 'minage') == 0) {
    dummy_stock <- imm_F
  } else {
    ## Setup up a dummy stock for females
    dummy_stock_F <- 
      gadget3::g3_stock(c(species = gadgetutils::g3_stock_name(imm_F), 'dummy'),
                        lengthgroups = seq(min(gadget3::g3_stock_def(imm_F, 'minlen')),
                                           max(gadget3::g3_stock_def(imm_F, 'maxlen')[1:((nrow(gadget3::g3_stock_def(imm_F, 'maxlen')))-1)]),
                                           1)) %>% 
      gadget3::g3s_livesonareas(areas[c('1')]) %>% 
      gadget3::g3s_age(minage = 0, maxage = gadget3::g3_stock_def(imm_F, 'minage')-1)
    
    ## Setup up a dummy stock for males
    dummy_stock_M <- 
      gadget3::g3_stock(c(species = gadgetutils::g3_stock_name(imm_M), 'dummy'),
                        lengthgroups = seq(min(gadget3::g3_stock_def(imm_M, 'minlen')),
                                           max(gadget3::g3_stock_def(imm_M, 'maxlen')[1:((nrow(gadget3::g3_stock_def(imm_M, 'maxlen')))-1)]),
                                           1)) %>% 
      gadget3::g3s_livesonareas(areas[c('1')]) %>% 
      gadget3::g3s_age(minage = 0, maxage = gadget3::g3_stock_def(imm_M, 'minage')-1)
    
  }
  
  dummy_actions <- 
    list(
      gadget3::g3a_age(dummy_stock_F, 
                       output_stocks = list(imm_F), 
                       run_f = ~cur_step == 1,
                       run_at = 1,
                       transition_at = 8),
      
      gadget3::g3a_age(dummy_stock_M, 
                       output_stocks = list(imm_M), 
                       run_f = ~cur_step == 1,
                       run_at = 1,
                       transition_at = 8)
      
      
    )
  
  spawning_actions <- 
    list(
      gadget3::g3a_spawn(
        stock = mat,
        output_stocks = list(dummy_stock_M, dummy_stock_F),
        recruitment_f = 
          g3a_spawn_recruitment_hockeystick(
            r0 = gadget3:::f_substitute(~scale * g3_param_table('project_rec',
                                                                expand.grid(cur_year = seq(end_year - minage, 
                                                                                           end_year + py)), ifmissing = 0),
                                        list(py = num_project_years,
                                             minage = gadget3::g3_stock_def(imm_F, 'minage'), ## Minimum ages are equal between stocks
                                             scale = 1e4)),
            blim = g3_parameterized('blim')), 
        proportion_f = 1,
        mean_f = g3_parameterized('recl', by_stock = 'species'),
        stddev_f = g3_parameterized('rec.sd', by_stock = 'species'),
        alpha_f = g3_parameterized('walpha', by_stock = comp_id),
        beta_f = g3_parameterized('wbeta', by_stock = comp_id),
        run_f = ~cur_year_projection && cur_step == 1) 
      
    )
  
  return(c(dummy_actions, spawning_actions))
  
}


runfun <- function(adfun, pars, mat, imm = NULL){
  
  age_range <- min(g3_stock_def(mat, 'maxage'))
  
  if (is.null(imm)) imm <- mat
  
  schedule <- 
    expand.grid(year = min(year_range):(max(year_range)+num_project_years),
                step = 1:length(defaults$timestep)) %>% 
    arrange_all() %>% 
    mutate(time = as.character(1:nrow(.)))
  
  
  reports <- adfun$report(gadget3::g3_tmb_par(pars))
  
  ## Collect recruitment
  immname <- paste0('proj_', gadgetutils::g3_stock_name(imm))
  matname <- paste0('proj_', gadgetutils::g3_stock_name(mat))
  
  rec <- 
    reports[[paste0(immname, '__num')]] %>% 
    as.data.frame.table(stringsAsFactors = FALSE) %>% 
    left_join(schedule, by = 'time') %>% 
    filter(age == paste0('age', gadget3::g3_stock_def(imm, 'minage')+1)) %>% 
    filter(year > start_year) %>% 
    group_by(year, step) %>% 
    summarise(rec = sum(Freq), .groups = 'drop')
  
  tmp <-
    list(num = reports$proj_cdred_mat__num,
         wgt = reports$proj_cdred_mat__wgt, 
         comm = reports$proj_cdred_mat__predby_comm_proj) %>% 
    map(as.data.frame.table, stringsAsFactors = FALSE) %>% 
    map(as_tibble) %>% 
    bind_rows(.id='var') %>% 
    left_join(schedule, by = 'time') %>%
    filter(!is.na(Freq)) %>% 
    pivot_wider(names_from = var, values_from = Freq) %>% 
    gadgetutils:::split_length() %>% 
    mutate(length = (ifelse(is.infinite(upper),lower + 1, upper) + lower )/2,
           age = gsub('age','',age) %>% as.numeric()) %>% 
    dplyr::select(-c(lower, upper, time))
  
  out <- 
    tmp %>% 
    filter(year > start_year) %>% 
    group_by(year, step) %>% 
    summarise(catch = sum(comm, na.rm = TRUE),
              ssb = sum((num*wgt)[step == 1], na.rm = TRUE), .groups = 'drop') %>% 
    left_join(
      tmp %>%
        mutate(catchnum = ifelse(wgt == 0, 0, comm/wgt)) %>% 
        filter(age %in% age_range) %>% 
        group_by(year, age) %>% 
        summarise(N0 = sum(num[step == 1], na.rm = TRUE), 
                  catch = sum(catchnum), .groups = 'keep') %>% 
        mutate(fbar = -log(1 - catch/N0)) %>% 
        replace_na(list(fbar = 0)) %>% 
        group_by(year) %>% 
        summarise(fbar = mean(fbar), .groups = 'drop') %>% 
        dplyr::select(year,fbar),
      by = 'year'
    ) %>% 
    left_join(rec, by = c('year', 'step'))
  
  return(out)
  
}
