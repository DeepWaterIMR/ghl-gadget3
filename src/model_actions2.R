init_abund2 <- function(imm,
                       mat,
                       comp_id = 'species',
                       mature = TRUE,
                       init_mode = 1,
                       exp_init = FALSE,
                       exp_init_scalar = FALSE,
                       exp_init_f = FALSE,
                       naturalmortality = gadget3::g3_parameterized('M', by_stock = TRUE),
                       allstocks = NULL){
  
  ## Checks
  stopifnot(inherits(imm, 'g3_stock'))
  stopifnot(inherits(mat, 'g3_stock'))
  
  g3a_initial_ageprop <- function(alpha, a50){
    gadget3:::f_substitute(
      ~bounded(-1*alpha*(age - a50),0,1),
      list(alpha = alpha, a50 = a50))
  }
  
  ## ---------------------------------------------------------------------------
  
  stock <- if (mature) mat else imm
  
  ## MODE 0: initialised at equilibrium (using carrying capacity B0) assuming constant natual M (M)
  if (init_mode == 0){
    
    ## Proportion mature
    prop_mat0 <- gadget3::g3_parameterized(name = 'prop_mat0', by_stock = comp_id)
    
    if(!mature){
      prop_mat0 <- gadget3:::f_substitute(~1-prop_mat0, list(prop_mat0 = prop_mat0))
    }
    
    ## Equilibrium age distribution
    init_scalar <- gadget3:::f_substitute(~p0 * B0 * (1-exp(-1*M))/(1-exp(-1*maxage*M)),
                                          list(p0 = prop_mat0,
                                               B0 = gadget3::g3_parameterized('B0',
                                                                              by_stock = comp_id),
                                               M = naturalmortality,
                                               maxage = gadget3::g3_step(~stock_with(mat, mat__maxage))))
    
    out <- gadget3::g3a_renewal_initabund(scalar = init_scalar,
                                          init = 1,
                                          M = naturalmortality,
                                          init_F = gadget3::g3_parameterized(name = 'init.F',
                                                                             by_stock = comp_id,
                                                                             by_year = FALSE,
                                                                             by_age = FALSE,
                                                                             exponentiate = exp_init_f))
    
    
  }else{
    
    ## MODE 1: Initial parameter per age group (across stocks)  
    if (init_mode == 1){
      
      ## Proportion mature at age
      p_age <- g3a_initial_ageprop(gadget3::g3_parameterized('mat_initial_alpha',
                                                             by_stock = allstocks),
                                   gadget3::g3_parameterized('mat_initial_a50',
                                                             by_stock = allstocks))
      
      ## Invert for immature stock
      if(!mature){
        p_age <- gadget3:::f_substitute(~1-p_age, list(p_age = p_age))
      }

            out <- gadget3:::f_substitute(~scalar*init*exp(-1*(M+init_F)*(age-minage))*p_age,
                                    list(scalar = gadget3::g3_parameterized('init.scalar', 
                                                                            by_stock = allstocks, 
                                                                            exponentiate = exp_init_scalar),
                                         init = gadget3::g3_parameterized(name = 'init', 
                                                                          by_stock = allstocks, 
                                                                          by_age = TRUE, 
                                                                          exponentiate = exp_init),
                                         M = naturalmortality,
                                         init_F = gadget3::g3_parameterized(name = 'init.F',
                                                                            by_stock = comp_id,
                                                                            exponentiate = exp_init_f),
                                         minage = gadget3::g3_step(~stock_with(imm, imm__minage)),
                                         p_age = p_age))
      
      
    }
    else{
      
      ## MODE 2: Initial parameter per age group per stock
      out <- gadget3::g3a_renewal_initabund(scalar = gadget3::g3_parameterized(name = 'init.scalar',
                                                                               by_stock = TRUE,
                                                                               exponentiate = exp_init_scalar),
                                            init = gadget3::g3_parameterized(name = 'init',
                                                                             by_stock = TRUE,
                                                                             by_age = TRUE,
                                                                             exponentiate = exp_init),
                                            M = naturalmortality,
                                            init_F = gadget3::g3_parameterized(name = 'init.F',
                                                                               by_stock = comp_id,
                                                                               exponentiate = exp_init_f))
      
    }
  }
  
  return(out)
  
}


model_actions2 <- function(imm, 
                          mat, 
                          mlgg = 15,
                          mature = TRUE, 
                          comp_id = 'species', 
                          rec_id = list(imm, mat),
                          rec_scalar_id = list(imm, mat),
                          init_mode = 1, 
                          parametric_sd = FALSE,
                          exp_params = c(),
                          tv_params = c(),
                          by_age_params = c(),
                          recruiting=!mature,
                          allstocks = NULL){
  
  
  ## Helper for g3_parameterized that modifies the name of a parameter if it is being exponentiated
  setup_g3_param <- function(name, 
                             by_stock,
                             tv_params, 
                             by_age_params,
                             exp_params, ...){
    
    gadget3::g3_parameterized(name,
                              by_stock = by_stock,
                              by_year = tolower(name) %in% tolower(tv_params),
                              by_age = tolower(name) %in% tolower(by_age_params), ## Add option age_params
                              exponentiate = tolower(name) %in% tolower(exp_params),
                              ...)
    
  }
  
  
  stock <- if(mature) mat else imm
  
  ## Stock specific variables
  if (mature) output_stock <- list() else output_stock <- list(mat)
  
  ## TIME VARYING PARAMETERS
  if (!is.null(tv_params)){ 
    param_list <- c('linf','k','walpha','wbeta','bbin','recl','rec.sd','mat_alpha','mat_l50','m')
    tv_params <- casefold(tv_params)
    if (!all(tv_params %in% param_list)){
      stop(paste0("The following parameters are not currently available as time-varying: ", 
                  paste0(tv_params[!(tv_params %in% param_list)], collapse = ', ')))
    }  
  }
  
  ## AGE VARYING PARAMETERS
  if (!is.null(by_age_params)){ 
    param_list <- c('linf','k','walpha','wbeta','bbin','mat_alpha','mat_l50','m')
    by_age_params <- casefold(by_age_params)
    if (!all(by_age_params %in% param_list)){
      stop(paste0("The following parameters are not currently available as age-varying: ", 
                  paste0(by_age_params[!(by_age_params %in% param_list)], collapse = ', ')))
    }  
  }
  
  ## EXPONENTIATING PARAMETERS
  if (!is.null(exp_params)){ 
    param_list <- c('linf','k','bbin','recl','rec.sd','mat_alpha','mat_l50',
                    'init','init.scalar','rec','rec.scalar','init.f','m','walpha','wbeta')
    if ('all' %in% exp_params){
      exp_params <- param_list
    }
    else{
      exp_params <- casefold(exp_params)
      
      if (!all(exp_params %in% param_list)){
        stop(paste0("The following parameters are not currently available to exponentiate: ", 
                    paste0(exp_params[!(exp_params %in% param_list)], collapse = ', ')))
      }
    }
  }
  
  ## Setup parameter references
  Linf <- setup_g3_param('Linf', comp_id, tv_params, by_age_params, exp_params)
  kk <- setup_g3_param('K', comp_id, tv_params, by_age_params, exp_params, scale = 0.001)
  walpha <- setup_g3_param('walpha', comp_id, tv_params, by_age_params, exp_params)
  wbeta <- setup_g3_param('wbeta', comp_id, tv_params, by_age_params, exp_params)
  bbin <- setup_g3_param('bbin', comp_id, tv_params, by_age_params, exp_params, scale = 10)
  recl <- setup_g3_param('recl', rec_id, tv_params, by_age_params, exp_params)
  recsd <- setup_g3_param('rec.sd', rec_id, tv_params, by_age_params, exp_params)
  mat_alpha <- setup_g3_param('mat_alpha', comp_id, tv_params, by_age_params, exp_params, scale = 0.001)
  mat_l50 <- setup_g3_param('mat_l50', comp_id, tv_params, by_age_params, exp_params)
  natm <- setup_g3_param('M', by_stock = TRUE, tv_params, by_age_params, exp_params)
  
  ## Create some variables
  initvonb <- gadget3::g3a_renewal_vonb(Linf = Linf, K = kk, recl = recl)
  
  ## ---------------------------------------------------------------------------
  ## SETUP ACTIONS
  ## ---------------------------------------------------------------------------
  
  stock_actions <- list(
    ## INITIAL CONDITIONS
    gadget3::g3a_initialconditions_normalparam(stock,
                                               # NB: area & age factor together (gadget2 just multiplied them)
                                               # initial abundance at age is 1e4 * q
                                               factor_f =
                                                 init_abund2(imm, mat, 
                                                            comp_id, 
                                                            mature, 
                                                            init_mode, 
                                                            exp_init = 'init' %in% exp_params,
                                                            exp_init_scalar = 'init.scalar' %in% exp_params,
                                                            exp_init_f = 'init.f' %in% exp_params,
                                                            natm,
                                                            allstocks = allstocks),
                                               mean_f = initvonb,
                                               stddev_f = init_sd(stock, 
                                                                  comp_id, 
                                                                  parametric = parametric_sd,
                                                                  mean_len = initvonb),
                                               alpha_f = walpha,
                                               beta_f = wbeta),
    
    ## NATURAL MORALITY
    gadget3::g3a_naturalmortality(stock, gadget3::g3a_naturalmortality_exp(natm)),
    
    ## AGING
    gadget3::g3a_age(stock, output_stocks = output_stock)
  )
  
  
  if(recruiting){
    
    stock_actions <- c(stock_actions, list(
      
      ## RENEWAL
      gadget3::g3a_renewal_normalparam(imm,
                                       factor_f = stock_renewal(imm, 
                                                                id = rec_id,
                                                                scalar_id = rec_scalar_id,
                                                                exponentiate_rec = 'rec' %in% exp_params, 
                                                                exponentiate_rec_scalar = 'rec.scalar' %in% exp_params),
                                       mean_f = initvonb,
                                       stddev_f = recsd,
                                       alpha_f = walpha,
                                       beta_f = wbeta,
                                       run_f = gadget3:::f_substitute(
                                         ~cur_step == 1 && age == minage && cur_time > 0 && !cur_year_projection,
                                         list(minage = gadget3::g3_step(~stock_with(imm, imm__minage)))))
    ))
  }
  
  if (!mature){
    
    stock_actions <- c(stock_actions, list(
      
      ## GROWTH AND MATURATION
      gadget3::g3a_growmature(
        stock = imm,
        impl_f = gadget3::g3a_grow_impl_bbinom(
          delta_len_f = gadget3::g3a_grow_lengthvbsimple(Linf, kk),      
          delta_wgt_f = gadget3::g3a_grow_weightsimple(walpha, wbeta),   
          beta_f = bbin,
          maxlengthgroupgrowth = mlgg),
        maturity_f = gadget3::g3a_mature_continuous(
          alpha = mat_alpha,
          l50 = mat_l50),
        output_stocks = list(mat),
        transition_f = ~cur_time > 0),
      list()
    ))
  }
  else{
    
    stock_actions <- c(stock_actions, list(
      
      gadget3::g3a_growmature(
        stock = mat,
        impl_f = gadget3::g3a_grow_impl_bbinom(
          delta_len_f = gadget3::g3a_grow_lengthvbsimple(Linf, kk),       
          delta_wgt_f = gadget3::g3a_grow_weightsimple(walpha, wbeta),    
          beta_f = bbin,
          maxlengthgroupgrowth = mlgg)
      ),
      list()
      
    ))
  }
  return(stock_actions)
}