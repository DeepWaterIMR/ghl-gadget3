jitter_param <- function(param, jitter_fraction = 0.1){
  
  ## A function to jitter initial parameters
  ## Mimics SS model approach
  
  if (is.infinite(param$lower) || is.infinite(param$upper)){
    warning(paste("Parameter", names(param), "not jittered as bounds are infinite."))
    return(param)
  }
  if (jitter_fraction <= 0){
    stop("The jitter_fraction parameter should be greater than zero.")
  }
  
  lp <- 0.001
  up <- 1 - lp
  mu <- (param$lower + param$upper)*0.5
  
  ## Assume normal dist and thus symetrical, SD:
  dsd <- (param$lower - mu)/qnorm(lp)
  
  ## Parameter value in normal space, aka Jitter shift term
  jst <- pnorm(param$value[[1]], mu, dsd)
  
  ## Pick a random value
  new.pr <- runif(n = 1, 
                  min = jst - jitter_fraction, 
                  max = jst + jitter_fraction)
  
  ## Within bounds?
  if (new.pr > up) new.pr <- up - 0.1*(up - jst)
  else{
    if (new.pr < lp) new.pr <- lp + 0.1*(jst -lp)  
  }
  
  
  ## Convert to value
  param$value[[1]] <- qnorm(new.pr, mu, dsd)
  return(param)
  
}

g3_jitter <- function(params, jitter_fraction = 0.1, patterns_to_ignore = '_weight'){
  
  if (!all(c('value', 'lower', 'upper') %in% names(params))){
    print("The columns 'value', 'lower', 'upper' were not all found in the 'params' parameter.")
    stop(invisible())
  }
  
  tmp <- lapply(split(params, params$switch), function(x, jitter_fraction){
  #  print(x)
    if (any(grepl(patterns_to_ignore, names(x))) || !x$optimise){ return(x) }
    else{ return(jitter_param(x, jitter_fraction = jitter_fraction)) }

  }, jitter_fraction = jitter_fraction)
  
  out <- do.call('rbind', tmp)
  return(out[match(params$switch, out$switch),])
}

