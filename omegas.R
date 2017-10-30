# Compute omega-squared and partial omega-squared
# By Arnoud Plantinga
# Based on http://stats.stackexchange.com/a/126520
#https://gist.github.com/arnoud999
# Functions ---------------------------------------------------------------

# Omega-squared
Omegas <- function(mod){
  aovMod <- mod
  if(!any(class(aovMod) %in% 'aov')) aovMod <- aov(mod)
  sumAov     <- summary(aovMod)[[1]]
  residRow   <- nrow(sumAov)
  msError    <- sumAov[residRow,3]
  dfEffects  <- sumAov[1:{residRow-1},1]
  ssEffects  <- sumAov[1:{residRow-1},2]
  msEffects  <- sumAov[1:{residRow-1},3]
  ssTotal    <- rep(sum(sumAov[1:residRow, 2]), 3)
  Omegas <- abs((ssEffects - dfEffects*msError)/(ssTotal + msError))
  names(Omegas) <- rownames(sumAov)[1:{residRow-1}]
  Omegas
}

# Partial omega-squared
partialOmegas <- function(mod){
  aovMod <- mod
  if(!any(class(aovMod) %in% 'aov')) aovMod <- aov(mod)
  sumAov     <- summary(aovMod)[[1]]
  residRow   <- nrow(sumAov)
  dfError    <- sumAov[residRow,1]
  msError    <- sumAov[residRow,3]
  nTotal     <- nrow(model.frame(aovMod))
  dfEffects  <- sumAov[1:{residRow-1},1]
  ssEffects  <- sumAov[1:{residRow-1},2]
  msEffects  <- sumAov[1:{residRow-1},3]
  partOmegas <- abs((dfEffects*(msEffects-msError)) /
                      (ssEffects + (nTotal -dfEffects)*msError))
  names(partOmegas) <- rownames(sumAov)[1:{residRow-1}]
  partOmegas
}

#https://stats.stackexchange.com/questions/2962/omega-squared-for-measure-of-effect-in-r
omega_sq <- function(aov_in, neg2zero=T){
  aovtab <- summary(aov_in)[[1]]
  n_terms <- length(aovtab[["Sum Sq"]]) - 1
  output <- rep(-1, n_terms)
  SSr <- aovtab[["Sum Sq"]][n_terms + 1]
  MSr <- aovtab[["Mean Sq"]][n_terms + 1]
  SSt <- sum(aovtab[["Sum Sq"]])
  for(i in 1:n_terms){
    SSm <- aovtab[["Sum Sq"]][i]
    DFm <- aovtab[["Df"]][i]
    output[i] <- (SSm-DFm*MSr)/(SSt+MSr)
    if(neg2zero & output[i] < 0){output[i] <- 0}
  }
  names(output) <- rownames(aovtab)[1:n_terms]
  
  return(output)
}