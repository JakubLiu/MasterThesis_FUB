BradleyLim <- function(alpha, n_simul){
  Lower <- round(alpha-qnorm(1-alpha/2)*sqrt(alpha*(1-alpha)/n_simul),4)
  Upper <- round(alpha+qnorm(1-alpha/2)*sqrt(alpha*(1-alpha)/n_simul),4)
  return(c(Lower, Upper))
}
