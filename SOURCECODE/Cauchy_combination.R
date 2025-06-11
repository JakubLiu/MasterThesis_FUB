
Cauchy <- function(pval_MCTP, pval_Glob, w_MCTP, w_Glob){
  C <- w_MCTP*tan((0.5-pval_MCTP)*pi) + w_Glob*tan((0.5-pval_Glob)*pi)
  pvalue <- pcauchy(C)
  return(pvalue)
}