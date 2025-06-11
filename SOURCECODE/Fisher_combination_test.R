
Fisher <- function(pval_MCTP, pval_Glob){
  Fish <- (-2)*(log(pval_MCTP) + log(pval_Glob))
  pvalue <- pchisq(Fish, 2, lower.tail = FALSE, log.p = FALSE)
  return(pvalue)
}