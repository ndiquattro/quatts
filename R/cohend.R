#'Cohen's D Calculator
#'
#'Calculates Cohen's D effect size for a paired t-test
#'@param tobj Results of a t.test()
#'@export
#'@note Based on formula from: http://www.missouristate.edu/assets/rstats/Effect_Size_Calcul  ator_RStats.xlsx

cohend <- function(tobj){

  # Make Dataframe
  CoDat <- data.frame(test  = tobj$data.name,
                      tstat = tobj$statistic,
                      n     = tobj$parameter + 1,
                      co.d  = tobj$statistic / sqrt(tobj$parameter + 1))
  # Return
  CoDat
}