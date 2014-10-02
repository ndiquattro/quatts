#' Partial Eta Squared Calculator
#'
#' Calculates the partial eta squared effect size statistic for each factor in a repeated measures ANOVA
#' @param modsum summary object, e.g. summary(anovamod)
#' @export
#' @note Formula from: http://jalt.org/test/bro_28.htm

paretasq <- function(modsum){
  # Make index vector for each condition, skipping sub
  ncon <- 2:length(names(modsum))

  # Function for getting Sum of Squares by condition
  sqrgrabber <- function(connum){
    sqrs <- c(names(modsum[connum]),
              modsum[connum][[1]][[1]][[2]][[1]],
              modsum[connum][[1]][[1]][[2]][[2]])
  }

  # Get sum of squares for each condition
  sqmat <- data.frame(do.call("rbind", lapply(ncon, sqrgrabber)),
                      stringsAsFactors = FALSE)
  names(sqmat) <- c("test", "sumsq", "sumerr")
  sqmat$sumsq <- as.numeric(sqmat$sumsq)
  sqmat$sumerr <- as.numeric(sqmat$sumerr)

  # Calculate partial eta squared
  sqmat$peta <- sqmat$sumsq / (sqmat$sumsq + sqmat$sumerr)

  # Return data.frame
  sqmat
}
