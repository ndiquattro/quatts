#' Format Model Output
#'
#' Takes model output and prints APA formatted results in a RMarkdown document.
#'
#' Works with output from t.test(), aov(), and nlme().
#' @param mod A fitted model object.
#' @param stratum Which stratum of a repeates measures ANOVA to format.
#' @return Markdown formatted text printed to console.
#' @importFrom magrittr "%>%"
#' @export
rformatter <- function(mod) {
  # P-value lookup
  plooker <- function(pval) {
    # Round it
    pval <- round(pval, 3)

    # Compare
    if (pval < .001) {
      return(paste0('< ', .001, ' ***'))
    } else if (pval < .01) {
      return(paste0('< ', .01, ' ***'))
    } else if (pval < .05) {
      return(paste0('< ', .05, ' ***'))
    } else {
      return(paste0('= ', pval))
    }
  }

  # t.test object -----------------------------------------------------------
  if (is(mod)[1] == 'htest') {
    # Format
    tform <- broom::tidy(mod) %>%
              dplyr::summarise(
                tline = paste0('* ', tmod$data.name,
                               ': _t_(', parameter, ') = ',
                               round(statistic, 2),
                               ', _p_ ', plooker(p.value))
              )
    return(cat(tform$tline, "\n\n"))
  }

  # aov object --------------------------------------------------------------
  if (is(mod)[1] == 'aov') {
    # Format
    aform <- broom::tidy(mod) %>%
              dplyr::mutate(resdf = df[term == 'Residuals']) %>%
              dplyr::filter(term != 'Residuals') %>%
              dplyr::rowwise() %>%
              dplyr::summarise(
                aline = paste0('* ', term,
                               ': _F_(', df, ',', resdf, ') = ',
                               round(statistic, 2), ' _p_ ', plooker(p.value))
              )

    # Print
    return(cat(aform$aline, "\n\n"))
  }

  # nlme object -------------------------------------------------------------
  if (is(mod)[1] == 'nlme') {
    # Format
    nform <- as.data.frame(summary(mod)$tTable) %>%
              dplyr::mutate(par = rownames(.)) %>%
              dplyr::rowwise() %>%
              summarise(
                tline = paste0('* ', par,' = ', round(Value, 2),
                               ', _t_(', DF, ') = ',
                               round(`t-value`, 2),
                               ', _p_ ',
                               plooker(`p-value`))
              )

    # Print
    return(cat(nform$tline, "\n\n"))
  }

}
