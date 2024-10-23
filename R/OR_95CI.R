#' @title Calculate Odds Ratio
#' @description Calculate Odds Ratio and 95\% Confidence Interval
#' @param coef Coefficinet of the predictor
#' @param se Standard error of the predictor
#' @param siglevel Significance level
#' @param roundto Number of decimal places to round to
#'
#' @export
#'


OR_95CI <- function(coef, se, siglevel, roundto) {
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)

  ORresult <- paste0(
    format(round(OR, roundto), nsmall = roundto), " (",
    format(round(ORlcl, roundto), nsmall = roundto), ", ",
    format(round(ORucl, roundto), nsmall = roundto), ")"
  )

  return(ORresult)
}
