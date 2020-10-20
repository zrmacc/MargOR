#' Calculate Marginal Summary Stats
#'
#' Calculate the marginal event rates and summary stats starting from stratified
#' event counts.
#' 
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @param alpha Type I error. 
#' @return List containing two data.frames, 
#' \itemize{
#'   \item 'Rates', returned by \code{\link{MargRate}}.
#'   \item 'OR', returned by \code{\link{OddsRatio}}.
#' }

CalcMargStats <- function(y0, n0, y1, n1, alpha = 0.05) {
  
  # Marginal rates.
  rates <- MargRate(
    y0 = y0,
    n0 = n0,
    y1 = y1,
    n1 = n1
  )
  n0 <- rates$N[1]
  n1 <- rates$N[2]
  p0 <- rates$Rate[1]
  p1 <- rates$Rate[2]
  
  # Risk difference.
  rd <- RiskDiff(p0, n0, p1, n1, alpha)
  
  # Risk ratio.
  rr <- RiskRatio(p0, n0, p1, n1, alpha)
  
  # Odds ratio.
  or <- OddsRatio(p0, n0, p1, n1, alpha)
  
  # Output.
  out <- list()
  out$Rates <- rates
  out$Stats <- rbind(rd, rr, or)
  return(out)
}