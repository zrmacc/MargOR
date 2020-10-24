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
#'   \item 'Rates', data.frame of marginal rates.
#'   \item 'Stats', data.frame of marginal contrasts.
#' }

CalcMargStats <- function(y0, n0, y1, n1, alpha = 0.05) {
  
  # Marginal rates.
  marg <- MargRate(y0, n0, y1, n1)
  rates <- data.frame(
    "Arm" = c(0, 1),
    "N" = c(sum(n0), sum(n1)),
    "Rates" = c(marg$p0, marg$p1)
  )
  
  # Risk difference.
  rd <- RiskDiff(y0, n0, y1, n1, alpha)
  
  # Risk ratio.
  rr <- RiskRatio(y0, n0, y1, n1, alpha)
  
  # Odds ratio.
  or <- OddsRatio(y0, n0, y1, n1, alpha)
  
  # Output.
  out <- list()
  out$Rates <- rates
  out$Stats <- rbind(rd, rr, or)
  return(out)
}
