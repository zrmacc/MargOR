#' Compare Marginal Event Rates.
#' 
#' Given stratified event count data for two arms, analyze the marginal
#' event rates via the risk difference, risk ratio, and odds ratio.
#' 
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @param alpha Type I error. 
#' @param reps Bootstrap replicates.
#' @importFrom methods new
#' @export
#' @return Object of class `margRates` containing these slots:
#' \itemize{
#'   \item `@Rates`, the marginal event rates in each arm.
#'   \item `@RD`, risk difference analysis.
#'   \item `@RR`, risk ratio analysis.
#'   \item `@OR`, odds ratio analysis.
#' }

CompMargRates <- function(
  y0, 
  n0, 
  y1, 
  n1, 
  alpha = 0.05,
  reps = 2e3
) {
  
  # Asymptotic inference.
  asymp <- CalcMargStats(
    y0 = y0,
    n0 = n0,
    y1 = y1,
    n1 = n1,
    alpha = alpha
  )
  
  # Rates.
  rates <- asymp$Rates
  stats_asymp <- asymp$Stats
  stats_asymp$Method <- "Asymptotic"
  
  # Bootstrap inference.
  stats_boot <- Stats.Boot(
    y0 = y0,
    n0 = n0,
    y1 = y1,
    n1 = n1,
    alpha = alpha,
    reps = reps
  )
  stats_boot$Method <- "Bootstrap"
  
  # Output.
  out <- rbind(stats_asymp, stats_boot)
  out <- out[, c(7, 1:6)]
  out <- new(
    Class = "margRates",
    Rates = rates,
    RD = out[out$Stat == "RiskDiff", ],
    RR = out[out$Stat == "RiskRatio", ],
    OR = out[out$Stat == "OddsRatio", ]
  )
  return(out)
}