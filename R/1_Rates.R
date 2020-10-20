# -----------------------------------------------------------------------------

#' Marginal Rates
#' 
#' Calculates the marginal event rates in each arm, weighting by the total 
#' stratum size.
#' 
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @return Data.frame containing the 'Arm', 'N', and the marginal 'Rate'.

MargRate <- function(y0, n0, y1, n1) {
  
  # Stratum proportions.
  n <- sum(n0 + n1)
  strat_props <- (n0 + n1) / n
  
  # Stratum specific event rates.
  r0 <- y0 / n0
  r1 <- y1 / n1
  
  # Marginal rates.
  p0 <- sum(r0 * strat_props)
  p1 <- sum(r1 * strat_props)
  
  # Output.
  out <- data.frame(
    "Arm" = c(0, 1),
    "N" = c(sum(n0), sum(n1)),
    "Rate" = c(p0, p1)
  )
  return(out)
}