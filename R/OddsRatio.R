
# -----------------------------------------------------------------------------

#' Marginal Rates
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


# -----------------------------------------------------------------------------

#' Asymptotic Odds Ratio
#' 
#' @param p0 Proportion in arm 0.
#' @param n0 Sample size in arm 0.
#' @param p1 Proportion in arm 1.
#' @param n1 Sample size in arm 1.
#' @param alpha Type 1 error rate.
#' @importFrom stats pnorm qnorm
#' @return Data.frame containing:
#' \itemize{
#'   \item The odds ratio 'OR'.
#'   \item The standard error 'SE'. 
#'   \item The 'Lower' and 'Upper' confidence bound.
#'   \item The asymptotic 'P' value.
#' }

OddsRatio <- function(p0, n0, p1, n1, alpha = 0.05) {
  
  # Odds ratio.
  odds_0 <- p0 / (1 - p0)
  odds_1 <- p1 / (1 - p1)
  odds_ratio <- odds_1 / odds_0
  
  # Sampling variance of log odds ratio.
  v <- 1 / (n1 * p1 * (1 - p1))  + 1 / (n0 * p0 * (1 - p0))
  se <- sqrt(v)
  
  # Z-score and p-value.
  z_stat <- log(odds_ratio) / se
  p_val <- 2 * pnorm(q = abs(z_stat), lower.tail = FALSE)
  
  # CI.
  crit <- qnorm(p = 1 - alpha / 2)
  lower <- odds_ratio * exp(-crit * se)
  upper <- odds_ratio * exp(+crit * se)
  
  # Output
  out <- data.frame(
    "OR" = odds_ratio,
    "SE" = se * odds_ratio,
    "Lower" = lower,
    "Upper" = upper,
    "P" = p_val
  )
  return(out)
}


# -----------------------------------------------------------------------------

#' Calculate Marginal Odds Ratio
#'
#' Calculate the marginal event rates and odds ratio starting from stratified
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

CalcMargOR <- function(y0, n0, y1, n1, alpha = 0.05) {
  
  # Marginal rates.
  rates <- MargRate(
    y0 = y0,
    n0 = n0,
    y1 = y1,
    n1 = n1
  )
  
  # Odds ratio.
  or <- OddsRatio(
    p0 = rates$Rate[1],
    n0 = rates$N[1],
    p1 = rates$Rate[2],
    n1 = rates$N[2],
    alpha = alpha
  )
  
  # Output.
  out <- list()
  out$Rates <- rates
  out$OR <- or
  return(out)
}


# -----------------------------------------------------------------------------

#' Bootstrap Odds Ratio
#' 
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @param alpha Type 1 error rate.
#' @param reps Bootstrap replicates.
#' @importFrom stats quantile sd
#' @return Data.frame containing:

OddsRatio.Boot <- function(
  y0,
  n0, 
  y1, 
  n1, 
  alpha = 0.05,
  reps
) {
  
  # Observed.
  obs_or <- CalcMargOR(
    y0 = y0,
    n0 = n0,
    y1 = y1,
    n1 = n1,
    alpha = alpha
  )
  obs_or <- obs_or$OR$OR
  obs_log_or <- log(obs_or)
  
  # Bootstrap.
  aux <- function(b) {
    
    # Bootstrap data.
    boot_data <- BootEvents(y0 = y0, n0 = n0, y1 = y1, n1 = n1)
    
    # Marginal odds ratio.
    boot_or <- CalcMargOR(
      y0 = boot_data$y0,
      n0 = n0,
      y1 = boot_data$y1,
      n1 = n1
    )
    boot_or <- boot_or$OR$OR
    boot_log_or <- log(boot_or)
    
    # Output
    out <- c(
      "OR" = boot_or,
      "P" = 1 * (sign(obs_log_or) != sign(boot_log_or))
    )
    return(out)
  }
  
  sim <- lapply(seq_len(reps), aux)
  sim <- do.call(rbind, sim)
  
  # CI.
  alpha2 = alpha / 2
  ci <- as.numeric(
    quantile(
      x = sim[, 1], 
      probs = c(alpha2, 1 - alpha2), 
      na.rm = TRUE
    )
  )
  
  # P-value.
  p_val <- 2 * mean(c(1, sim[, 2]))
  
  # Output
  out <- data.frame(
    "OR" = obs_or,
    "SE" = sd(sim[, 1]),
    "Lower" = ci[1],
    "Upper" = ci[2],
    "P" = p_val
  )
  return(out)
}


# -----------------------------------------------------------------------------


#' Estimate the Marginal Odds Ratio.
#' 
#' Given stratified event count data for two arms, calculates the marginal 
#' odds ratio, and constructs the asymptotic and bootstrap confidence intervals.
#' 
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @param alpha Type I error. 
#' @param reps Bootstrap replicates.
#' @importFrom methods new
#' @export
#' @return Object of class `margOR` containing these slots:
#' \itemize{
#'   \item `@Rates`, the marginal event rates in each arm.
#'   \item `@CIs`, asymptotic and bootstrap confidence intervals. 
#' }

MargOR <- function(
  y0, 
  n0, 
  y1, 
  n1, 
  alpha = 0.05,
  reps = 2e3
  ) {
  
  # Asymptotic inference.
  or_asymp <- CalcMargOR(
    y0 = y0,
    n0 = n0,
    y1 = y1,
    n1 = n1,
    alpha = alpha
  )
  
  # Rates
  rates <- or_asymp$Rates
  or_asymp <- or_asymp$OR
  or_asymp$Method <- "Asymptotic"
  
  # Bootstrap inference.
  or_boot <- OddsRatio.Boot(
    y0 = y0,
    n0 = n0,
    y1 = y1,
    n1 = n1,
    alpha = alpha,
    reps = reps
  )
  or_boot$Method <- "Bootstrap"

  # Output.
  or <- rbind(or_asymp, or_boot)
  or <- or[, c(6, 1:5)]
  out <- new(
    Class = "margOR",
    CIs = or,
    Rates = rates
  )
  return(out)
}