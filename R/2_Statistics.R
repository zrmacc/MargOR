#' Asymptotic Risk Difference
#' 
#' @param p0 Marginal proportion in arm 0.
#' @param n0 Sample size in arm 0.
#' @param p1 Marginal proportion in arm 1.
#' @param n1 Sample size in arm 1.
#' @param alpha Type 1 error rate.
#' @importFrom stats pnorm qnorm
#' @return Data.frame containing:
#' \itemize{
#'   \item 'Est', the estimated risk difference, arm 1 minus arm 0.
#'   \item The standard error 'SE'. 
#'   \item The 'Lower' and 'Upper' confidence bound.
#'   \item The asymptotic 'P' value.
#' }

RiskDiff <- function(p0, n0, p1, n1, alpha = 0.05) {
  
  # Risk Difference.
  risk_diff <- p1 - p0
  
  # Sampling variance of the risk difference.
  v <- p1 * (1 - p1) / n1 + p0 * (1 - p0) / n0
  se <- sqrt(v)
  
  # Z-score and p-value.
  z_stat <- risk_diff / se
  p_val <- 2 * pnorm(q = abs(z_stat), lower.tail = FALSE)
  
  # CI.
  crit <- qnorm(p = 1 - alpha / 2)
  lower <- risk_diff - crit * se
  upper <- risk_diff + crit * se
  
  # Output
  out <- data.frame(
    "Stat" = "RiskDiff",
    "Est" = risk_diff,
    "SE" = se,
    "Lower" = lower,
    "Upper" = upper,
    "P" = p_val
  )
  return(out)
}


# -----------------------------------------------------------------------------

#' Asymptotic Risk Difference
#' 
#' @param p0 Marginal proportion in arm 0.
#' @param n0 Sample size in arm 0.
#' @param p1 Marginal proportion in arm 1.
#' @param n1 Sample size in arm 1.
#' @param alpha Type 1 error rate.
#' @importFrom stats pnorm qnorm
#' @return Data.frame containing:
#' \itemize{
#'   \item The odds ratio 'RR', arm 1 over arm 0.
#'   \item The standard error 'SE'. 
#'   \item The 'Lower' and 'Upper' confidence bound.
#'   \item The asymptotic 'P' value.
#' }

RiskRatio <- function(p0, n0, p1, n1, alpha = 0.05) {
  
  # Risk Difference.
  risk_ratio <- p1 / p0
  
  # Sampling variance of log risk ratio.
  v <- (1 - p1) / (p1 * n1) + (1 - p0) / (p0 * n0)
  se <- sqrt(v)
  
  # Z-score and p-value.
  z_stat <- log(risk_ratio) / se
  p_val <- 2 * pnorm(q = abs(z_stat), lower.tail = FALSE)
  
  # CI.
  crit <- qnorm(p = 1 - alpha / 2)
  lower <- risk_ratio * exp(-crit * se)
  upper <- risk_ratio * exp(+crit * se)
  
  # Output
  out <- data.frame(
    "Stat" = "RiskRatio",
    "Est" = risk_ratio,
    "SE" = se * risk_ratio,
    "Lower" = lower,
    "Upper" = upper,
    "P" = p_val
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
    "Stat" = "OddsRatio",
    "Est" = odds_ratio,
    "SE" = se * odds_ratio,
    "Lower" = lower,
    "Upper" = upper,
    "P" = p_val
  )
  return(out)
}


# -----------------------------------------------------------------------------

