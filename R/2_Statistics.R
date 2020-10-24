#' Asymptotic Risk Difference
#' 
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @param alpha Type 1 error rate.
#' @importFrom stats pnorm qnorm
#' @return Data.frame containing:
#' \itemize{
#'   \item 'Est', the estimated risk difference, arm 1 minus arm 0.
#'   \item The standard error 'SE'. 
#'   \item The 'Lower' and 'Upper' confidence bound.
#'   \item The asymptotic 'P' value.
#' }

RiskDiff <- function(y0, n0, y1, n1, alpha = 0.05) {
  
  # Marginal rates.
  marg <- MargRate(y0, n0, y1, n1)
  p0 <- marg$p0
  r0 <- marg$r0
  p1 <- marg$p1
  r1 <- marg$r1
  weights <- marg$weights
  
  # Risk Difference.
  risk_diff <- p1 - p0
  
  # Sampling variance of the risk difference.
  v <- sum(r1 * (1 - r1) / n1 * weights^2) + sum(r0 * (1 - r0) / n0 * weights^2)
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
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @param alpha Type 1 error rate.
#' @importFrom stats pnorm qnorm
#' @return Data.frame containing:
#' \itemize{
#'   \item The odds ratio 'RR', arm 1 over arm 0.
#'   \item The standard error 'SE'. 
#'   \item The 'Lower' and 'Upper' confidence bound.
#'   \item The asymptotic 'P' value.
#' }

RiskRatio <- function(y0, n0, y1, n1, alpha = 0.05) {
  
  # Marginal rates.
  marg <- MargRate(y0, n0, y1, n1)
  p0 <- marg$p0
  r0 <- marg$r0
  p1 <- marg$p1
  r1 <- marg$r1
  weights <- marg$weights
  
  # Risk Difference.
  risk_ratio <- p1 / p0
  
  # Sampling variance of log risk ratio.
  v <- sum(r1 * (1 - r1) / n1 * weights^2) / p1^2 +
    sum(r0 * (1 - r0) / n0 * weights^2) / p0^2
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
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @param alpha Type 1 error rate.
#' @importFrom stats pnorm qnorm
#' @return Data.frame containing:
#' \itemize{
#'   \item The odds ratio 'OR'.
#'   \item The standard error 'SE'. 
#'   \item The 'Lower' and 'Upper' confidence bound.
#'   \item The asymptotic 'P' value.
#' }

OddsRatio <- function(y0, n0, y1, n1, alpha = 0.05) {
  
  # Marginal rates.
  marg <- MargRate(y0, n0, y1, n1)
  p0 <- marg$p0
  r0 <- marg$r0
  p1 <- marg$p1
  r1 <- marg$r1
  weights <- marg$weights
  
  # Marginal odds ratio.
  odds_ratio <- p1 / (1 - p1) / (p0 / (1 - p0))
  
  # Sampling variance of log odds ratio.
  v <- sum(r1 * (1 - r1) / n1 * weights^2) / (p1 * (1 - p1))^2 +
    sum(r0 * (1 - r0) / n0 * weights^2) / (p0 * (1 - p0))^2
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

