#' Bootstrap Events under the Null
#'
#' Bootstrap the number of events observed in each cell of a contingency table,
#' keeping the number of subjects in each cell fixed, and assuming no difference
#' between treatment arms.
#'
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @importFrom stats rbinom
#' @return List containing the bootstrapped event counts.

BootEventsNull <- function(y0, n0, y1, n1) {
  
  # Categories.
  k <- length(n0)
  
  # Event rates. 
  n <- c(n0, n1)
  rates <- rep((y0 + y1) / (n0 + n1), times = 2)
  cells <- length(rates)
  
  # Bootstrap counts.
  y_boot <- lapply(
    seq_len(cells),
    function(i){
      rbinom(n = 1, size = n[i], prob = rates[i])
    }
  )
  y_boot <- do.call(c, y_boot)
  
  # Output.
  out <- list()
  out$y0 <- y_boot[1:k]
  out$y1 <- y_boot[(k + 1):(2 * k)]
  return(out)
}


# -----------------------------------------------------------------------------

#' Test the Null Hypothesis via Bootstrap
#' 
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @param alpha Type 1 error rate.
#' @param reps Bootstrap replicates.
#' @importFrom stats quantile sd
#' @return Data.frame containing:

Test.Null <- function(
  y0,
  n0, 
  y1, 
  n1, 
  alpha = 0.05,
  reps
) {
  
  # Observed.
  obs <- CalcMargStats(
    y0 = y0,
    n0 = n0,
    y1 = y1,
    n1 = n1,
    alpha = alpha
  )
  obs_stats <- obs$Stats
  obs_rd <- obs_stats$Est[1]
  obs_rr <- obs_stats$Est[2]
  obs_or <- obs_stats$Est[3]
  
  # Bootstrap.
  aux <- function(b) {
    
    # Bootstrap data.
    boot_data <- BootEventsNull(y0 = y0, n0 = n0, y1 = y1, n1 = n1)
    
    # Marginal odds ratio.
    boot <- CalcMargStats(
      y0 = boot_data$y0,
      n0 = n0,
      y1 = boot_data$y1,
      n1 = n1
    )
    boot_stats <- boot$Stats
    boot_rd <- boot_stats$Est[1]
    boot_rr <- boot_stats$Est[2]
    boot_or <- boot_stats$Est[3]
    
    # Output
    out <- c(
      "RD_P" = (abs(boot_rd) >= abs(obs_rd)),
      "RR_P" = (abs(log(boot_rr)) >= abs(log(obs_rr))),
      "OR_P" = (abs(log(boot_or)) >= abs(log(obs_or)))
    )
    return(out)
  }
  sim <- lapply(seq_len(reps), aux)
  sim <- do.call(rbind, sim)
  sim <- rbind(c(1, 1, 1), sim)
  
  # Output
  out <- obs_stats[, 1:2]
  out$P <- apply(sim, 2, mean)
  return(out)
}