#' Bootstrap Events
#'
#' Bootstrap the number of events observed in each cell of a contingency table,
#' keeping the number of subjects in each cell fixed.
#'
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @importFrom stats rbinom
#' @return List containing the bootstrapped event counts.

BootEvents <- function(y0, n0, y1, n1) {
  
  # Categories.
  k <- length(n0)
  
  # Event rates. 
  n <- c(n0, n1)
  rates <- c(y0 / n0, y1 / n1)
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