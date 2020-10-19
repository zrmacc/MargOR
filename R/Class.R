#' Compare CICs Object
#'
#' Defines the object class returned by \code{\link{MargOR}}. 
#'
#' @slot CIs Confidence intervals. 
#' @slot Rates Marginal rates.
#' @name margOR-class
#' @rdname margOR-class
#' @exportClass margOR

setClass(
  Class = "margOR",
  representation = representation(
   CIs = "data.frame",
   Rates = "data.frame"
  )
)

# -----------------------------------------------------------------------------
# Print Method
# -----------------------------------------------------------------------------

#' Print Method
#'
#' Print method for objects of class \code{margOR}.
#'
#' @param x An object of class \code{margOR}.
#' @param ... Unused.
#' @export

print.margOR <- function (x, ...) {
  
  # Rates
  cat('Marginal Rates:\n')
  show(x@Rates)
  cat('\n\n')
  
  # CIs.
  cat('CIs:\n')
  show(x@CIs)
  cat('\n\n')

}

# -----------------------------------------------------------------------------
# Show Method
# -----------------------------------------------------------------------------

#' Show Method
#'
#' @param object An object of class \code{margOR}.
#' @rdname fit-method
#' @importFrom methods show

setMethod(
  f = "show",
  signature = c(object = "margOR"),
  definition = function(object) {print.margOR(x = object)}
)

