#' Print \code{predict_adjust} object
#'
#' Print an object of class \code{predict_adjust} produced
#' by the \code{\link[api2lm]{predict_adjust}} function.
#'
#' @param x An \code{predict_adjust} object produced by the
#'   \code{\link[api2lm]{predict_adjust}} function.
#' @param ... Additional arguments to the
#'   \code{\link[base]{print.default}} function, such as
#'   \code{digits}.
#' @return NULL
#' @author Joshua French
#' @export
#' @examples
#' fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
#' (cia <- predict_adjust(fit))
#' print(cia, digits = 3)
print.predict_adjust = function(x, ...) {
  method <- attr(x, "method")
  interval <- attr(x, "interval")
  if(interval != "none") {
    if (method == "none") {
      cat(paste("\nUnadjusted", interval, "intervals\n"))
    } else if (method == "scheffe") {
      cat(paste("\nScheffe-adjusted", interval, "intervals\n"))
    } else if (method == "bonferroni") {
      cat(paste("\nBonferroni-adjusted", interval, "intervals\n"))
    } else if (method == "wh") {
      cat(paste("\nWorking-Hotelling-adjusted", interval, "intervals\n"))
    }
    cat(paste("\nFamily-wise confidence level of at least",
              attr(x, "adj_level")),"\n\n")
  }

  if (is.element("matrix", class(x))) {
    print.default(x[seq_len(nrow(x)),], ...)
  } else {
    print.default(x[seq_along(x)], ...)
  }
}
