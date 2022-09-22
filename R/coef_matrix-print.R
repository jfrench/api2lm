#'
#' Print an object of class \code{coef_matrix} produced
#' by the \code{\link[api2lm]{coef_matrix}} function.
#'
#' @param x An \code{coef_matrix} object produced by the
#'   \code{\link[api2lm]{coef_matrix}} function.
#' @param ... Additional arguments to the
#'   \code{\link[base]{print.data.frame}} function, such as
#'   \code{digits}.
#' @return NULL
#' @author Joshua French
#' @export
#' @examples
#' fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
#' (coefm <- coef_matrix(fit))
#' print(coefm, digits = 3)
print.coef_matrix = function(x, ...) {
  print.data.frame(as.data.frame(x), ...)
}
