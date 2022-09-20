#' Return coefficient matrix
#'
#' \code{coef_matrix} returns the \code{coefficients}
#' element of the \code{summary} function, which is a
#' matrix with columns for the estimated coefficients, their
#' standard error, t-statistic and corresponding (two-sided)
#' p-values.
#'
#' @inheritParams stats::summary.lm
#' @inheritParams base:signif
#' @param ...
#'
#' @return A \eqn{p \times 4} matrix with columns for the
#'   estimated coefficient, its standard error, t-statistic
#'   and corresponding (two-sided) p-value. Aliased
#'   coefficients are omitted.
#' @export
#' @author Joshua P. French
#' @examples
#' ## a fitted model
#' fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
#' coef_matrix(fit)
coef_matrix <- function(object, digits = 3) {
 signif(summary(object)$coefficients,
        digits = digits)
}