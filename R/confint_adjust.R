#' Adjust confidence intervals for multiple comparisons
#'
#' A function to produce confidence intervals with a family confidence
#' coefficient of at least \code{level} for \code{lm} objects. The function
#' slightly revises the code used in the \code{\link[stats]{confint.lm}} function.
#'
#' Let \code{a = 1 - level}. All intervals are computed using the formula
#' \code{estimate +/- m * ese}, where \code{m} is a multiplier and
#' \code{ese} is the estimated standard error of the \code{estimate}.
#'
#' \code{method = "none"} (no correction) produces the standard t-based confidence intervals with
#' multiplier \code{qt(1 - a/2, df = object$df.residual)}.
#'
#' \code{method = "bonferroni"} produces Bonferroni-adjusted intervals
#' that use the multiplier \code{m = qt(1 - a/(2 * k), df = object$df.residual)},
#' where \code{k} is the number of intervals being produced.
#'
#' \code{method = "wh"} produces Working-Hotelling-adjusted intervals that
#' are valid for all linear combinations of the regression coefficients, which uses
#' the multiplier \code{m = sqrt(k * qf(level, df1 = k, df2 = object$df.residual))}.

#' @inheritParams stats::confint.lm
#' @param method A character string indicating the type of adjustment to make. The default choice is \code{"none"}.
#' The other available options are \code{"bonferroni"} and \code{"wh"} (Working-Hotelling).
#'
#' @inherit stats::confint.lm return
#' @export
#' @references
#' Bonferroni, C. (1936). Teoria statistica delle classi e calcolo delle probabilita. Pubblicazioni del R Istituto Superiore di Scienze Economiche e Commericiali di Firenze, 8, 3-62.
#'
#' Working, H., & Hotelling, H. (1929). Applications of the theory of error to the interpretation of trends. Journal of the American Statistical Association, 24(165A), 73-85. doi:10.1080/01621459.1929.10506274
#' Working, H., & Hotelling, H. (1929). Applications of the theory of error to the interpretation of trends. Journal of the American Statistical Association, 24(165A), 73-85. doi:10.1080/01621459.1929.10506274
#' @examples
#' ## an extension of the documentation for confint.lm
#' fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
#' # standard intervals
#' confint_adjust(fit)
#' # bonferroni-adjusted intervals
#' confint_adjust(fit, method = "b")
#' # working-hotelling adjusted intervals
#' confint_adjust(fit, method = "wh")
confint_adjust <- function(object, parm, level = 0.95, method = "none") {
  # match method
  method <- match.arg(method, c("none", "bonferroni", "wh"))
  # estimated coefficients
  cf <- stats::coef(object)
  # estimated standard errors
  ses <- sqrt(diag(stats::vcov(object)))
  # select variables
  pnames <- names(ses)
  if (is.matrix(cf)) {
    cf <- stats::setNames(as.vector(cf), pnames)
  }
  if (missing(parm)) {
    parm <- pnames
  } else if (is.numeric(parm)) {
    parm <- pnames[parm]
  }
  # alpha/2, 1-alpha/2
  a <- (1 - level)/2
  # number of intervals
  k <- length(parm)
  if (method == "none") {
    fac <- stats::qt(c(a, 1 - a), object$df.residual)
  } else if (method == "bonferroni") {
    fac <- stats::qt(c(a/length(parm), 1 - a/length(parm)), object$df.residual)
  } else if (method == "wh") {
    fac <- c(-1, 1) * sqrt(k * stats::qf(level, df1 = k, df2 = object$df.residual))
  }
  # format returned object
  pct <- format_perc_api2lm(c(a, 1 - a), 3)
  ci <- array(NA_real_,
              dim = c(length(parm), 2L),
              dimnames = list(parm, pct))
  ci[] <- cf[parm] + ses[parm] %o% fac
  ci
}

