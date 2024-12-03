#' Compute variance inflation factors
#'
#' \code{vif_stats} computes the variance inflation factors
#' of an object. If \code{object} is a \code{data.frame},
#' then the variance inflation factors are computed for the
#' \code{numeric} variables. If \code{object} is an
#' \code{lm} object, then variance inflation factors for the
#' matrix of regressor variables not including the
#' intercept.
#'
#' @param object A \code{data.frame} or \code{lm} object.
#' @param ... Not currently implemented.
#'
#' @return A \code{numeric} vector
#' @export
#'
#' @examples
#' # compute vif stats for a data frame
#' vif_stats(crime2009)
#' lmod <- lm(violent ~ hs_grad + poverty + white + single + urban,
#'            data = crime2009)
#' # compute vif stats for a fitted model
#' vif_stats(lmod)
vif_stats <- function(object, ...) {
  UseMethod("vif_stats")
}

#' @export
vif_stats.lm <- function(object, ...) {
  # formula for x matrix alone
  xform <- stats::formula(stats::delete.response(stats::terms(object)))
  # extract predictors from formula
  # x <- stats::model.frame(xform, data = object$model)
  # diag(solve(crossprod(scale(x))/(nobs(object) - 1)))
  # compute correlation matrix of x w/o intercept
  r <- stats::cor(stats::model.frame(xform, data = object$model))
  # formula from 10.39 of p. 408 of
  # Applied Linear Statistical Models, 5e by Kutner et al.
  diag(solve(r))
}

#' @export
vif_stats.data.frame <- function(object, ...) {
  # compute correlation matrix of object
  r <- stats::cor(object[, unlist(lapply(object, is.numeric))])
  # formula from 10.39 of p. 408 of
  # Applied Linear Statistical Models, 5e by Kutner et al.
  diag(solve(r))
}