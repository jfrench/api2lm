#' Compute correlation matrix
#'
#' \code{cor_stats} creates a correlation matrix for an
#' object. The correlation is only computed for
#' \code{numeric} variables.
#'
#' If \code{x} is a \code{data.frame} object, then the
#' correlation matrix is computed for all columns of
#' \code{x} that are \code{numeric}. If \code{x} is an
#' \code{lm} object, then the response variable column is
#' removed from \code{x$model} and the correlation matrix is
#' computed for all remaining columns that are
#' \code{numeric}.
#'
#' @param x A `data.frame` or `lm` object.
#' @param ... Currently unimplemented.
#' @param digits The number of decimal places to print. The
#'   default is \code{2}.
#' @param threshold A threshold determining which
#'   correlation statistics to print. Statistics above the
#'   threshold are printed. The default is \code{0.7}.
#'   Otherwise, a "\code{.}" is displayed in place of the
#'   number.
#' @author Joshua French
#' @export
#' @examples
#' cor_stats(home_sales)
#' lmod <- lm(price ~ ., data = home_sales)
#' cstats <- cor_stats(lmod)
#' cstats
#' print(cstats, digits = 3, threshold = 0.5)
#' cstats_sum <- summary(cstats)
#' cstats_sum
#' print(cstats_sum, digits = 3, threshold = 0.5)
#' @export
cor_stats <- function(x, ..., digits = 2, threshold = 0.7) {
  UseMethod("cor_stats")
}

#' @export
cor_stats.data.frame <- function(x, ...) {
  structure(stats::cor(x[, unlist(lapply(x, is.numeric))]),
            class = c("cor_stats", "matrix", "array")
            )
}

#' @export
cor_stats.lm <- function(x, ..., digits = 2, threshold = 0.7) {
  # strip response from formula
  xform <- stats::formula(stats::delete.response(stats::terms(x)))
  # extract predictors from formula
  x <- stats::model.frame(xform, data = x$model)
  cor_stats(x, digits = digits, threshold = threshold)
}

#' @export
#' @rdname cor_stats
print.cor_stats <- function(x, ..., digits = 2, threshold = 0.7) {
  # convert numeric matrix to character
  xchar <- matrix(as.character(round(x, digits = digits)),
                  nrow = nrow(x), ncol = ncol(x))
  # determine indices with values less than threshold
  wsmall <- which(x <= threshold, arr.ind = TRUE)
  # dot small values
  xchar[wsmall] <- "."
  # determine upper triangle
  wupper <- which(upper.tri(x, diag = FALSE), arr.ind = TRUE)
  # blank upper triangle indices
  xchar[wupper] <- ""
  # add row and column names
  rownames(xchar) <- rownames(x)
  # rownames(xchar)[1] <- ""
  # add column names along diagonal
  # diag(xchar) <- colnames(x)
  colnames(xchar) <- colnames(x)
  print(noquote(xchar, right = TRUE), ...)
}