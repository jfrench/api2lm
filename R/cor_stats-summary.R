#' Summarize \code{cor_stats} object
#'
#' Summarize an object of class \code{cor_stats} produced
#' by the \code{\link[api2lm]{cor_stats}} function. Pairs
#' of variables are ordered in descending order by their
#' sample correlation.
#'
#' @param object A \code{cor_stats} object produced by the
#'   \code{\link[api2lm]{cor_stats}} function.
#' @param ... Not currently implemented
#' @inheritParams cor_stats
#' @return NULL
#' @author Joshua French
#' @export
#' @examples
#' cstats <- cor_stats(home_sales)
#' cstats_sum <- summary(cstats)
#' cstats_sum
#' print(cstats_sum, digits = 3, threshold = 0.5)
summary.cor_stats <- function(object, ...) {
  # remove duplicates and diagonal
  w <- which(upper.tri(object, diag = TRUE), arr.ind = TRUE)
  object[w] <- NA
  # get names for pairs
  nm <- rownames(object)
  # data frame of all unique pairs
  dtf <- data.frame(var1 = rep(nm, times = length(nm)),
                    var2 = rep(nm, each = length(nm)),
                    cor = c(object))
  # remove NA rows
  dtf <- stats::na.omit(dtf)
  # order in descending order
  o <- order(dtf$cor, decreasing = TRUE)
  dtf <- dtf[o,]
  # update class
  class(dtf) <- c("cor_stats_summary", class(dtf))
  return(dtf)
}

#' @export
#' @rdname summary.cor_stats
print.cor_stats_summary <- function(x, ..., digits = 2, threshold = 0.7) {
  tx <- x[x$cor > threshold,]
  tx$cor <- round(tx$cor, digits = digits)
  rownames(tx) <- seq_len(nrow(tx))
  class(tx) <- class(tx)[class(tx) != "cor_stats_summary"]
  print(tx)
}