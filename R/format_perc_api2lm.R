#' Format percentages
#'
#' A recreation of the \code{stats:::format.prec} function that is only available internally to the \code{stats} package.
#' @param probs A vector of probabilities.
#' @inheritParams base::format
#'
#' @return A vector of percentages
#' @export
#' @keywords internal
#' @seealso \code{\link[base]{format}}
#' @examples
#' format_perc_api2lm(c(0.5, 0.90, 0.95), digits = 1)
#' format_perc_api2lm(c(0.5, 0.90, 0.95), digits = 1)
format_perc_api2lm <- function (probs, digits) {
  paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
        "%")
}