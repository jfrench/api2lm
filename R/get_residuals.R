#' Extract residuals from a model
#'
#' @param x An `lm` object
#' @param type The desired residual type. The options are
#'   `"ordinary"`, `"standardized"`, `"studentized"`,
#'   `"jackknife"`, `"loo"`, `"deleted"`, `"internally
#'   studentized"`, and `"externally studentized"`.
#'
#' @return A vector of residals.
#' @export
#'
#' @examples
#' lmod <- lm(Girth ~ Height, data = trees)
#' # ordinary residuals
#' rord <- get_residuals(lmod)
#' all.equal(rord, residuals(lmod))
#' # standardized residuals
#' rstand <- get_residuals(lmod, "standardized")
#' all.equal(rstand, rstandard(lmod))
#' # studentized residuals
#' rstud <- get_residuals(lmod, "studentized")
#' all.equal(rstud, rstudent(lmod))
#' # loo residuals
#' rl <- get_residuals(lmod, "loo")
#' all.equal(rl, rloo(lmod))
get_residuals <- function(x,
                          type = c(
                            "ordinary",
                            "standardized",
                            "studentized",
                            "jackknife",
                            "loo",
                            "deleted",
                            "internally studentized",
                            "externally studentized"
                          )) {
  if (!is.element("lm", class(x))) {
    stop("x must be of class lm")
  }
  type <- match.arg(type,
                    c("ordinary", "standardized",
                      "studentized", "jackknife",
                      "loo", "deleted",
                      "internally studentized",
                      "externally studentized"))
  switch(type,
         ordinary = stats::residuals(x),
         standardized = stats::rstandard(x),
         studentized = stats::rstudent(x),
         loo = rloo(x),
         jackknife = rloo(x),
         deleted = rloo(x),
         internally_studentized = stats::rstandard(x),
         externally_studentized = stats::rstudent(x))
}
