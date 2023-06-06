#' Compare coefficients of 2 models
#'
#' Compare the coefficients to two fitted models. The models
#' must have the same coefficients.
#'
#' @param model1 A fitted model object from the
#'   \code{\link[stats]{lm}} function.
#' @param model2 A fitted model object from the
#'   \code{\link[stats]{lm}} function.
#' @param inheritParms
#'
#' @return A data frame
#' @export
#' @examples
#' # fit model
#' lmod1 <- lm(murder ~ hs_grad + urban + poverty + single,
#'            data = crime2009)
#' #fit without DC
#' lmod2 <- lm(murder ~ hs_grad + urban + poverty + single,
#'             data = crime2009[-9, ])
#' #compare coefficients of models
#' compare_coef(lmod1, lmod2)
compare_coef <- function(model1, model2, digits = 3) {
  if (!is.element("lm", class(model1))) {
    stop("model 1 must be an lm object")
  }
  if (!is.element("lm", class(model1))) {
    stop("model 2 must be an lm object")
  }
  if (!identical(names(stats::coef(model1)),
                 names(stats::coef(model2)))) {
    stop("The models must have the same coefficients")
  }
  temp = lapply(seq_len(length(stats::coef(model1))),
                        combine_coefs,
                a = t(stats::summary.lm(model1)$coef[,1:2]),
                b = t(stats::summary.lm(model2)$coef[,1:2]),
                digits = digits)
  # combine lists into single data frame
  temp = do.call(rbind, temp)
  # format data frame
  print.data.frame(temp, row.names = FALSE)
}

combine_coefs <- function(i, a, b, digits = NULL) {
  temp = data.frame(` ` = c(colnames(a)[i], "Std.Error", ""),
                    `Model 1` = c(a[,i], NA),
                    `Model 2` = c(b[,i], NA),
                    check.names = FALSE)
  # temp = replace(temp, is.na(temp), "")
  temp = format(temp, trim = TRUE, digits = digits)
  temp[3,2:3] <- ""
  # temp <- sapply(temp, as.character)
  # temp <- rbind(temp, c("", "", ""))
  temp
}
