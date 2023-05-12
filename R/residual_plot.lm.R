#' Plot residuals of a fitted `lm` object
#'
#' `residual_plot.lm` plots the residuals of a fitted `lm`
#' object. In general, it is intended to provide similar
#' functionality to \code{\link[stats]{plot.lm}}` when
#' `which = 1`, but can be used for different types of
#' residuals and can also plot first-order predictor
#' variables along the x-axis instead of only the fitted
#' values.
#'
#' @param model A fitted model object from the
#'   \code{\link[stats]{lm}} function.
#' @param type The residual type to plot. The default is
#'   `"ordinary"`. The other options are `"standardized"`,
#'   `"studentized"`, `"loo"`, `"jackknife"`, `"deleted"`,
#'   `"internally studentized"`, `"externally studentized"`.
#' @param xaxis The variable to use on the x-axis of the
#'   plot(s). The default is `fitted` to use fitted values.
#'   The other options are `"index"` and `"predictors"`.
#' @param id_n The number of points to identify with labels.
#'   The default is `3`.
#' @param add_reference A logical value indicating whether a
#'   reference line should be added. The default is `TRUE`.
#' @param add_smooth A logical value indicating whether a
#'   smooth should be added to each plot produced. The
#'   default is `TRUE`.
#' @param predictors A formula describing the first-order
#'   predictors to plot the residuals against. The default
#'   is all available first-order predictors.
#' @param smooth A function with a \code{\link[stats]{formula}}
#'   argument to smooth the desired plot. The
#'   default function is \code{\link[stats]{loess}.}
#' @param ... Additional arguments passed to the
#'   \code{\link[graphics]{plot}} function.
#' @param text_arglist Additional arguments passed to the
#'   \code{\link[graphics]{text}} function, which is used to
#'   display the points that are identified.
#' @param abline_arglist A named list specifying additional
#'   arguments passed to the \code{\link[graphics]{abline}}
#'   function for the horizontal reference line added to the
#'   plot.
#' @param smooth_arglist A named list specifying additional
#'   arguments passed to the function provided in the
#'   `smooth` argument.
#' @param lines_arglist A named list specifying additional
#' arguments passed to the \code{\link[graphics]{lines}}
#' function for plotting the result of applying the `smooth`
#' function.
#' @param extendrange_f Positive number(s) specifying the
#'   fraction by which the range of the residuals should be
#'   extended using the \code{\link[grDevices]{extendrange}}
#'   function. If longer than one, `f[1]` is used on the
#'   left and `f[2]` on the right.
#' @author Joshua French
#' @seealso \code{\link[graphics]{plot}},
#'   \code{\link[graphics]{text}},
#'   \code{\link[graphics]{text}},
#'   \code{\link[stats]{loess}}.
#' @export
#' @examples
#' lmod <- lm(Petal.Length ~ Sepal.Length + Species,
#'            data = iris)
#' # similarity with built-in plot.lm functionality
#' residual_plot(lmod)
#' plot(lmod, which = 1)
#' # residual plot for other residual types
#' residual_plot(lmod, type = "standardized", id_n = 0)
#' # residual plot for predictors
#' residual_plot(lmod, xaxis = "pred", id_n = 2)
#' # residual plot for individual predictors
#' residual_plot(lmod, xaxis = "pred",
#'               predictors = ~ Sepal.Length, id_n = 2)
#' residual_plot(lmod, xaxis = "pred",
#'               predictors = ~ Species,)
residual_plot.lm <-
  function(model, type = "ordinary", xaxis = "fitted",
           id_n = 3, predictors = ~ .,
           smooth = stats::loess,
           add_reference = TRUE,
           add_smooth = TRUE,
           ...,
           text_arglist = list(),
           abline_arglist = list(),
           smooth_arglist = list(),
           lines_arglist = list(),
           extendrange_f = 0.08) {
  arglist <- list(...)
  arg_check_residual_plot_lm(type, xaxis, id_n, smooth,
                             add_reference, add_smooth,
                             text_arglist, abline_arglist,
                             smooth_arglist, lines_arglist,
                             extendrange_f)

  type <- match.arg(type, c("ordinary",
                          "standardized",
                          "studentized",
                          "loo",
                          "jackknife",
                          "deleted",
                          "internally studentized",
                          "externally studentized"))
  if(is.null(arglist$ylab)) {
    arglist$ylab <- paste(type, "residuals")
  }

  # get desired residuals
  r <- get_residuals(model, type)
  # determine id_n most extreme observations
  idd <- order(abs(r), decreasing = TRUE)[seq_len(id_n)]

  if (xaxis == "fitted") {
    xv <- stats::fitted(model)
    rplot_raw(x = xv, y = r,
              labels = row.names(model$model),
              idd = idd,
              xlab = "fitted values",
              add_reference = add_reference,
              smooth = smooth,
              add_smooth = add_smooth,
              arglist = arglist,
              text_arglist = text_arglist,
              abline_arglist = abline_arglist,
              smooth_arglist = smooth_arglist,
              lines_arglist = lines_arglist,
              extendrange_f = extendrange_f)
  } else {
    # determine first-order variables in original model
    all_preds <- all.vars(stats::formula(model))[-1]
    # get all terms from predictors formula
    xv <- stats::model.frame(predictors,
                             model$model[,-1, drop = FALSE])
    # only keep the intersection of all_preds and names(preds_x)
    # it should filter out interaction, offset, and I terms.
    xv <- xv[intersect(all_preds, names(xv))]
    # save current par values
    curpar <- graphics::par(no.readonly = TRUE)
    # adjust for multiple plots
    graphics::par(mfrow = auto_mfrow(ncol(xv)))
    # get predictor names
    xnames = names(xv)
    # residual plot for each predictor
    for (j in seq_along(xnames)) {
      is_factor <- is.factor(xv[,j])
      temp_idd <- idd
      if (is_factor) {
        temp_idd <- integer(0)
      }
      rplot_raw(x = xv[,j], y = r,
                idd = temp_idd,
                labels = row.names(model$model),
                xlab = xnames[j],
                add_reference = ifelse(!is_factor,
                                       add_reference,
                                       FALSE),
                smooth = smooth,
                add_smooth = ifelse(!is_factor,
                                    add_smooth,
                                    FALSE),
                arglist = arglist,
                text_arglist = text_arglist,
                abline_arglist = abline_arglist,
                smooth_arglist = smooth_arglist,
                lines_arglist = lines_arglist,
                extendrange_f = extendrange_f)
    }
    # proper exit
    on.exit(graphics::par(curpar))
  }
}
