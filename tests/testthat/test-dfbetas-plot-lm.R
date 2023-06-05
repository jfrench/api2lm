if (require("vdiffr")) {
  test_that("dfbetas_plot produces correct results", {
    lmod <- lm(Petal.Length ~ Sepal.Length + Species,
               data = iris)
    p <- function() dfbetas_plot(lmod)
    expect_doppelganger("default cooks_plot", p)
    p <- function() dfbetas_plot(lmod, id_n = 6)
    expect_doppelganger("dfbetas_plot custom 1", p)
    p <- function() {
      dfbetas_plot(lmod, regressors = "Sepal.Length")
    }
    expect_doppelganger("cooks_plot 1 variable", p)
    p <- function() {
      dfbetas_plot(lmod, id_n = 1,
                   text_arglist = list(col = "blue", cex = 2),
                   abline_arglist = list(col = "red", lwd = 2, h = c(-0.2, 0.2)))
    }
    expect_doppelganger("cooks_plot custom 2", p)
  })
}