if (require("vdiffr")) {
test_that("residual_plot.lm produces correct results", {
  lmod <- lm(Petal.Length ~ Sepal.Length + Species,
             data = iris)
  p <- function() residual_plot(lmod)
  expect_doppelganger("default residual_plot", p)
  p <- function() residual_plot(lmod, type = "standardized")
  expect_doppelganger("standardized residual_plot", p)
  p <- function() residual_plot(lmod, type = "studentized")
  expect_doppelganger("studentized residual_plot", p)
  p <- function() residual_plot(lmod, type = "loo")
  expect_doppelganger("loo residual_plot", p)
  p <- function() residual_plot(lmod, xaxis = "pred", id_n = 2)
  expect_doppelganger("residual_plot all pres", p)
  p <- function() {
    residual_plot(lmod, xaxis = "pred",
                  predictors = ~ Sepal.Length, id_n = 2)
  }
  expect_doppelganger("residual_plot Sepal.Length", p)
  p <- function() {
    residual_plot(lmod, xaxis = "pred",
                  predictors = ~ Species, id_n = 2)
  }
  expect_doppelganger("residual_plot Species", p)
})
}