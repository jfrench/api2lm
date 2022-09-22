# p. 245 of ALSM5
test_that("confint_adjust produces correct confidence limits", {
  data(dwaine)
  lmod <- lm(sales ~ targetpop + dpi, data = dwaine)

  # unadjusted
  ciu <- confint_adjust(lmod, parm = 2:3)
  ciui <- matrix(unlist(ciu[,3:4]), nrow = 2, ncol = 2)
  ciu_truth <- confint(lmod, parm = 2:3)
  ciui_truth <- matrix(c(ciu_truth), nrow = 2)

  # bonferroni adjustment
  cib <- confint_adjust(lmod, parm = 2:3, method = "bonferroni",
                        level = 0.90)
  cibi <- round(matrix(unlist(cib[,3:4]), nrow = 2, ncol = 2), 2)
  cibi_truth <- cbind(c(1.01, 0.83), c(1.90, 17.90))

  # # wh adjustment
  # cis <- confint_adjust(lmod, parm = 2:3, method = "wh",
  #                       level = 0.90)
  # cisi <- round(matrix(unlist(cis[,3:4]), nrow = 2, ncol = 2), 2)
  # newx = data.frame(targetpop)
  # cis_truth <- predict(lmod, )
  #
  # expect_equal(ciui, ciui_truth)
  # expect_equal(cibi, cibi_truth)
})



