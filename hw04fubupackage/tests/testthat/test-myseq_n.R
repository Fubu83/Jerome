test_that("myseq_n() retruns element n", {
  x <- c(2,4,3)
  y <- 3
  c <- 5
  z <- 3
  n <- 2.7

  expect_identical(myseq_n(x, y), z)
  expect_identical(myseq_n(x, c), n)
})
