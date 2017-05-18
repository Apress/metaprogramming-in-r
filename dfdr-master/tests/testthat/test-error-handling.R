context("Error handling")

test_that("We get an error with an unexpected call", {
  f <- function(x) rnorm(1, x)
  expect_error(d(d(f,"x"), "x"), "Unexpected call")
})
