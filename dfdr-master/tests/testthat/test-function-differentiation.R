context("Differentiation of function calls")

test_that("we can differentiate known functions", {
  df <- d(sin, "x")
  expect_equal(df, cos)

  df <- d(cos, "x")
  expect_equal(body(df), quote(-sin(x)))

  df <- d(exp, "x")
  expect_equal(df, exp)
})

test_that("we can differentiate expressions with functions", {
  f <- function(x) -sin(x)
  df <- d(f, "x")
  expect_equal(body(df), quote(-cos(x)))

  f <- function(x) -cos(x)
  df <- d(f, "x")
  expect_equal(body(df), quote(sin(x)))

  f <- function(x) -exp(x)
  df <- d(f, "x")
  expect_equal(body(df), quote(-exp(x)))
})

test_that("we can differentiate general functions with the chain rule", {
  f <- function(x, y) x^2 * y
  g <- function(z) f(2*z, z^2)
  h <- function(z) 4*z^4

  zs <- seq(1,100,5)
  expect_equal(g(zs), h(zs))

  dg <- Vectorize(d(g,"z"))
  dh <- d(h,"z")
  expect_equal(dg(zs), dh(zs))
})

