context("Constants and single variable expressions")

test_that("we can differentiate constants", {
  f <- function(x) 4
  df <- d(f, "x")
  expect_equal(body(df), quote(0))

  f <- function(x) y
  df <- d(f, "x")
  expect_equal(body(df), quote(0))
})

test_that("we can differentiate variables", {
  f <- function(x) x
  df <- d(f, "x")
  expect_equal(body(df), quote(1))

  f <- function(x) y
  df <- d(f, "x")
  expect_equal(body(df), quote(0))
})

test_that("we can differentiate variables times constants", {
  f <- function(x) 2*x
  df <- d(f, "x")
  expect_equal(body(df), 2)

  f <- function(x) -2*x
  df <- d(f, "x")
  expect_equal(body(df), -2)

  f <- function(x) y*x
  df <- d(f, "x")
  expect_equal(body(df), quote(y))

  f <- function(x) 1/x
  df <- d(f, "x")
  expect_equal(body(df), quote(-1/x^2))

})

test_that("we can differentiate expressions with parentheses", {
  f <- function(x) 2*(x + 5)
  df <- d(f, "x")
  expect_equal(body(df), 2)

  f <- function(x, y) 2*(x + y)
  df <- d(f, "x")
  expect_equal(body(df), 2)
})

test_that("we can differentiate exponetiation", {
  f <- function(x) x^2
  df <- d(f, "x")
  expect_equal(body(df), quote(2*x))

  f <- function(x) x^-2
  df <- d(f, "x")
  expect_equal(body(df), quote(-2*x^-3))
})

test_that("we can differentiate addition and subtraction", {
  f <- function(x) x + x
  df <- d(f, "x")
  expect_equal(body(df), 2)

  f <- function(x) x - x
  df <- d(f, "x")
  expect_equal(body(df), 0)

  f <- function(x) x + y
  df <- d(f, "x")
  expect_equal(body(df), 1)

  f <- function(x) x - y
  df <- d(f, "x")
  expect_equal(body(df), 1)

  f <- function(x) -x
  df <- d(f, "x")
  expect_equal(body(df), -1)
})

