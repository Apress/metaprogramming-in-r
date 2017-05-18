context("Expression simplification")

test_that("we can simplify numeric expressions", {
  expect_equal(simplify_expr(quote(1)), 1)
  expect_equal(simplify_expr(quote(0)), 0)

  expect_equal(simplify_expr(quote(1 + 0)), 1)
  expect_equal(simplify_expr(quote(1 + 1)), 2)

  expect_equal(simplify_expr(quote(1 - 0)), 1)
  expect_equal(simplify_expr(quote(3 - 1)), 2)
  expect_equal(simplify_expr(quote(-1)), -1)
  expect_equal(simplify_expr(quote(0 - 1)), -1)

  expect_equal(simplify_expr(quote(1 * 0)), 0)
  expect_equal(simplify_expr(quote(3 * 1)), 3)
  expect_equal(simplify_expr(quote(2 * 2)), 4)

  expect_equal(simplify_expr(quote(2 / 1)), 2)
  expect_equal(simplify_expr(quote(3 / 1)), 3)
  expect_equal(simplify_expr(quote(0 / 1)), 0)
  expect_equal(simplify_expr(quote(1 / 2)), 0.5)

  expect_equal(simplify_expr(quote(0 ** 0)), 1)
  expect_equal(simplify_expr(quote(0 ** 1)), 0)
  expect_equal(simplify_expr(quote(1 ** 0)), 1)
  expect_equal(simplify_expr(quote(1 ** 2)), 1)
  expect_equal(simplify_expr(quote(5 ** 0)), 1)
  expect_equal(simplify_expr(quote(5 ** 0)), 1)
  expect_equal(simplify_expr(quote(5 ** 1)), 5)
  expect_equal(simplify_expr(quote(2 ** 2)), 4)
})

test_that("we can do some simplifications when there are variables involved", {
  expect_equal(simplify_expr(quote(x)), quote(x))
  expect_equal(simplify_expr(quote(x + 0)), quote(x))
  expect_equal(simplify_expr(quote(0 + x)), quote(x))
  expect_equal(simplify_expr(quote(x + x)), quote(x + x)) # we don't actually simplify something like this...

  expect_equal(simplify_expr(quote(x - 0)), quote(x))
  expect_equal(simplify_expr(quote(0 - x)), quote(-x))
  expect_equal(simplify_expr(quote(-x)), quote(-x))
  expect_equal(simplify_expr(quote(x - x)), quote(x - x)) # we don't actually simplify something like this...

  expect_equal(simplify_expr(quote(1 * x)), quote(x))
  expect_equal(simplify_expr(quote(x * 1)), quote(x))
  expect_equal(simplify_expr(quote(x * x)), quote(x * x)) # don't know if we want to simplify this...
  expect_equal(simplify_expr(quote(x / 1)), quote(x))
  expect_equal(simplify_expr(quote(x / x)), quote(x / x)) # we probably want to simplify this...

  expect_equal(simplify_expr(quote(x ^ y)), quote(x ^ y))

  expect_equal(simplify_expr(quote(2*(x + 0))), quote(2*x))
  expect_equal(simplify_expr(quote(2*(x + y))), quote(2*(x + y)))
  expect_equal(simplify_expr(quote(2*(0 + (4 + 5*x)*1))),
               quote(2 * (4 + 5 * x)))
})

test_that("we can handle some simple functions", {
  expect_equal(simplify_expr(quote(sin(x))), quote(sin(x)))
  expect_equal(simplify_expr(quote(sin(1*x))), quote(sin(x)))

  expect_equal(simplify_expr(quote(sin(0*x))), 0)
})

test_that("we give up with unexpected expressoins", {
  expect_equal(simplify_expr(quote(x %in% y)), quote(x %in% y))
})
