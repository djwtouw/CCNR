skip_if_not_installed("tibble")

set.seed(1)
n <- 40L; m <- 2L; L <- 3L
x <- matrix(stats::rnorm(n * m), n, m,
            dimnames = list(NULL, c("x1", "x2")))
eta <- x %*% matrix(c(1.5, -1, 0.5, -1.5, 1, 0.5), m, L)
y <- (eta > 0) * 1L
colnames(y) <- c("a", "b", "c")

x_tbl <- tibble::as_tibble(x)
y_tbl <- tibble::as_tibble(y)

test_that("ccn gives the same fit for tibble and matrix input", {
  fit_mat <- ccn(x, y, q = 1, lambda = 0.01)
  fit_tbl <- ccn(x_tbl, y_tbl, q = 1, lambda = 0.01)
  expect_equal(fit_tbl$coefficients_flat, fit_mat$coefficients_flat)
  expect_equal(fit_tbl$fitted.values, fit_mat$fitted.values)
  expect_equal(fit_tbl$label_names, c("a", "b", "c"))
})

test_that("ccn accepts a tibble y with only logical columns", {
  y_lgl <- tibble::as_tibble(y == 1)
  fit_num <- ccn(x, y, q = 1, lambda = 0.01)
  fit_lgl <- ccn(x_tbl, y_lgl, q = 1, lambda = 0.01)
  expect_equal(fit_lgl$coefficients_flat, fit_num$coefficients_flat)
  expect_equal(fit_lgl$label_names, c("a", "b", "c"))
})

test_that("predict accepts tibble newdata", {
  fit <- ccn(x, y, q = 1, lambda = 0.01)
  expect_equal(predict(fit, newdata = x_tbl, type = "response"),
               predict(fit, newdata = x, type = "response"))
})

test_that("ccn rejects a tibble with a factor column", {
  x_bad <- tibble::tibble(x1 = x[, 1], x2 = factor(x[, 2] > 0))
  expect_error(ccn(x_bad, y_tbl, q = 1, lambda = 0.01), "x must be numeric")
})

test_that("predict rejects tibble columns in a different order", {
  fit <- ccn(x_tbl, y_tbl, q = 1, lambda = 0.01)
  expect_error(predict(fit, newdata = x_tbl[, c("x2", "x1")]), "same order")
})
