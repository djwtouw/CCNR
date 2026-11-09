set.seed(2)
n <- 40L; m <- 2L; L <- 3L
x <- matrix(stats::rnorm(n * m), n, m)
eta <- x %*% matrix(c(1.5, -1, 0.5, -1.5, 1, 0.5), m, L)
y <- (eta > 0) * 1L
y_named <- y; colnames(y_named) <- c("A", "B", "C")

fit        <- ccn(x, y,       q = 1, lambda = 0.01)
fit_named  <- ccn(x, y_named, q = 1, lambda = 0.01)
fit_lo     <- ccn(x, y_named, q = 1, lambda = 0.01, label_order = c(3L, 1L, 2L))

# ---- predict.ccn -------------------------------------------------------

test_that("predict with no newdata thresholds the fitted probabilities", {
  expect_equal(predict(fit, type = "response"), fit$fitted.values)
  expect_equal(predict(fit, type = "class"), (fit$fitted.values >= 0.5) * 1L)
  expect_equal(predict(fit, threshold = 0.9), (fit$fitted.values >= 0.9) * 1L)
})

test_that("predict type='response' returns [0,1] probabilities of dim n x L", {
  pred <- predict(fit, type = "response")
  expect_equal(dim(pred), c(n, L))
  expect_true(all(pred >= 0 & pred <= 1))
})

test_that("predict type='class' returns binary matrix of dim n x L", {
  pred <- predict(fit, type = "class")
  expect_equal(dim(pred), c(n, L))
  expect_true(all(pred %in% c(0L, 1L)))
})

test_that("predict with newdata returns correct number of rows", {
  newx <- x[1:10, , drop = FALSE]
  pred <- predict(fit, newdata = newx, type = "class")
  expect_equal(dim(pred), c(10L, L))
})

test_that("predict threshold=0.01 gives >= predictions than threshold=0.99", {
  low  <- predict(fit, type = "class", threshold = 0.01)
  high <- predict(fit, type = "class", threshold = 0.99)
  expect_gte(sum(low), sum(high))
})

test_that("predict per-label threshold returns a valid binary matrix", {
  pred <- predict(fit, type = "class", threshold = c(0.3, 0.5, 0.7))
  expect_equal(dim(pred), c(n, L))
  expect_true(all(pred %in% c(0L, 1L)))
})

test_that("predict attaches label names to output when available", {
  pred <- predict(fit_named, newdata = x[1:5, ])
  expect_equal(colnames(pred), c("A", "B", "C"))
})

test_that("predict attaches generated label names when y has none", {
  pred <- predict(fit, newdata = x[1:5, ])
  expect_equal(colnames(pred), c("L1", "L2", "L3"))
})

test_that("predict with custom label_order returns original column order", {
  pred <- predict(fit_lo, newdata = x[1:5, ])
  expect_equal(colnames(pred), c("A", "B", "C"))
})

test_that("predict rejects newdata with wrong number of columns", {
  bad <- matrix(0, 5, m + 1)
  expect_error(predict(fit, newdata = bad))
})

test_that("predict rejects column names in a different order", {
  x_named <- x; colnames(x_named) <- c("u", "v")
  fit_x <- ccn(x_named, y, q = 1, lambda = 0.01)
  expect_error(predict(fit_x, newdata = x_named[, c("v", "u")]),
               "same order: u, v")
})

test_that("predict rejects different column names", {
  x_named <- x; colnames(x_named) <- c("u", "v")
  fit_x <- ccn(x_named, y, q = 1, lambda = 0.01)
  bad <- x_named; colnames(bad) <- c("u", "z")
  expect_error(predict(fit_x, newdata = bad), "column names of newdata")
})

test_that("predict accepts duplicate column names in the training order", {
  x_dup <- x; colnames(x_dup) <- c("a", "a")
  fit_dup <- ccn(x_dup, y, q = 1, lambda = 0.01)
  expect_equal(predict(fit_dup, newdata = x_dup, type = "response"),
               fit_dup$fitted.values)
})

test_that("predict matches by position when either side lacks names", {
  x_named <- x; colnames(x_named) <- c("u", "v")
  fit_x <- ccn(x_named, y, q = 1, lambda = 0.01)
  expect_equal(predict(fit_x, newdata = unname(x_named), type = "response"),
               predict(fit_x, newdata = x_named, type = "response"))
  expect_equal(predict(fit, newdata = x_named, type = "response"),
               predict(fit, newdata = x, type = "response"))
})

test_that("predict reads a vector as a single observation", {
  expect_equal(predict(fit, newdata = x[1, ], type = "response"),
               predict(fit, newdata = x[1, , drop = FALSE], type = "response"))
})

test_that("predict checks the names of a vector", {
  x_named <- x; colnames(x_named) <- c("u", "v")
  fit_x <- ccn(x_named, y, q = 1, lambda = 0.01)
  expect_equal(predict(fit_x, newdata = x_named[1, ], type = "response"),
               predict(fit_x, newdata = x_named[1, , drop = FALSE],
                       type = "response"))
  expect_error(predict(fit_x, newdata = c(v = x[1, 2], u = x[1, 1])),
               "same order")
})

test_that("predict explains a vector of the wrong length", {
  expect_error(predict(fit, newdata = c(1, 2, 3)), "one-column matrix")
  fit_1 <- ccn(x[, 1, drop = FALSE], y, q = 1, lambda = 0.01)
  expect_error(predict(fit_1, newdata = x[1:5, 1]), "one-column matrix")
  expect_equal(dim(predict(fit_1, newdata = x[1:5, 1, drop = FALSE])), c(5L, L))
})

test_that("predict rejects missing or infinite values in newdata", {
  expect_error(predict(fit, newdata = rbind(c(Inf, 0))), "missing or infinite")
  expect_error(predict(fit, newdata = rbind(c(NA, 0))), "missing or infinite")
})

test_that("predict rejects threshold outside (0, 1)", {
  expect_error(predict(fit, threshold = 0))
  expect_error(predict(fit, threshold = 1))
  expect_error(predict(fit, threshold = -0.5))
  expect_error(predict(fit, threshold = 1.5))
})

# ---- fitted.ccn --------------------------------------------------------

test_that("fitted returns the fitted probabilities", {
  expect_equal(fitted(fit), fit$fitted.values)
  expect_equal(fitted(fit), predict(fit, newdata = x, type = "response"))
})

# ---- residuals.ccn -----------------------------------------------------

test_that("residuals has dim n x L", {
  expect_equal(dim(residuals(fit)), c(n, L))
})

test_that("residuals equals y minus fitted probabilities", {
  expect_equal(residuals(fit), fit$y - fitted(fit))
})

# ---- coef.ccn ----------------------------------------------------------

test_that("coef returns list with elements b, W, C", {
  coefs <- coef(fit)
  expect_named(coefs, c("b", "W", "C"))
})

test_that("coef b has length L, W is m x L, C is L x L", {
  coefs <- coef(fit)
  expect_length(coefs$b, L)
  expect_equal(dim(coefs$W), c(m, L))
  expect_equal(dim(coefs$C), c(L, L))
})

# ---- print / summary ---------------------------------------------------

test_that("print.ccn produces output without error", {
  expect_output(print(fit))
})

test_that("print.ccn reports the label names", {
  expect_output(print(fit_named), "A, B, C")
  expect_output(print(fit), "L1, L2, L3")
})

test_that("summary.ccn labels the coefficients in chain order", {
  expect_output(summary(fit_lo), "C\\s+A\\s+B")
})

test_that("summary.ccn produces output without error", {
  expect_output(summary(fit))
})

test_that("summary.ccn works when y has no column names", {
  expect_output(summary(fit))
})

test_that("summary.ccn works when y has column names", {
  expect_output(summary(fit_named))
})

test_that("summary.ccn prints all three blocks by default", {
  out <- capture.output(summary(fit))
  expect_length(grep("^Intercepts \\(b\\):$", out), 1)
  expect_length(grep("^Coefficients of the explanatory variables", out), 1)
  expect_length(grep("^Coefficients of the label predictions", out), 1)
})

test_that("summary.ccn 'which' selects the printed blocks", {
  out <- capture.output(summary(fit, which = c("b", "C")))
  expect_length(grep("^Intercepts \\(b\\):$", out), 1)
  expect_length(grep("^Coefficients of the explanatory variables", out), 0)
  expect_length(grep("^Coefficients of the label predictions", out), 1)

  out <- capture.output(summary(fit, which = "W"))
  expect_length(grep("^Intercepts \\(b\\):$", out), 0)
  expect_length(grep("^Coefficients of the explanatory variables", out), 1)
  expect_length(grep("^Coefficients of the label predictions", out), 0)
})

test_that("summary.ccn rejects an unknown block", {
  expect_error(summary(fit, which = "Z"))
})

test_that("summary.ccn 'digits' controls the number of decimals", {
  out <- capture.output(summary(fit, which = "b", digits = 1))
  block <- out[(grep("^Intercepts \\(b\\):$", out) + 1):length(out)]
  expect_true(any(grepl("-?\\d\\.\\d(\\s|$)", block)))
  expect_false(any(grepl("\\d\\.\\d{2}", block)))
})

test_that("summary.ccn returns the object invisibly", {
  capture.output(res <- withVisible(summary(fit)))
  expect_false(res$visible)
  expect_identical(res$value, fit)
})
