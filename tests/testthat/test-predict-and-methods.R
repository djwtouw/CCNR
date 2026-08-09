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

test_that("predict with no newdata matches stored fitted.values", {
  expect_equal(predict(fit, type = "class"), fit$fitted.values)
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

test_that("predict rejects threshold outside (0, 1)", {
  expect_error(predict(fit, threshold = 0))
  expect_error(predict(fit, threshold = 1))
  expect_error(predict(fit, threshold = -0.5))
  expect_error(predict(fit, threshold = 1.5))
})

# ---- fitted.ccn --------------------------------------------------------

test_that("fitted type='class' matches fit$fitted.values", {
  expect_equal(fitted(fit, type = "class"), fit$fitted.values)
})

test_that("fitted type='response' matches fit$fitted.probs", {
  expect_equal(fitted(fit, type = "response"), fit$fitted.probs)
})

test_that("fitted default type is 'class'", {
  expect_equal(fitted(fit), fit$fitted.values)
})

# ---- residuals.ccn -----------------------------------------------------

test_that("residuals has dim n x L", {
  expect_equal(dim(residuals(fit)), c(n, L))
})

test_that("residuals equals y minus fitted probabilities", {
  expect_equal(residuals(fit), fit$y - fit$fitted.probs)
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
