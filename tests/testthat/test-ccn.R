set.seed(1)
n <- 40L; m <- 2L; L <- 3L
x <- matrix(stats::rnorm(n * m), n, m)
eta <- x %*% matrix(c(1.5, -1, 0.5, -1.5, 1, 0.5), m, L)
y <- (eta > 0) * 1L

# ---- Class and structure -----------------------------------------------

test_that("ccn returns class 'ccn'", {
  fit <- ccn(x, y, q = 1, lambda = 0.01)
  expect_s3_class(fit, "ccn")
})

test_that("ccn output contains required components", {
  fit <- ccn(x, y, q = 1, lambda = 0.01)
  expect_true(all(c("coefficients", "coefficients_flat", "label_order", "label_names",
                    "y", "fitted.values", "n_params", "dims",
                    "hyperparams", "n_starts", "loss", "call",
                    "variable_names") %in% names(fit)))
})

test_that("ccn dims are recorded correctly", {
  fit <- ccn(x, y, q = 1, lambda = 0.01)
  expect_equal(fit$dims, list(n = n, m = m, L = L))
})

test_that("ccn hyperparams are stored correctly", {
  fit <- ccn(x, y, q = 1.5, lambda = 0.2)
  expect_equal(fit$hyperparams$q, 1.5)
  expect_equal(fit$hyperparams$lambda, 0.2)
})

# ---- Coefficient structure ---------------------------------------------

test_that("ccn n_params matches formula and coefficients_flat length", {
  fit <- ccn(x, y, q = 1, lambda = 0.01)
  expected <- L + m * L + (L * L - L) %/% 2L
  expect_equal(fit$n_params, expected)
  expect_length(fit$coefficients_flat, expected)
})

test_that("ccn coefficient list has correct dimensions", {
  fit <- ccn(x, y, q = 1, lambda = 0.01)
  coefs <- fit$coefficients
  expect_length(coefs$b, L)
  expect_equal(dim(coefs$W), c(m, L))
  expect_equal(dim(coefs$C), c(L, L))
})

test_that("ccn C matrix has NA on diagonal and upper triangle, weights below", {
  fit <- ccn(x, y, q = 1, lambda = 0.01)
  C <- fit$coefficients$C
  expect_true(all(is.na(diag(C))))
  expect_true(all(is.na(C[upper.tri(C)])))
  expect_false(any(is.na(C[lower.tri(C)])))
})

# ---- Fitted values -----------------------------------------------------

test_that("ccn fitted.values is an n x L matrix of probabilities", {
  fit <- ccn(x, y, q = 1, lambda = 0.01)
  expect_equal(dim(fit$fitted.values), c(n, L))
  expect_true(all(fit$fitted.values >= 0 & fit$fitted.values <= 1))
  expect_false(all(fit$fitted.values %in% c(0, 1)))
})

test_that("ccn loss is finite and non-negative", {
  fit <- ccn(x, y, q = 1, lambda = 0.01)
  expect_true(is.finite(fit$loss))
  expect_gte(fit$loss, 0)
})

# ---- Label names -------------------------------------------------------

test_that("ccn preserves column names of y", {
  y_named <- y
  colnames(y_named) <- c("A", "B", "C")
  fit <- ccn(x, y_named, q = 1, lambda = 0.01)
  expect_equal(fit$label_names, c("A", "B", "C"))
})

test_that("ccn falls back to generated label names when y is unnamed", {
  fit <- ccn(x, y, q = 1, lambda = 0.01)
  expect_equal(fit$label_names, c("L1", "L2", "L3"))
  expect_equal(colnames(fit$fitted.values), c("L1", "L2", "L3"))
  expect_equal(colnames(fit$y), c("L1", "L2", "L3"))
})

test_that("ccn stores y in the original column order with label names", {
  y_named <- y
  colnames(y_named) <- c("A", "B", "C")
  fit <- ccn(x, y_named, q = 1, lambda = 0.01, label_order = c(3L, 1L, 2L))
  expect_equal(colnames(fit$y), c("A", "B", "C"))
  expect_equal(unname(fit$y), unname(y_named))
})

# ---- Coefficient names -------------------------------------------------

test_that("ccn names the coefficients after the labels and the explanatory variables", {
  y_named <- y
  colnames(y_named) <- c("A", "B", "C")
  x_named <- x
  colnames(x_named) <- c("v1", "v2")
  fit   <- ccn(x_named, y_named, q = 1, lambda = 0.01)
  coefs <- coef(fit)
  expect_equal(names(coefs$b), c("A", "B", "C"))
  expect_equal(colnames(coefs$W), c("A", "B", "C"))
  expect_equal(rownames(coefs$W), c("v1", "v2"))
  expect_equal(dimnames(coefs$C), list(c("A", "B", "C"), c("A", "B", "C")))
})

test_that("ccn names the coefficients in chain order, not column order", {
  y_named <- y
  colnames(y_named) <- c("A", "B", "C")
  order <- c(3L, 1L, 2L)
  fit   <- ccn(x, y_named, q = 1, lambda = 0.01, label_order = order)
  coefs <- coef(fit)
  expect_equal(names(coefs$b), c("C", "A", "B"))
  expect_equal(colnames(coefs$W), c("C", "A", "B"))
  expect_equal(dimnames(coefs$C), list(c("C", "A", "B"), c("C", "A", "B")))
})

test_that("ccn falls back to generated variable names when x is unnamed", {
  fit <- ccn(x, y, q = 1, lambda = 0.01)
  expect_equal(rownames(coef(fit)$W), c("X1", "X2"))
})

test_that("ccn fitted.values carry label names", {
  y_named <- y
  colnames(y_named) <- c("A", "B", "C")
  fit <- ccn(x, y_named, q = 1, lambda = 0.01)
  expect_equal(colnames(fit$fitted.values), c("A", "B", "C"))
})

test_that("ccn with custom label_order outputs in original column order", {
  y_named <- y
  colnames(y_named) <- c("A", "B", "C")
  fit <- ccn(x, y_named, q = 1, lambda = 0.01, label_order = c(3L, 1L, 2L))
  expect_equal(colnames(fit$fitted.values), c("A", "B", "C"))
  expect_equal(fit$label_order, c(3L, 1L, 2L))
})

# ---- Edge cases --------------------------------------------------------

test_that("ccn works with a single label (L = 1)", {
  y1 <- y[, 1L, drop = FALSE]
  fit <- ccn(x, y1, q = 1, lambda = 0.01)
  expect_s3_class(fit, "ccn")
  expect_equal(fit$dims$L, 1L)
  expect_length(fit$coefficients$C, 0L)
})

test_that("ccn reads a vector y as a single label", {
  fit_vec <- ccn(x, y[, 1L], q = 1, lambda = 0.01)
  fit_mat <- ccn(x, y[, 1L, drop = FALSE], q = 1, lambda = 0.01)
  expect_equal(fit_vec$coefficients_flat, fit_mat$coefficients_flat)
  expect_equal(dim(predict(fit_vec, newdata = x)), c(n, 1L))
})

test_that("ccn with n_starts > 1 runs and returns a valid fit", {
  fit <- ccn(x, y, q = 1, lambda = 0.01, n_starts = 3L)
  expect_s3_class(fit, "ccn")
  expect_equal(fit$n_starts, 3L)
})

# ---- Input validation --------------------------------------------------

test_that("ccn rejects non-numeric x", {
  x_char <- matrix(as.character(x), n, m)
  expect_error(ccn(x_char, y, q = 1, lambda = 0.01))
})

test_that("ccn rejects non-numeric y", {
  y_char <- matrix(as.character(y), n, L)
  expect_error(ccn(x, y_char, q = 1, lambda = 0.01))
})

test_that("ccn gives the same fit for logical and 0/1 y", {
  y_lgl <- y == 1
  fit_num <- ccn(x, y, q = 1, lambda = 0.01)
  fit_lgl <- ccn(x, y_lgl, q = 1, lambda = 0.01)
  expect_equal(fit_lgl$coefficients_flat, fit_num$coefficients_flat)
  expect_equal(fit_lgl$fitted.values, fit_num$fitted.values)
})

test_that("ccn rejects infinite values in x", {
  x_inf <- x; x_inf[1, 1] <- Inf
  expect_error(ccn(x_inf, y, q = 1, lambda = 0.01), "missing or infinite")
})

test_that("ccn checks the optimizer settings", {
  expect_error(ccn(x, y, q = 1, lambda = 0.01, conv_tol = -1), "conv_tol")
  expect_error(ccn(x, y, q = 1, lambda = 0.01, conv_tol = 0), "conv_tol")
  expect_error(ccn(x, y, q = 1, lambda = 0.01, restart_scale = c(1, 2)),
               "restart_scale")
  for (bad in list(NA, 0, 2.5, c(1, 2), "2")) {
    expect_error(ccn(x, y, q = 1, lambda = 0.01, n_starts = bad), "n_starts")
  }
})

test_that("ccn rejects missing values in x and y", {
  x_na <- x; x_na[1, 1] <- NA
  expect_error(ccn(x_na, y, q = 1, lambda = 0.01))
  y_na <- y; y_na[2, 2] <- NA
  expect_error(ccn(x, y_na, q = 1, lambda = 0.01))
})

test_that("ccn rejects non-binary y", {
  y_cont <- y + 0.5
  expect_error(ccn(x, y_cont, q = 1, lambda = 0.01))
  y_neg <- y; y_neg[1, 1] <- -1
  expect_error(ccn(x, y_neg, q = 1, lambda = 0.01))
})

test_that("ccn reports a row count mismatch", {
  expect_error(ccn(x[-1, ], y, q = 1, lambda = 0.01),
               "x has 39 rows but y has 40")
})

test_that("ccn rejects q and lambda that are not a single number", {
  for (bad in list(c(1, 2), NA, "1", Inf)) {
    expect_error(ccn(x, y, q = bad, lambda = 0.01), "q must be a single")
    expect_error(ccn(x, y, q = 1, lambda = bad), "lambda must be a single")
  }
})

test_that("ccn rejects q < 1", {
  expect_error(ccn(x, y, q = 0.5, lambda = 0.01))
})

test_that("ccn rejects negative lambda", {
  expect_error(ccn(x, y, q = 1, lambda = -0.01))
})

test_that("ccn rejects invalid label_order", {
  expect_error(ccn(x, y, q = 1, lambda = 0.01, label_order = c(1L, 2L)))        # wrong length
  expect_error(ccn(x, y, q = 1, lambda = 0.01, label_order = c(1L, 2L, 2L)))    # not a permutation
})

test_that("ccn accepts valid label_order", {
  expect_no_error(ccn(x, y, q = 1, lambda = 0.01, label_order = c(3L, 1L, 2L)))
})
