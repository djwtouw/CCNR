set.seed(5)
n <- 40L; m <- 2L; L <- 2L
x <- matrix(stats::rnorm(n * m), n, m)
eta <- x %*% matrix(c(1.5, -1, 0.5, -1.5), m, L)
y <- (eta > 0) * 1L

# ---- Class and structure -----------------------------------------------

test_that("ccn_cv returns class 'ccn_cv'", {
  cv <- ccn_cv(x, y, q = 1, lambda = 0.01, folds = 3L, seed = 1L)
  expect_s3_class(cv, "ccn_cv")
})

test_that("ccn_cv output contains required components", {
  cv <- ccn_cv(x, y, q = 1, lambda = 0.01, folds = 3L, seed = 1L)
  expect_true(all(c("best_model", "best_params", "scores",
                    "fold_assignments", "folds") %in% names(cv)))
})

test_that("ccn_cv best_model is a 'ccn' object", {
  cv <- ccn_cv(x, y, q = 1, lambda = 0.01, folds = 3L, seed = 1L)
  expect_s3_class(cv$best_model, "ccn")
})

# ---- Grid search -------------------------------------------------------

test_that("ccn_cv scores has one row per grid point with correct columns", {
  cv <- ccn_cv(x, y, q = c(1, 2), lambda = c(0.01, 0.1), folds = 3L, seed = 1L)
  expect_equal(nrow(cv$scores), 4L)
  expect_named(cv$scores, c("q", "lambda", "mean_score", "sd_score"))
})

test_that("ccn_cv best_params is a one-row data frame from the supplied grid", {
  q_vals <- c(1, 2); lambda_vals <- c(0.01, 0.1)
  cv <- ccn_cv(x, y, q = q_vals, lambda = lambda_vals, folds = 3L, seed = 1L)
  expect_equal(nrow(cv$best_params), 1L)
  expect_true(cv$best_params$q      %in% q_vals)
  expect_true(cv$best_params$lambda %in% lambda_vals)
})

# ---- Fold assignments --------------------------------------------------

test_that("ccn_cv fold_assignments covers all folds 1..k", {
  k <- 4L
  cv <- ccn_cv(x, y, q = 1, lambda = 0.01, folds = k, seed = 1L)
  expect_length(cv$fold_assignments, n)
  expect_equal(sort(unique(cv$fold_assignments)), seq_len(k))
})

test_that("ccn_cv seed produces identical fold assignments", {
  cv1 <- ccn_cv(x, y, q = 1, lambda = 0.01, folds = 3L, seed = 99L)
  cv2 <- ccn_cv(x, y, q = 1, lambda = 0.01, folds = 3L, seed = 99L)
  expect_equal(cv1$fold_assignments, cv2$fold_assignments)
})

test_that("ccn_cv uses supplied fold_assignments", {
  folds_manual <- rep(1:4, length.out = n)
  cv <- ccn_cv(x, y, q = 1, lambda = 0.01, fold_assignments = folds_manual)
  expect_equal(cv$fold_assignments, folds_manual)
  expect_equal(cv$folds, 4L)
})

# ---- Custom scoring ----------------------------------------------------

test_that("ccn_cv accepts custom scoring function and returns valid scores", {
  cv <- ccn_cv(x, y, q = 1, lambda = c(0.01, 0.1), folds = 3L, seed = 1L,
               scoring = ccn_exact_match)
  expect_true(all(cv$scores$mean_score >= 0 & cv$scores$mean_score <= 1))
})

# ---- Input validation --------------------------------------------------

test_that("ccn_cv rejects folds < 2", {
  expect_error(ccn_cv(x, y, q = 1, lambda = 0.01, folds = 1L))
})

test_that("ccn_cv rejects folds > n", {
  expect_error(ccn_cv(x, y, q = 1, lambda = 0.01, folds = n + 1L))
})

test_that("ccn_cv rejects a fractional number of folds", {
  expect_error(ccn_cv(x, y, q = 1, lambda = 0.01, folds = 2.5),
               "whole number")
})

test_that("ccn_cv rejects invalid fold_assignments", {
  one_fold <- rep(1L, n)
  with_na  <- c(NA, rep(1:2, length.out = n - 1L))
  fraction <- c(1.5, rep(1:2, length.out = n - 1L))
  expect_error(ccn_cv(x, y, q = 1, lambda = 0.01, fold_assignments = one_fold),
               "at least 2 folds")
  expect_error(ccn_cv(x, y, q = 1, lambda = 0.01, fold_assignments = with_na),
               "whole numbers without missing values")
  expect_error(ccn_cv(x, y, q = 1, lambda = 0.01, fold_assignments = fraction),
               "whole numbers without missing values")
})

test_that("ccn_cv rejects a scoring function without the named arguments", {
  positional <- function(a, b, c) mean(a == b)
  expect_error(ccn_cv(x, y, q = 1, lambda = 0.01, folds = 3L,
                      scoring = positional),
               "named arguments y_true, y_class and y_prob")
})

test_that("ccn_cv rejects a scoring function that returns several values", {
  per_label <- function(y_true, y_class, ...) colMeans(y_true == y_class)
  expect_error(ccn_cv(x, y, q = 1, lambda = 0.01, folds = 3L, seed = 1L,
                      scoring = per_label),
               "single number")
})

test_that("ccn_cv with a seed leaves the random number stream unchanged", {
  set.seed(10); expected <- stats::runif(1)
  set.seed(10)
  ccn_cv(x, y, q = 1, lambda = 0.01, folds = 3L, seed = 1L)
  expect_equal(stats::runif(1), expected)
})

test_that("ccn_cv rejects non-binary y", {
  y_cont <- y + 0.5
  expect_error(ccn_cv(x, y_cont, q = 1, lambda = 0.01, folds = 3L))
})

test_that("ccn_cv rejects q < 1", {
  expect_error(ccn_cv(x, y, q = 0.5, lambda = 0.01, folds = 3L))
})

test_that("ccn_cv rejects missing or non-finite q and lambda", {
  expect_error(ccn_cv(x, y, q = c(1, NA), lambda = 0.01, folds = 3L),
               "values of q must be finite")
  expect_error(ccn_cv(x, y, q = 1, lambda = c(0.1, Inf), folds = 3L),
               "values of lambda must be finite")
})

test_that("ccn_cv reports a row count mismatch", {
  expect_error(ccn_cv(x[-1, ], y, q = 1, lambda = 0.01, folds = 3L),
               "rows but y has")
})

test_that("ccn_cv rejects infinite values in x", {
  x_inf <- x; x_inf[1, 1] <- Inf
  expect_error(ccn_cv(x_inf, y, q = 1, lambda = 0.01, folds = 3L),
               "missing or infinite")
})

test_that("ccn_cv does not select a grid point with a failed fit", {
  # Unequal folds make the training size identify the fold, so this order
  # fails in the first fold only
  folds <- rep(1:3, c(10L, 15L, 15L))
  flaky <- function(x, y) {
    if (nrow(x) == 30L) stop("fails in fold 1")
    seq_len(ncol(y))
  }
  expect_warning(
    cv <- ccn_cv(x, y, q = 1, lambda = 0.01, fold_assignments = folds,
                 label_order = list(stable = NULL, flaky = flaky)),
    "fold 1 failed"
  )
  expect_true(is.na(cv$scores$mean_score[cv$scores$label_order == "flaky"]))
  expect_equal(cv$best_params$label_order, "stable")
})

test_that("ccn_cv stops when every fit fails", {
  broken <- function(x, y) stop("always fails")
  expect_error(
    suppressWarnings(ccn_cv(x, y, q = 1, lambda = 0.01, folds = 3L,
                            label_order = broken)),
    "no combination of tuning parameters could be scored"
  )
})

test_that("an NA score in one fold makes the grid point NA", {
  # The scoring function returns NA for the 10 observations of fold 1, which
  # affects every grid point, so no combination can be selected
  folds <- rep(1:3, c(10L, 15L, 15L))
  na_in_fold_1 <- function(y_true, y_class, ...) {
    if (nrow(y_true) == 10L) NA_real_ else mean(y_true == y_class)
  }
  expect_error(
    ccn_cv(x, y, q = 1, lambda = c(0.01, 0.1), fold_assignments = folds,
           scoring = na_in_fold_1),
    "returned NA"
  )
})

test_that("ccn_cv rejects negative lambda", {
  expect_error(ccn_cv(x, y, q = 1, lambda = -0.1, folds = 3L))
})

# ---- print / summary ---------------------------------------------------

test_that("print.ccn_cv produces output without error", {
  cv <- ccn_cv(x, y, q = 1, lambda = 0.01, folds = 3L, seed = 1L)
  expect_output(print(cv))
})

test_that("summary.ccn_cv produces output without error", {
  cv <- ccn_cv(x, y, q = 1, lambda = 0.01, folds = 3L, seed = 1L)
  expect_output(summary(cv))
})

# ---- predict / coef / fitted -------------------------------------------

test_that("predict.ccn_cv matches predict on the best model", {
  cv <- ccn_cv(x, y, q = 1, lambda = 0.01, folds = 3L, seed = 1L)
  newx <- x[1:10, , drop = FALSE]
  expect_equal(predict(cv, newdata = newx),
               predict(cv$best_model, newdata = newx))
  expect_equal(predict(cv, newdata = newx, type = "response"),
               predict(cv$best_model, newdata = newx, type = "response"))
  expect_equal(predict(cv, newdata = newx, threshold = 0.3),
               predict(cv$best_model, newdata = newx, threshold = 0.3))
})

test_that("coef.ccn_cv matches coef on the best model", {
  cv <- ccn_cv(x, y, q = 1, lambda = 0.01, folds = 3L, seed = 1L)
  expect_equal(coef(cv), coef(cv$best_model))
})

test_that("fitted.ccn_cv matches fitted on the best model", {
  cv <- ccn_cv(x, y, q = 1, lambda = 0.01, folds = 3L, seed = 1L)
  expect_equal(fitted(cv), fitted(cv$best_model))
})

test_that("residuals.ccn_cv matches residuals on the best model", {
  cv <- ccn_cv(x, y, q = 1, lambda = 0.01, folds = 3L, seed = 1L)
  expect_equal(residuals(cv), residuals(cv$best_model))
})
