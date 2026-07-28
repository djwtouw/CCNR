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

test_that("ccn_cv rejects non-binary y", {
  y_cont <- y + 0.5
  expect_error(ccn_cv(x, y_cont, q = 1, lambda = 0.01, folds = 3L))
})

test_that("ccn_cv rejects q < 1", {
  expect_error(ccn_cv(x, y, q = 0.5, lambda = 0.01, folds = 3L))
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
  expect_equal(fitted(cv, type = "response"),
               fitted(cv$best_model, type = "response"))
})
