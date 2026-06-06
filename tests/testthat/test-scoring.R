# 6 observations, 2 labels — chosen so every label has both classes present.
y_true <- matrix(c(1,0,1,0,1,0,
                   1,0,0,1,1,0), nrow = 6, ncol = 2)
y_perf  <- y_true          # perfect class predictions
y_wrong <- 1L - y_true     # all labels flipped
y_prob_good <- matrix(c(0.9,0.1,0.9,0.1,0.9,0.1,
                        0.9,0.1,0.1,0.9,0.9,0.1), nrow = 6, ncol = 2)
y_prob_bad  <- 1 - y_prob_good

# ---- Hamming accuracy --------------------------------------------------

test_that("hamming_accuracy is 1 for perfect predictions", {
  expect_equal(ccn_hamming_accuracy(y_true, y_perf, y_prob_good), 1)
})

test_that("hamming_accuracy is 0 when all labels are flipped", {
  expect_equal(ccn_hamming_accuracy(y_true, y_wrong, y_prob_bad), 0)
})

test_that("hamming_accuracy is in [0, 1]", {
  y_partial <- y_perf; y_partial[1, 1] <- 1L - y_partial[1, 1]
  score <- ccn_hamming_accuracy(y_true, y_partial, y_prob_good)
  expect_gte(score, 0)
  expect_lte(score, 1)
})

# ---- Exact match -------------------------------------------------------

test_that("exact_match is 1 for perfect predictions", {
  expect_equal(ccn_exact_match(y_true, y_perf, y_prob_good), 1)
})

test_that("exact_match is 0 when every observation has at least one wrong label", {
  expect_equal(ccn_exact_match(y_true, y_wrong, y_prob_bad), 0)
})

test_that("exact_match returns correct partial score", {
  y_partial <- y_perf; y_partial[1, 1] <- 1L - y_partial[1, 1]  # one obs wrong
  expect_equal(ccn_exact_match(y_true, y_partial, y_prob_good), 5 / 6)
})

# ---- Log-likelihood ----------------------------------------------------

test_that("log_likelihood is non-positive", {
  expect_lte(ccn_log_likelihood(y_true, y_perf, y_prob_good), 0)
})

test_that("log_likelihood is higher for good probabilities than bad ones", {
  good <- ccn_log_likelihood(y_true, y_perf,  y_prob_good)
  bad  <- ccn_log_likelihood(y_true, y_wrong, y_prob_bad)
  expect_gt(good, bad)
})

# ---- Micro-F1 ----------------------------------------------------------

test_that("micro_f1 is 1 for perfect predictions", {
  expect_equal(ccn_micro_f1(y_true, y_perf, y_prob_good), 1)
})

test_that("micro_f1 is in [0, 1]", {
  score <- ccn_micro_f1(y_true, y_wrong, y_prob_bad)
  expect_gte(score, 0)
  expect_lte(score, 1)
})

test_that("micro_f1 returns 0 when all predictions are flipped", {
  expect_equal(ccn_micro_f1(y_true, y_wrong, y_prob_bad), 0)
})

# ---- Macro-F1 ----------------------------------------------------------

test_that("macro_f1 is 1 for perfect predictions", {
  expect_equal(ccn_macro_f1(y_true, y_perf, y_prob_good), 1)
})

test_that("macro_f1 is in [0, 1]", {
  score <- ccn_macro_f1(y_true, y_wrong, y_prob_bad)
  expect_gte(score, 0)
  expect_lte(score, 1)
})

test_that("macro_f1 returns 0 when all predictions are flipped", {
  expect_equal(ccn_macro_f1(y_true, y_wrong, y_prob_bad), 0)
})

# ---- Mean AUC ----------------------------------------------------------

test_that("mean_auc is 1 for a perfect probability ranking", {
  expect_equal(ccn_mean_auc(y_true, y_perf, y_prob_good), 1)
})

test_that("mean_auc is in [0, 1]", {
  score <- ccn_mean_auc(y_true, y_perf, y_prob_good)
  expect_gte(score, 0)
  expect_lte(score, 1)
})

test_that("mean_auc handles a constant label column gracefully (returns NA for that label)", {
  y_const <- y_true; y_const[, 1] <- 1L   # column 1 all ones, AUC undefined
  score <- ccn_mean_auc(y_const, y_perf, y_prob_good)
  expect_true(is.finite(score))            # mean over remaining labels
  expect_gte(score, 0)
  expect_lte(score, 1)
})
