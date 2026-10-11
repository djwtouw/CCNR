# 6 observations, 2 labels, chosen so every label has both classes present.
y_true <- matrix(c(1,0,1,0,1,0,
                   1,0,0,1,1,0), nrow = 6, ncol = 2)
y_perf  <- y_true          # perfect class predictions
y_wrong <- 1L - y_true     # all labels flipped
y_prob_good <- matrix(c(0.9,0.1,0.9,0.1,0.9,0.1,
                        0.9,0.1,0.1,0.9,0.9,0.1), nrow = 6, ncol = 2)
y_prob_bad  <- 1 - y_prob_good

# ---- Hamming accuracy --------------------------------------------------

test_that("hamming_accuracy is 1 for perfect predictions", {
  expect_equal(ccn_hamming_accuracy(y_true, y_perf), 1)
})

test_that("hamming_accuracy is 0 when all labels are flipped", {
  expect_equal(ccn_hamming_accuracy(y_true, y_wrong), 0)
})

test_that("hamming_accuracy is in [0, 1]", {
  y_partial <- y_perf; y_partial[1, 1] <- 1L - y_partial[1, 1]
  score <- ccn_hamming_accuracy(y_true, y_partial)
  expect_gte(score, 0)
  expect_lte(score, 1)
})

# ---- Exact match -------------------------------------------------------

test_that("exact_match is 1 for perfect predictions", {
  expect_equal(ccn_exact_match(y_true, y_perf), 1)
})

test_that("exact_match is 0 when every observation has at least one wrong label", {
  expect_equal(ccn_exact_match(y_true, y_wrong), 0)
})

test_that("exact_match returns correct partial score", {
  y_partial <- y_perf; y_partial[1, 1] <- 1L - y_partial[1, 1]  # one obs wrong
  expect_equal(ccn_exact_match(y_true, y_partial), 5 / 6)
})

# ---- Log-likelihood ----------------------------------------------------

test_that("log_likelihood is non-positive", {
  expect_lte(ccn_log_likelihood(y_true, y_prob_good), 0)
})

test_that("log_likelihood is higher for good probabilities than bad ones", {
  good <- ccn_log_likelihood(y_true, y_prob_good)
  bad  <- ccn_log_likelihood(y_true, y_prob_bad)
  expect_gt(good, bad)
})

test_that("log_likelihood averages over label-observation pairs", {
  expected <- mean(y_true * log(y_prob_good) +
                   (1 - y_true) * log(1 - y_prob_good))
  expect_equal(ccn_log_likelihood(y_true, y_prob_good), expected)
})

test_that("log_likelihood is finite for probabilities at 0 and 1", {
  expect_true(is.finite(ccn_log_likelihood(y_true, y_wrong)))
})

# ---- Micro-F1 ----------------------------------------------------------

test_that("micro_f1 is 1 for perfect predictions", {
  expect_equal(ccn_micro_f1(y_true, y_perf), 1)
})

test_that("micro_f1 is in [0, 1]", {
  score <- ccn_micro_f1(y_true, y_wrong)
  expect_gte(score, 0)
  expect_lte(score, 1)
})

test_that("micro_f1 returns 0 when all predictions are flipped", {
  expect_equal(ccn_micro_f1(y_true, y_wrong), 0)
})

# ---- Macro-F1 ----------------------------------------------------------

test_that("macro_f1 is 1 for perfect predictions", {
  expect_equal(ccn_macro_f1(y_true, y_perf), 1)
})

test_that("macro_f1 is in [0, 1]", {
  score <- ccn_macro_f1(y_true, y_wrong)
  expect_gte(score, 0)
  expect_lte(score, 1)
})

test_that("macro_f1 returns 0 when all predictions are flipped", {
  expect_equal(ccn_macro_f1(y_true, y_wrong), 0)
})

# ---- Mean AUC ----------------------------------------------------------

test_that("mean_auc is 1 for a perfect probability ranking", {
  expect_equal(ccn_mean_auc(y_true, y_prob_good), 1)
})

test_that("mean_auc is in [0, 1]", {
  score <- ccn_mean_auc(y_true, y_prob_bad)
  expect_gte(score, 0)
  expect_lte(score, 1)
})

test_that("mean_auc handles a constant label column gracefully (returns NA for that label)", {
  y_const <- y_true; y_const[, 1] <- 1L   # column 1 all ones, AUC undefined
  score <- ccn_mean_auc(y_const, y_prob_good)
  expect_true(is.finite(score))            # mean over remaining labels
  expect_gte(score, 0)
  expect_lte(score, 1)
})

# ---- Scoring interface -------------------------------------------------

# ccn_cv calls every scoring function with the same three named arguments, so
# each one must tolerate the two it does not use.

scorers <- list(
  ccn_hamming_accuracy = ccn_hamming_accuracy,
  ccn_exact_match      = ccn_exact_match,
  ccn_log_likelihood   = ccn_log_likelihood,
  ccn_micro_f1         = ccn_micro_f1,
  ccn_macro_f1         = ccn_macro_f1,
  ccn_mean_auc         = ccn_mean_auc
)

test_that("every scoring function accepts the full ccn_cv argument set", {
  for (name in names(scorers)) {
    score <- scorers[[name]](y_true = y_true, y_class = y_perf,
                             y_prob = y_prob_good)
    expect_length(score, 1L)
    expect_true(is.finite(score), info = name)
  }
})

test_that("scoring functions ignore the arguments they do not use", {
  # A label based score does not depend on the probabilities it is handed
  expect_equal(
    ccn_hamming_accuracy(y_true = y_true, y_class = y_perf,
                         y_prob = y_prob_good),
    ccn_hamming_accuracy(y_true = y_true, y_class = y_perf,
                         y_prob = y_prob_bad)
  )
  # A probability based score does not depend on the labels it is handed
  expect_equal(
    ccn_mean_auc(y_true = y_true, y_class = y_perf, y_prob = y_prob_good),
    ccn_mean_auc(y_true = y_true, y_class = y_wrong, y_prob = y_prob_good)
  )
})

test_that("scoring functions accept data frame input", {
  df_true  <- as.data.frame(y_true)
  df_class <- as.data.frame(y_perf)
  df_prob  <- as.data.frame(y_prob_good)
  expect_equal(ccn_hamming_accuracy(df_true, df_class), 1)
  expect_equal(ccn_exact_match(df_true, df_class), 1)
  expect_equal(ccn_micro_f1(df_true, df_class), 1)
  expect_equal(ccn_macro_f1(df_true, df_class), 1)
  expect_equal(ccn_log_likelihood(df_true, df_prob),
               ccn_log_likelihood(y_true, y_prob_good))
  expect_equal(ccn_mean_auc(df_true, df_prob), 1)
})
