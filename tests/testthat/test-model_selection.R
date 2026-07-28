# Tests for the multi-label stratified fold generator.
#
# The equivalence test reads frozen fixtures (strat_y.csv / strat_folds.csv)
# whose golden fold assignments are the deterministic (seed = NULL) split, so
# any drift in the algorithm is caught here.

set.seed(11)
n <- 200L; L <- 4L
.mk_y <- function(n, L, seed) {
  set.seed(seed)
  matrix(stats::rbinom(n * L, 1, 0.4), n, L)
}
y <- .mk_y(n, L, 11)

.read_fixture <- function(file) {
  as.matrix(read.csv(test_path("fixtures", file), header = FALSE))
}

# ---- Partition / coverage ----------------------------------------------

test_that("multilabel_stratified_folds covers all folds 1..k exactly once", {
  k <- 5L
  f <- multilabel_stratified_folds(y, folds = k, seed = 1L)
  expect_length(f, n)
  expect_equal(sort(unique(f)), seq_len(k))
  # Fold sizes differ by at most one.
  sizes <- as.integer(table(f))
  expect_lte(max(sizes) - min(sizes), 1L)
})

# ---- Label variation in every fold -------------------------------------

test_that("every fold contains both classes of every label", {
  k <- 5L
  f <- multilabel_stratified_folds(y, folds = k)
  for (fold in seq_len(k)) {
    col_sums <- colSums(y[f == fold, , drop = FALSE])
    n_fold <- sum(f == fold)
    expect_true(all(col_sums > 0))
    expect_true(all(col_sums < n_fold))
  }
})

# ---- Label proportions preserved ---------------------------------------

test_that("per-fold label prevalence stays close to the overall prevalence", {
  k <- 5L
  f <- multilabel_stratified_folds(y, folds = k)
  overall <- colMeans(y)
  for (fold in seq_len(k)) {
    prev <- colMeans(y[f == fold, , drop = FALSE])
    expect_true(all(abs(prev - overall) < 0.1))
  }
})

# ---- Determinism and the effect of a seed ------------------------------

test_that("a seed is reproducible", {
  a  <- multilabel_stratified_folds(y, folds = 5L, seed = 42L)
  a2 <- multilabel_stratified_folds(y, folds = 5L, seed = 42L)
  expect_equal(a, a2)
})

test_that("different seeds give different (but valid) assignments", {
  a <- multilabel_stratified_folds(y, folds = 5L, seed = 1L)
  b <- multilabel_stratified_folds(y, folds = 5L, seed = 2L)
  expect_equal(sort(unique(a)), 1:5)
  expect_equal(sort(unique(b)), 1:5)
  expect_false(isTRUE(all.equal(a, b)))
})

test_that("seed = NULL draws from the ambient random number stream", {
  set.seed(123); a <- multilabel_stratified_folds(y, folds = 5L)
  set.seed(123); b <- multilabel_stratified_folds(y, folds = 5L)
  set.seed(456); d <- multilabel_stratified_folds(y, folds = 5L)
  expect_equal(a, b)                       # same ambient seed -> same folds
  expect_false(isTRUE(all.equal(a, d)))    # different ambient seed -> different
})

# ---- Input validation --------------------------------------------------

test_that("multilabel_stratified_folds rejects folds < 2", {
  expect_error(multilabel_stratified_folds(y, folds = 1L))
})

test_that("multilabel_stratified_folds rejects folds > n", {
  expect_error(multilabel_stratified_folds(y, folds = n + 1L))
})

test_that("multilabel_stratified_folds rejects non-binary y", {
  expect_error(multilabel_stratified_folds(y + 0.5, folds = 3L))
})

# ---- Equivalence with the committed golden fixture ---------------------

test_that("identity-order folds match the committed golden fixture", {
  y_fix  <- .read_fixture("strat_y.csv")
  golden <- as.integer(.read_fixture("strat_folds.csv"))
  dimnames(y_fix) <- NULL
  # Drive the identity visiting order so the split is reproducible and matches
  # the frozen golden regardless of the ambient random number stream.
  f <- .iterative_stratification(y_fix, 5L, seq_len(nrow(y_fix)))
  expect_equal(f, golden)
})

# ---- Integration with ccn_cv -------------------------------------------

test_that("ccn_cv uses stratified folds by default", {
  set.seed(3)
  d <- generate_dataset(n = 120)
  cv <- ccn_cv(d$X, d$Y, q = 1, lambda = 0.01, folds = 5L, seed = 1L)
  # Folds are the stratified assignment, not a plain shuffle.
  expect_equal(cv$fold_assignments,
               multilabel_stratified_folds(d$Y, folds = 5L, seed = 1L))
})
