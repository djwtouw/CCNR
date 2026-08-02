reference_matrix <- function(y) {
    # H(y_j | y_i) straight from the definition, one pair at a time
    L   <- ncol(y)
    out <- matrix(0, L, L)
    for (i in seq_len(L)) {
        for (j in seq_len(L)) {
            if (i == j) next
            total <- 0
            for (a in c(0, 1)) {
                p_a <- mean(y[, i] == a)
                if (p_a == 0) next
                for (b in c(0, 1)) {
                    p_ab <- mean(y[, i] == a & y[, j] == b)
                    if (p_ab > 0) total <- total - p_ab * log2(p_ab / p_a)
                }
            }
            out[i, j] <- total
        }
    }
    out
}

random_labels <- function(n, L, seed) {
    set.seed(seed)
    p <- runif(L, 0.1, 0.9)
    matrix(rbinom(n * L, 1, rep(p, each = n)), nrow = n)
}


# -- conditional_entropy_matrix ----------------------------------------------

test_that("conditional_entropy_matrix matches the definition", {
  for (seed in 1:5) {
    y <- random_labels(80, 5, seed)
    expect_equal(conditional_entropy_matrix(y), reference_matrix(y))
  }
})

test_that("independent labels leave a full bit of entropy", {
  y <- rbind(c(0, 0), c(0, 1), c(1, 0), c(1, 1))
  expect_equal(conditional_entropy_matrix(y), matrix(c(0, 1, 1, 0), 2, 2))
})

test_that("identical labels leave no residual entropy", {
  y <- rbind(c(0, 0), c(1, 1), c(0, 0), c(1, 1))
  expect_equal(conditional_entropy_matrix(y), matrix(0, 2, 2))
})

test_that("constant labels do not produce NaN", {
  # Label 2 is constant: it carries no entropy, and conditioning on it leaves
  # the entropy of label 1 untouched
  y <- rbind(c(0, 1), c(1, 1), c(0, 1), c(1, 1))
  ce <- conditional_entropy_matrix(y)
  expect_false(anyNA(ce))
  expect_equal(ce, matrix(c(0, 1, 0, 0), 2, 2))
  expect_false(anyNA(conditional_entropy_matrix(matrix(0, 5, 3))))
})

test_that("the matrix is square with a zero diagonal", {
  y  <- random_labels(100, 4, 10)
  ce <- conditional_entropy_matrix(y)
  expect_equal(dim(ce), c(4L, 4L))
  expect_equal(diag(ce), rep(0, 4))
})

test_that("asymmetry equals the difference in marginal entropies", {
  # H(y_j | y_i) - H(y_i | y_j) = H(y_j) - H(y_i), the mutual information
  # cancels. Prevalences 4/8 and 2/8 give 1 and 0.811 bits.
  y <- rbind(c(0, 0), c(0, 0), c(0, 0), c(0, 0),
             c(1, 0), c(1, 0), c(1, 1), c(1, 1))
  ce <- conditional_entropy_matrix(y)
  h2 <- -(0.25 * log2(0.25) + 0.75 * log2(0.75))
  expect_equal(ce[1, 2] - ce[2, 1], h2 - 1)
})

test_that("column names of y are carried over", {
  y <- random_labels(50, 3, 11)
  colnames(y) <- c("a", "b", "c")
  expect_equal(dimnames(conditional_entropy_matrix(y)),
               list(c("a", "b", "c"), c("a", "b", "c")))
})

test_that("conditional_entropy_matrix validates its input", {
  expect_error(conditional_entropy_matrix(matrix(c(0, 2), 1, 2)),
               "only 0 and 1")
  expect_error(conditional_entropy_matrix(matrix(c(0, NA), 1, 2)),
               "missing values")
})


# -- entropy_label_order -----------------------------------------------------

test_that("every method returns a permutation of 1:L", {
  y <- random_labels(200, 5, 20)
  for (method in c("cebcc1", "cebcc2", "cebcc3", "cebcc4", "ebcc")) {
    o <- entropy_label_order(y, method)
    expect_type(o, "integer")
    expect_equal(sort(o), seq_len(5))
  }
})

test_that("the default method is cebcc1", {
  y <- random_labels(200, 5, 21)
  expect_equal(entropy_label_order(y), entropy_label_order(y, "cebcc1"))
})

test_that("an unknown method is rejected", {
  y <- random_labels(50, 3, 22)
  expect_error(entropy_label_order(y, "cebcc5"))
})

test_that("ebcc sorts by increasing marginal entropy", {
  # Prevalences 0.5, 0.1 and 0.3 give entropies 1.0, 0.469 and 0.881, so the
  # order runs from the most extreme prevalence to the most balanced label
  y <- matrix(0, nrow = 100, ncol = 3)
  y[1:50, 1]  <- 1
  y[1:10, 2]  <- 1
  y[1:30, 3]  <- 1
  expect_equal(entropy_label_order(y, "ebcc"), c(2L, 3L, 1L))
})

test_that("each cebcc rule uses its documented extremum and end", {
  y <- rbind(c(0, 0, 0), c(1, 0, 1), c(1, 1, 0), c(0, 1, 1),
             c(1, 0, 0), c(1, 1, 1), c(0, 0, 1), c(1, 1, 0))
  ce <- conditional_entropy_matrix(y)

  expect_equal(entropy_label_order(y, "cebcc1")[3], which.min(rowSums(ce)))
  expect_equal(entropy_label_order(y, "cebcc2")[1], which.max(rowSums(ce)))
  expect_equal(entropy_label_order(y, "cebcc3")[1], which.min(colSums(ce)))
  expect_equal(entropy_label_order(y, "cebcc4")[3], which.max(colSums(ce)))
})

test_that("ties are resolved toward the lowest remaining index", {
  # Independent, perfectly balanced labels make every sum equal, so each step
  # falls back on the lowest remaining index
  y <- rbind(c(0, 0, 0), c(0, 1, 1), c(1, 0, 1), c(1, 1, 0),
             c(1, 1, 1), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
  expect_equal(entropy_label_order(y, "cebcc3"), c(1L, 2L, 3L))
  expect_equal(entropy_label_order(y, "cebcc1"), c(3L, 2L, 1L))
})

test_that("a single label is handled", {
  y <- matrix(c(0, 1, 1), ncol = 1)
  for (method in c("cebcc1", "cebcc2", "cebcc3", "cebcc4", "ebcc")) {
    expect_equal(entropy_label_order(y, method), 1L)
  }
})


# -- ccn with a function for label_order ------------------------------------

test_that("a label_order function gives the same fit as the order it returns", {
  set.seed(30)
  data <- generate_dataset(n = 150)
  rule <- function(x, y) entropy_label_order(y, "cebcc1")

  fit_rule   <- ccn(data$X, data$Y, q = 1, lambda = 0.01, label_order = rule)
  fit_static <- ccn(data$X, data$Y, q = 1, lambda = 0.01,
                    label_order = entropy_label_order(data$Y, "cebcc1"))

  expect_equal(fit_rule$label_order, fit_static$label_order)
  expect_equal(fit_rule$coefficients_flat, fit_static$coefficients_flat)
})

test_that("a label_order function only sees the data given to that fit", {
  set.seed(31)
  data <- generate_dataset(n = 150)
  seen <- NULL
  rule <- function(x, y) {
    seen <<- nrow(x)
    entropy_label_order(y, "cebcc1")
  }

  ccn(data$X[1:60, ], data$Y[1:60, ], q = 1, lambda = 0.01, label_order = rule)
  expect_equal(seen, 60L)
})

test_that("the resolved order is stored, not the function", {
  set.seed(32)
  data <- generate_dataset(n = 120)
  rule <- function(x, y) entropy_label_order(y, "cebcc1")
  fit  <- ccn(data$X, data$Y, q = 1, lambda = 0.01, label_order = rule)

  expect_type(fit$label_order, "integer")
  expect_equal(sort(fit$label_order), seq_len(ncol(data$Y)))
})

test_that("a label_order function returning an invalid order errors", {
  set.seed(33)
  data <- generate_dataset(n = 100)
  L    <- ncol(data$Y)

  expect_error(ccn(data$X, data$Y, q = 1, lambda = 0.01,
                   label_order = function(x, y) rep(1L, L)),
               "label_order function must be a permutation")
  expect_error(ccn(data$X, data$Y, q = 1, lambda = 0.01,
                   label_order = function(x, y) seq_len(L - 1)),
               "label_order function must have length L")
  expect_error(ccn(data$X, data$Y, q = 1, lambda = 0.01,
                   label_order = function(x, y) NULL),
               "label_order function must return a permutation")
})

test_that("a permuted chain does not permute the output columns", {
  set.seed(34)
  data <- generate_dataset(n = 120)
  rule <- function(x, y) rev(seq_len(ncol(y)))
  fit  <- ccn(data$X, data$Y, q = 1, lambda = 0.01, label_order = rule)

  expect_equal(fit$label_order, rev(seq_len(ncol(data$Y))))
  expect_equal(dim(fitted(fit, type = "response")), dim(data$Y))
  expect_equal(dim(predict(fit, newdata = data$X)), dim(data$Y))
})


# -- ccn_cv over candidate orders -------------------------------------------

test_that("ccn_cv searches over candidate label orders", {
  set.seed(40)
  data <- generate_dataset(n = 150)
  rule <- function(x, y) entropy_label_order(y, "cebcc1")

  cv <- ccn_cv(data$X, data$Y, q = c(1, 2), lambda = 0.01, folds = 3, seed = 1,
               label_order = list(default = NULL, cebcc1 = rule))

  expect_equal(nrow(cv$scores), 4L)
  expect_named(cv$scores,
               c("q", "lambda", "label_order", "mean_score", "sd_score"))
  expect_equal(sort(unique(cv$scores$label_order)), c("cebcc1", "default"))
  expect_true(cv$best_params$label_order %in% c("cebcc1", "default"))
  expect_named(cv$label_orders, c("default", "cebcc1"))
})

test_that("ccn_cv refits the winning rule on the full data", {
  set.seed(41)
  data <- generate_dataset(n = 150)
  rule <- function(x, y) entropy_label_order(y, "cebcc1")

  cv <- ccn_cv(data$X, data$Y, q = 1, lambda = 0.01, folds = 3, seed = 1,
               label_order = list(cebcc1 = rule))

  expect_equal(cv$best_model$label_order,
               entropy_label_order(data$Y, "cebcc1"))
})

test_that("ccn_cv accepts a single candidate that is not in a list", {
  set.seed(42)
  data <- generate_dataset(n = 120)
  o    <- rev(seq_len(ncol(data$Y)))

  cv <- ccn_cv(data$X, data$Y, q = 1, lambda = 0.01, folds = 3, seed = 1,
               label_order = o)

  expect_equal(cv$best_model$label_order, o)
  # A single candidate adds no information, so the column is left out
  expect_named(cv$scores, c("q", "lambda", "mean_score", "sd_score"))
})

test_that("ccn_cv labels unnamed candidates", {
  set.seed(43)
  data <- generate_dataset(n = 120)
  rule <- function(x, y) entropy_label_order(y, "cebcc3")

  cv <- ccn_cv(data$X, data$Y, q = 1, lambda = 0.01, folds = 3, seed = 1,
               label_order = list(NULL, rule))

  expect_named(cv$label_orders, c("default", "order2"))
})

test_that("ccn_cv rejects a malformed candidate", {
  set.seed(44)
  data <- generate_dataset(n = 100)

  expect_error(ccn_cv(data$X, data$Y, q = 1, lambda = 0.01, folds = 3,
                      label_order = list(c(1, 2))),
               "must be NULL, a function, or")
  expect_error(ccn_cv(data$X, data$Y, q = 1, lambda = 0.01, folds = 3,
                      label_order = list()),
               "at least one candidate")
})
