# Cross-language parity / regression test.
#
# Fits the model on frozen sample data with a deterministic informed start
# (n_starts = 1, no random restarts) and checks the fitted coefficients and
# predicted probabilities against committed reference values, for both the
# default label order and a custom one.
#
# The identical fixtures (parity_*.csv) and reference values are shipped with
# the Python package and checked by its parity test with the same
# hyperparameters and tolerance. Passing in both languages therefore guards
# against regressions and proves the two implementations produce the same
# output. The fixtures are regenerated from the Python package
# (tests/fixtures/generate_fixtures.py) if the algorithm intentionally changes.

# Must match the Python parity test (test_parity.py) and the fixtures.
.parity_q        <- 2.0
.parity_lambda   <- 0.1
.parity_decimals <- 5            # required agreement; tolerance 10^-decimals

.parity_read <- function(file) {
    m <- as.matrix(read.csv(test_path("fixtures", file), header = FALSE))
    dimnames(m) <- NULL
    m
}

.parity_check <- function(fit, x, coef_file, proba_file) {
    expected_coef  <- as.numeric(.parity_read(coef_file))
    expected_proba <- .parity_read(proba_file)

    proba <- predict(fit, newdata = x, type = "response")
    dimnames(proba) <- NULL

    tol <- 10^(-.parity_decimals)
    expect_lt(max(abs(fit$coefficients_flat - expected_coef)), tol)
    expect_lt(max(abs(proba - expected_proba)), tol)
}

test_that("informed start matches the committed reference values", {
    x <- .parity_read("parity_x.csv")
    y <- .parity_read("parity_y.csv")

    fit <- ccn(x = x, y = y, q = .parity_q, lambda = .parity_lambda,
               n_starts = 1L)

    .parity_check(fit, x, "parity_coef.csv", "parity_proba.csv")
})

test_that("informed start with a custom label order matches the reference", {
    x <- .parity_read("parity_x.csv")
    y <- .parity_read("parity_y.csv")

    # 1-based; the Python test uses the 0-based equivalent [3, 0, 2, 1].
    label_order <- c(4, 1, 3, 2)

    fit <- ccn(x = x, y = y, q = .parity_q, lambda = .parity_lambda,
               n_starts = 1L, label_order = label_order)

    .parity_check(fit, x, "parity_coef_lo.csv", "parity_proba_lo.csv")
})
