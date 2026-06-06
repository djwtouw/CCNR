#' Generate a synthetic multilabel classification dataset
#'
#' Generates correlated explanatory variables and multilabel binary outcomes
#' with label interdependence. The dataset always has \eqn{m = 3} explanatory
#' variables and \eqn{L = 4} binary labels.
#'
#' The explanatory variables are sampled from a multivariate normal
#' distribution with compound-symmetry covariance (variance 2, covariance
#' 0.4). Each label is generated using a logistic model with random
#' coefficients drawn from \eqn{U(-2, 2)} and optional dependence on all
#' previous labels via coefficients drawn from \eqn{U(-4, 4)}.
#'
#' @param n Integer. Number of observations.
#'
#' @return A list with:
#' \describe{
#'   \item{X}{Numeric matrix of explanatory variables (\eqn{n \times 3}).}
#'   \item{Y}{Binary integer matrix of multilabel outcomes (\eqn{n \times 4}).}
#' }
#'
#' @example inst/doc/examples/example-generate_dataset.R
#'
#' @export
generate_dataset <- function(n) {
    if (!is.numeric(n) || length(n) != 1 || n <= 0) {
        stop("'n' must be a positive integer.")
    }

    n <- as.integer(n)

    X <- MASS::mvrnorm(
        n = n,
        mu = c(0.0, 0.0, 0.0),
        Sigma = matrix(
            c(
                2.0, 0.4, 0.4,
                0.4, 2.0, 0.4,
                0.4, 0.4, 2.0
            ),
            nrow = 3,
            byrow = TRUE
        )
    )

    Y <- matrix(0.0, nrow = nrow(X), ncol = 4)

    for (i in seq_len(ncol(Y))) {
        beta <- stats::runif(ncol(X), min = -2, max = 2)
        eta <- X %*% beta

        if (i > 1) {
            gamma <- stats::runif(i - 1, min = -4, max = 4)
            eta <- eta + Y[, 1:(i - 1), drop = FALSE] %*% gamma
        }

        probs <- 1 / (1 + exp(-eta))

        Y[, i] <- stats::rbinom(
            n = n,
            size = 1,
            prob = probs
        )
    }

    list(
        X = X,
        Y = Y
    )
}
