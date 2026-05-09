#' Fit a mymodel
#'
#' @param x A numeric matrix of predictors.
#' @param y A numeric vector of responses.
#' @param ... Additional arguments passed to the C routine.
#' @return An object of class \code{"mymodel"}.
#' @export
ccn <- function(x, y, q, lambda, conv_tol = 1e-6) {
    x <- as.matrix(x)
    y <- as.matrix(y)
    stopifnot(nrow(x) == nrow(y))
    L = dim(y)[2]
    m = dim(x)[2]

    # Further input validation.
    if (!is.numeric(x)) stop("x must be numeric")
    if (!is.numeric(y)) stop("y must be numeric")
    if (any(is.na(x)))  stop("x contains missing values")
    if (any(is.na(y)))  stop("y contains missing values")
    if (q <= 0)         stop("q must be positive")
    if (lambda < 0)     stop("lambda must be non-negative")

    n_params = L + m * L + (L * L - L) / 2
    params_init = rep(0, n_params)

    # Fit the parameters.
    fit <- .ccn_fit(t(x), t(y), params_init, q, lambda, conv_tol, 1e-6, 0.9, "cross_entropy", 0.0, 0.0)
    fitted.probs <- .ccn_predict(t(x), fit$coefficients, L)

    structure(
        list(
            coefficients  = fit$coefficients,
            fitted.values = (t(fitted.probs) >= 0.5) * 1L,
            fitted.probs  = t(fitted.probs),
            n_params      = n_params,
            dims          = list(n = nrow(x), m = m, L = L),
            hyperparams   = list(q = q, lambda = lambda),
            convergence   = fit$convergence,
            call          = match.call()
        ),
        class = "ccn"
    )
}
