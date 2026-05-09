#' Fit a Classifier Chain Network
#'
#' @param x A numeric matrix of predictors (n x m).
#' @param y A numeric matrix of binary responses (n x L).
#' @param q Numeric; exponent of the per-observation Lq norm used to aggregate
#' label losses, must be \eqn{\geq 1}. Given per-label losses \eqn{h_{il}}, the
#' per-observation loss is \eqn{\left(\sum_l h_{il}^q\right)^{1/q}}. When
#' \code{q = 1} all labels contribute equally. As \code{q} increases, larger
#' label losses receive disproportionately more weight. As \code{q}
#' \eqn{\to \infty} this approaches the infinity norm, where only the
#' worst-predicted label drives the loss.
#' @param lambda Non-negative numeric; regularization penalty.
#' @param conv_tol Convergence tolerance for the optimizer. Default 1e-6.
#' @param n_starts Number of random restarts; the fit with the lowest training
#' loss is returned. The first restart always uses \code{params_init};
#' subsequent restarts draw from \eqn{N(0, \sigma^2)} where \eqn{\sigma} is
#' \code{restart_scale}. Default \code{5}.
#' @param n_starts Number of random restarts. The start with the lowest loss
#' is returned. The first start always uses \code{params_init}. Default 5.
#' @param params_init Optional numeric vector of initial parameter values.
#' Length must equal \code{L + m*L + (L*L - L)/2}. Defaults to all zeros.
#' @param restart_scale Standard deviation of the normal distribution used to
#' generate random starting values for restarts 2 through \code{n_starts}.
#' Ignored when \code{n_starts = 1}. Default \code{0.1}.
#'
#' @return An object of class \code{"ccn"}.
#' @export
ccn <- function(x, y, q, lambda, conv_tol = 1e-6, n_starts = 1L,
                params_init = NULL, restart_scale = 0.1) {

    ## Input coercion
    x <- as.matrix(x)
    y <- as.matrix(y)

    ## Input validation
    stopifnot(nrow(x) == nrow(y))
    if (!is.numeric(x))          stop("x must be numeric")
    if (!is.numeric(y))          stop("y must be numeric")
    if (any(is.na(x)))           stop("x contains missing values")
    if (any(is.na(y)))           stop("y contains missing values")
    if (q <= 1)                  stop("q must be at least 1")
    if (lambda < 0)              stop("lambda must be non-negative")
    if (!is.numeric(restart_scale) ||
        restart_scale <= 0)      stop("restart_scale must be a positive number")
    n_starts <- as.integer(n_starts)
    if (n_starts < 1L)           stop("n_starts must be a positive integer")

    ## Derived dimensions
    L        <- ncol(y)
    m        <- ncol(x)
    n_params <- L + m * L + (L * L - L) / 2

    ## Build list of starting vectors
    # Start 1: user-supplied init (or zeros).  Starts 2..n_starts: random.
    base_init <- if (!is.null(params_init)) {
        if (length(params_init) != n_params)
            stop(sprintf("params_init must have length %d (got %d)",
                         n_params, length(params_init)))
        as.numeric(params_init)
    } else {
        rep(0, n_params)
    }

    starts <- vector("list", n_starts)
    starts[[1]] <- base_init
    if (n_starts > 1L) {
        for (i in seq(2, n_starts)) {
            starts[[i]] <- rnorm(n_params, mean = 0, sd = restart_scale)
        }
    }

    ## Run optimizer for each start, keep the best
    best_fit  <- NULL
    best_loss <- Inf

    for (i in seq_along(starts)) {
        fit_i <- tryCatch(
            .ccn_fit(t(x), t(y), starts[[i]], q, lambda, 1e-6, 0.9, conv_tol,
                     "cross_entropy", 0.0, 0.0),
            error = function(e) {
                warning(sprintf("Start %d failed: %s", i, conditionMessage(e)))
                NULL
            }
        )
        if (!is.null(fit_i) && fit_i$loss < best_loss) {
            best_fit  <- fit_i
            best_loss <- fit_i$loss
        }
    }

    if (is.null(best_fit))
        stop("All optimization starts failed. Check your data and hyperparameters.")

    ## Reshape flat coefficient vector into a structured list
    coefs = list()
    coefs$b = best_fit$coefficients[seq_len(L)]

    coefs$W = matrix(0, nrow = m, ncol = L)
    for (label_l in seq_len(L)) {
        a <- L + (label_l - 1) * m + 1
        b <- a + m - 1
        coefs$W[, label_l] <- best_fit$coefficients[a:b]
    }

    if (L > 1) {
        # Select the c coefficients
        a = (length(best_fit$coefficients) - (L * L - L) %/% 2 + 1)
        b = length(best_fit$coefficients)
        c <- best_fit$coefficients[a:b]

        # Fill the lower triangular part of C row-major.
        coefs$C <- matrix(NA, nrow = L, ncol = L)
        coefs$C[upper.tri(coefs$C)] <- c
        coefs$C <- t(coefs$C)
    } else {
        coefs$C <- numeric(0)
    }

    ## Fitted values
    fitted_probs <- t(.ccn_predict(t(x), best_fit$coefficients, L))

    ## Assemble S3 object
    structure(
        list(
            coefficients  = coefs,
            fitted.values = (fitted_probs >= 0.5) * 1L,
            fitted.probs  = fitted_probs,
            n_params      = n_params,
            dims          = list(n = nrow(x), m = m, L = L),
            hyperparams   = list(q = q, lambda = lambda),
            n_starts      = n_starts,
            loss          = best_loss,
            call          = match.call()
        ),
        class = "ccn"
    )
}
