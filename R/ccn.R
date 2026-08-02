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
#' @param label_order Integer vector of length L specifying the order in which
#' labels are chained. Entry \code{label_order[l]} gives the index of the
#' original column of \code{y} that occupies position \code{l} in the chain.
#' Earlier labels in the chain are used as features when predicting later
#' labels. If \code{NULL} (default), the natural column order \code{1:L} is
#' used. Outputs are always returned in the original column order of \code{y}.
#'
#' A function is also accepted, and is called as \code{label_order(x, y)} on
#' the data supplied to \code{ccn} before fitting; it must return a
#' permutation of \code{1:L}. This defers the choice of order to fit time, so
#' that under cross-validation every fold derives its order from its own
#' training observations. See \code{\link{entropy_label_order}} for ready made
#' rules and \code{\link{ccn_cv}} for comparing several of them.
#' @param conv_tol Convergence tolerance for the optimizer. Default 1e-6.
#' @param n_starts Number of random restarts; the fit with the lowest training
#' loss is returned. The first restart always uses a deterministic
#' initialization; subsequent restarts draw from \eqn{N(0, \sigma^2)} where
#' \eqn{\sigma} is \code{restart_scale}. Default \code{1}.
#' @param restart_scale Standard deviation of the normal distribution used to
#' generate random starting values for restarts 2 through \code{n_starts}.
#' Ignored when \code{n_starts = 1}. Default \code{0.1}.
#'
#' @return An object of class \code{"ccn"}.
#' @seealso \code{\link{predict.ccn}}, \code{\link{summary.ccn}},
#' \code{\link{coef.ccn}}, \code{\link{fitted.ccn}},
#' \code{\link{residuals.ccn}}, \code{\link{ccn_cv}} for hyperparameter
#' selection via cross-validation.
#'
#' @example inst/doc/examples/example-ccn.R
#'
#' @export
ccn <- function(x, y, q, lambda, label_order = NULL, conv_tol = 1e-6,
                n_starts = 1L, restart_scale = 0.1) {
    ## Defaults
    wolfe_1 = 1e-6
    wolfe_2 = 0.9

    ## Input coercion
    x <- as.matrix(x)
    y <- as.matrix(y)

    ## Input validation
    stopifnot(nrow(x) == nrow(y))
    if (!is.numeric(x))          stop("x must be numeric")
    if (!is.numeric(y))          stop("y must be numeric")
    if (any(is.na(x)))           stop("x contains missing values")
    if (any(is.na(y)))           stop("y contains missing values")
    if (!all(y %in% c(0, 1)))    stop("y must contain only 0 and 1")
    if (q < 1)                   stop("q must be at least 1")
    if (lambda < 0)              stop("lambda must be non-negative")
    if (!is.numeric(restart_scale) ||
        restart_scale <= 0)      stop("restart_scale must be a positive number")
    n_starts <- as.integer(n_starts)
    if (n_starts < 1L)           stop("n_starts must be a positive integer")

    ## Derived dimensions
    L        <- ncol(y)
    m        <- ncol(x)
    n_params <- L + m * L + (L * L - L) %/% 2

    ## Label order
    # First store the column names of y in unpermuted order
    label_names <- colnames(y)

    # A function is evaluated on the data given to this fit, so the order
    # follows the observations it is derived from
    order_source <- "label_order"
    if (is.function(label_order)) {
        label_order  <- label_order(x, y)
        order_source <- "the label_order function"
        if (is.null(label_order))
            stop(order_source, " must return a permutation of 1:L")
    }

    # The order is 1..L by default
    if (is.null(label_order)) {
        label_order <- seq_len(L)
    } else {
        label_order <- suppressWarnings(as.integer(label_order))
        if (length(label_order) != L)
            stop(order_source, " must have length L (", L, ")")
        if (anyNA(label_order) || !setequal(label_order, seq_len(L)))
            stop(order_source, " must be a permutation of 1:L")
    }

    y <- y[, label_order, drop = FALSE]

    ## Build list of starting vectors
    # Start 1: sequential fit of each label. Each link is fitted on the original
    # predictors augmented with the predicted probabilities of the earlier
    # links; those probabilities (not the observed labels) are carried forward.
    # Starts 2..n_starts: random.
    base_init <- rep(0, n_params)
    x_ccs     <- x
    for (label_l in seq_len(L)) {
        fit_l <- tryCatch(
            .ccs_fit(
                t(x_ccs), y[, label_l], lambda, wolfe_1, wolfe_2, conv_tol,
                "cross_entropy", 0.0, 0.0
            ),
            error = function(e) {
                rep(0, m + label_l)
            }
        )

        # Constant
        base_init[label_l] <- fit_l[1]

        # X coefficients
        start <- L + m * (label_l - 1) + 1
        end   <- start + m - 1
        base_init[start:end] <- fit_l[2:(m + 1)]

        # Y coefficients
        if (label_l > 1) {
            start <- L + m * L + ((label_l - 1)^2 - (label_l - 1)) %/% 2 + 1
            end   <- start + label_l - 2
            base_init[start:end] <- fit_l[-c(1:(m + 1))]
        }

        # Enrich the design matrix with this link's predicted probabilities,
        # used as features when initializing the later labels.
        eta   <- fit_l[1] + x_ccs %*% fit_l[-1]
        eta   <- pmin(pmax(eta, -700), 700)
        x_ccs <- cbind(x_ccs, 1 / (1 + exp(-eta)))
    }

    starts <- vector("list", n_starts)
    starts[[1]] <- base_init
    if (n_starts > 1L) {
        for (i in seq(2, n_starts)) {
            starts[[i]] <- stats::rnorm(n_params, mean = 0, sd = restart_scale)
        }
    }

    ## Run optimizer for each start, keep the best
    best_fit  <- NULL
    best_loss <- Inf

    for (i in seq_along(starts)) {
        fit_i <- tryCatch(
            .ccn_fit(
                t(x), t(y), starts[[i]], q, lambda, wolfe_1, wolfe_2, conv_tol,
                "cross_entropy", 0.0, 0.0
            ),
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
        a <- (length(best_fit$coefficients) - (L * L - L) %/% 2 + 1)
        b <- length(best_fit$coefficients)
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

    # Restore original label order
    inv_order    <- order(label_order)
    fitted_probs <- fitted_probs[, inv_order, drop = FALSE]
    if (!is.null(label_names)) colnames(fitted_probs) <- label_names

    ## Assemble S3 object
    structure(
        list(
            coefficients       = coefs,
            coefficients_flat  = best_fit$coefficients,
            label_order        = label_order,
            label_names        = label_names,
            y                  = y[, inv_order, drop = FALSE],
            fitted.values      = (fitted_probs >= 0.5) * 1L,
            fitted.probs       = fitted_probs,
            n_params           = n_params,
            dims               = list(n = nrow(x), m = m, L = L),
            hyperparams        = list(q = q, lambda = lambda),
            n_starts           = n_starts,
            loss               = best_loss,
            call               = match.call()
        ),
        class = "ccn"
    )
}
