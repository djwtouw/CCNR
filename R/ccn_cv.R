#' Cross-validation for a Classifier Chain Network
#'
#' Performs k-fold cross-validation over a grid of hyperparameters, returning
#' the best model refitted on the full data.
#'
#' @param x A numeric matrix of predictors (n x m).
#' @param y A numeric matrix of binary responses (n x L).
#' @param q Numeric vector; candidate values for the Lq norm exponent. All
#' combinations with \code{lambda} are evaluated.
#' @param lambda Numeric vector; candidate regularization penalties. All
#' combinations with \code{q} are evaluated.
#' @param folds Positive integer; number of cross-validation folds. Ignored if
#' \code{fold_assignments} is provided. Default \code{5L}.
#' @param fold_assignments Optional integer vector of length n assigning each
#' observation to a fold. Allows the same folds to be reused across methods for
#' fair comparison. If \code{NULL} (default), folds are generated internally by
#' \code{\link{multilabel_stratified_folds}} using \code{seed}, so that the
#' proportion of each label is kept roughly constant across folds.
#' @param scoring A function with signature \code{function(y_true, y_class,
#' y_prob)} returning a single numeric value. Higher values are assumed to be
#' better. Defaults to \code{\link{ccn_hamming_accuracy}}.
#' @param seed Integer; random seed for fold generation. Ignored if
#' \code{fold_assignments} is provided. Default \code{NULL}.
#' @param ... Additional arguments passed to \code{\link{ccn}}.
#'
#' @return An object of class \code{"ccn_cv"} containing:
#' \describe{
#'   \item{best_model}{A \code{"ccn"} object fitted on the full data with the
#'     best hyperparameters.}
#'   \item{best_params}{A one-row data frame with the best \code{q} and
#'     \code{lambda}.}
#'   \item{scores}{A data frame with one row per hyperparameter combination
#'     and columns for \code{q}, \code{lambda}, mean score, and sd of score
#'     across folds.}
#'   \item{fold_assignments}{Integer vector mapping each observation to a
#'     fold, for reproducibility and comparison with other methods.}
#'   \item{folds}{The number of folds used.}
#' }
#' @seealso \code{\link{ccn_hamming_accuracy}}, \code{\link{ccn_exact_match}},
#' \code{\link{ccn_mean_auc}}, \code{\link{ccn_log_likelihood}},
#' \code{\link{ccn_micro_f1}}, \code{\link{ccn_macro_f1}} for built-in scoring
#' functions; \code{\link{multilabel_stratified_folds}} for the fold generator;
#' \code{\link{predict.ccn_cv}}, \code{\link{coef.ccn_cv}},
#' \code{\link{fitted.ccn_cv}} for working with the best model.
#'
#' @example inst/doc/examples/example-ccn_cv.R
#'
#' @export
ccn_cv <- function(x, y, q, lambda, folds = 5L, fold_assignments = NULL,
                   scoring = ccn_hamming_accuracy, seed = NULL, ...) {
    ## Input coercion
    x <- as.matrix(x)
    y <- as.matrix(y)

    ## Input validation
    stopifnot(nrow(x) == nrow(y))
    if (!is.numeric(x))        stop("x must be numeric")
    if (!is.numeric(y))        stop("y must be numeric")
    if (any(is.na(x)))         stop("x contains missing values")
    if (any(is.na(y)))         stop("y contains missing values")
    if (!all(y %in% c(0, 1)))  stop("y must contain only 0 and 1")
    if (!is.numeric(q) || any(q < 1))
        stop("all values of q must be >= 1")
    if (!is.numeric(lambda) || any(lambda < 0))
        stop("all values of lambda must be non-negative")
    if (!is.function(scoring)) stop("scoring must be a function")

    n <- nrow(x)

    ## Fold assignments
    if (!is.null(fold_assignments)) {
        fold_assignments <- as.integer(fold_assignments)
        if (length(fold_assignments) != n)
            stop("fold_assignments must have length n (", n, ")")
        if (!setequal(fold_assignments, seq_len(max(fold_assignments))))
            stop("fold_assignments must contain consecutive integers from 1 to k")
        folds <- max(fold_assignments)
    } else {
        folds <- as.integer(folds)
        if (folds < 2L)      stop("folds must be at least 2")
        if (folds > n)       stop("folds cannot exceed the number of observations")
        fold_assignments <- multilabel_stratified_folds(y, folds = folds,
                                                        seed = seed)
    }

    ## Build hyperparameter grid
    grid <- expand.grid(q = q, lambda = lambda)

    ## Evaluate each hyperparameter combination
    grid_scores <- vector("list", nrow(grid))

    for (g in seq_len(nrow(grid))) {
        q_g      <- grid$q[g]
        lambda_g <- grid$lambda[g]
        fold_scores <- numeric(folds)

        for (k in seq_len(folds)) {
            test_idx  <- which(fold_assignments == k)
            train_idx <- which(fold_assignments != k)
            x_train <- x[train_idx, , drop = FALSE]
            y_train <- y[train_idx, , drop = FALSE]

            fit_k <- tryCatch(
                ccn(x = x_train, y = y_train, q = q_g, lambda = lambda_g, ...),
                error = function(e) {
                    warning(sprintf(
                        "Grid point (q=%.2f, lambda=%.4f), fold %d failed: %s",
                        q_g, lambda_g, k, conditionMessage(e)
                    ))
                    NULL
                }
            )

            if (is.null(fit_k)) {
                fold_scores[k] <- NA_real_
                next
            }

            fold_scores[k] <- scoring(
                y_true  = y[test_idx, , drop = FALSE],
                y_class = predict(fit_k, newdata = x[test_idx, , drop = FALSE],
                                  type = "class"),
                y_prob  = predict(fit_k, newdata = x[test_idx, , drop = FALSE],
                                  type = "response")
            )
        }

        grid_scores[[g]] <- data.frame(
            q          = q_g,
            lambda     = lambda_g,
            mean_score = mean(fold_scores, na.rm = TRUE),
            sd_score   = stats::sd(fold_scores, na.rm = TRUE)
        )
    }

    scores <- do.call(rbind, grid_scores)

    ## Find best hyperparameters
    best_idx    <- which.max(scores$mean_score)
    best_params <- scores[best_idx, c("q", "lambda")]

    ## Refit on full data with best hyperparameters
    best_model <- ccn(
        x = x, y = y, q = best_params$q, lambda = best_params$lambda, ...
    )

    structure(
        list(
            best_model       = best_model,
            best_params      = best_params,
            scores           = scores,
            fold_assignments = fold_assignments,
            folds            = folds
        ),
        class = "ccn_cv"
    )
}


#' Hamming accuracy scoring function
#'
#' Computes the proportion of correctly predicted individual labels, for use
#' as a \code{scoring} function in \code{\link{ccn_cv}}.
#'
#' @param y_true A binary matrix of true labels (n x L).
#' @param y_class A binary matrix of predicted labels (n x L).
#' @param y_prob A numeric matrix of predicted probabilities (n x L). Not used
#' by this function but required by the scoring interface.
#' @return A single numeric value between 0 and 1.
#' @seealso \code{\link{ccn_cv}} where this function is intended to be used.
#'
#' @example inst/doc/examples/example-scoring.R
#'
#' @export
ccn_hamming_accuracy <- function(y_true, y_class, y_prob) {
    mean(y_true == y_class)
}


#' Exact match ratio scoring function
#'
#' Computes the proportion of observations where all labels are predicted
#' correctly, for use as a \code{scoring} function in \code{\link{ccn_cv}}. This
#' scoring function is closely related to the zero-one loss, which measures the
#' proportion of observations where at least one label is predicted incorrectly.
#'
#' @param y_true A binary matrix of true labels (n x L).
#' @param y_class A binary matrix of predicted labels (n x L).
#' @param y_prob A numeric matrix of predicted probabilities (n x L). Not used
#' by this function but required by the scoring interface.
#' @return A single numeric value between 0 and 1.
#' @seealso \code{\link{ccn_cv}} where this function is intended to be used.
#'
#' @example inst/doc/examples/example-scoring.R
#'
#' @export
ccn_exact_match <- function(y_true, y_class, y_prob) {
    mean(rowSums(y_true != y_class) == 0)
}


#' Mean log-likelihood scoring function
#'
#' Computes the mean per-observation log-likelihood under the predicted
#' probabilities, for use as a \code{scoring} function in
#' \code{\link{ccn_cv}}.
#'
#' @param y_true A binary matrix of true labels (n x L).
#' @param y_class A binary matrix of predicted labels (n x L). Not used by this
#' function but required by the scoring interface.
#' @param y_prob A numeric matrix of predicted probabilities (n x L).
#' @param eps Numeric; probabilities are clipped to \code{[eps, 1 - eps]} before
#' taking the log to avoid \code{-Inf}. Default \code{1e-15}.
#' @return A single numeric value; less negative values are better.
#' @seealso \code{\link{ccn_cv}} where this function is intended to be used.
#'
#' @example inst/doc/examples/example-scoring.R
#'
#' @export
ccn_log_likelihood <- function(y_true, y_class, y_prob, eps = 1e-15) {
    y_prob <- pmax(pmin(y_prob, 1 - eps), eps)
    mean(rowSums(y_true * log(y_prob) + (1 - y_true) * log(1 - y_prob)))
}


#' Micro-F1 scoring function
#'
#' Computes the micro-averaged F1 score, where the confusion matrix is
#' accumulated across all labels before computing F1. This weights each
#' label-observation pair equally, so labels with more positive instances
#' contribute more to the score.
#'
#' @param y_true A binary matrix of true labels (n x L).
#' @param y_class A binary matrix of predicted labels (n x L).
#' @param y_prob A numeric matrix of predicted probabilities (n x L). Not used
#' by this function but required by the scoring interface.
#' @return A single numeric value between 0 and 1.
#' @seealso \code{\link{ccn_cv}} where this function is intended to be used.
#'
#' @example inst/doc/examples/example-scoring.R
#'
#' @export
ccn_micro_f1 <- function(y_true, y_class, y_prob) {
    tp <- sum(y_true == 1 & y_class == 1)
    fp <- sum(y_true == 0 & y_class == 1)
    fn <- sum(y_true == 1 & y_class == 0)
    if ((2 * tp + fp + fn) == 0) 0 else 2 * tp / (2 * tp + fp + fn)
}


#' Macro-F1 scoring function
#'
#' Computes the macro-averaged F1 score, where F1 is computed per label and
#' then averaged. This weights each label equally regardless of class imbalance.
#'
#' @param y_true A binary matrix of true labels (n x L).
#' @param y_class A binary matrix of predicted labels (n x L).
#' @param y_prob A numeric matrix of predicted probabilities (n x L). Not used
#' by this function but required by the scoring interface.
#' @return A single numeric value between 0 and 1.
#' @seealso \code{\link{ccn_cv}} where this function is intended to be used.
#'
#' @example inst/doc/examples/example-scoring.R
#'
#' @export
ccn_macro_f1 <- function(y_true, y_class, y_prob) {
    L <- ncol(y_true)
    f1_per_label <- sapply(seq_len(L), function(l) {
        tp <- sum(y_true[, l] == 1 & y_class[, l] == 1)
        fp <- sum(y_true[, l] == 0 & y_class[, l] == 1)
        fn <- sum(y_true[, l] == 1 & y_class[, l] == 0)
        if ((2 * tp + fp + fn) == 0) 0 else 2 * tp / (2 * tp + fp + fn)
    })
    mean(f1_per_label)
}


#' Mean AUC scoring function
#'
#' Computes the mean per-label AUC, for use as a \code{scoring} function in
#' \code{\link{ccn_cv}}.
#'
#' @param y_true A binary matrix of true labels (n x L).
#' @param y_class A binary matrix of predicted labels (n x L). Not used by this
#' function but required by the scoring interface.
#' @param y_prob A numeric matrix of predicted probabilities (n x L).
#' @return A single numeric value between 0 and 1.
#' @seealso \code{\link{ccn_cv}} where this function is intended to be used.
#'
#' @example inst/doc/examples/example-scoring.R
#'
#' @export
ccn_mean_auc <- function(y_true, y_class, y_prob) {
    L <- ncol(y_true)
    aucs <- sapply(seq_len(L), function(l) {
        tryCatch(
            auc_binary(y_true[, l], y_prob[, l]),
            error = function(e) NA_real_
        )
    })
    mean(aucs, na.rm = TRUE)
}


auc_binary <- function(y_true, y_prob) {
    if (length(unique(y_true)) < 2L)
        stop("AUC undefined: label has only one class in this fold")
    n_pos    <- sum(y_true == 1)
    n_neg    <- sum(y_true == 0)
    rank_sum <- sum(rank(y_prob)[y_true == 1])
    (rank_sum - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}
