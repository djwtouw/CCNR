#' Predict from a Classifier Chain Network
#'
#' @param object A fitted \code{"ccn"} object.
#' @param newdata Numeric matrix or data frame of explanatory variables
#' (n x m). If \code{NULL}, returns the fitted values from training. Columns
#' are matched by position. When both the training data and \code{newdata}
#' have column names, these must be identical and in the same order. A vector
#' is read as a single observation.
#' @param type Character; the type of prediction. \code{"class"} (default)
#' returns a binary 0/1 matrix. \code{"response"} returns predicted
#' probabilities.
#' @param threshold Numeric; decision threshold for converting probabilities to
#' class predictions. Either a scalar applied to all labels or a vector of
#' length L for per-label thresholds. Default \code{0.5}. Ignored when
#' \code{type = "response"}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return An n x L matrix of predictions, in the original column order of
#' \code{y} used during training.
#' @seealso \code{\link{ccn}} for fitting a model.
#'
#' @example inst/doc/examples/example-predict.ccn.R
#'
#' @export
predict.ccn <- function(object, newdata = NULL, type = c("class", "response"),
                        threshold = 0.5, ...) {
    type <- match.arg(type)

    ## Validate threshold
    L <- object$dims$L
    if (!is.numeric(threshold) || any(threshold <= 0) || any(threshold >= 1))
        stop("threshold must be numeric and strictly between 0 and 1")
    if (length(threshold) != 1L && length(threshold) != L)
        stop("threshold must be a scalar or a vector of length L (", L, ")")

    ## Return fitted values if no new data
    if (is.null(newdata)) {
        probs <- object$fitted.values
    } else {
        ## A plain vector is one observation, its names are the column names
        is_vector <- is.atomic(newdata) && is.null(dim(newdata))
        if (is_vector)
            newdata <- matrix(newdata, nrow = 1,
                              dimnames = list(NULL, names(newdata)))

        ## Validate newdata
        newdata <- as.matrix(newdata)
        if (!is.numeric(newdata))
            stop("newdata must be numeric")
        if (!all(is.finite(newdata)))
            stop("newdata contains missing or infinite values")

        if (ncol(newdata) != object$dims$m)
            stop("newdata has ", ncol(newdata), " columns but the model was ",
                 "trained with ", object$dims$m, " explanatory variables",
                 if (is_vector) paste0(". A vector is read as one ",
                                       "observation; pass a one-column matrix ",
                                       "for several observations of a single ",
                                       "variable"))

        ## Columns are used by position, so named columns must be in the
        ## training order
        trained <- object$variable_names
        if (!is.null(trained) && !is.null(colnames(newdata)) &&
            !identical(colnames(newdata), trained))
            stop("the column names of newdata must match those used in ",
                 "training, in the same order: ",
                 paste(trained, collapse = ", "))

        ## Compute probabilities and restore original label order
        inv_order <- order(object$label_order)
        probs     <- t(.ccn_predict(
            t(newdata), object$coefficients_flat, object$dims$L
        ))
        probs <- probs[, inv_order, drop = FALSE]
    }

    ## Attach label names
    colnames(probs) <- object$label_names

    if (type == "response") return(probs)

    ## Threshold to binary
    if (length(threshold) == 1L) {
        (probs >= threshold) * 1L
    } else {
        sweep(probs, 2, threshold, ">=") * 1L
    }
}


#' Predict from a Cross-validated Classifier Chain Network
#'
#' Computes predictions from the best model selected by cross-validation.
#' Equivalent to calling \code{\link{predict.ccn}} on \code{object$best_model}.
#'
#' @param object A fitted \code{"ccn_cv"} object.
#' @param newdata Numeric matrix or data frame of explanatory variables
#' (n x m). If \code{NULL}, returns the fitted values from training. Columns
#' are matched by position. When both the training data and \code{newdata}
#' have column names, these must be identical and in the same order. A vector
#' is read as a single observation.
#' @param type Character; the type of prediction. \code{"class"} (default)
#' returns a binary 0/1 matrix. \code{"response"} returns predicted
#' probabilities.
#' @param threshold Numeric; decision threshold for converting probabilities to
#' class predictions. Either a scalar applied to all labels or a vector of
#' length L for per-label thresholds. Default \code{0.5}. Ignored when
#' \code{type = "response"}.
#' @param ... Further arguments passed to \code{\link{predict.ccn}}.
#'
#' @return An n x L matrix of predictions, in the original column order of
#' \code{y} used during training.
#' @seealso \code{\link{ccn_cv}}, \code{\link{predict.ccn}}.
#'
#' @example inst/doc/examples/example-ccn_cv.R
#'
#' @export
predict.ccn_cv <- function(object, newdata = NULL,
                           type = c("class", "response"), threshold = 0.5,
                           ...) {
    predict(object$best_model, newdata = newdata, type = type,
            threshold = threshold, ...)
}
