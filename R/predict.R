#' Predict from a Classifier Chain Network
#'
#' @param object A fitted \code{"ccn"} object.
#' @param newdata Numeric matrix of predictors (n x m). If \code{NULL}, returns
#' the fitted values from training.
#' @param type Character; the type of prediction. \code{"class"} (default)
#' returns a binary 0/1 matrix. \code{"response"} returns predicted
#' probabilities.
#' @param threshold Numeric; decision threshold for converting probabilities to
#' class predictions. Either a scalar applied to all labels or a vector of
#' length L for per-label thresholds. Default \code{0.5}. Ignored when
#' \code{type = "response"}.
#'
#' @return An n x L matrix of predictions, in the original column order of
#' \code{y} used during training.
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
        probs <- object$fitted.probs
    } else {
        ## Validate newdata
        newdata <- as.matrix(newdata)
        if (!is.numeric(newdata))
            stop("newdata must be numeric")
        if (any(is.na(newdata)))
            stop("newdata contains missing values")
        if (ncol(newdata) != object$dims$m)
            stop("newdata has ", ncol(newdata), " columns but the model was ",
                 "trained with ", object$dims$m, " predictors")

        ## Compute probabilities and restore original label order
        inv_order <- order(object$label_order)
        probs     <- t(.ccn_predict(
            t(newdata), object$coefficients_flat, object$dims$L
        ))
        probs <- probs[, inv_order, drop = FALSE]
    }

    ## Attach label names if available
    if (!is.null(object$label_names)) colnames(probs) <- object$label_names

    if (type == "response") return(probs)

    ## Threshold to binary
    if (length(threshold) == 1L) {
        (probs >= threshold) * 1L
    } else {
        sweep(probs, 2, threshold, ">=") * 1L
    }
}
