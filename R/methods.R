#' Print a Classifier Chain Network
#'
#' @param x A fitted \code{"ccn"} object.
#' @param ... Further arguments passed to or from other methods.
#' @export
print.ccn <- function(x, ...) {
    cat("Classifier Chain Network\n")
    cat("------------------------\n")
    cat("Labels (L)    :", x$dims$L, "\n")
    cat("Predictors (m):", x$dims$m, "\n")
    cat("Observations  :", x$dims$n, "\n")
    cat("Parameters    :", x$n_params, "\n")
    cat("\nHyperparameters:\n")
    cat("  q      :", x$hyperparams$q, "\n")
    cat("  lambda :", x$hyperparams$lambda, "\n")
    cat("\nTraining loss :", round(x$loss, 6), "\n")
    if (!is.null(x$label_names))
        cat("Labels        :", paste(x$label_names, collapse = ", "), "\n")
    cat("Label order   :", x$label_order, "\n")
    invisible(x)
}

#' Summarize a Classifier Chain Network
#'
#' @param object A fitted \code{"ccn"} object.
#' @param ... Further arguments passed to or from other methods.
#' @export
summary.ccn <- function(object, ...) {
    label_names <- object$label_names %||% paste0("L", seq_len(object$dims$L))
    label_names <- label_names[object$label_order]

    cat("Classifier Chain Network - Summary\n")
    cat("----------------------------------\n")
    cat("Labels (L)    :", object$dims$L, "\n")
    cat("Predictors (m):", object$dims$m, "\n")
    cat("Observations  :", object$dims$n, "\n")
    cat("Parameters    :", object$n_params, "\n")
    cat("\nHyperparameters:\n")
    cat("  q      :", object$hyperparams$q, "\n")
    cat("  lambda :", object$hyperparams$lambda, "\n")
    cat("\nTraining loss        :", round(object$loss, 6), "\n")
    cat("Label order          :", object$label_order, "\n")
    cat("Deterministic starts :", 1, "\n")
    cat("Random starts        :", object$n_starts - 1, "\n")

    cat("\nIntercepts (b):\n")
    b <- object$coefficients$b
    names(b) <- label_names
    print(round(b, 4))

    cat("\nWeight matrix (W), m x L:\n")
    W <- object$coefficients$W
    colnames(W) <- label_names
    print(round(W, 4))

    if (length(object$coefficients$C) > 0) {
        cat("\nChain matrix (C), L x L:\n")
        C <- object$coefficients$C
        dn <- label_names
        dimnames(C) <- list(dn, dn)
        print(round(C, 4))
    }

    invisible(object)
}

#' Extract coefficients from a Classifier Chain Network
#'
#' @param object A fitted \code{"ccn"} object.
#' @param ... Further arguments passed to or from other methods.
#' @return A list with elements \code{b} (intercepts), \code{W} (predictor
#' weights), and \code{C} (chain weights).
#' @export
coef.ccn <- function(object, ...) {
    object$coefficients
}

#' Extract fitted values from a Classifier Chain Network
#'
#' @param object A fitted \code{"ccn"} object.
#' @param type Character; \code{"class"} (default) returns binary predictions,
#' \code{"response"} returns probabilities.
#' @param ... Further arguments passed to or from other methods.
#' @export
fitted.ccn <- function(object, type = c("class", "response"), ...) {
    type <- match.arg(type)
    if (type == "response") object$fitted.probs else object$fitted.values
}

#' Extract residuals from a Classifier Chain Network
#'
#' Returns the difference between the binary response matrix and the fitted
#' probabilities.
#'
#' @param object A fitted \code{"ccn"} object.
#' @param ... Further arguments passed to or from other methods.
#' @export
residuals.ccn <- function(object, ...) {
    object$y - object$fitted.probs
}
