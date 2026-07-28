#' Print a Classifier Chain Network
#'
#' @param x A fitted \code{"ccn"} object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @example inst/doc/examples/example-methods.R
#'
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
#'
#' @example inst/doc/examples/example-methods.R
#'
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
#'
#' @example inst/doc/examples/example-methods.R
#'
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
#' @return An n x L matrix of fitted values in the original column order of
#' \code{y}.
#'
#' @example inst/doc/examples/example-methods.R
#'
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
#' @return An n x L numeric matrix of response residuals (\code{y - fitted
#' probabilities}) in the original column order of \code{y}.
#'
#' @example inst/doc/examples/example-methods.R
#'
#' @export
residuals.ccn <- function(object, ...) {
    object$y - object$fitted.probs
}


#' Print a Classifier Chain Network Cross-validation
#'
#' @param x A fitted \code{"ccn_cv"} object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @example inst/doc/examples/example-ccn_cv.R
#'
#' @export
print.ccn_cv <- function(x, ...) {
    cat("CCN Cross-validation\n")
    cat("--------------------\n")
    cat("Folds  :", x$folds, "\n")
    cat("Grid   :", nrow(x$scores), "hyperparameter combinations\n\n")
    cat("Best parameters:\n")
    cat("  q      :", x$best_params$q, "\n")
    cat("  lambda :", x$best_params$lambda, "\n\n")
    cat("Full grid results:\n")
    print(x$scores, row.names = FALSE, digits = 4)
    invisible(x)
}


#' Summarize Classifier Chain Network Cross-Validation Results
#'
#' @param object A fitted \code{"ccn_cv"} object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @example inst/doc/examples/example-ccn_cv.R
#'
#' @export
summary.ccn_cv <- function(object, ...) {
    print(object, ...)
}


#' Extract Coefficients from a Cross-validated Classifier Chain Network
#'
#' Extracts the coefficients of the best model selected by cross-validation.
#' Equivalent to calling \code{\link{coef.ccn}} on \code{object$best_model}.
#'
#' @param object A fitted \code{"ccn_cv"} object.
#' @param ... Further arguments passed to \code{\link{coef.ccn}}.
#' @return A list with elements \code{b} (intercepts), \code{W} (predictor
#' weights), and \code{C} (chain weights).
#' @seealso \code{\link{ccn_cv}}, \code{\link{coef.ccn}}.
#'
#' @example inst/doc/examples/example-ccn_cv.R
#'
#' @export
coef.ccn_cv <- function(object, ...) {
    coef(object$best_model, ...)
}


#' Extract Fitted Values from a Cross-validated Classifier Chain Network
#'
#' Extracts the fitted values of the best model selected by cross-validation,
#' which is refitted on the full data. Equivalent to calling
#' \code{\link{fitted.ccn}} on \code{object$best_model}.
#'
#' @param object A fitted \code{"ccn_cv"} object.
#' @param type Character; \code{"class"} (default) returns binary predictions,
#' \code{"response"} returns probabilities.
#' @param ... Further arguments passed to \code{\link{fitted.ccn}}.
#' @return An n x L matrix of fitted values in the original column order of
#' \code{y}.
#' @seealso \code{\link{ccn_cv}}, \code{\link{fitted.ccn}}.
#'
#' @example inst/doc/examples/example-ccn_cv.R
#'
#' @export
fitted.ccn_cv <- function(object, type = c("class", "response"), ...) {
    fitted(object$best_model, type = type, ...)
}
