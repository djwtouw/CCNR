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
    cat("Labels        :", paste(x$label_names, collapse = ", "), "\n")
    cat("Label order   :", x$label_order, "\n")
    invisible(x)
}


#' Summarize a Classifier Chain Network
#'
#' Prints an overview of the fitted model followed by the estimated
#' coefficients. Each set of coefficients carries the names of the labels and
#' of the explanatory variables. For a model with many explanatory variables
#' \code{W} dominates the output, in which case \code{which} can be used to
#' leave it out.
#'
#' Of \code{C} only the rows and columns that hold coefficients are printed:
#' the first row and the last column are omitted, since the first label in the
#' chain has no predecessors and the last precedes none. Entries on and above
#' the diagonal are printed blank. \code{\link[=coef.ccn]{coef}} returns the
#' full \code{L} by \code{L} matrix, with \code{NA} in those positions.
#'
#' @param object A fitted \code{"ccn"} object.
#' @param which Character vector selecting which sets of coefficients are
#' printed: any subset of \code{"b"} (intercepts), \code{"W"} (coefficients of
#' the explanatory variables), and \code{"C"} (coefficients of the label
#' predictions). All three are printed by default.
#' @param digits Number of decimal places used for the coefficients. Default
#' \code{4}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @example inst/doc/examples/example-methods.R
#'
#' @export
summary.ccn <- function(object, which = c("b", "W", "C"), digits = 4, ...) {
    which <- match.arg(which, several.ok = TRUE)
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

    if ("b" %in% which) {
        cat("\nIntercepts (b):\n")
        print(round(object$coefficients$b, digits))
    }

    if ("W" %in% which) {
        cat("\nCoefficients of the explanatory variables (W), m x L:\n")
        print(round(object$coefficients$W, digits))
    }

    C <- object$coefficients$C
    if ("C" %in% which && is.matrix(C) && nrow(C) > 1) {
        cat("\nCoefficients of the label predictions (C):\n")
        C_formatted <- C[-1, -ncol(C), drop = FALSE]
        out <- formatC(C_formatted, format = "f", digits = digits)
        out[is.na(C_formatted)] <- ""
        print(out, quote = FALSE, right = TRUE)
    }

    invisible(object)
}


#' Extract coefficients from a Classifier Chain Network
#'
#' Unlike predictions and fitted values, which are returned in the original
#' column order of \code{y}, the coefficients are in chain order: position
#' \code{l} refers to the label that occupies position \code{l} in the chain,
#' that is column \code{label_order[l]} of \code{y}. The names carried by
#' \code{b}, the columns of \code{W}, and both margins of \code{C} identify
#' which label each position belongs to. The rows of \code{W} are named after
#' the columns of \code{x}.
#'
#' @param object A fitted \code{"ccn"} object.
#' @param ... Further arguments passed to or from other methods.
#' @return A list with elements \code{b} (intercepts), \code{W} (coefficients
#' of the explanatory variables), and \code{C} (coefficients of the label
#' predictions).
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
    cat("  lambda :", x$best_params$lambda, "\n")
    if (length(x$label_orders) > 1)
        cat("  order  :", x$best_params$label_order, "\n")
    cat("\n")
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
#' @return A list with elements \code{b} (intercepts), \code{W} (coefficients
#' of the explanatory variables), and \code{C} (coefficients of the label
#' predictions).
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
