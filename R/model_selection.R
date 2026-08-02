#' Stratified fold assignments for multi-label data
#'
#' Splits observations into \code{folds} groups by iterative stratification,
#' keeping the proportion of positive examples of every label as close as
#' possible to its proportion in the full data. This produces folds in which
#' each label typically has both classes present, which is desirable for
#' cross-validation of multi-label models. The result plugs directly into the
#' \code{fold_assignments} argument of \code{\link{ccn_cv}}.
#'
#' The algorithm is that of Sechidis, Tsoumakas and Vlahavas (2011), "On the
#' Stratification of Multi-Label Data". Rows are placed one label at a time,
#' rarest label first, each into the fold that most needs an example of that
#' label. Every tie is broken at random, as in the original paper, so repeated
#' calls give different (equally stratified) assignments. \code{seed = NULL}
#' draws from the current random number stream (set \code{\link{set.seed}}
#' beforehand for reproducibility); a \code{seed} gives a reproducible
#' assignment.
#'
#' @param y A numeric matrix of binary responses (n x L), values in
#' \code{{0, 1}}.
#' @param folds Positive integer; number of folds. Default \code{5L}.
#' @param seed Integer or \code{NULL}; random seed controlling the visiting
#' order and the tie-breaking. \code{NULL} (default) uses the current random
#' number stream.
#'
#' @return An integer vector of length n assigning each observation to a fold
#' in \code{1:folds}.
#'
#' @references Sechidis, K., Tsoumakas, G. and Vlahavas, I. (2011). On the
#' Stratification of Multi-Label Data. In \emph{Machine Learning and Knowledge
#' Discovery in Databases} (ECML PKDD), pp. 145-158.
#'
#' @seealso \code{\link{ccn_cv}}, which uses this to generate folds when no
#' \code{fold_assignments} are supplied.
#'
#' @export
multilabel_stratified_folds <- function(y, folds = 5L, seed = NULL) {
    ## Input coercion and validation
    y <- as.matrix(y)
    if (!is.numeric(y))       stop("y must be numeric")
    if (any(is.na(y)))        stop("y contains missing values")
    if (!all(y %in% c(0, 1))) stop("y must contain only 0 and 1")

    k <- as.integer(folds)
    n <- nrow(y)
    if (is.na(k) || k < 2L) stop("folds must be at least 2")
    if (k > n)              stop("folds cannot exceed the number of observations")

    ## Visiting order: a fresh shuffle, or a reproducible one from a seed. The
    ## same stream also drives the random tie-breaking below.
    if (!is.null(seed)) set.seed(seed)
    visit <- sample.int(n)

    .iterative_stratification(y, k, visit)
}


## Pick one of several tied candidates uniformly at random. A single candidate
## is returned as is, since sample(x) with length(x) == 1 permutes seq_len(x).
.break_tie <- function(cand) {
    if (length(cand) == 1L) cand else cand[sample.int(length(cand), 1L)]
}


## Core of iterative stratification for an explicit visiting order.
.iterative_stratification <- function(y, k, visit) {
    n <- nrow(y)
    L <- ncol(y)

    ## Desired number of examples still to place in each fold, overall and per
    ## label. Both are float targets decremented as rows are placed.
    desired_fold <- rep(n / k, k)
    label_counts <- colSums(y)
    desired_label_fold <- matrix(rep(label_counts / k, each = k),
                                 nrow = k, ncol = L)

    label_remaining <- label_counts  # unplaced positives per label
    assigned <- logical(n)
    fold <- integer(n)
    remaining <- n

    while (remaining > 0) {
        positive <- which(label_remaining > 0)

        if (length(positive) == 0) {
            ## Only label-free rows are left; spread them by the overall target.
            for (idx in visit) {
                if (assigned[idx]) next
                j <- .break_tie(which(desired_fold == max(desired_fold)))
                fold[idx] <- j
                assigned[idx] <- TRUE
                desired_fold[j] <- desired_fold[j] - 1
            }
            break
        }

        ## Rarest remaining label first; ties broken at random.
        scarce <- label_remaining[positive]
        label <- .break_tie(positive[scarce == min(scarce)])

        for (idx in visit) {
            if (assigned[idx] || y[idx, label] == 0) next

            ## Fold that most needs an example of this label; ties are settled
            ## by the overall target and any remaining tie at random.
            col <- desired_label_fold[, label]
            cand <- which(col == max(col))
            if (length(cand) > 1) {
                overall <- desired_fold[cand]
                cand <- cand[overall == max(overall)]
            }
            j <- .break_tie(cand)

            fold[idx] <- j
            assigned[idx] <- TRUE
            row_labels <- which(y[idx, ] == 1)
            desired_label_fold[j, row_labels] <-
                desired_label_fold[j, row_labels] - 1
            desired_fold[j] <- desired_fold[j] - 1
            label_remaining[row_labels] <- label_remaining[row_labels] - 1
            remaining <- remaining - 1
        }
    }

    fold
}
