#' Pairwise Conditional Entropies of the Labels
#'
#' Computes the conditional entropy of every label given every other label,
#' estimated by plugging the joint frequency table of the two labels into the
#' definition. Entry \code{[i, j]} of the result is \eqn{H(y_j \mid y_i)}, the
#' uncertainty left in label \code{j} once label \code{i} is known, in bits. The
#' diagonal is zero.
#'
#' Row sums and column sums answer different questions. The row sum
#' \eqn{\sum_j H(y_j \mid y_i)} is small when label \code{i} explains the other
#' labels well, while the column sum \eqn{\sum_i H(y_j \mid y_i)} is small when
#' the other labels explain label \code{j} well.
#' \code{\link{entropy_label_order}} builds chain orders from these sums.
#'
#' A label that is constant contributes zero entropy, and conditioning on it
#' leaves the entropy of the other label unchanged.
#'
#' @param y A numeric matrix of binary responses (n x L), values in
#' \code{{0, 1}}.
#'
#' @return An L x L numeric matrix of conditional entropies in bits, with
#' \code{dimnames} taken from the column names of \code{y}, or \code{L1} up to
#' \code{LL} when \code{y} carries none.
#'
#' @references Jun, X., Lu, Y., Lei, Z. and Guolun, D. (2019). Conditional
#' entropy based classifier chains for multi-label classification.
#' \emph{Neurocomputing}, 335, 185-194.
#'
#' @seealso \code{\link{entropy_label_order}}, which turns this matrix into a
#' chain order for the \code{label_order} argument of \code{\link{ccn}}.
#'
#' @example inst/doc/examples/example-label_order.R
#'
#' @export
conditional_entropy_matrix <- function(y) {
    ## Input coercion and validation
    y <- as.matrix(y)
    if (!is.numeric(y))       stop("y must be numeric")
    if (any(is.na(y)))        stop("y contains missing values")
    if (!all(y %in% c(0, 1))) stop("y must contain only 0 and 1")

    n  <- nrow(y)
    y0 <- 1 - y

    ## Joint counts for the four cells of every label pair: entry [i, j] of
    ## n_ab counts the observations with y_i = a and y_j = b.
    n11 <- crossprod(y)
    n10 <- crossprod(y, y0)
    n01 <- crossprod(y0, y)
    n00 <- crossprod(y0)

    ## H(y_j | y_i) conditions on y_i, so the margins vary along the rows.
    ## Recycling a vector against a matrix runs down the columns, which indexes
    ## exactly the row margins needed here.
    p1     <- colMeans(y)
    result <- .entropy_term(n11, n, p1) + .entropy_term(n10, n, p1) +
              .entropy_term(n01, n, 1 - p1) + .entropy_term(n00, n, 1 - p1)
    diag(result) <- 0

    ## Fall back to the same default names that ccn() uses, so that the labels
    ## are identified consistently across the output of the package.
    label_names <- colnames(y) %||% paste0("L", seq_len(ncol(y)))
    dimnames(result) <- list(label_names, label_names)

    result
}


## Term p * log2(margin / p) of a conditional entropy, for one cell of every
## label pair. Cells with zero probability contribute nothing (0 log 0 = 0),
## which also covers a zero margin, since that forces a zero count.
.entropy_term <- function(count, n, margin) {
    p <- count / n
    p * ifelse(p > 0, log2(margin / p), 0)
}


#' Chain Order Derived from the Entropy of the Labels
#'
#' Determines the order in which labels are chained from the labels alone,
#' following Jun et al. (2019). All rules place labels that are more certain
#' earlier in the chain, so that error propagates from well-determined labels
#' toward poorly determined ones rather than the other way around. The result is
#' a permutation suitable for the \code{label_order} argument of
#' \code{\link{ccn}}.
#'
#' Writing \code{H} for the matrix returned by
#' \code{\link{conditional_entropy_matrix}}, the methods are:
#' \describe{
#'   \item{\code{"cebcc1"}}{The label with the smallest row sum of \code{H} is
#'     placed at the end of the chain. The best performing rule in Jun et al.
#'     (2019), hence the default.}
#'   \item{\code{"cebcc2"}}{The label with the largest row sum is placed at the
#'     beginning of the chain.}
#'   \item{\code{"cebcc3"}}{The label with the smallest column sum is placed at
#'     the beginning of the chain.}
#'   \item{\code{"cebcc4"}}{The label with the largest column sum is placed at
#'     the end of the chain.}
#'   \item{\code{"ebcc"}}{Labels are sorted by increasing marginal entropy
#'     \eqn{H(y_l)}, ignoring the dependence between labels.}
#' }
#'
#' The four conditional entropy rules apply their criterion greedily: after
#' placing a label, its row and column are dropped and the sums are recomputed
#' over the labels not yet placed. The rules therefore differ even where they
#' agree on the criterion. Ties are resolved toward the lowest remaining label
#' index, so the result can depend on the column order of \code{y}.
#'
#' Deriving an order from the same labels that are later used for evaluation
#' leaks information. Passing a function to the \code{label_order} argument of
#' \code{\link{ccn}} avoids that, because every fit then derives its order from
#' the observations it is given; see \code{\link{ccn_cv}} for cross-validating
#' several rules against each other.
#'
#' @param y A numeric matrix of binary responses (n x L), values in
#' \code{{0, 1}}.
#' @param method Character; the ordering rule, one of \code{"cebcc1"} (default),
#' \code{"cebcc2"}, \code{"cebcc3"}, \code{"cebcc4"}, or \code{"ebcc"}. See
#' details.
#'
#' @return An integer vector of length L containing a permutation of
#' \code{1:L}, in chain order.
#'
#' @references Jun, X., Lu, Y., Lei, Z. and Guolun, D. (2019). Conditional
#' entropy based classifier chains for multi-label classification.
#' \emph{Neurocomputing}, 335, 185-194.
#'
#' @seealso \code{\link{conditional_entropy_matrix}} for the underlying
#' quantities, \code{\link{ccn}} for the \code{label_order} argument, and
#' \code{\link{ccn_cv}} for selecting among several orders.
#'
#' @example inst/doc/examples/example-label_order.R
#'
#' @export
entropy_label_order <- function(y, method = c("cebcc1", "cebcc2", "cebcc3",
                                              "cebcc4", "ebcc")) {
    method <- match.arg(method)

    ## Input coercion and validation
    y <- as.matrix(y)
    if (!is.numeric(y))       stop("y must be numeric")
    if (any(is.na(y)))        stop("y contains missing values")
    if (!all(y %in% c(0, 1))) stop("y must contain only 0 and 1")

    if (method == "ebcc") {
        ## Removing a label does not change the marginal entropy of any other,
        ## so a single stable sort is equivalent to placing them greedily.
        p <- colMeans(y)
        h <- ifelse(p > 0 & p < 1, -(p * log2(p) + (1 - p) * log2(1 - p)), 0)
        return(order(h))
    }

    .greedy_label_order(conditional_entropy_matrix(y), method)
}


## Greedy chain order from a conditional entropy matrix. Repeatedly places the
## extreme label at one end of the chain and drops its row and column, so later
## steps aggregate only over the labels still unplaced.
.greedy_label_order <- function(ce_matrix, method) {
    use_rows <- method %in% c("cebcc1", "cebcc2")
    take_max <- method %in% c("cebcc2", "cebcc4")
    from_end <- method %in% c("cebcc1", "cebcc4")

    labels <- seq_len(ncol(ce_matrix))
    picks  <- integer(0)

    while (length(labels) > 0) {
        totals <- if (use_rows) rowSums(ce_matrix) else colSums(ce_matrix)

        ## which.min and which.max return the first extremum, which resolves
        ## ties toward the lowest remaining label index.
        idx <- if (take_max) which.max(totals) else which.min(totals)

        picks     <- c(picks, labels[idx])
        labels    <- labels[-idx]
        ce_matrix <- ce_matrix[-idx, -idx, drop = FALSE]
    }

    ## Labels destined for the end of the chain were picked first.
    if (from_end) rev(picks) else picks
}
