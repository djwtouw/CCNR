## Coerce y to a numeric label matrix and check that it holds only 0 and 1.
## Logical columns become 0/1 integers.
.as_label_matrix <- function(y) {
    y <- as.matrix(y)
    if (is.logical(y))        storage.mode(y) <- "integer"
    if (!is.numeric(y))       stop("y must be numeric or logical")
    if (any(is.na(y)))        stop("y contains missing values")
    if (!all(y %in% c(0, 1))) stop("y must contain only 0 and 1")
    y
}


## TRUE when v is a single finite number.
.is_single_number <- function(v) {
    is.numeric(v) && length(v) == 1L && is.finite(v)
}


## TRUE when v is a single whole number of at least lower.
.is_whole_number <- function(v, lower) {
    .is_single_number(v) && v >= lower && v == round(v)
}


## Evaluates expr with the random number stream seeded by seed, then restores
## the stream of the caller. With seed = NULL, expr draws from the current
## stream as usual.
.with_seed <- function(seed, expr) {
    if (is.null(seed)) return(expr)
    env <- globalenv()
    old <- get0(".Random.seed", envir = env, inherits = FALSE)
    on.exit(
        if (is.null(old)) rm(".Random.seed", envir = env)
        else assign(".Random.seed", old, envir = env)
    )
    set.seed(seed)
    expr
}
