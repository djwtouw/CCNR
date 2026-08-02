# Generate sample data
set.seed(2)
data <- generate_dataset(n = 200)

# Pairwise conditional entropies of the labels, in bits: entry [i, j] is
# H(y_j | y_i), the uncertainty left in label j once label i is known. Row sums
# say how much a label explains the others, column sums how much the others
# explain it, and the ordering rules below aggregate one or the other.
H <- conditional_entropy_matrix(data$Y)
round(H, 3)
round(rowSums(H), 3)
round(colSums(H), 3)

# Each rule turns that matrix into a chain order. Different rules often agree
# on a given dataset, and they need not agree on a subsample of it.
for (method in c("cebcc1", "cebcc2", "cebcc3", "cebcc4", "ebcc")) {
    cat(method, ":", entropy_label_order(data$Y, method), "\n")
}

# Fit with an order computed up front
order_1 <- entropy_label_order(data$Y, method = "cebcc1")
fit <- ccn(x = data$X, y = data$Y, q = 1, lambda = 0.01,
           label_order = order_1)
fit$label_order

# Or hand ccn the rule itself. label_order also accepts a function f(x, y)
# returning a permutation, evaluated at the start of every fit, so the order
# always comes from the observations that fit actually sees.
cebcc1_order <- function(x, y) entropy_label_order(y, method = "cebcc1")

fit_rule <- ccn(x = data$X, y = data$Y, q = 1, lambda = 0.01,
                label_order = cebcc1_order)
fit_rule$label_order

# On a subsample the same rule can pick a different order, which is exactly
# what makes it worth deferring the choice to fit time
sub <- ccn(x = data$X[1:60, ], y = data$Y[1:60, ], q = 1, lambda = 0.01,
           label_order = cebcc1_order)
sub$label_order

# Cross-validate the rules against each other and against the column order of
# y (the NULL candidate). Passing rules rather than permutations keeps the
# comparison honest: a permutation computed on all of y has already seen the
# validation folds, which flatters it. Any function works here, so a rule of
# your own is just another candidate.
cebcc3_order  <- function(x, y) entropy_label_order(y, method = "cebcc3")
reverse_order <- function(x, y) rev(seq_len(ncol(y)))

cv <- ccn_cv(x = data$X, y = data$Y, q = c(1, 2), lambda = 0.01, folds = 3,
             seed = 1,
             label_order = list(default  = NULL,
                                cebcc1   = cebcc1_order,
                                cebcc3   = cebcc3_order,
                                reversed = reverse_order))
cv$scores
cv$best_params

# The winning candidate is refitted on the full data, so a rule is applied
# again there rather than reusing a fold's order
cv$best_model$label_order
