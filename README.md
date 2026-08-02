# CCNR: Classifier Chain Networks for Multi-Label Classification

[![R-CMD-check](https://github.com/djwtouw/CCNR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/djwtouw/CCNR/actions/workflows/R-CMD-check.yaml)

Fit a Classifier Chain Network (CCN) for multi-label binary classification. The model jointly learns label dependencies through a chain structure and minimizes an L*q* norm aggregated cross-entropy loss, estimated with a BFGS optimizer with Wolfe line search implemented in C++ via [Rcpp](https://cran.r-project.org/package=Rcpp) and Eigen. The package provides standard methods for prediction, model inspection, and k-fold cross-validation with grid search over the hyperparameters.

More information on the method can be found in the following article:

D.J.W. Touw and M. Van de Velden (2025). *Classifier Chain Networks for Multi-Label Classification*. Expert Systems with Applications, 286, 128048. doi: [10.1016/j.eswa.2025.128048](https://doi.org/10.1016/j.eswa.2025.128048)


## Installation

The package can be installed directly from GitHub using the `remotes` package:

```r
install.packages("remotes")
remotes::install_github("djwtouw/CCNR")
```

The first line can be skipped if `remotes` is already installed. Since `CCNR` includes compiled C++ code, you may first need to set up a toolchain: see the [tools for MacOS](https://cran.r-project.org/bin/macosx/tools/) or [Rtools for Windows](https://cran.r-project.org/bin/windows/Rtools/).


## Example

The main functions are `ccn()`, which fits a classifier chain network for a given set of hyperparameters, and `ccn_cv()`, which selects the hyperparameters via k-fold cross-validation. Both return objects that work with the usual `predict()`, `coef()`, `fitted()`, and `summary()` methods.

```r
library(CCNR)

# Generate sample data: n observations, 3 predictors, 4 binary labels
set.seed(1)
data <- generate_dataset(n = 200)

# Fit a classifier chain network
fit <- ccn(x = data$X, y = data$Y, q = 1, lambda = 0.01)
summary(fit)

# Predicted classes and probabilities
predict(fit, newdata = data$X)
predict(fit, newdata = data$X, type = "response")

# Select hyperparameters via 5-fold cross-validation over a grid
cv <- ccn_cv(x = data$X, y = data$Y, q = c(1, 1.5, 2),
             lambda = c(0.01, 0.1, 1), folds = 5, seed = 1)
print(cv)

# Predict directly from the cross-validation object (uses the best model)
predict(cv, newdata = data$X)
```

Runnable example scripts for all exported functions are available in [`inst/doc/examples`](inst/doc/examples) and on the help pages of the package.


## Label order

Earlier labels in the chain are used as features for later ones, so the order matters. By default the columns of `y` are chained left to right. A different order can be passed as a permutation, or as a rule that derives one from the data:

```r
# Pairwise conditional entropies: entry [i, j] is H(y_j | y_i)
H <- conditional_entropy_matrix(data$Y)

# A chain order derived from those entropies
order_1 <- entropy_label_order(data$Y, method = "cebcc1")
fit <- ccn(x = data$X, y = data$Y, q = 1, lambda = 0.01, label_order = order_1)
```

`entropy_label_order()` implements the orderings of Jun et al. (2019), selected with `method`: `"cebcc1"` (the default, and the best performer in that paper), `"cebcc2"`, `"cebcc3"`, `"cebcc4"`, and `"ebcc"`, which ignores the dependence between labels and sorts by marginal entropy alone.

`label_order` also accepts a function `f(x, y)` returning a permutation, which is evaluated at the start of every fit. Under cross-validation each fold then derives its order from its own training observations, so candidate rules can be compared without the order having seen the validation folds:

```r
cebcc1_order <- function(x, y) entropy_label_order(y, method = "cebcc1")

cv <- ccn_cv(x = data$X, y = data$Y, q = c(1, 2), lambda = c(0.01, 0.1),
             folds = 5, seed = 1,
             label_order = list(default = NULL, cebcc1 = cebcc1_order))
cv$scores
```

Outputs are always returned in the original column order of `y`, whatever the chain order. See [`inst/doc/examples/example-label_order.R`](inst/doc/examples/example-label_order.R) for a fuller walkthrough.

X. Jun, Y. Lu, Z. Lei and D. Guolun (2019). *Conditional entropy based classifier chains for multi-label classification*. Neurocomputing, 335, 185-194. doi: [10.1016/j.neucom.2019.01.039](https://doi.org/10.1016/j.neucom.2019.01.039)


## Correspondence with CCNPy

A Python implementation with the same functionality is available as [CCNPy](https://github.com/djwtouw/CCNPy), which follows the scikit-learn API. The main equivalences are:

| CCNR (R) | CCNPy (Python) |
|----------|----------------|
| `ccn(x, y, q, lambda, ...)` | `CCN(q=..., alpha=..., ...).fit(X, Y)` |
| `predict(fit, type = "class")` | `model.predict(X)` |
| `predict(fit, type = "response")` | `model.predict_proba(X)` |
| `coef(fit)` | `model.coef_` |
| `ccn_cv(...)` | `ccn_cv(...)` or `GridSearchCV` |
| `ccn_hamming_accuracy`, ... | `ccnpy.metrics.*` |
| `generate_dataset(n)` | `generate_dataset(n)` |
| `entropy_label_order(y, method)` | `entropy_label_order(Y, method=...)` |
| `conditional_entropy_matrix(y)` | `conditional_entropy_matrix(Y)` |

The regularization penalty is named `lambda` in R and `alpha` in Python (`lambda` is a reserved word; `alpha` is the scikit-learn convention).


## Bugs and feature requests

Found a bug or have an idea for a new feature? Open an issue in the [*Issues*](https://github.com/djwtouw/CCNR/issues) tab. Before posting, a quick check of existing issues helps avoid duplicates.


## Contributing

Contributions are welcome. Fork the repository, implement your changes, and open a pull request.


## Citation

If you use this package in your research, please cite the accompanying article:

> Touw, D.J.W. and Van de Velden, M. (2025). Classifier chain networks for multi-label classification. *Expert Systems with Applications*, 286, 128048. doi: [10.1016/j.eswa.2025.128048](https://doi.org/10.1016/j.eswa.2025.128048)

You can obtain this citation from within `R` via `citation("CCNR")`.


## License

GPL-3