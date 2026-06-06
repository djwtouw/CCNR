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