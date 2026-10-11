# CCNR 0.2.0

* Initial release.
* Fits a Classifier Chain Network (CCN) for multi-label binary classification.
* Supports k-fold cross-validation with grid search over q, lambda, and candidate label orders.
* The order in which labels are chained can be given as a permutation, or as a function of the training data that is evaluated at the start of every fit.
* `conditional_entropy_matrix()` returns the conditional entropy of every label given every other label, and `entropy_label_order()` turns those entropies into a chain order, with methods `"cebcc1"` to `"cebcc4"` and `"ebcc"` following Jun et al. (2019).
* Standard methods for fitted models (`predict`, `coef`, `fitted`, `residuals`, `print`, `summary`) and for cross-validation results (`predict`, `coef`, `fitted`, `print`, `summary`).
* Built-in scoring functions: Hamming accuracy, exact match ratio, log-likelihood, micro-F1, macro-F1, and mean AUC.
* BFGS optimizer with Wolfe line search and optional random restarts.
* `amphibians`: presence of seven amphibian species at 189 Polish water reservoirs, with the site characteristics reduced to 25 principal components. Provides a multilabel data set with rare labels for examples and illustrations.
