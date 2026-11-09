# CCNR 0.3.0

* `fitted()` returns the fitted probabilities, as `fitted()` does for a `glm`, and no longer has a `type` argument. Use `predict()` without `newdata` for the fitted classes. The fitted object stores the probabilities in `fitted.values` and no longer has a `fitted.probs` element.
* Added `residuals()` for `ccn_cv` objects.
* `ccn_cv()` and `multilabel_stratified_folds()` with a `seed` leave the random number stream of the caller unchanged. Previously, they reseeded it, so that random draws after the call repeated across calls.
* `ccn_cv()` stops with a clear error when `scoring` does not accept the named arguments `y_true`, `y_class` and `y_prob`, or returns more than one value.
* `ccn_cv()` checks that `fold_assignments` holds whole numbers without missing values and uses at least 2 folds.
* `folds` in `ccn_cv()` and `multilabel_stratified_folds()`, and `n` in `generate_dataset()`, must be whole numbers. A fraction was previously rounded down.
* `conditional_entropy_matrix()` no longer returns tiny negative entropies caused by rounding.
* Added example for `multilabel_stratified_folds`.
* Added a vignette with an example of an analysis of a multi-label data set.
* Label matrices `y` may be logical as well as numeric, so `TRUE`/`FALSE` columns no longer need converting to 0/1.
* `predict()` stops with an error when the training data and `newdata` both have column names that differ, or are in a different order, instead of predicting from the wrong columns.
* `predict()` reads a vector `newdata` as a single observation.
* `ccn()`, `ccn_cv()` and `predict()` reject infinite values, and `ccn()` checks that `conv_tol`, `restart_scale` and `n_starts` are single positive numbers, with `n_starts` a whole number. A negative `conv_tol` previously made the optimizer run forever.
* In `ccn_cv()`, a combination of tuning parameters with a failed fit or an `NA` score in any fold gets `NA` scores and is not selected. Previously, the remaining folds were averaged. `ccn_cv()` stops with a clear error when no combination could be scored in every fold.
* `ccn()` checks that `q` and `lambda` are single finite numbers, and `ccn()` and `ccn_cv()` report a row count mismatch between `x` and `y` with both counts.

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
