# CCNR 0.1.0

* Initial release.
* Fits a Classifier Chain Network (CCN) for multi-label binary classification.
* Supports k-fold cross-validation with grid search over q and lambda.
* Standard methods for fitted models (`predict`, `coef`, `fitted`, `residuals`, `print`, `summary`) and for cross-validation results (`predict`, `coef`, `fitted`, `print`, `summary`).
* Built-in scoring functions: Hamming accuracy, exact match ratio, log-likelihood, micro-F1, macro-F1, and mean AUC.
* BFGS optimizer with Wolfe line search and optional random restarts.
