# Generate sample data
set.seed(2)
data <- generate_dataset(n = 200)

# Fit a classifier chain network and obtain predictions
fit <- ccn(x = data$X, y = data$Y, q = 1, lambda = 0.01)
y_class <- predict(fit, newdata = data$X, type = "class")
y_prob <- predict(fit, newdata = data$X, type = "response")

# Scoring functions that compare predicted labels to observed labels
ccn_hamming_accuracy(data$Y, y_class)
ccn_exact_match(data$Y, y_class)
ccn_micro_f1(data$Y, y_class)
ccn_macro_f1(data$Y, y_class)

# Scoring functions that work on the predicted probabilities
ccn_log_likelihood(data$Y, y_prob)
ccn_mean_auc(data$Y, y_prob)

# Use a scoring function to select hyperparameters in cross-validation
cv <- ccn_cv(x = data$X, y = data$Y, q = 1, lambda = c(0.01, 0.1),
             folds = 3, seed = 1, scoring = ccn_mean_auc)
cv$best_params
