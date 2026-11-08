# Generate sample data
set.seed(2)
data <- generate_dataset(n = 200)

# Assign every observation to one of five folds
fold_id <- multilabel_stratified_folds(data$Y, folds = 5, seed = 1)
table(fold_id)

# The proportion of each label in every fold stays close to its proportion in
# the full data
per_fold <- t(sapply(1:5, function(k) colMeans(data$Y[fold_id == k, ])))
round(rbind(full = colMeans(data$Y), per_fold), 3)

# The same seed gives the same assignment
identical(fold_id, multilabel_stratified_folds(data$Y, folds = 5, seed = 1))

# Pass the folds to ccn_cv, so that several searches use the same split
cv <- ccn_cv(x = data$X, y = data$Y, q = c(1, 2), lambda = c(0.01, 0.1),
             fold_assignments = fold_id)
cv$best_params
