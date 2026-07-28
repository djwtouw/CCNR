# Generate sample data and split into training and test sets
set.seed(2)
data <- generate_dataset(n = 250)
train <- 1:200

# Fit a classifier chain network on the training data
fit <- ccn(x = data$X[train, ], y = data$Y[train, ], q = 1, lambda = 0.01)

# Predicted classes and probabilities for the test data
x_test <- data$X[-train, ]
head(predict(fit, newdata = x_test))
head(predict(fit, newdata = x_test, type = "response"))

# Without newdata, predictions for the training data are returned
head(predict(fit))

# Custom decision threshold, either a scalar for all labels or one
# threshold per label
head(predict(fit, newdata = x_test, threshold = 0.3))
head(predict(fit, newdata = x_test, threshold = c(0.3, 0.5, 0.5, 0.7)))
