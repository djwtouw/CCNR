# Generate sample data
set.seed(2)
data <- generate_dataset(n = 200)

# Fit a classifier chain network
fit <- ccn(x = data$X, y = data$Y, q = 1, lambda = 0.01)

# Print a compact overview and a full summary of the model
print(fit)
summary(fit)

# Coefficients: intercepts (b), predictor weights (W), and chain
# weights (C)
coef(fit)

# Fitted classes and fitted probabilities
head(fitted(fit))
head(fitted(fit, type = "response"))

# Response residuals: y minus the fitted probabilities
head(residuals(fit))
