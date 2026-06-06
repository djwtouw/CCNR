# Generate sample data
set.seed(2)
data <- generate_dataset(n = 200)

# Fit a classifier chain network
fit <- ccn(x = data$X, y = data$Y, q = 1, lambda = 0.01)
fit

# Inspect the estimated coefficients
coef(fit)

# In-sample predicted classes and probabilities
head(fitted(fit))
head(fitted(fit, type = "response"))

# Fit with a custom label order: labels earlier in the chain are used as
# features for later labels, outputs remain in the original column order
fit_lo <- ccn(x = data$X, y = data$Y, q = 1, lambda = 0.01,
              label_order = c(4, 1, 3, 2))
fit_lo
