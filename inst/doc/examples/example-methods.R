# Generate sample data
set.seed(2)
data <- generate_dataset(n = 200)

# Fit a classifier chain network
fit <- ccn(x = data$X, y = data$Y, q = 1, lambda = 0.01)

# Print a compact overview and a full summary of the model
print(fit)
summary(fit)

# With many explanatory variables, W dominates the summary and
# can be left out
summary(fit, which = c("b", "C"))

# Coefficients: intercepts (b), coefficients of the explanatory
# variables (W), and coefficients of the label predictions (C)
coef(fit)

# Fitted probabilities; predict() without newdata gives the fitted classes
head(fitted(fit))
head(predict(fit))

# Response residuals: y minus the fitted probabilities
head(residuals(fit))
