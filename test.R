library(CCNR)

data = generate_dataset(100)
X1 = data$X
Y1 = data$Y

fit = ccn(X1, Y1, 1, 0.01, label_order = c(4, 1, 3, 2))
mean((fit$fitted.values - Y1) == 0)
predict(fit, X1) - fit$fitted.values



print(fit)
summary(fit)
