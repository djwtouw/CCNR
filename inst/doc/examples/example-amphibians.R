# Load the data
data(amphibians)

X <- amphibians$X
Y <- amphibians$Y

# Principal components (189 x 25) and species presence (189 x 7)
dim(X)
dim(Y)

# Prevalence of each species
round(colMeans(Y), 3)

# Fit a classifier chain network
model <- ccn(X, Y, q = 1.5, lambda = 0.1)
model
