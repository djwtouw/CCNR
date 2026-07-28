# Generate sample data
set.seed(123)
data <- generate_dataset(n = 100)

# Explanatory variables (n x 3) and binary labels (n x 4)
dim(data$X)
dim(data$Y)

head(data$X)
head(data$Y)
