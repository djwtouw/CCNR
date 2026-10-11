# Builds data/amphibians.rda from data-raw/amphibians.csv.
#
# The raw file is the Amphibians data set of the UCI Machine Learning
# Repository (https://doi.org/10.24432/C5H90N). It holds 189 water reservoirs
# in Poland, described by 16 variables (an ID and 15 site characteristics) and
# 7 binary labels recording the presence of amphibian species. The first line
# of the file gives the variable types and the second the variable names, so
# the first line is skipped when reading.
#
# Run from the package root with:
#     Rscript data-raw/amphibians.R

target_variance <- 0.8

raw <- read.csv2("data-raw/amphibians.csv", skip = 1)

x <- raw[, 2:16]
y <- raw[, 17:23]

# Abbreviated species names, so that printed output stays readable.
colnames(y) <- c("GreenFrog", "BrownFrog", "Toad", "FireToad", "TreeFrog",
                 "Newt", "CrestNewt")

categorical <- c("Motorway", "TR", "VR", "SUR1", "SUR2", "SUR3", "UR", "FR",
                 "MR", "CR")

x[categorical] <- lapply(x[categorical], as.factor)

# One-hot encode the categorical variables, keeping every level.
one_hot <- lapply(categorical, function(column) {
    mm <- model.matrix(~ x[[column]] - 1)
    colnames(mm) <- paste0(column, "_", levels(x[[column]]))
    as.data.frame(mm)
})

x <- cbind(x[, !(names(x) %in% categorical)], do.call(cbind, one_hot))

# Reduce the 56 encoded variables to the smallest number of principal
# components that explains at least target_variance of the total variance.
pca <- prcomp(x, center = TRUE, scale. = TRUE)
cumulative_var <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
n_components <- which(cumulative_var >= target_variance)[1]

X <- pca$x[, seq_len(n_components)]
Y <- as.matrix(y)

dimnames(X) <- list(NULL, paste0("PC", seq_len(n_components)))
dimnames(Y) <- list(NULL, colnames(y))
storage.mode(Y) <- "integer"

amphibians <- list(X = X, Y = Y)

cat("Components retained:", n_components, "\n")
cat("Variance explained :", round(cumulative_var[n_components], 4), "\n")
cat("Label prevalence   :", round(colMeans(Y), 3), "\n")

save(amphibians, file = "data/amphibians.rda", compress = "xz")
