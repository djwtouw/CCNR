test_that("amphibians is a list named X and Y", {
  expect_type(amphibians, "list")
  expect_named(amphibians, c("X", "Y"))
})

test_that("amphibians dimensions are 189 x 25 and 189 x 7", {
  expect_equal(dim(amphibians$X), c(189L, 25L))
  expect_equal(dim(amphibians$Y), c(189L, 7L))
})

test_that("amphibians X is numeric with named components", {
  expect_true(is.numeric(amphibians$X))
  expect_equal(colnames(amphibians$X), paste0("PC", 1:25))
  expect_false(anyNA(amphibians$X))
})

test_that("amphibians Y is binary with named species", {
  expect_true(all(amphibians$Y %in% c(0L, 1L)))
  expect_equal(
    colnames(amphibians$Y),
    c("GreenFrog", "BrownFrog", "Toad", "FireToad", "TreeFrog", "Newt",
      "CrestNewt")
  )
  expect_false(anyNA(amphibians$Y))
})

test_that("amphibians labels vary and include a rare one", {
  prevalence <- colMeans(amphibians$Y)
  expect_true(all(prevalence > 0 & prevalence < 1))
  expect_equal(unname(round(prevalence[["CrestNewt"]], 3)), 0.111)
  expect_equal(unname(round(prevalence[["BrownFrog"]], 3)), 0.783)
})

test_that("amphibians can be fit with ccn", {
  model <- ccn(amphibians$X, amphibians$Y, q = 1.5, lambda = 0.1)
  expect_s3_class(model, "ccn")
  expect_equal(dim(fitted(model)), dim(amphibians$Y))
})
