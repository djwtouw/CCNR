test_that("generate_dataset returns a list named X and Y", {
  dat <- generate_dataset(30)
  expect_type(dat, "list")
  expect_named(dat, c("X", "Y"))
})

test_that("generate_dataset dimensions are n x 3 and n x 4", {
  dat <- generate_dataset(50)
  expect_equal(dim(dat$X), c(50L, 3L))
  expect_equal(dim(dat$Y), c(50L, 4L))
})

test_that("generate_dataset X is numeric and Y is binary", {
  dat <- generate_dataset(40)
  expect_true(is.numeric(dat$X))
  expect_true(is.numeric(dat$Y))
  expect_true(all(dat$Y %in% c(0, 1)))
})

test_that("generate_dataset rejects invalid n", {
  expect_error(generate_dataset(0))
  expect_error(generate_dataset(-10))
  expect_error(generate_dataset("a"))
  expect_error(generate_dataset(c(10, 20)))
})
