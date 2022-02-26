test_that("generics work", {
  # create test data
  vals <- 1:27
  my_landscape <- FitLandDF(array(vals, dim = rep(3, 3)))

  # test mean
  expect_equal(mean(my_landscape), mean(vals))

  # test median
  expect_equal(median(my_landscape), median(vals))

  # ?test print
  expect_output(print(my_landscape), regexp = "A fitness landscape")
})

test_that("non-generic methods work", {
  # create test data
  vals <- 1:27
  vals_array <- array(vals, dim = rep(3, 3))
  my_landscape <- FitLandDF(vals_array)
  my_df <- extract_df(my_landscape)

  # test is.FitLandDF
  expect_true(is.FitLandDF(my_landscape))
  expect_false(is.FitLandDF(vals))

  # test is_FitLandDF
  expect_true(is_FitLandDF(my_landscape))
  expect_false(is_FitLandDF(vals))

  # test dims
  expect_equal(dims(my_landscape), dim(vals_array))

  # test sdev
  expect_equal(sdev(my_landscape), sd(vals))

  # test variance
  expect_equal(variance(my_landscape), var(vals))

  # test min_fit
  expect_equal(min_fit(my_landscape), min(vals))

  # test max_fit
  expect_equal(max_fit(my_landscape), max(vals))

  # test extract_df
  expect_equal(ncol(my_df), length(dims(my_landscape)) + 1)
  expect_equal(nrow(my_df), length(vals))
  expect_equal(my_df$Value, vals)
})

test_that("constructor works", {
  # create test data
  vals <- 1:27
  vals_array <- array(vals, dim = rep(3, 3))
  my_landscape <- FitLandDF(vals_array)

  # test constructor with data frame (cannot accept array format)
  vals_df <- extract_df(my_landscape)
  my_landscape2 <- new_FitLandDF(vals_df, dims = dims(my_landscape))
  expect_true(is_FitLandDF(my_landscape2))
})

test_that("validator works", {
  # create test data
  vals <- 1:27
  vals_array <- array(vals, dim = rep(3, 3))
  my_landscape <- FitLandDF(vals_array)

  # confirm validator works as expected
  expect_identical(my_landscape, validate_FitLandDF(my_landscape))
  expect_error(validate_FitLandDF(vals))
  expect_error(validate_FitLandDF(vals_array))
})
