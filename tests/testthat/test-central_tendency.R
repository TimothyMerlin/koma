# Create a sample named list for testing
quantiles <- list(
  q_mean = 5.5,
  q_50 = 6.0
)

# Test for valid 'mean' case
test_that("get_central_tendency returns mean value", {
  result <- get_central_tendency("mean", quantiles)
  expect_equal(result, 5.5)
})

# Test for valid 'median' case
test_that("get_central_tendency returns median value", {
  result <- get_central_tendency("median", quantiles)
  expect_equal(result, 6.0)
})

# Test for invalid central tendency argument
test_that("get_central_tendency throws an error for invalid argument", {
  expect_error(
    get_central_tendency("mode", quantiles),
    'Central tendency must be either "mean" or "median".'
  )
})
