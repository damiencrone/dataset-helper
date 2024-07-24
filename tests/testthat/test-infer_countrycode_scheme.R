library(testthat)

test_that("infer_countrycode_scheme works correctly", {
  # Test ISO2 codes
  expect_equal(infer_countrycode_scheme(c("US", "CA", "MX")), "ecb")

  # Test ISO3 codes
  expect_equal(infer_countrycode_scheme(c("USA", "CAN", "MEX")), "cowc")

  # Test mixed case
  expect_equal(infer_countrycode_scheme(c("us", "CA", "mx")), "ecb")

  # Test with some NAs
  expect_equal(infer_countrycode_scheme(c("US", NA, "MX")), "ecb")

  # Test error for numeric input
  expect_error(infer_countrycode_scheme(c("US", "CA", "123")),
               "Unexpected input: Input contains numbers.")

  # Test error for no plausible match
  expect_error(infer_countrycode_scheme(c("XXXX", "YYYY", "ZZZZ")),
               "No plausible match found for country code scheme.")
})
