library(testthat)

# Test data
values <- c("AL", "AT", "AL", "BE", "AT", "AL", "BE", NA)
labels <- c("Albania" = "AL", "Austria" = "AT", "Belgium" = "BE")
x <- haven::labelled(x, labels)


test_that("tabulate_high_frequency_labels works correctly", {
  result <- tabulate_high_frequency_labels(x, threshold = 2, verbose = FALSE)

  expect_type(result, "integer")
  expect_named(result)
  expect_equal(length(result), 3)
  expect_equal(names(result), c("AL", "AT", "BE"))
  expect_equal(as.vector(result), c(3, 2, 2))

  # Test threshold as proportion
  result_prop <- tabulate_high_frequency_labels(x, threshold = 0.2, verbose = FALSE)
  expect_equal(result, result_prop)

  # Test verbose output
  expect_output(tabulate_high_frequency_labels(x, threshold = 2, verbose = TRUE),
                "Number of labels above threshold: 3 of 3")
})

test_that("get_high_frequency_labels works correctly", {
  result <- get_high_frequency_labels(x, threshold = 2)

  expect_type(result, "list")
  expect_named(result, c("kv_pairs", "freq", "json"))

  expect_equal(result$kv_pairs, c(Albania = "AL", Austria = "AT", Belgium = "BE"))
  expect_equal(as.vector(result$freq), c(3, 2, 2))
  expect_type(result$json, "character")
  expect_true(grepl('"Albania":"AL"', result$json))

  # Test error for non-character input
  expect_error(get_high_frequency_labels(1:10, threshold = 2),
               regexp = "^Invalid input")
})

test_that("Input validation works", {
  expect_error(tabulate_high_frequency_labels(x, threshold = "a"),
               "Threshold must be a numeric value.")
  expect_error(tabulate_high_frequency_labels(x, threshold = -1),
               "Threshold must be a non-negative number.")
})
