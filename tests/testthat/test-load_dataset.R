library(testthat)

# Set up the correct data directory path
data_dir <- normalizePath(file.path(testthat::test_path(), "..", "..", "data"), mustWork = FALSE)

# Generate a sample dataset before running tests
generate_sample_dataset(n = 30, file_name = "test_sample.sav", dir = data_dir)

test_that("load_dataset loads SPSS file correctly", {
  data <- load_dataset("test_sample", to_csv = FALSE, dataset_dir = data_dir)
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 30)
  expect_true(all(c("id", "age", "gender", "score") %in% names(data)))
  expect_s3_class(data$gender, "haven_labelled")
  expect_equal(attr(data$age, "label"), "Age of respondent")
})

test_that("load_dataset creates CSV file when requested", {
  csv_path <- file.path(data_dir, "test_sample.csv")
  if (file.exists(csv_path)) {
    file.remove(csv_path)
  }
  load_dataset("test_sample", to_csv = TRUE, dataset_dir = data_dir)
  expect_true(file.exists(csv_path))
  file.remove(csv_path)
})

test_that("load_dataset handles non-existent files correctly", {
  expect_error(
    load_dataset("non_existent_file", dataset_dir = data_dir),
    paste0("'", file.path(data_dir, "non_existent_file.sav"), "' does not exist"),
    fixed = TRUE
  )
})

# Clean up: remove the test SPSS file
file.remove(file.path(data_dir, "test_sample.sav"))
