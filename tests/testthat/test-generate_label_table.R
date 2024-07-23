library(testthat)

# Set up the correct data directory path
data_dir <- normalizePath(file.path(testthat::test_path(), "..", "..", "data"), mustWork = FALSE)

# Generate a sample dataset before running tests
sample_data <- generate_sample_dataset(n = 10, file_name = "test_sample.sav", dir = data_dir)

test_that("generate_label_table creates correct structure", {
  label_table <- generate_label_table(sample_data, to_csv = FALSE)
  expect_s3_class(label_table, "data.frame")
  expect_equal(ncol(label_table), 8)
  expect_equal(nrow(label_table), ncol(sample_data))
})

test_that("generate_label_table creates correct columns", {
  label_table <- generate_label_table(sample_data, to_csv = FALSE)
  expected_cols <- c("item_name", "item_label", "min_label", "max_label", "min_value", "max_value", "n_values", "value_labels")
  expect_equal(colnames(label_table), expected_cols)
})

test_that("generate_label_table creates CSV file when requested", {
  generate_label_table(sample_data, to_csv = TRUE, dataset_dir = data_dir)
  expect_true(file.exists(file.path(data_dir, "label_table.csv")))
  file.remove(file.path(data_dir, "label_table.csv"))
})

# Clean up: remove the test SPSS file
file.remove(file.path(data_dir, "test_sample.sav"))
