library(testthat)
library(haven)

test_that("construct_country_mapping works with character vector input", {
  x <- c("USA", "CAN", "MEX", "USA", "CAN", "FRA", "DEU", "USA")
  result <- construct_country_mapping(x)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 5)
  expect_equal(nrow(result), 5)  # 5 unique countries
  expect_equal(colnames(result), c("Country", "Code", "Alpha_Code", "Source_Var", "Freq"))

  expect_true(all(c("United States", "Canada", "Mexico", "France", "Germany") %in% result$Country))
  expect_equal(result$Code, 0:4)
  expect_true(all(c("USA", "CAN", "MEX", "FRA", "DEU") %in% result$Alpha_Code))

  expect_equal(result$Freq[result$Source_Var == "USA"], 3)
  expect_equal(result$Freq[result$Source_Var == "CAN"], 2)
  expect_equal(sum(result$Freq), length(x))
})

test_that("construct_country_mapping works with haven_labelled input", {
  y <- labelled(
    c(1, 4, 3, 1, 2, 1, 3, 3, 2, 2),
    labels = c("Argentina" = 1, "Australia" = 2, "Brazil" = 3, "Canada" = 4)
  )
  result <- construct_country_mapping(y)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 5)
  expect_equal(nrow(result), 4)  # 4 unique countries
  expect_equal(colnames(result), c("Country", "Code", "Alpha_Code", "Source_Var", "Freq"))

  expect_true(all(c("Argentina", "Australia", "Brazil", "Canada") %in% result$Country))
  expect_equal(result$Code, 0:3)
  expect_true(all(c("ARG", "AUS", "BRA", "CAN") %in% result$Alpha_Code))

  expect_equal(result$Freq[result$Source_Var == 1], 3)
  expect_equal(result$Freq[result$Source_Var == 2], 3)
  expect_equal(sum(result$Freq), length(y))
})

test_that("construct_country_mapping handles warnings correctly", {
  x <- c("USA", "CAN", "XYZ")  # XYZ is not a valid country code

  expect_warning(construct_country_mapping(x), "Some values were not matched unambiguously")
})

test_that("construct_country_mapping handles different alpha_code parameter", {
  x <- c("USA", "CAN", "MEX")
  result_iso2 <- construct_country_mapping(x, alpha_code = "iso2c")

  expect_true(all(c("US", "CA", "MX") %in% result_iso2$Alpha_Code))
})

test_that("construct_country_mapping handles haven_labelled data with negative values", {
  library(haven)
  library(labelled)

  # Create a sample haven_labelled vector with negative values
  x <- labelled(
    c(32, 76, -1, 156, -2, 392, -3, 484, -4, 643, 643, -5),
    labels = c(
      "Don't know" = -1,
      "No answer" = -2,
      "Not applicable" = -3,
      "Not asked in survey" = -4,
      "Missing; Unknown" = -5,
      "Argentina" = 32,
      "Brazil" = 76,
      "China" = 156,
      "Japan" = 392,
      "Mexico" = 484,
      "Russia" = 643
    )
  )

  result <- construct_country_mapping(x)

  # Check structure of the result
  expect_s3_class(result, "data.frame")

  # Check that negative values are not in the result
  expect_false(any(result$Source_Var < 0, na.rm = TRUE))

  # Check that we have the correct number of countries (6)
  expect_equal(nrow(result), 6)

  # Check that specific countries are present
  expect_true(all(c("Argentina", "Brazil", "China", "Japan", "Mexico", "Russia") %in% result$Country))

  # Check that Alpha_Codes are correct
  expect_true(all(c("ARG", "BRA", "CHN", "JPN", "MEX", "RUS") %in% result$Alpha_Code))

  # Check that Codes are assigned correctly (0 to 5 for countries)
  expect_equal(sort(result$Code[!is.na(result$Code)]), 0:5)

  # Check that frequencies are correct
  expect_equal(result$Freq[result$Source_Var == 32], 1)
  expect_equal(result$Freq[result$Source_Var == 643], 2)
})
