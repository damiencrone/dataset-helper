#' Generate and Save Sample Dataset
#'
#' This function creates a small sample dataset and saves it as a .sav file
#' in the specified directory.
#'
#' @param n Number of observations to generate (default is 50)
#' @param file_name Name of the file to save (default is "sample_dataset.sav")
#' @param dir Directory to save the file (default is "data")
#'
#' @return Invisible NULL. The function is called for its side effect of creating a file.
#'
#' @importFrom haven write_sav
#' @export
#'
#' @examples
#' \dontrun{
#' generate_sample_dataset()
#' generate_sample_dataset(n = 100, file_name = "larger_sample.sav")
#' }
generate_sample_dataset <- function(n = 50, file_name = "sample_dataset.sav", dir = "data") {
  set.seed(123)  # for reproducibility

  sample_data <- data.frame(
    id = 1:n,
    age = sample(18:80, n, replace = TRUE),
    gender = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    score = round(rnorm(n, mean = 70, sd = 15), 1)
  )

  # Add some labels
  attr(sample_data$gender, "labels") <- c(Male = 1, Female = 2)
  attr(sample_data$age, "label") <- "Age of respondent"
  attr(sample_data$gender, "label") <- "Gender of respondent"
  attr(sample_data$score, "label") <- "Test score"

  full_dir <- normalizePath(dir, mustWork = FALSE)

  # Ensure the directory exists
  if (!dir.exists(full_dir)) {
    dir.create(full_dir, recursive = TRUE)
  }

  # Save the dataset as a .sav file
  full_path <- file.path(dir, file_name)
  haven::write_sav(sample_data, full_path)

  message(sprintf("Sample dataset has been created and saved in %s", full_path))

  return(sample_data)
}
