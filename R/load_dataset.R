#' Load a dataset from an SPSS file
#'
#' This function loads a dataset from an SPSS file and optionally saves it as a CSV.
#'
#' @param dataset_name The name of the dataset (without file extension)
#' @param to_csv Logical; if TRUE, saves the dataset as a CSV file
#' @param dataset_dir Optional; the directory containing the dataset
#'
#' @return A data frame containing the loaded dataset
#' @export
#'
#' @importFrom haven read_sav
load_dataset <- function(dataset_name,
                        to_csv = TRUE,
                        dataset_dir = NULL) {
  if (!is.null(dataset_dir)) {
    dataset_name <- paste0(dataset_dir, "/", dataset_name)
  }
  dat <- haven::read_sav(paste0(dataset_name, ".sav"))
  if (to_csv) {
    csv_exists <- file.exists(paste0(dataset_name, ".csv"))
    if (!csv_exists) {
      write.csv(dat, file = "processed.csv", row.names = FALSE)
    }
  }
  return(dat)
}
