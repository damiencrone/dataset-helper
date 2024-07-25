#' Get Region Information for a Specific Country
#'
#' This function extracts region information for a specified country from a dataset.
#' It returns a data frame with region codes, names, and counts.
#'
#' @param x A data frame containing the country and region information.
#' @param country_col The name of the column in 'x' that contains country codes.
#' @param region_col The name of the column in 'x' that contains region codes.
#' @param country_code The specific country code to analyze.
#'
#' @return A data frame with columns:
#'   \item{region_code}{The unique region codes for the specified country}
#'   \item{region_name}{The names of the regions (if available)}
#'   \item{freq}{The number of occurrences of each region in the dataset}
#'
#' @details
#' The function expects the region column to be a numeric haven_labelled variable.
#' It will throw an error if the column is of a different class.
#'
#' @importFrom labelled val_labels
#'
#' @export
get_country_regions <- function(x, country_col, region_col, country_code) {
  if (!all(c(country_col, region_col) %in% names(x))) {
    stop("Specified columns not found in the data frame")
  }
  country_ind = x[[country_col]] == country_code
  country_data <- x[country_ind, c(country_col, region_col)]
  country_regions <- unique(country_data[[region_col]])
  is_labelled_numeric = is.numeric(x[[region_col]]) & "haven_labelled" %in% class(x[[region_col]])
  if (is_labelled_numeric) {
    region_labels <- labelled::val_labels(x[[region_col]])
  } else {
    stop("Unexpected class for region_col; expecting numeric haven_labelled variable")
  }

  result <- data.frame(
    region_code = country_regions,
    region_name = names(region_labels)[match(country_regions, region_labels)],
    freq = sapply(country_regions, function(r) sum(country_data[[region_col]] == r, na.rm = TRUE))
  )
  result <- result[order(result$region_code), ]

  missing_count <- sum(is.na(country_data[[region_col]]))
  missing_row <- data.frame(
    region_code = NA,
    region_name = "Missing",
    freq = missing_count
  )
  result <- rbind(result, missing_row)
  rownames(result) <- NULL

  return(result)
}
