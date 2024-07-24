#' Tabulate High Frequency Labels
#'
#' This function creates a frequency table of labels in a vector, filtered by a specified threshold.
#'
#' @param x A vector of labels to be tabulated.
#' @param threshold Numeric. If >= 1, it's treated as an absolute count. If < 1, it's treated as a proportion of non-NA values in x.
#' @param verbose Logical. If TRUE (default), prints summary statistics to the console.
#'
#' @return A named integer vector of label frequencies, sorted in descending order, including only labels that meet or exceed the threshold.
#'
#' @details
#' The function first determines the threshold value. If the input threshold is less than 1, it's interpreted as a proportion
#' of non-NA values in x. It then creates a frequency table, filters it based on the threshold, and sorts it in descending order.
#' If verbose is TRUE, it prints summary statistics including the number of labels above the threshold and the number of
#' participants (non-NA values) represented by these labels.
#'
#' @examples
#' x <- c("A", "B", "A", "C", "B", "D", "A", "B", NA)
#' tabulate_high_frequency_labels(x, threshold = 2)
#' tabulate_high_frequency_labels(x, threshold = 0.3, verbose = FALSE)
#'
#' @export
tabulate_high_frequency_labels = function (x, threshold, verbose = TRUE) {
  if (!is.numeric(threshold)) {stop("Threshold must be a numeric value.")}
  if (threshold < 0) {stop("Threshold must be a non-negative number.")}
  if (threshold < 1) {
    threshold <- threshold * sum(!is.na(x))
  }
  freq <- table(as.character(x))
  freq_subset <- freq[freq >= threshold]
  freq_subset <- freq_subset[order(-freq_subset)]
  if (verbose) {
    s = paste0(
      sprintf("Number of labels above threshold: %d of %d", length(freq_subset), length(freq)), "\n",
      sprintf("Number of observations with labels above threshold: %d of %d (%d non-missing)", sum(freq_subset), sum(freq), sum(!is.na(x)))
    )
    cat(s)
  }
  return(freq_subset)
}
