#' Get High Frequency Labels with Value Pairs
#'
#' This function identifies high-frequency labels in a vector and returns associated value-label pairs.
#'
#' @param x A labelled vector (e.g., from haven::read_sav) to be analyzed.
#' @param threshold Numeric. If >= 1, it's treated as an absolute count. If < 1, it's treated as a proportion of non-NA values in x.
#'
#' @return A list containing:
#'   \item{kv_pairs}{A named vector of value-label pairs for high-frequency labels}
#'   \item{freq}{A named vector of frequencies for high-frequency labels}
#'   \item{json}{A JSON string representation of the value-label pairs}
#'
#' @details
#' The function first calls `tabulate_high_frequency_labels` to identify labels meeting the threshold.
#' It then extracts the value-label pairs for these high-frequency labels from the input vector.
#' The result includes both the value-label pairs and their frequencies, as well as a JSON representation of the pairs.
#'
#' @examples
#' library(haven)
#' values <- c("AL", "AT", "AL", "BE", "AT", "AL", "BE", NA)
#' labels <- c("Albania" = "AL", "Austria" = "AT", "Belgium" = "BE")
#' x <- haven::labelled(values, labels)
#' get_high_frequency_labels(x, threshold = 2)
#'
#' @seealso \code{\link{tabulate_high_frequency_labels}}
#'
#' @importFrom labelled val_labels
#' @importFrom rjson toJSON
#'
#' @export
get_high_frequency_labels = function (x, threshold) {
  if (!"haven_labelled" %in% class(x)) { stop("Invalid input: x must be a vector of class 'haven_labelled'")}
  freq <- tabulate_high_frequency_labels(x, threshold, verbose = FALSE)
  kv <- labelled::val_labels(x)
  kv <- kv[kv %in% names(freq)]
  result = list(kv_pairs = kv, freq = freq, json = rjson::toJSON(kv))
  return(result)
}
