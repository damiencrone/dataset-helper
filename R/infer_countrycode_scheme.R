#' Infer Country Code Scheme
#'
#' This function attempts to infer the country code scheme used in a vector of country codes.
#'
#' @param x A character vector of country codes.
#'
#' @return A character string representing the inferred country code scheme.
#'
#' @details
#' The function checks the input against known country code schemes in the countrycode package.
#' It first looks for a perfect match where all input codes are found in a single scheme.
#' If no perfect match is found, it returns the scheme with the most matches.
#' If multiple schemes have the same number of matches, it returns the first one.
#'
#' @examples
#' # ISO2 country codes
#' x <- c("US", "CA", "MX", "FR", "DE")
#' infer_countrycode_scheme(x)
#'
#' # ISO3 country codes
#' y <- c("USA", "CAN", "MEX", "FRA", "DEU")
#' infer_countrycode_scheme(y)
#'
#' @importFrom countrycode codelist
#'
#' @export
infer_countrycode_scheme <- function (x) {
  # Verify that x does not contain numbers
  if (any(grepl("[0-9]", x))) {
    stop("Unexpected input: Input contains numbers.")
  }
  uv <- unique(x[!is.na(x)])
  match_mat <- apply(X = countrycode::codelist, uv = uv, MARGIN = 2, FUN = function (x, uv) uv %in% x)
  perfect <- colnames(match_mat)[apply(match_mat, 2, all)]
  if (length(perfect) >= 1) {
    message("Perfect match found for ", length(perfect), " code scheme(s): ", print_codes(perfect), "\nReturning ", perfect[1], " as the country code scheme.")
    return(perfect[1])
  }
  match_sums <- colSums(match_mat)
  max_match <- max(match_sums)
  best <- colnames(match_mat)[match_sums == max_match]
  if (max_match > 0) {
    if (length(best) > 1) {
      message("Multiple (", length(best), ") best matches found for code schemes: ", print_codes(best), "\nReturning ", best[1], " (matching ", max_match, " codes) as the country code scheme.")
      return(best[1])
    } else {
      message("Best match found for codes: ", print_codes(best), "\nReturning ", best, " (matching ", max_match, " codes) as the country code scheme.")
      return(best)
    }
  } else {
    stop("No plausible match found for country code scheme.")
  }
}

print_codes <- function(x, n = 5) {
  if (length(x) > n) {
    formatted <- paste(x[1:n], collapse = ", ")
    return(paste(formatted, "..."))
  } else {
    return(paste(x, collapse = ", "))
  }
}
