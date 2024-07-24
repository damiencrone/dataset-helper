#' Construct Country Mapping
#'
#' This function creates a data frame mapping country codes to names, numeric codes, and frequencies.
#'
#' @param x A character vector of country codes or a haven_labelled numeric vector.
#' @param alpha_code Character. The desired alpha code format for the output. Default is "iso3c".
#' @param warn Logical. Whether to show warnings when country codes cannot be matched. Default is TRUE.
#'
#' @return A data frame with columns:
#'   \item{Country}{Full country name}
#'   \item{Code}{Numeric code (0 to n-1)}
#'   \item{Alpha_Code}{Country code in the specified alpha_code format}
#'   \item{Source_Var}{Original country code or value from the input}
#'   \item{Freq}{Frequency of each country code or value in the input vector}
#'
#' @details
#' The function handles two types of input:
#' 1. A character vector of country codes
#' 2. A haven_labelled numeric vector where the labels represent country names or codes
#'
#' It first infers the country code scheme of the input using `infer_countrycode_scheme`.
#' It then uses the countrycode package to convert between different country code formats.
#' Frequencies of each country code or value in the original input are calculated and included.
#'
#' @examples
#' # Character vector input
#' x <- c("USA", "CAN", "MEX", "USA", "CAN", "FRA", "DEU", "USA")
#' construct_country_mapping(x)
#'
#' # haven_labelled numeric vector input
#' library(haven)
#' y <- labelled(
#'   c(1, 4, 3, 1, 2, 1, 3, 3, 2, 2),
#'   labels = c("Argentina" = 1, "Australia" = 2, "Brazil" = 3, "Canada" = 4)
#' )
#' construct_country_mapping(y)
#'
#' @importFrom countrycode countrycode
#' @importFrom labelled val_labels
#'
#' @export
construct_country_mapping <- function (x, alpha_code = "iso3c", warn = TRUE) {
  ux <- unique(x[!is.na(x)])
  if (is.numeric(x) & "haven_labelled" %in% class(x)) {
    uv <- names(labelled::val_labels(x))
  } else {
    uv <- ux
  }
  scheme <- infer_countrycode_scheme(uv)
  freq_table <- table(x)
  country_mapping <- data.frame(
    Country = countrycode::countrycode(sourcevar = uv, origin = scheme, destination = "country.name", warn = warn),
    Code = 0:(length(uv)-1),
    Alpha_Code = countrycode::countrycode(sourcevar = uv, origin = scheme, destination = alpha_code, warn = warn),
    Source_Var = ux,
    Freq = as.vector(freq_table[ux])
  )
  return(country_mapping)
}
