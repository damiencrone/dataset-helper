#' Construct Country Mapping
#'
#' This function creates a data frame mapping country codes to names, numeric codes, and frequencies.
#'
#' @param x A character vector of country codes.
#' @param alpha_code Character. The desired alpha code format for the output. Default is "iso3c".
#' @param warn Logical. Whether to show warnings when country codes cannot be matched. Default is TRUE.
#'
#' @return A data frame with columns:
#'   \item{Country}{Full country name}
#'   \item{Code}{Numeric code (0 to n-1)}
#'   \item{Alpha_Code}{Country code in the specified alpha_code format}
#'   \item{Source_Var}{Original country code from the input}
#'   \item{Freq}{Frequency of each country code in the input vector}
#'
#' @details
#' The function first infers the country code scheme of the input using `infer_countrycode_scheme`.
#' It then uses the countrycode package to convert between different country code formats.
#' Frequencies of each country code in the original input are calculated and included.
#'
#' @examples
#' x <- c("USA", "CAN", "MEX", "USA", "CAN", "FRA", "DEU", "USA")
#' construct_country_mapping(x)
#'
#' @importFrom countrycode countrycode
#'
#' @export
construct_country_mapping <- function (x, alpha_code = "iso3c", warn = TRUE) {
  uv <- unique(x[!is.na(x)])
  scheme <- infer_countrycode_scheme(uv)
  freq_table <- table(x)
  country_mapping <- data.frame(
    Country = countrycode::countrycode(sourcevar = uv, origin = scheme, destination = "country.name", warn = warn),
    Code = 0:(length(uv)-1),
    Alpha_Code = countrycode::countrycode(sourcevar = uv, origin = scheme, destination = alpha_code, warn = warn),
    Source_Var = uv,
    Freq = as.vector(freq_table[uv])
  )
  return(country_mapping)
}
