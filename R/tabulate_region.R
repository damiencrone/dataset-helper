#' Tabulate regions for countries and optionally return results
#'
#' @param x A data frame containing country and region data
#' @param config A list containing configuration parameters:
#'   \itemize{
#'     \item country_var: Name of the column in `x` containing country information
#'     \item region_vars: A list of region variables, each with `var_name` and `format` fields
#'   }
#' @param return_result Logical, if TRUE, returns the results as a nested list; default is FALSE
#'
#' @return If return_result is TRUE, returns a nested list with country codes as top-level names,
#'         each containing a list of region data frames named by their format.
#'         If return_result is FALSE, returns nothing but writes CSV files to the "regions/" directory.
#'
#' @details This function processes country and region data, creating a mapping between countries
#'          and their regions. It writes the results to CSV files in a "regions/" directory,
#'          with filenames formatted as "{country_code}_{region_format}.csv".
#'
#' @importFrom utils write.csv
#'
#' @export
tabulate_region <- function (x, config, return_result = FALSE) {
  country_var = config$country_var
  country_mapping <- construct_country_mapping(x[[country_var]])
  dir.create("regions/", showWarnings = FALSE)
  if (return_result) {result = list()}
  for (i in 1:nrow(country_mapping)) {
    cc = country_mapping$Alpha_Code[i]
    if (return_result) {result[[cc]] = list()}
    for (region_var in config$region_vars) {
      cr = get_country_regions(x = x,
                               country_col = country_var,
                               region_col = region_var$var_name,
                               country_code = country_mapping$Source_Var[i])
      write.csv(x = cr,
                file = paste0("regions/", cc, "_", region_var$format, ".csv"),
                row.names = FALSE)
      if (return_result) {result[[cc]][[region_var$format]] = cr}
    }
  }
  if (return_result) {return(result)}
}
