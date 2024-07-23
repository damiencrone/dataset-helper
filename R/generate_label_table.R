#' Generate Label Table
#'
#' This function generates a label table from a given dataset.
#'
#' @param dat A data frame, typically loaded from an SPSS file using haven::read_sav()
#' @param to_csv Logical; if TRUE, saves the label table as a CSV file (default is TRUE)
#' @param file_name Name of the output CSV file (default is "label_table.csv")
#' @param dataset_dir Optional; the directory to save the CSV file (default is NULL)
#'
#' @return A data frame containing the label table
#'
#' @importFrom labelled var_label val_labels
#' @importFrom rjson toJSON
#' @export
#'
#' @examples
#' \dontrun{
#' data <- haven::read_sav("your_data.sav")
#' label_table <- generate_label_table(data)
#' }
generate_label_table <- function(dat,
                                 to_csv = TRUE,
                                 file_name = "label_table.csv",
                                 dataset_dir = NULL) {
  var_labels <- labelled::var_label(dat)
  var_labels <- lapply(var_labels, function(x) if (is.null(x)) NA else x)
  value_labels <- sapply(dat, labelled::val_labels)

  label_table <- data.frame(
    item_name = names(var_labels),
    item_label = unlist(var_labels, use.names = FALSE),
    min_label = NA,
    max_label = NA,
    min_value = NA,
    max_value = NA,
    n_values = NA,
    value_labels = NA
  )

  for (i in seq_along(var_labels)) {
    item_name <- label_table$item_name[i]
    if (item_name %in% names(var_labels)) {
      if (!is.null(var_labels[[item_name]])) {
        label_table$item_label[i] <- var_labels[[item_name]]
      }
      if ("haven_labelled" %in% class(dat[[item_name]])) {
        lvl <- labelled::val_labels(dat[[item_name]])
        if (length(lvl) > 0) {
          n = length(lvl)
          label_table$min_label[i] <- names(lvl)[1]
          label_table$max_label[i] <- names(lvl)[n]
          label_table$min_value[i] <- lvl[1]
          label_table$max_value[i] <- lvl[n]
          label_table$n_values[i] <- length(lvl)
          label_table$value_labels[i] <- rjson::toJSON(as.list(lvl))
        }
      }
    }
  }

  if (to_csv) {
    if (!is.null(dataset_dir)) {
      if (!dir.exists(dataset_dir)) {
        dir.create(dataset_dir, recursive = TRUE)
      }
      file_path <- file.path(dataset_dir, file_name)
    } else {
      file_path <- file_name
    }
    write.csv(label_table, file = file_path, row.names = FALSE)
    message(sprintf("Label table has been saved as %s", file_path))
  }

  return(label_table)
}
