#' Write Model Result CSV
#' @description
#' This function exports the intermediate data table, regression data, and model parameters into separate CSV files for easy access and further analysis.
#'
#' @param dest_dir A character string specifying the directory where the CSV files will be saved.
#' @param name A character string specifying the base name for the output files.
#' @param data A data.table containing the intermediate data used in the model.
#' @param model_result A list containing the model results, including parameter values and regression data.
#'
#' @details
#' This function generates three CSV files:
#' \enumerate{
#' \item \strong{raw_data.csv:} Contains the original raw data used in the model.
#' \item \strong{regression_data.csv:} Includes the regression data with predicted electron transport rate (ETR) values.
#' \item \strong{model_result.csv:} Summarizes the parameter values derived from the model results (excluding regression data), such as \code{alpha} or \code{beta}.
#' }
#' The `name` parameter serves as a prefix for each file, ensuring clarity and organization in the output directory.
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam/tree/docs#functions}
#'
#' @return No return value, called for side effects
#'
#' @examples
#' path <- file.path(system.file("extdata/dual_pam_data", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' result <- eilers_peeters_generate_regression_ETR_I(data)
#' write_model_result_csv(tempdir(), "20240925", data, result)
#'
#' @export
write_model_result_csv <- function(dest_dir, name, data, model_result) {
  data_dest <- file.path(dest_dir, paste(name, "_raw_data.csv", sep = ""))
  regression_data_dest <- file.path(dest_dir, paste(name, "_regression_data.csv", sep = ""))
  model_result_dest <- file.path(dest_dir, paste(name, "_model_result.csv", sep = ""))

  utils::write.csv(
    data,
    file = data_dest,
    quote = TRUE,
    row.names = FALSE
  )

  utils::write.csv(
    get_etr_regression_data_from_model_result(model_result),
    file = regression_data_dest,
    quote = TRUE,
    row.names = FALSE
  )

  df <- data.frame()
  df[1, ] <- NA

  for (n in names(model_result)) {
    if (n == "etr_regression_data" || n == "etr_type") {
      next()
    }

    entry <- data.frame(
      stats::setNames(
        list(
          c(model_result[[n]])
        ),
        c(n)
      )
    )

    df <- cbind(df, NewCol = entry)
  }

  utils::write.csv(
    df,
    file = model_result_dest,
    quote = TRUE,
    row.names = FALSE
  )
}
