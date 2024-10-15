#' @title Control Plot for Vollenweider Model (1965)
#' @description This function creates a plot for the Vollenweider Model based on the provided data and model results.
#'
#' @param data A `data.table` containing the original ETR and yield data for the plot.
#' @param model_result A list containing the fitting results of the Vollenweider Model and the calculated paramters (alpha, ik...).
#' @param etr_type A character string describing the ETR type (ETR I or ETR II).
#' @param title A character string that specifies the title of the plot.
#'
#' @return A plot displaying the original ETR and Yield values and the regression data. A table below the plot shows the calculated data (alpha, ik...)
#'
#' @references
#' Vollenweider, R. A. (1965). Calculation models of photosynthesis-depth curves
#' and some implications regarding day rate estimates in primary production measurements,
#' p. 427-457. In C. R. Goldman [ed.], *Primary Productivity in Aquatic Environments*.
#' Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.
#'
#' @export
vollenweider_control_plot <- function(data, model_result, title) {
  names <- c()
  values <- c()

  for (name in names(model_result)) {
    if (name == "etr_regression_data") {
      next()
    }

    if (name == "etr_type") {
      next()
    }

    names <- c(names, name)
    value <- model_result[[name]]

    if (name == "sdiff") {
      value <- as.character(round(value, 3))
    } else if (name == "pmax") {
      value <- as.character(round(value, 7))
    } else if (name == "a") {
      value <- as.character(round(value, 6))
    } else if (name == "alpha") {
      value <- as.character(round(value, 6))
    } else {
      value <- as.character(round(value, 3))
    }

    values <- c(values, value)
  }

  params <- data.frame(
    Parameter = names,
    Value = values
  )

  return(
    plot_control(
      data,
      model_result,
      title,
      color_vollenweider,
      params
    )
  )
}
