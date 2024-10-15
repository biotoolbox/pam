#' @title Control Plot for Eilers-Peeters Model (1988)
#' @description This function creates a plot for the Eilers-Peeters Model based on the provided data and model results.
#'
#' @param data A `data.table` containing the original ETR and yield data for the plot.
#' @param model_result A list containing the fitting results of the Eilers-Peeters Model and the calculated paramters (alpha, ik...).
#' @param etr_type A character string describing the ETR type (ETR I or ETR II).
#' @param title A character string that specifies the title of the plot.
#'
#' @return A plot displaying the original ETR and Yield values and the regression data. A table below the plot shows the calculated data (alpha, ik...)
#'
#' @references
#' Eilers, P. H. C., & Peeters, J. C. H. (1988). A model for the relationship
#' between light intensity and the rate of photosynthesis in phytoplankton.
#' Ecological Modelling, 42(3-4), 199-215. \doi{10.1016/0304-3800(88)90057-9}.
#'
#' @export
eilers_peeters_control_plot <- function(data, model_result, title) {
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
    } else if (name == "a") {
      value <- as.character(round(value, 10))
    } else if (name == "b") {
      value <- as.character(round(value, 6))
    } else if (name == "c") {
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
      color_eilers_peeters,
      params
    )
  )
}
