#' @title Control Plot for Walsby Model (1997)
#' @description This function creates a plot for the Walsby Model based on the provided data and model results.
#'
#' @param data A `data.table` containing the original ETR and yield data for the plot.
#' @param model_result A list containing the fitting results of the Walsby Model and the calculated paramters (alpha, beta...).
#' @param etr_type A character string describing the ETR type (ETR I or ETR II).
#' @param title A character string that specifies the title of the plot.
#'
#' @return A plot displaying the original ETR and Yield values and the regression data. A table below the plot shows the calculated data (alpha, beta...)
#'

#' @references
#' Walsby, A. E. (1997). Numerical integration of phytoplankton photosynthesis
#' through time and depth in a water column. *Journal of Plankton Research*,
#' 19(3), 487-502. https://doi.org/10.1093/plankt/19.3.487
#'
#' Romoth, K., Nowak, P., Kempke, D., Dietrich, A., Porsche, C., & Schubert, H. (2019).
#' Acclimation limits of *Fucus evanescens* along the salinity gradient of the
#' southwestern Baltic Sea. *Botanica Marina*, 62(1), 1-12. https://doi.org/10.1515/bot-2018-0098
walsby_control_plot <- function(data, model_result, title) {
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
    } else if (name == "etr_max") {
      value <- as.character(round(value, 7))
    } else if (name == "alpha") {
      value <- as.character(round(value, 6))
    } else if (name == "beta") {
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
      color_walsby,
      params
    )
  )
}
