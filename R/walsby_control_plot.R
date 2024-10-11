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

walsby_control_plot_bulk_pdf_ETR_I <- function(
    csv_src_dir,
    pdf_dest_path,
    etr_max_start_value = walsby_default_start_value_etr_max,
    alpha_start_value = walsby_default_start_value_alpha,
    beta_start_value = walsby_default_start_value_beta) {
  return(walsby_control_plot_bulk_pdf_internal(
    csv_src_dir,
    pdf_dest_path,
    etr_I_type,
    etr_max_start_value,
    alpha_start_value,
    beta_start_value
  ))
}

walsby_control_plot_bulk_pdf_ETR_II <- function(
    csv_src_dir,
    pdf_dest_path,
    etr_max_start_value = walsby_default_start_value_etr_max,
    alpha_start_value = walsby_default_start_value_alpha,
    beta_start_value = walsby_default_start_value_beta) {
  return(walsby_control_plot_bulk_pdf_internal(
    csv_src_dir,
    pdf_dest_path,
    etr_II_type,
    etr_max_start_value,
    alpha_start_value,
    beta_start_value
  ))
}

#' Generate PDF Reports for Walsby Model (1997)
#'
#' This function generates PDF reports containing plots of the Walsby
#' regression model for ETR based on multiple csv files. Each plot displays
#' the regression results for the datasets processed.
#'
#' @param csv_src_dir Directory containing input csv files (with `.csv`
#' extension) for analysis.
#'
#' @param pdf_dest_path Path to the destination PDF file where plots will
#' be saved.
#'
#' @param etr_type Type of ETR to analyze (e.g., "ETR I" or "ETR II").
#'
#' @param etr_max_start_value Starting value for the parameter 'etr_max'
#' in the Walsby model (default: \code{etr_max_start_value_walsby_default}).
#'
#' @param alpha_start_value Starting value for parameter 'alpha' in the
#' Walsby model (default: \code{alpha_start_value_walsby_default}).
#'
#' @param beta_start_value Starting value for parameter 'beta' in the
#' Walsby model (default: \code{beta_start_value_walsby_default}).
#'
#' @return A PDF file with plots for each regression model applied to the
#' datasets.
#'
#' @details
#' The function scans for csv files, reads data using \code{read_pam_data},
#' generates the regression model via
#' \code{generate_regression_walsby_internal}, and plots results with
#' \code{plot_control_walsby}. Any errors encountered during file processing
#' are reported without halting execution.
#'
#' @references
#' Walsby, A. E. (1997). Numerical integration of phytoplankton photosynthesis
#' through time and depth in a water column. *Journal of Plankton Research*,
#' 19(3), 487-502. https://doi.org/10.1093/plankt/19.3.487
#'
#' Romoth, K., Nowak, P., Kempke, D., Dietrich, A., Porsche, C., & Schubert, H. (2019).
#' Acclimation limits of *Fucus evanescens* along the salinity gradient of the
#' southwestern Baltic Sea. *Botanica Marina*, 62(1), 1-12. https://doi.org/10.1515/bot-2018-0098
#'
#' @importFrom data.table fread
#' @export
walsby_control_plot_bulk_pdf_internal <- function(
    csv_src_dir,
    pdf_dest_path,
    etr_type,
    etr_max_start_value = walsby_default_start_value_etr_max,
    alpha_start_value = walsby_default_start_value_alpha,
    beta_start_value = walsby_default_start_value_beta) {
  library(data.table)

  csv_files <- list.files(csv_src_dir, pattern = ".csv", full.names = TRUE)
  pdf(pdf_dest_path)
  for (file in csv_files) {
    title <- basename(file)
    data <- read_pam_data(file)
    try({
      model_result <- walsby_generate_regression_internal(
        data,
        etr_type,
        etr_max_start_value,
        alpha_start_value,
        beta_start_value
      )
      print(walsby_control_plot(data, model_result, title))
    })
  }
  dev.off()
}
