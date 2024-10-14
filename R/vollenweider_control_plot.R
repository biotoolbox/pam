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

#' @export
vollenweider_control_plot_bulk_pdf_ETR_I <- function(
    csv_src_dir,
    pdf_dest_path,
    pmax_start_value = vollenweider_default_start_value_pmax,
    a_start_value = vollenweider_default_start_value_a,
    alpha_start_value = vollenweider_default_start_value_alpha,
    n_start_value = vollenweider_default_start_value_n) {
  return(vollenweider_control_plot_bulk_pdf_internal(
    csv_src_dir,
    pdf_dest_path,
    etr_I_type,
    pmax_start_value,
    a_start_value,
    alpha_start_value,
    n_start_value
  ))
}

#' @export
vollenweider_control_plot_bulk_pdf_ETR_II <- function(
    csv_src_dir,
    pdf_dest_path,
    pmax_start_value = vollenweider_default_start_value_pmax,
    a_start_value = vollenweider_default_start_value_a,
    alpha_start_value = vollenweider_default_start_value_alpha,
    n_start_value = vollenweider_default_start_value_n) {
  return(vollenweider_control_plot_bulk_pdf_internal(
    csv_src_dir,
    pdf_dest_path,
    etr_II_type,
    pmax_start_value,
    a_start_value,
    alpha_start_value,
    n_start_value
  ))
}

#' Generate PDF Reports for Vollenweider Model (1965)
#'
#' This function generates PDF reports containing plots of the Vollenweider
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
#' @param pmax_start_value Starting value for the parameter 'pmax' in the
#' Vollenweider model (default: \code{pmax_start_value_vollenweider_default}).
#'
#' @param a_start_value Starting value for parameter 'a' in the
#' Vollenweider model (default: \code{a_start_value_vollenweider_default}).
#'
#' @param alpha_start_value Starting value for parameter 'alpha' in the
#' Vollenweider model (default: \code{alpha_start_value_vollenweider_default}).
#'
#' @param n_start_value Starting value for parameter 'n' in the Vollenweider
#' model (default: \code{n_start_value_vollenweider_default}).
#'
#' @return A PDF file with plots for each regression model applied to the
#' datasets.
#'
#' @details
#' The function scans for csv files, reads data using \code{read_pam_data},
#' generates the regression model via
#' \code{generate_regression_vollenweider_internal}, and plots results with
#' \code{plot_control_vollenweider}. Any errors encountered during file
#' processing are reported without halting execution.
#'
#' @references
#' Vollenweider, R. A. (1965). Calculation models of photosynthesis-depth curves
#' and some implications regarding day rate estimates in primary production measurements,
#' p. 427-457. In C. R. Goldman [ed.], *Primary Productivity in Aquatic Environments*.
#' Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.
#'
#' @importFrom data.table fread
#' @export
vollenweider_control_plot_bulk_pdf_internal <- function(
    csv_src_dir,
    pdf_dest_path,
    etr_type,
    pmax_start_value = vollenweider_default_start_value_pmax,
    a_start_value = vollenweider_default_start_value_a,
    alpha_start_value = vollenweider_default_start_value_alpha,
    n_start_value = vollenweider_default_start_value_n) {
  library(data.table)
  csv_files <- list.files(csv_src_dir, pattern = ".csv", full.names = TRUE)
  pdf(pdf_dest_path)
  for (file in csv_files) {
    title <- basename(file)
    data <- read_pam_data(file)
    try({
      model_result <- vollenweider_generate_regression_internal(
        data,
        etr_type,
        pmax_start_value,
        a_start_value,
        alpha_start_value,
        n_start_value
      )
      print(vollenweider_control_plot(data, model_result, title))
    })
  }
  dev.off()
}
