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

eilers_peeters_control_plot_bulk_pdf_ETR_I <- function(
    csv_src_dir,
    pdf_dest_path,
    a_start_value = eilers_peeters_default_start_value_a,
    b_start_value = eilers_peeters_default_start_value_b,
    c_start_value = eilers_peeters_default_start_value_c) {
  return(eilers_peeters_control_plot_bulk_pdf_internal(
    csv_src_dir,
    pdf_dest_path,
    etr_I_type,
    a_start_value,
    b_start_value,
    c_start_value
  ))
}

eilers_peeters_control_plot_bulk_pdf_ETR_II <- function(
    csv_src_dir,
    pdf_dest_path,
    a_start_value = eilers_peeters_default_start_value_a,
    b_start_value = eilers_peeters_default_start_value_b,
    c_start_value = eilers_peeters_default_start_value_c) {
  return(eilers_peeters_control_plot_bulk_pdf_internal(
    csv_src_dir,
    pdf_dest_path,
    etr_II_type,
    a_start_value,
    b_start_value,
    c_start_value
  ))
}

#' Generate PDF reports for Eilers-Peeters Model (1988)
#'
#' This function generates PDF reports containing plots of Eilers-Peeters
#' regression model for ETR based on multiple CSV files. Each plot displays
#' the regression results for the datasets processed.
#'
#' @param csv_src_dir Directory containing input CSV files (with `.csv`
#' extension) for analysis.
#'
#' @param pdf_dest_path Path to the destination PDF file where plots will
#' be saved.
#'
#' @param etr_type Type of ETR to analyze (e.g., "ETR I" or "ETR II").
#'
#' @param a_start_value Starting value for parameter 'a' in the Eilers-Peeters
#' model (default: \code{a_start_values_eilers_peeters_default}).
#'
#' @param b_start_value Starting value for parameter 'b' in the Eilers-Peeters
#' model (default: \code{b_start_values_eilers_peeters_default}).
#'
#' @param c_start_value Starting value for parameter 'c' in the Eilers-Peeters
#' model (default: \code{c_start_values_eilers_peeters_default}).
#'
#' @return A PDF file with plots for each regression model applied to the
#' datasets.
#'
#' @details
#' The function  scans for csv files, reads data using \code{read_pam_data},
#' generates the regression model via
#' \code{generate_regression_eilers_peeters_internal}, and plots results
#' with \code{plot_control_eilers_peeters}. Any errors encountered during
#' file processing are reported without halting execution.
#'
#' #' @references
#' Eilers, P. H. C., & Peeters, J. C. H. (1988). A model for the relationship
#' between light intensity and the rate of photosynthesis in phytoplankton.
#' *Ecological Modelling*, 42(3-4), 199-215. \doi{10.1016/0304-3800(88)90057-9}.
#'
#' @importFrom data.table fread
#' @export
eilers_peeters_control_plot_bulk_pdf_internal <- function(
    csv_src_dir,
    pdf_dest_path,
    etr_type,
    a_start_value = eilers_peeters_default_start_value_a,
    b_start_value = eilers_peeters_default_start_value_b,
    c_start_value = eilers_peeters_default_start_value_c) {
  library(data.table)
  csv_files <- list.files(csv_src_dir, pattern = ".csv", full.names = TRUE)
  pdf(pdf_dest_path)
  for (file in csv_files) {
    title <- basename(file)
    data <- read_pam_data(file)
    try({
      model_result <- eilers_peeters_generate_regression_internal(
        data,
        etr_type,
        a_start_value,
        b_start_value,
        c_start_value
      )
      print(eilers_peeters_control_plot(data, model_result, title))
    })
  }
  dev.off()
}
