#' @title Control Plot for Platt Model (1980)
#' @description This function creates a plot for the Platt Model based on the provided data and model results.
#'
#' @param data A `data.table` containing the original ETR and yield data for the plot.
#' @param model_result A list containing the fitting results of the Platt Model and the calculated paramters (alpha, ik...).
#' @param etr_type A character string describing the ETR type (ETR I or ETR II).
#' @param title A character string that specifies the title of the plot.
#'
#' @return A plot displaying the original ETR and Yield values and the regression data. A table below the plot shows the calculated data (alpha, ik...)
#'
#' @references
#' Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton.
#' Journal of Marine Research, 38(4). Retrieved from \url{https://elischolar.library.yale.edu/journal_of_marine_research/1525}.
#'
#' @export
platt_control_plot <- function(data, model_result, title) {
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
    } else if (name == "alpha") {
      value <- as.character(round(value, 7))
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
      color_platt,
      params
    )
  )
}

platt_control_plot_bulk_pdf_ETR_I <- function(
    csv_src_dir,
    pdf_dest_path,
    alpha_start_value = platt_default_start_value_alpha,
    beta_start_value = platt_default_start_value_beta,
    ps_start_value = platt_default_start_value_ps) {
  return(platt_control_plot_bulk_pdf_internal(
    csv_src_dir,
    pdf_dest_path,
    etr_I_type,
    alpha_start_value,
    beta_start_value,
    ps_start_value
  ))
}

platt_control_plot_bulk_pdf_ETR_II <- function(
    csv_src_dir,
    pdf_dest_path,
    alpha_start_value = platt_default_start_value_alpha,
    beta_start_value = platt_default_start_value_beta,
    ps_start_value = platt_default_start_value_ps) {
  return(platt_control_plot_bulk_pdf_internal(
    csv_src_dir,
    pdf_dest_path,
    etr_I_type,
    alpha_start_value,
    beta_start_value,
    ps_start_value
  ))
}

platt_control_plot_bulk_pdf_internal <- function(
    csv_src_dir,
    pdf_dest_path,
    etr_type,
    alpha_start_value = platt_default_start_value_alpha,
    beta_start_value = platt_default_start_value_beta,
    ps_start_value = platt_default_start_value_ps) {
  library(data.table)

  csv_files <- list.files(csv_src_dir, pattern = ".csv", full.names = TRUE)
  pdf(pdf_dest_path)
  for (file in csv_files) {
    title <- basename(file)
    data <- read_pam_data(file)
    try({
      model_result <- platt_generate_regression_internal(
        data,
        etr_type,
        alpha_start_value = platt_default_start_value_alpha,
        beta_start_value = platt_default_start_value_beta,
        ps_start_value = platt_default_start_value_ps
      )
      print(platt_control_plot(data, model_result, title))
    })
  }
  dev.off()
}
