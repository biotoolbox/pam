#' function for generating control plots bulk according to eilers and peeters
#' @param data : data from csv_src_folder, pdf_dest_path, etr_type
#' @return bulk of control plots with data points, regression and calculated result values (pm, alpha, ik...)
#' @export
wrapper_pdf_eilers_peeters <- function(
    csv_src_dir,
    pdf_dest_path,
    etr_type,
    a_start_value = a_start_values_eilers_peeters_default,
    b_start_value = b_start_values_eilers_peeters_default,
    c_start_value = c_start_values_eilers_peeters_default) {
  library(data.table)
  csv_files <- list.files(csv_src_dir, pattern = ".csv", full.names = TRUE)
  pdf(pdf_dest_path)
  for (file in csv_files) {
    title <- basename(file)
    data <- read_pam_data(file)
    try({
      model_result <- generate_regression_eilers_peeters_internal(
        data,
        etr_type,
        a_start_value,
        b_start_value,
        c_start_value
      )
      print(plot_control_eilers_peeters(data, model_result, etr_type, title))
    })
  }
  dev.off()
}

#' function for generating control plots bulk according to walsby
#' @param data : data from csv_src_folder, pdf_dest_path, etr_type
#' @return bulk of control plots with data points, regression and calculated result values (etrmax, alpha, beta...)
#' @export
wrapper_pdf_walsby <- function(
    csv_src_dir,
    pdf_dest_path,
    etr_type,
    etr_max_start_value = etr_max_start_value_walsby_default,
    alpha_start_value = alpha_start_value_walsby_default,
    beta_start_value = beta_start_value_walsby_default) {
  library(data.table)
  csv_files <- list.files(csv_src_dir, pattern = ".csv", full.names = TRUE)
  pdf(pdf_dest_path)
  for (file in csv_files) {
    title <- basename(file)
    data <- read_pam_data(file)
    try({
      model_result <- generate_regression_walsby_internal(
        data,
        etr_type,
        etr_max_start_value,
        alpha_start_value,
        beta_start_value
      )
      print(plot_control_walsby(data, model_result, title, etr_type))
    })
  }
  dev.off()
}

#' function for generating control plots bulk according to vollenweider
#' @param csv_src_dir: directory where the csv files are located
#' @return bulk of control plots with data points, regression and calculated result values (pmax, alpha, ik...)
#' @export
wrapper_pdf_vollenweider <- function(
    csv_src_dir,
    pdf_dest_path,
    etr_type,
    pmax_start_value = pmax_start_value_vollenweider_default,
    a_start_value = a_start_value_vollenweider_default,
    alpha_start_value = alpha_start_value_vollenweider_default,
    n_start_value = n_start_value_vollenweider_default) {
  library(data.table)
  csv_files <- list.files(csv_src_dir, pattern = ".csv", full.names = TRUE)
  pdf(pdf_dest_path)
  for (file in csv_files) {
    title <- basename(file)
    data <- read_pam_data(file)
    try({
      model_result <- generate_regression_vollenweider_internal(
        data,
        etr_type,
        pmax_start_value,
        a_start_value,
        alpha_start_value,
        n_start_value
      )
      print(plot_control_vollenweider(data, model_result, title, etr_type))
    })
  }
  dev.off()
}
