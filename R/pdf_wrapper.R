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

# platt fehlt oder ich bin dumm