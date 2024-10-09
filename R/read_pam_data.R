#' @title Read and process raw data
#' @description This function reads the original csv file as created by the DualPAM software, processes it by calculating ETR values, and returns a cleaned dataset.
#'
#' @param csv_path A string representing the file path to the CSV file.
#' @param remove_recovery Automatic removal of recovery measurements after the actual Pi curve for an accurate regression. Default is \code{TRUE}.
#' @param etr_factor A numeric value used as a factor for calculating ETR. Default is \code{0.84}.
#' @param p_ratio A numeric value representing the ratio of PS II / PSI used in the ETR calculation formula. Default is \code{0.5}. \deqn{P-Ratio = (PPS2) / (PPS1+2)}
#'
#' @details
#' ETR  are calculated using the following formula:
#' \deqn{ETR = PAR * ETR-Factor * P-Ratio * Y}
#' 
#' The function processes the provided csv file by:
#' \itemize{
#'   \item Reading the csv data using `read.csv()`.
#'   \item Validating the data structure using `validate_data()`.
#'   \item Filtering rows where the column \code{ID} equals "SP".
#'   \item Combining the \code{Date} and \code{Time} columns to create a new \code{DateTime} column.
#'   \item Calculating the ETR values for both \code{Y.I.} and \code{Y.II.} using the function `calc_etr()`.
#'   \item Removing rows after the recovery period if \code{remove_recovery = TRUE}.
#' }
#'
#' @return A `data.table` containing the processed data with additional columns for recalculated ETR values.
#'
#' @examples
#' # Example usage:
#' result <- read_pam_data("path/to/data.csv", remove_recovery = TRUE, etr_factor = 0.84, p_ratio = 0.5)
#' 
#' @seealso \code{\link{data.table}}, \code{\link{dplyr}}
#' @importFrom data.table data.table
#' @importFrom dplyr mutate select
#' 
#' @references
#' Heinz Walz GmbH. (2024). DUAL-PAM-100 DUAL-PAM/F MANUAL, 5th Edition, April 2024, Chapter 7 (pp. 162-172). 
#' Heinz Walz GmbH, Effeltrich, Germany. Available at:
#' \url{https://www.walz.com/files/downloads/manuals/dual-pam-100/DualPamEd06.pdf}

#'
#' @export
read_pam_data <- function(
    csv_path,
    remove_recovery = TRUE,
    etr_factor = 0.84,
    p_ratio = 0.5) {
  library(data.table)
  library(dplyr)

  tryCatch(
    {
      data <- read.csv(csv_path, sep = ";", dec = ".")
      data <- as.data.table(data)

      validate_data(data)
      data <- data[data$ID == "SP", ]

      date_time_col_values <- c()
      for (i in seq_len(nrow(data))) {
        row <- data[i, ]

        date_time_row_value <- as.POSIXct(
          paste(row$Date, row$Time, sep = " "),
          tz = "GMT", "%d.%m.%y %H:%M:%S"
        )
        date_time_col_values <- c(date_time_col_values, date_time_row_value)
      }

      data <- data %>%
        mutate(DateTime = date_time_col_values) %>%
        select(DateTime, everything())
      data <- data[order(data$DateTime), ]

      result <- data.table()
      last_par <- as.numeric(0)
      for (i in seq_len(nrow(data))) {
        row <- data[i, ]
        current_par <- row$PAR

        if (remove_recovery && last_par != 0 && current_par < last_par) {
          break
        }

        yield_I <- row$Y.I.
        recalc_ETRI <- calc_etr(yield_I, current_par, etr_factor, p_ratio)
        row <- cbind(row, etr_I_col_name = recalc_ETRI)
        setnames(row, old = "etr_I_col_name", new = etr_I_type)

        yield_II <- row$Y.II.
        recalc_ETRII <- calc_etr(yield_II, current_par, etr_factor, p_ratio)
        row <- cbind(row, etr_II_col_name = recalc_ETRII)
        setnames(row, old = "etr_II_col_name", new = etr_II_type)

        result <- rbind(result, row)

        last_par <- current_par
      }

      result <- result %>%
        select(!!etr_II_type, everything())

      result <- result %>%
        select(!!etr_I_type, everything())

      result <- result %>%
        select(DateTime, everything())

      return(result)
    },
    warning = function(w) {
      stop("Warning in file: ", csv_path, " Warning: ", w)
    },
    error = function(e) {
      stop("Error in file: ", csv_path, " Error: ", e)
    }
  )
}

calc_etr <- function(yield, par, etr_factor, p_ratio) {
  if (is.na(yield)) {
    return(NA_real_)
  }

  if (!is.numeric(yield)) {
    stop("yield is not numeric")
  }

  if (!is.numeric(par)) {
    stop("par is not numeric")
  }

  if (!is.numeric(etr_factor)) {
    stop("etr_factor is not numeric")
  }

  if (!is.numeric(p_ratio)) {
    stop("p_ratio is not numeric")
  }

  return(yield * par * etr_factor * p_ratio)
}
