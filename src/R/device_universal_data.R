#' Read and Process Universal PAM Data
#'
#' Reads a standardized CSV file containing PAR and yield data for photosystem I and/or II, calculates electron transport rates (ETR), and returns a cleaned and validated dataset. The function is device-agnostic but requires a predefined column structure.
#'
#' @param csv_path File path to the CSV file.
#' @param etr_factor Numeric. Factor for ETR calculation. Default is \code{0.84}.
#' @param fraction_photosystem_I Numeric. Relative distribution of absorbed PAR
#' to photosystem I. Default is \code{0.5}.
#' @param fraction_photosystem_II Numeric. Relative distribution of absorbed PAR
#' to photosystem II. Default is \code{0.5}.
#'
#' @details
#' Calculates ETR using:
#' \deqn{\text{ETR} = \text{PAR} \cdot \text{ETR-Factor} \cdot \text{Fraction of Photosystem (I or II)} \cdot \text{Yield (I or II)}}
#'
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam/tree/main#functions}
#'
#' @return A \code{data.table} containing:
#' \itemize{
#'   \item \code{par}: Photosynthetically active radiation.
#'   \item \code{yield_1}: Yield for photosystem I.
#'   \item \code{yield_2}: Yield for photosystem II.
#'   \item \code{etr_1}: Calculated ETR for photosystem I.
#'   \item \code{etr_2}: Calculated ETR for photosystem II.
#' }
#'
#' @references{
#'   Heinz Walz GmbH. (2024). \emph{DUAL-PAM-100 DUAL-PAM/F MANUAL, 5th Edition, April 2024, Chapter 7 (pp. 162-172).}
#'   Heinz Walz GmbH, Effeltrich, Germany.
#'   Available at: \url{https://www.walz.com/files/downloads/dualpamed05.pdf}
#' }
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "universal_data", "universal_data.csv")
#' data <- read_universal_data(path)
#' @export
read_universal_data <- function(csv_path,
                                etr_factor = 0.84,
                                fraction_photosystem_I = 0.5,
                                fraction_photosystem_II = 0.5) {
  if (fraction_photosystem_I + fraction_photosystem_II != 1) {
    stop("The sum of fraction_photosystem_I and fraction_photosystem_II must be equal 1.")
  }

  data <- utils::read.csv(csv_path, sep = ";", dec = ".")
  data <- data.table::as.data.table(data)
  validate_universal_data(data)

  result <- data.table::data.table(
    par = numeric(),
    yield_1 = numeric(),
    yield_2 = numeric(),
    etr_1 = numeric(),
    etr_2 = numeric()
  )
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    current_par <- row$par

    yield_1 <- row$yield_1
    recalc_etr_1 <- calc_etr(yield_1, current_par, etr_factor, fraction_photosystem_I)

    yield_2 <- row$yield_2
    recalc_etr_2 <- calc_etr(yield_2, current_par, etr_factor, fraction_photosystem_II)

    new_row <- list(
      par = current_par,
      yield_1 = yield_1,
      yield_2 = yield_2,
      etr_1 = recalc_etr_1,
      etr_2 = recalc_etr_2
    )
    result <- rbind(result, new_row)
  }

  validate_intermediate_data(result)
  return(result)
}

validate_universal_data <- function(data) {
  validate_data_not_empty(data)

  if (!"par" %in% colnames(data)) {
    stop("required col 'par' not found")
  }

  if (!"yield_1" %in% colnames(data)) {
    stop("required col 'yield_1' not found")
  }

  if (!"yield_2" %in% colnames(data)) {
    stop("required col 'yield_2' not found")
  }
}
