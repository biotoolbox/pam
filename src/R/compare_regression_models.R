#' Compare Regression Models for ETR I
#'
#' Compares multiple regression models for electron transport rate (ETR) data using predefined performance metrics.
#'
#' @param data_dir A character string specifying the directory containing input data files.
#'
#' @return A vector with total points assigned to each regression model based on their performance. Models are ranked as follows:
#' \itemize{
#'   \item 1st place: 3 points
#'   \item 2nd place: 2 points
#'   \item 3rd place: 1 point
#'   \item 4th place: 0 points
#' }
#' If regression is not possible for a model, no points are awarded for any of the models for the respective file.
#'
#' @details This function compares the performance of the following models:
#' \itemize{
#'   \item Eilers-Peeters (1988)
#'   \item Platt (1980)
#'   \item Vollenweider (1965)
#'   \item Walsby (1997)
#' }
#' Models are ranked based on the deviation between observed and predicted values. The results guide users in selecting the most appropriate model for their dataset. Start values for parameters cannot be adjusted within this function.
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#walsby_modified}
#'
#'
#' @references{
#'   Eilers, P. H. C., & Peeters, J. C. H. (1988). \emph{A model for the relationship between light intensity and the rate of photosynthesis in phytoplankton.}
#'   Ecological Modelling, 42(3-4), 199-215. Available at: \doi{10.1016/0304-3800(88)90057-9}
#'
#'   Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). \emph{Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton.}
#'   Journal of Marine Research, 38(4). Available at: \url{https://elischolar.library.yale.edu/journal_of_marine_research/1525/}.
#'
#'   Romoth, K., Nowak, P., Kempke, D., Dietrich, A., Porsche, C., & Schubert, H. (2019). \emph{Acclimation limits of Fucus evanescens along the salinity gradient of the southwestern Baltic Sea.}
#'   Botanica Marina, 62(1), 1-12. Available at: \doi{10.1515/bot-2018-0098}.
#'
#'   Vollenweider, R. A. (1965). \emph{Calculation models of photosynthesis-depth curves and some implications regarding day rate estimates in primary production measurements.}
#'   In C. R. Goldman (Ed.), \emph{Primary Productivity in Aquatic Environments} (pp. 427-457). Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.
#'
#'   Walsby, A. E. (1997). \emph{Numerical integration of phytoplankton photosynthesis through time and depth in a water column.}
#'   New Phytologist, 136(2), 189-209. Available at: \doi{10.1046/j.1469-8137.1997.00736.x}.
#' }
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"))
#' points <- compare_regression_models_ETR_I(path)
#'
#' @export
compare_regression_models_ETR_I <- function(data_dir) {
  return(compare_regression_models(data_dir, etr_I_type))
}


#' Compare Regression Models for ETR II
#'
#' Compares multiple regression models for electron transport rate (ETR) data using predefined performance metrics.
#'
#' @param data_dir A character string specifying the directory containing input data files.
#'
#' @return A vector with total points assigned to each regression model based on their performance. Models are ranked as follows:
#' \itemize{
#'   \item 1st place: 3 points
#'   \item 2nd place: 2 points
#'   \item 3rd place: 1 point
#'   \item 4th place: 0 points
#' }
#' If regression is not possible for a model, no points are awarded for any of the models for the respective file.
#'
#' @details This function compares the performance of the following models:
#' \itemize{
#'   \item Eilers-Peeters (1988)
#'   \item Platt (1980)
#'   \item Vollenweider (1965)
#'   \item Walsby (1997)
#' }
#' Models are ranked based on the deviation between observed and predicted values. The results guide users in selecting the most appropriate model for their dataset. Start values for parameters cannot be adjusted within this function.
#' A detailed documentation can be found in the README.
#'
#'
#' @references{
#'   Eilers, P. H. C., & Peeters, J. C. H. (1988). \emph{A model for the relationship between light intensity and the rate of photosynthesis in phytoplankton.}
#'   Ecological Modelling, 42(3-4), 199-215. Available at: \doi{10.1016/0304-3800(88)90057-9}
#'
#'   Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). \emph{Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton.}
#'   Journal of Marine Research, 38(4). Available at: \url{https://elischolar.library.yale.edu/journal_of_marine_research/1525/}.
#'
#'   Romoth, K., Nowak, P., Kempke, D., Dietrich, A., Porsche, C., & Schubert, H. (2019). \emph{Acclimation limits of Fucus evanescens along the salinity gradient of the southwestern Baltic Sea.}
#'   Botanica Marina, 62(1), 1-12. Available at: \doi{10.1515/bot-2018-0098}.
#'
#'   Vollenweider, R. A. (1965). \emph{Calculation models of photosynthesis-depth curves and some implications regarding day rate estimates in primary production measurements.}
#'   In C. R. Goldman (Ed.), \emph{Primary Productivity in Aquatic Environments} (pp. 427-457). Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.
#'
#'   Walsby, A. E. (1997). \emph{Numerical integration of phytoplankton photosynthesis through time and depth in a water column.}
#'   New Phytologist, 136(2), 189-209. Available at: \doi{10.1046/j.1469-8137.1997.00736.x}.
#' }
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"))
#' points <- compare_regression_models_ETR_II(path)
#'
#' @export
compare_regression_models_ETR_II <- function(data_dir) {
  return(compare_regression_models(data_dir, etr_II_type))
}

compare_regression_models <- function(data_dir, etr_type) {
  csv_files <- list.files(data_dir, pattern = ".csv", full.names = TRUE)

  eilers_peeters_points <- 0
  platt_points <- 0
  vollenweider_points <- 0
  walsby_points <- 0

  for (file in csv_files) {
    title <- basename(file)
    data <- read_dual_pam_data(file)

    tryCatch(
      {
        eilers_peeters <- eilers_peeters_generate_regression_internal(data, etr_type)
        eilers_peeters_sdiff <- eilers_peeters[["sdiff"]]
        if (!is.numeric(eilers_peeters_sdiff)) {
          stop("eilers_peeters sdiff result is not numeric")
        }
        if (is.na(eilers_peeters_sdiff)) {
          stop("failed to calculate sdiff with eilers_peeters")
        }

        platt <- platt_generate_regression_internal(data, etr_type)
        platt_sdiff <- platt[["sdiff"]]
        if (!is.numeric(eilers_peeters_sdiff)) {
          stop("platt sdiff result is not numeric")
        }
        if (is.na(platt_sdiff)) {
          stop("failed to calculate sdiff with platt")
        }

        vollenweider <- vollenweider_generate_regression_internal(data, etr_type)
        vollenweider_sdiff <- vollenweider[["sdiff"]]
        if (!is.numeric(eilers_peeters_sdiff)) {
          stop("vollenweider sdiff result is not numeric")
        }
        if (is.na(vollenweider_sdiff)) {
          stop("failed to calculate sdiff with vollenweider")
        }

        walsby <- walsby_generate_regression_internal(data, etr_type)
        walsby_sdiff <- walsby[["sdiff"]]
        if (!is.numeric(eilers_peeters_sdiff)) {
          stop("walsby sdiff result is not numeric")
        }
        if (is.na(walsby_sdiff)) {
          stop("failed to calculate sdiff with walsby")
        }

        data1 <- data.table::data.table(group = "eilers_peeters", value = eilers_peeters_sdiff)
        data2 <- data.table::data.table(group = "platt", value = platt_sdiff)
        data3 <- data.table::data.table(group = "vollenweider", value = vollenweider_sdiff)
        data4 <- data.table::data.table(group = "walsby", value = walsby_sdiff)

        combined_data <- rbind(data1, data2, data3, data4)
        combined_data <- combined_data[order(combined_data$value), ]

        for (i in seq_len(nrow(combined_data))) {
          row <- combined_data[i, ]
          points <- 4 - i
          if (row$group == "eilers_peeters") {
            eilers_peeters_points <- eilers_peeters_points + points
          } else if (row$group == "platt") {
            platt_points <- platt_points + points
          } else if (row$group == "vollenweider") {
            vollenweider_points <- vollenweider_points + points
          } else if (row$group == "walsby") {
            walsby_points <- walsby_points + points
          }
        }
      },
      warning = function(w) {
        message("file: ", title, " processed with warning: ", w)
      },
      error = function(e) {
        message("skipped file: ", title, " because of error: ", e)
      }
    )
  }

  return(c(
    eilers_peeters = eilers_peeters_points,
    platt = platt_points,
    vollenweider = vollenweider_points,
    walsby = walsby_points
  ))
}
