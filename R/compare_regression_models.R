#' Compare Regression Models for ETR I
#'
#' This function compares different regression models for the
#' ETR I.
#'
#' @param data_dir A character string specifying the directory
#' where the input data files are located.
#'
#' @return A vector containing the total points assigned to
#' each regression model based on their performance. Models are ranked
#' based on the calculated deviation of the difference between
#' observed and predicted values. Rating: 1st: 3 points; 2nd: 2 points;
#' 3rd: 1 point; 4th: 0 points.
#'
#' @details
#' This function calls the \code{compare_regression_models} function,
#' passing the directory of the data and the specified model type for ETR I.
#' This allows for a straightforward comparison of the models:
#' Eilers-Peeters (1988), Platt (1980), Vollenweider (1965), and Walsby (1997).
#' The results can guide users in selecting
#' the most appropriate model for their data.
#'
#' @references
#' Eilers, P. H. C., & Peeters, J. C. H. (1988). A model for the relationship
#' between light intensity and the rate of photosynthesis in phytoplankton.
#' Ecological Modelling, 42(3-4), 199-215. \doi{10.1016/0304-3800(88)90057-9}.
#'
#' Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton.
#' Journal of Marine Research, 38(4). Retrieved from \url{https://elischolar.library.yale.edu/journal_of_marine_research/1525}.
#'
#' Romoth, K., Nowak, P., Kempke, D., Dietrich, A., Porsche, C., & Schubert, H. (2019).
#' Acclimation limits of Fucus evanescens along the salinity gradient of the
#' southwestern Baltic Sea. Botanica Marina, 62(1), 1-12. \doi{10.1515/bot-2018-0098}.
#'
#' Vollenweider, R. A. (1965). Calculation models of photosynthesis-depth curves
#' and some implications regarding day rate estimates in primary production measurements,
#' p. 427-457. In C. R. Goldman [ed.], Primary Productivity in Aquatic Environments.
#' Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.
#'
#' Walsby, A. E. (1997). Numerical integration of phytoplankton photosynthesis
#' through time and depth in a water column. New Phytologist, 136(2), 189-209.
#' \doi{10.1046/j.1469-8137.1997.00736.x}.
#' @export
compare_regression_models_ETR_I <- function(data_dir) {
  return(compare_regression_models(data_dir, etr_I_type))
}

#' Compare Regression Models for ETR II
#'
#' This function compares different regression models for the
#' ETR II.
#'
#' @param data_dir A character string specifying the directory
#' where the input data files are located.
#'
#' @return A vector containing the total points assigned to
#' each regression model based on their performance. Models are ranked
#' based on the calculated deviation of the difference between
#' observed and predicted values. Rating: 1st: 3 points; 2nd: 2 points;
#' 3rd: 1 point; 4th: 0 points.
#'
#' @details
#' This function calls the \code{compare_regression_models} function,
#' passing the directory of the data and the specified model type for ETR I.
#' This allows for a straightforward comparison of the models:
#' Eilers-Peeters (1988), Platt (1980), Vollenweider (1965), and Walsby (1997).
#' The results can guide users in selecting
#' the most appropriate model for their data.
#'
#' @references
#' Eilers, P. H. C., & Peeters, J. C. H. (1988). A model for the relationship
#' between light intensity and the rate of photosynthesis in phytoplankton.
#' Ecological Modelling, 42(3-4), 199-215. \doi{10.1016/0304-3800(88)90057-9}.
#'
#' Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton.
#' Journal of Marine Research, 38(4). Retrieved from \url{https://elischolar.library.yale.edu/journal_of_marine_research/1525}.
#'
#' Romoth, K., Nowak, P., Kempke, D., Dietrich, A., Porsche, C., & Schubert, H. (2019).
#' Acclimation limits of Fucus evanescens along the salinity gradient of the
#' southwestern Baltic Sea. Botanica Marina, 62(1), 1-12. \doi{10.1515/bot-2018-0098}.
#'
#' Vollenweider, R. A. (1965). Calculation models of photosynthesis-depth curves
#' and some implications regarding day rate estimates in primary production measurements,
#' p. 427-457. In C. R. Goldman [ed.], Primary Productivity in Aquatic Environments.
#' Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.
#'
#' Walsby, A. E. (1997). Numerical integration of phytoplankton photosynthesis
#' through time and depth in a water column. New Phytologist, 136(2), 189-209.
#' \doi{10.1046/j.1469-8137.1997.00736.x}.
#' @export
compare_regression_models_ETR_II <- function(data_dir) {
  return(compare_regression_models(data_dir, etr_II_type))
}

compare_regression_models <- function(data_dir, etr_type) {
  library(data.table)
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

        platt <- platt_generate_regression_internal(data, etr_type)
        platt_sdiff <- platt[["sdiff"]]
        if (!is.numeric(eilers_peeters_sdiff)) {
          stop("platt sdiff result is not numeric")
        }

        vollenweider <- vollenweider_generate_regression_internal(data, etr_type)
        vollenweider_sdiff <- vollenweider[["sdiff"]]
        if (!is.numeric(eilers_peeters_sdiff)) {
          stop("vollenweider sdiff result is not numeric")
        }

        walsby <- walsby_generate_regression_internal(data, etr_type)
        walsby_sdiff <- walsby[["sdiff"]]
        if (!is.numeric(eilers_peeters_sdiff)) {
          stop("walsby sdiff result is not numeric")
        }

        if (is.na(eilers_peeters_sdiff)) {
          stop("failed to calculate sdiff with eilers_peeters")
        }
        if (is.na(platt_sdiff)) {
          stop("failed to calculate sdiff with platt")
        }
        if (is.na(vollenweider_sdiff)) {
          stop("failed to calculate sdiff with vollenweider")
        }
        if (is.na(walsby_sdiff)) {
          stop("failed to calculate sdiff with walsby")
        }

        data1 <- data.table(group = "eilers_peeters", value = eilers_peeters_sdiff)
        data2 <- data.table(group = "platt", value = platt_sdiff)
        data3 <- data.table(group = "vollenweider", value = vollenweider_sdiff)
        data4 <- data.table(group = "walsby", value = walsby_sdiff)

        combined_data <- rbind(data1, data2, data3, data4)
        combined_data <- combined_data[order(combined_data$value), ]

        # TODO: check if order is correct

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
        message("Skipped file: ", title, " because of warning: ", w)
      },
      error = function(e) {
        message("Skipped file: ", title, " because of error: ", e)
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
