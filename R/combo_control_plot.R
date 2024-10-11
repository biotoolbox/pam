#' Generate combined ETR Plots for ETR I
#'
#' This function creates a combined plot for ETR I. It visualizes the regression results from
#' the models: Eilers-Peeters (1988), Platt (1975), Vollenweider (1965),
#' and Walsby (1996).
#'
#' @param title A character string specifying the title of the plot.
#'
#' @param data A data frame containing the data to be plotted. The data frame
#' should include the relevant PAR and ETR values.
#'
#' @return A ggplot object containing the combined ETR plot for ETR I.
#'
#' @details
#' This function utilizes \code{\link{plot_combo_internal}} to generate
#' the combined plot by passing the specified ETR I type along with the
#' provided data.
#'
#' @references
#' Eilers, P. H. C., & Peeters, J. C. H. (1988). A model for the relationship
#' between light intensity and the rate of photosynthesis in phytoplankton.
#' *Ecological Modelling*, 42(3-4), 199-215. \doi{10.1016/0304-3800(88)90057-9}.
#'
#' Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton.
#' \emph{Journal of Marine Research}, \strong{38}(4), 687–701.
#' \doi{10.1357/002224080786845395}
#'
#' Romoth, K., Nowak, P., Kempke, D., Dietrich, A., Porsche, C., & Schubert, H. (2019).
#' Acclimation limits of *Fucus evanescens* along the salinity gradient of the
#' southwestern Baltic Sea. *Botanica Marina*, 62(1), 1-12. https://doi.org/10.1515/bot-2018-0098
#'
#' Vollenweider, R. A. (1965). Calculation models of photosynthesis-depth curves
#' and some implications regarding day rate estimates in primary production measurements,
#' p. 427-457. In C. R. Goldman [ed.], *Primary Productivity in Aquatic Environments*.
#' Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.
#'
#' Walsby, A. E. (1997). Numerical integration of phytoplankton photosynthesis
#' through time and depth in a water column. *Journal of Plankton Research*,
#' 19(3), 487-502. https://doi.org/10.1093/plankt/19.3.487
#' @export
combo_control_plot_etr_I <- function(title, data) {
  return(combo_control_plot_internal(title, data, etr_I_type))
}

#' Generate combined ETR Plots for ETR II
#'
#' This function creates a combined plot for ETR II. It visualizes the regression
#' results from the models: Eilers-Peeters (1988), Platt (1975), Vollenweider (1965),
#' and Walsby (1996).
#'
#' @param title A character string specifying the title of the plot.
#'
#' @param data A data frame containing the data to be plotted. The data frame
#' should include the relevant PAR and ETR values.
#'
#' @return A ggplot object containing the combined ETR plot for ETR II.
#'
#' @details
#' This function utilizes \code{\link{plot_combo_internal}} to generate
#' the combined plot by passing the specified ETR II type along with the
#' provided data.
#'
#' @references
#' Eilers, P. H. C., & Peeters, J. C. H. (1988). A model for the relationship
#' between light intensity and the rate of photosynthesis in phytoplankton.
#' *Ecological Modelling*, 42(3-4), 199-215. \doi{10.1016/0304-3800(88)90057-9}.
#'
#' Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton.
#' \emph{Journal of Marine Research}, \strong{38}(4), 687–701.
#' \doi{10.1357/002224080786845395}
#'
#' Romoth, K., Nowak, P., Kempke, D., Dietrich, A., Porsche, C., & Schubert, H. (2019).
#' Acclimation limits of *Fucus evanescens* along the salinity gradient of the
#' southwestern Baltic Sea. *Botanica Marina*, 62(1), 1-12. https://doi.org/10.1515/bot-2018-0098
#'
#' Vollenweider, R. A. (1965). Calculation models of photosynthesis-depth curves
#' and some implications regarding day rate estimates in primary production measurements,
#' p. 427-457. In C. R. Goldman [ed.], *Primary Productivity in Aquatic Environments*.
#' Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.
#'
#' Walsby, A. E. (1997). Numerical integration of phytoplankton photosynthesis
#' through time and depth in a water column. *Journal of Plankton Research*,
#' 19(3), 487-502. https://doi.org/10.1093/plankt/19.3.487
#' @export
combo_control_plot_etr_II <- function(title, data) {
  return(combo_control_plot_internal(title, data, etr_II_type))
}

combo_control_plot_internal <- function(title, data, etr_type) {
  library(ggplot2)
  library(ggthemes)

  eiler_peeters <- eilers_peeters_generate_regression_internal(data, etr_type)
  platt <- platt_generate_regression_internal(data, etr_type)
  vollenweider <- vollenweider_generate_regression_internal(data, etr_type)
  walsby <- walsby_generate_regression_internal(data, etr_type)

  etr_regression_data_eilers_peeters <- eiler_peeters[["etr_regression_data"]]
  etr_regression_data_platt <- platt[["etr_regression_data"]]
  etr_regression_data_vollenweider <- vollenweider[["etr_regression_data"]]
  etr_regression_data_walsby <- walsby[["etr_regression_data"]]

  plot <- ggplot(data, aes(x = data$PAR, y = get(etr_type))) +
    geom_point() +
    geom_line(
      data = etr_regression_data_eilers_peeters,
      aes(
        x = PAR,
        y = prediction,
        color = "Eilers-Peeters"
      ),
      alpha = 0.4
    ) +
    geom_line(
      data = etr_regression_data_platt,
      aes(
        x = PAR,
        y = prediction,
        color = "Platt"
      ),
      alpha = 0.4
    ) +
    geom_line(
      data = etr_regression_data_vollenweider,
      aes(
        x = PAR,
        y = prediction,
        color = "Vollenweider"
      ),
      alpha = 0.4
    ) +
    geom_line(
      data = etr_regression_data_walsby,
      aes(
        x = PAR,
        y = prediction,
        color = "Walsby"
      ),
      alpha = 0.4
    ) +
    scale_color_manual(values = c(
      "Eilers-Peeters" = color_eilers_peeters,
      "Platt" = color_platt,
      "Vollenweider" = color_vollenweider,
      "Walsby" = color_walsby
    )) +
    labs(x = par_label, y = etr_label, title = eval(title), color = NULL) +
    theme_base() +
    theme(legend.position = "bottom")

  return(plot)
}
