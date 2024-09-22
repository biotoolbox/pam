plot_combo_etr_I <- function(title, data) {
  return(plot_combo_internal(title, data, etr_I_col_name))
}

plot_combo_etr_II <- function(title, data) {
  return(plot_combo_internal(title, data, etr_II_col_name))
}

plot_combo_internal <- function(title, data, etr_type) {
  library(ggplot2)
  library(ggthemes)

  eiler_peeters <- generate_regression_eilers_peeters_internal(data, etr_type)
  platt <- generate_regression_platt_internal(data, etr_type)
  vollenweider <- generate_regression_vollenweider_internal(data, etr_type)
  walsby <- generate_regression_walsby_internal(data, etr_type)

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
        color = "Eilers-Peeters 1988"
      ),
      alpha = 0.4
    ) +
    geom_line(
      data = etr_regression_data_platt,
      aes(
        x = PAR,
        y = prediction,
        color = "Platt 1975"
      ),
      alpha = 0.4
    ) +
    geom_line(
      data = etr_regression_data_vollenweider,
      aes(
        x = PAR,
        y = prediction,
        color = "Vollenweider 1965"
      ),
      alpha = 0.4
    ) +
    geom_line(
      data = etr_regression_data_walsby,
      aes(
        x = PAR,
        y = prediction,
        color = "Walsby 1996"
      ),
      alpha = 0.4
    ) +
    scale_color_manual(values = c(
      "Eilers-Peeters 1988" = color_eilers_peeters,
      "Platt 1975" = color_platt,
      "Vollenweider 1965" = color_vollenweider,
      "Walsby 1996" = color_walsby
    )) +
    labs(x = par_label, y = etr_label, title = eval(title), color = NULL) +
    theme_base() +
    theme(legend.position = "bottom")

  return(plot)
}
