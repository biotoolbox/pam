calculate_sdiff <- function(data, etr_regression_data, etr_type) {
  validate_data(data)
  validate_etr_regression_data(etr_regression_data)
  validate_etr_type(etr_type)

  final_sdiff <- 0
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    real_etr <- row[[etr_type]]
    predicted_etr <- etr_regression_data[etr_regression_data$PAR == row$PAR, ][[prediction_name]]

    sdiff <- (predicted_etr - real_etr)^2
    final_sdiff <- final_sdiff + sdiff
  }

  return(final_sdiff)
}

remove_det_row_by_etr <- function(data, etr_type) {
  library(dplyr)
  validate_data(data)
  validate_etr_type(etr_type)

  if (etr_type == etr_I_type) {
    data <- data %>% filter(data$Action != "Fm-Det.")
    if (length(data[data$Action == "Pm.-Det.", ]) == 0) {
      stop("Pm.-Det. is required but not present")
    }
  } else {
    data <- data %>% filter(data$Action != "Pm.-Det.")
    if (length(data[data$Action == "Fm-Det.", ]) == 0) {
      stop("Fm-Det. is required but not present")
    }
  }

  return(data)
}

create_regression_data <- function(pars, predictions) {
  library(data.table)

  if (!is.vector(pars)) {
    stop("pars is not a valid vector")
  }

  if (!is.vector(predictions)) {
    stop("predictions is not a valid vector")
  }

  if (length(pars) != length(predictions)) {
    stop("pars and predictions need to be of the same length")
  }

  regression_data <- data.table(
    "PAR" = pars,
    "prediction" = predictions
  )
  return(regression_data)
}

plot_control <- function(
    data,
    model_result,
    etr_type,
    title,
    color,
    params) {
  library(ggplot2)
  library(ggthemes)
  library(gridExtra)

  validate_data(data)
  validate_etr_type(etr_type)

  data <- remove_det_row_by_etr(data, etr_type)

  # TODO: validate params
  params_transposed <- t(params)
  colnames(params_transposed) <- NULL
  rownames(params_transposed) <- NULL

  yield <- NA_real_
  if (etr_type == etr_I_type) {
    yield <- "Y.I."
  } else {
    yield <- "Y.II."
  }

  etr_regression_data <- model_result[["etr_regression_data"]]
  validate_etr_regression_data(etr_regression_data)

  max_etr <- max(etr_regression_data$prediction)

  plot <- ggplot(data, aes(x = data$PAR, y = get(etr_type))) +
    geom_point() +
    geom_line(
      data = etr_regression_data,
      aes(
        x = etr_regression_data$PAR,
        y = etr_regression_data$prediction
      ),
      color = color
    ) +
    geom_point(data = data, aes(y = get(yield) * max_etr)) +
    geom_line(data = data, aes(y = get(yield) * max_etr)) +
    labs(x = par_label, y = etr_label, title = eval(title)) +
    scale_y_continuous(
      sec.axis = sec_axis(~ . / max_etr, name = "Yield")
    ) +
    theme_base()

  custom_theme <- ttheme_minimal(
    core = list(fg_params = list(cex = 0.7)), # font size for cell text
    colhead = list(fg_params = list(cex = 0.7)), # font size for column headers
    rowhead = list(fg_params = list(cex = 0.7)) # font size for row headers
  )

  # use different library for grid arrange because of printout
  table <- tableGrob(
    params_transposed,
    rows = NULL,
    theme = custom_theme
  )

  full_plot <- grid.arrange(
    plot,
    table,
    ncol = 1,
    heights = c(3, 0.2),
    widths = 1.5
  )

  return(full_plot)
}
