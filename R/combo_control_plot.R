combo_control_plot <- function(
    title,
    data,
    model_results,
    name_list,
    color_list) {
  library(ggplot2)
  library(ggthemes)

  validate_data(data)

  if (length(model_results) <= 0) {
    stop("empty model_results")
  }

  if (!is.list(model_results) || !is.list(name_list) || !is.list(color_list)) {
    stop("model_results, name_list and color_list all need to be lists")
  }

  if (length(model_results) != length(color_list)) {
    stop("model_results length not equal to color_list length")
  }

  if (length(model_results) != length(name_list)) {
    stop("model_results length not equal to name_list length")
  }

  etr_regression_data <- get_etr_regression_data_from_model_result(model_results[[1]])
  etr_type <- get_etr_type_from_model_result(model_results[[1]])
  max_etr <- max(etr_regression_data$prediction)

  validate_etr_type(etr_type)

  yield <- NA_real_
  if (etr_type == etr_I_type) {
    yield <- "Y.I."
  } else {
    yield <- "Y.II."
  }

  plot <- ggplot(data, aes(x = data$PAR, y = get(etr_type))) +
    geom_point() +
    geom_point(data = data, aes(y = get(yield) * max_etr)) +
    geom_line(data = data, aes(y = get(yield) * max_etr))

  for (i in seq_along(model_results)) {
    name <- name_list[[i]]
    model_result <- model_results[[i]]

    validate_model_result(model_result)

    if (get_etr_type_from_model_result(model_result) != etr_type) {
      stop("all model results need to be calculated with the same ETR type")
    }

    reg_data <- get_etr_regression_data_from_model_result(model_result)
    reg_data <- cbind(reg_data, names = name)

    plot <- plot + geom_line(
      data = reg_data,
      aes(
        x = PAR,
        y = prediction,
        color = names
      )
    )
  }

  plot <- plot +
    scale_color_manual(
      values = setNames(
        unlist(color_list),
        unlist(name_list)
      )
    ) +
    labs(x = par_label, y = etr_label, title = eval(title), color = NULL) +
    theme_base() +
    theme(legend.position = "bottom")

  return(plot)
}
