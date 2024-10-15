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

  etr_type <- get_etr_type_from_model_result(model_results[[1]])
  validate_etr_type(etr_type)

  plot <- ggplot(data, aes(x = data$PAR, y = get(etr_type))) +
    geom_point()

  for (i in seq_along(model_results)) {
    # name <- name_list[[i]]
    color <- color_list[[i]]
    model_result <- model_results[[i]]

    validate_model_result(model_result)

    if (get_etr_type_from_model_result(model_result) != etr_type) {
      stop("all model results need to be calculated with the same ETR type")
    }

    plot <- plot + geom_line(
      data = get_etr_regression_data_from_model_result(model_result),
      aes(
        x = PAR,
        y = prediction
      ),
      color = color,
      alpha = 0.4,
      show.legend = TRUE
    )
  }

  plot <- plot +
    labs(x = par_label, y = etr_label, title = eval(title), color = NULL) +
    theme_base() +
    theme(legend.position = "bottom")

  return(plot)
}
