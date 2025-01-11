#' Combined ETR Plot and Summary Table
#'
#' Generates a plot of ETR data with different regression model predictions and a summary table.
#'
#' @param title Character. Plot title.
#' @param data Data frame. ETR and PAR data.
#' @param model_results List. Regression data and parameters.
#' @param name_list List. Names for models (legend and table).
#' @param color_list List. Colors for model lines.
#'
#' @details
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#combo_control_plot}.
#'
#' @return A plot with ETR data, regression results, and a summary table.
#'
#' #' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' model_results_eilers_peeters <- eilers_peeters_generate_regression_ETR_I(data)
#' model_results_platt <- platt_generate_regression_ETR_I(data)
#' model_results <- list(eilers_peeters_modified(model_results), platt_modified(model_results))
#' name_list <- c("Eilers-Peeters", "Platt")
#' color_list <- c("red", "pink")
#' plot <- combo_plot_control("test", data, model_results, name_list, color_list)
#' @export
combo_plot_control <- function(
    title,
    data,
    model_results,
    name_list,
    color_list) {
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

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = data$PAR, y = get(etr_type))) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = data, ggplot2::aes(y = get(yield) * max_etr)) +
    ggplot2::geom_line(data = data, ggplot2::aes(y = get(yield) * max_etr)) +
    ggplot2::labs(x = par_label, y = etr_label, title = eval(title)) +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(~ . / max_etr, name = "Yield")
    )

  custom_theme <- gridExtra::ttheme_minimal(
    core = list(
      fg_params = list(
        cex = 0.7,
        fontface = 3
      ),
      bg_params = list(
        fill = "lightgray",
        col = "black"
      )
    ), # font size for cell text
    colhead = list(
      fg_params = list(cex = 0.7),
      bg_params = list(
        fill = "lightgray",
        col = "black"
      )
    ), # font size for column headers
    rowhead = list(
      fg_params = list(cex = 0.7),
      bg_params = list(
        fill = "lightgray",
        col = "black"
      )
    ), # font size for row headers
  )

  tbl <- NULL

  for (i in seq_along(model_results)) {
    name <- name_list[[i]]
    model_result <- model_results[[i]]

    validate_modified_model_result(model_result)

    if (get_etr_type_from_model_result(model_result) != etr_type) {
      stop("all model results need to be calculated with the same ETR type")
    }

    reg_data <- get_etr_regression_data_from_model_result(model_result)
    reg_data <- cbind(reg_data, names = name)

    plot <- plot + ggplot2::geom_line(
      data = reg_data,
      ggplot2::aes(
        x = reg_data$PAR,
        y = reg_data$prediction,
        color = names
      )
    )

    if (is.null(tbl)) {
      tbl <- data.frame(name = NA)
      for (i in names(model_result)) {
        if (i == "etr_type" || i == "etr_regression_data") {
          next()
        }

        tbl[[i]] <- NA
      }
    }

    row <- c(name)
    for (i in names(model_result)) {
      if (i == "etr_type" || i == "etr_regression_data") {
        next()
      }

      row <- append(row, model_result[[i]])
    }
    tbl <- rbind(tbl, row)
  }

  tbl <- tbl[-1, ]
  name_col <- tbl[, 0]
  tbl <- tbl[, -1]

  row <- NULL
  tbl_list <- list()
  row_count <- 1
  entries_per_row <- 4
  count <- 1
  col_names <- c("model")

  for (i in seq_len(ncol(tbl))) {
    col <- tbl[, i]
    col_names <- append(col_names, colnames(tbl)[i])

    if (is.null(row)) {
      row <- data.frame(name_col)
      row <- cbind(row, unlist(name_list))
    }

    row <- cbind(row, col)

    if (count == entries_per_row) {
      colnames(row) <- col_names
      tbl_list[[row_count]] <- gridExtra::tableGrob(
        row,
        rows = NULL,
        theme = custom_theme
      )
      row <- NULL
      row_count <- row_count + 1
      count <- 0
      col_names <- c("model")
    }

    count <- count + 1
  }

  if (is.null(row) == FALSE) {
    colnames(row) <- col_names
    tbl_list[[row_count]] <- gridExtra::tableGrob(
      row,
      rows = NULL,
      theme = custom_theme
    )
  }

  plot <- plot +
    ggplot2::scale_color_manual(
      values = stats::setNames(
        unlist(color_list),
        unlist(name_list)
      )
    ) +
    ggplot2::labs(x = par_label, y = etr_label, title = eval(title), color = NULL) +
    ggthemes::theme_base() +
    ggplot2::theme(legend.position = "bottom")

  tbl <- cowplot::plot_grid(
    plotlist = tbl_list,
    ncol = 1
  )

  plot <- cowplot::plot_grid(
    plot,
    tbl,
    ncol = 1,
    rel_heights = c(0.7, 0.3)
  )

  return(plot)
}
