combo_control_plot <- function(
    title,
    data,
    model_results,
    name_list,
    color_list) {
  library(ggplot2)
  library(ggthemes)
  library(gridExtra)
  library(cowplot)

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

  custom_theme <- ttheme_minimal(
    core = list(
      fg_params = list(
        cex = 0.7,
        fontface = 3,
        col = "darkblue"
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

    plot <- plot + geom_line(
      data = reg_data,
      aes(
        x = PAR,
        y = prediction,
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
  col_names <- c("name")

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
      tbl_list[[row_count]] <- tableGrob(
        row,
        rows = NULL,
        theme = custom_theme
      )
      row <- NULL
      row_count <- row_count + 1
      count <- 0
      col_names <- c("name")
    }

    count <- count + 1
  }

  if (is.null(row) == FALSE) {
    colnames(row) <- col_names
    tbl_list[[row_count]] <- tableGrob(
      row,
      rows = NULL,
      theme = custom_theme
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

  tbl <- plot_grid(
    plotlist = tbl_list,
    ncol = 1
  )

  plot <- plot_grid(
    plot,
    tbl,
    ncol = 1,
    rel_heights = c(0.7, 0.3)
  )

  return(plot)
}

plot_combo_control_plot_table <- function() {
  library(ggthemes)
  library(gridExtra)
  library(cowplot)

  validate_model_result(model_result)

  custom_theme <- ttheme_minimal(
    core = list(
      fg_params = list(
        cex = 0.7,
        fontface = 3,
        col = "darkblue"
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

  tbl_list <- list()
  row <- NULL

  row_count <- 1
  count <- 1

  for (i in names(model_result)) {
    if (i == "etr_type" || i == "etr_regression_data") {
      next()
    }

    value <- model_result[[i]]

    if (is.null(row)) {
      row <- data.frame(tmp = NA)
    }

    row[[i]] <- c(value)

    if (count == entries_per_row) {
      row$tmp <- NULL
      tbl_list[[row_count]] <- tableGrob(
        row,
        rows = NULL,
        theme = custom_theme
      )

      row <- NULL
      row_count <- row_count + 1
      count <- 1
    } else {
      count <- count + 1
    }
  }

  if (is.null(row) == FALSE) {
    row$tmp <- NULL
    tbl_list[[row_count]] <- tableGrob(
      row,
      rows = NULL,
      theme = custom_theme
    )
  }

  tbl <- plot_grid(
    plotlist = tbl_list,
    ncol = 1
  )
  return(tbl)
}
