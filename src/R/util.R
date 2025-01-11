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
  validate_data(data)
  validate_etr_type(etr_type)

  if (etr_type == etr_I_type) {
    data <- dplyr::filter(data, data$Action != "Fm-Det.")
    if (length(data[data$Action == "Pm.-Det.", ]) == 0) {
      stop("Pm.-Det. is required but not present")
    }
  } else {
    data <- dplyr::filter(data, data$Action != "Pm.-Det.")
    if (length(data[data$Action == "Fm-Det.", ]) == 0) {
      stop("Fm-Det. is required but not present")
    }
  }

  return(data)
}

create_regression_data <- function(pars, predictions) {
  if (!is.vector(pars)) {
    stop("pars is not a valid vector")
  }

  if (!is.vector(predictions)) {
    stop("predictions is not a valid vector")
  }

  if (length(pars) != length(predictions)) {
    stop("pars and predictions need to be of the same length")
  }

  regression_data <- data.table::data.table(
    "PAR" = pars,
    "prediction" = predictions
  )
  return(regression_data)
}

get_etr_type_from_model_result <- function(model_result) {
  return(model_result[["etr_type"]])
}

get_etr_regression_data_from_model_result <- function(model_result) {
  return(model_result[["etr_regression_data"]])
}

get_sdiff_from_model_result <- function(model_result) {
  return(model_result[["sdiff"]])
}

plot_table <- function(model_result, entries_per_row) {
  validate_model_result(model_result)

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
      tbl_list[[row_count]] <- gridExtra::tableGrob(
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
    tbl_list[[row_count]] <- gridExtra::tableGrob(
      row,
      rows = NULL,
      theme = custom_theme
    )
  }

  tbl <- cowplot::plot_grid(
    plotlist = tbl_list,
    ncol = 1
  )
  return(tbl)
}

#' @title Plot Control
#' @description This function creates a control plot for the used model based on the provided data and model results.
#'
#' @param data A `data.table` containing the original ETR and yield data for the plot.
#' @param model_result A list containing the fitting results of the used model and the calculated paramters (alpha, ik...).
#' @param title A character string that specifies the title of the plot.
#' @param color A color specification for the regression line in the plot.
#'
#' @details
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#plot_control}
#'
#' @return A plot displaying the original ETR and Yield values and the regression data. A table below the plot shows the calculated data (alpha, ik...)
#'
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' result <- eilers_peeters_generate_regression_ETR_I(data)
#' plot_control(data, result, "Control Plot")
#'
#' @export
plot_control <- function(
    data,
    model_result,
    title,
    color = "black") {
  validate_data(data)
  validate_model_result(model_result)

  etr_type <- get_etr_type_from_model_result(model_result)
  validate_etr_type(etr_type)
  data <- remove_det_row_by_etr(data, etr_type)

  yield <- NA_real_
  if (etr_type == etr_I_type) {
    yield <- "Y.I."
  } else {
    yield <- "Y.II."
  }

  etr_regression_data <- get_etr_regression_data_from_model_result(model_result)
  validate_etr_regression_data(etr_regression_data)

  max_etr <- max(etr_regression_data$prediction)

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = data$PAR, y = get(etr_type))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(
      data = etr_regression_data,
      ggplot2::aes(
        x = etr_regression_data$PAR,
        y = etr_regression_data$prediction
      ),
      color = color
    ) +
    ggplot2::geom_point(data = data, ggplot2::aes(y = get(yield) * max_etr)) +
    ggplot2::geom_line(data = data, ggplot2::aes(y = get(yield) * max_etr)) +
    ggplot2::labs(x = par_label, y = etr_label, title = eval(title)) +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(~ . / max_etr, name = "Yield")
    ) +
    ggthemes::theme_base()

  tbl <- plot_table(model_result, 4)

  plot <- cowplot::plot_grid(
    plot,
    tbl,
    ncol = 1,
    rel_heights = c(0.7, 0.3)
  )
  return(plot)
}

create_modified_model_result <- function(
    etr_type,
    etr_regression_data,
    sdiff,
    a,
    b,
    c,
    d,
    alpha,
    beta,
    etrmax_with_photoinhibition,
    etrmax_without_photoinhibition,
    ik_with_photoinhibition,
    ik_without_photoinhibition,
    im_with_photoinhibition,
    w,
    ib,
    etrmax_with_without_ratio) {
  result <- list(
    etr_type = etr_type,
    etr_regression_data = etr_regression_data,
    sdiff = sdiff,
    a = a,
    b = b,
    c = c,
    d = d,
    alpha = alpha,
    beta = beta,
    etrmax_with_photoinhibition = etrmax_with_photoinhibition,
    etrmax_without_photoinhibition = etrmax_without_photoinhibition,
    ik_with_photoinhibition = ik_with_photoinhibition,
    ik_without_photoinhibition = ik_without_photoinhibition,
    im_with_photoinhibition = im_with_photoinhibition,
    w = w,
    ib = ib,
    etrmax_with_without_ratio = etrmax_with_without_ratio
  )
  validate_modified_model_result(result)
  return(result)
}

#' Write Model Result CSV
#' @description
#' This function exports the raw input data, regression data, and model parameters into separate CSV files for easy access and further analysis.
#'
#' @param dest_dir A character string specifying the directory where the CSV files will be saved.
#' @param name A character string specifying the base name for the output files.
#' @param data A data frame containing the raw input data used in the model.
#' @param model_result A list containing the model results, including parameter values and regression data.
#'
#' @details
#' This function generates three CSV files:
#' \enumerate{
#' \item \strong{raw_data.csv:} Contains the original raw data used in the model.
#' \item \strong{regression_data.csv:} Includes the regression data with predicted electron transport rate (ETR) values.
#' \item \strong{model_result.csv:} Summarizes the parameter values derived from the model results (excluding regression data), such as \code{alpha} or \code{beta}.
#' }
#' The `name` parameter serves as a prefix for each file, ensuring clarity and organization in the output directory.
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#write_model_result_csv}
#'
#' @return No return value, called for side effects
#'
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' result <- eilers_peeters_generate_regression_ETR_I(data)
#' write_model_result_csv(tempdir(), "20240925", data, result)
#'
#' @export
write_model_result_csv <- function(dest_dir, name, data, model_result) {
  data_dest <- file.path(dest_dir, paste(name, "_raw_data.csv", sep = ""))
  regression_data_dest <- file.path(dest_dir, paste(name, "_regression_data.csv", sep = ""))
  model_result_dest <- file.path(dest_dir, paste(name, "_model_result.csv", sep = ""))

  utils::write.csv(
    data,
    file = data_dest,
    quote = TRUE,
    row.names = FALSE
  )

  utils::write.csv(
    get_etr_regression_data_from_model_result(model_result),
    file = regression_data_dest,
    quote = TRUE,
    row.names = FALSE
  )

  df <- data.frame()
  df[1, ] <- NA

  for (n in names(model_result)) {
    if (n == "etr_regression_data" || n == "etr_type") {
      next()
    }

    entry <- data.frame(
      stats::setNames(
        list(
          c(model_result[[n]])
        ),
        c(n)
      )
    )

    df <- cbind(df, NewCol = entry)
  }

  utils::write.csv(
    df,
    file = model_result_dest,
    quote = TRUE,
    row.names = FALSE
  )
}
