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
      count <- 0
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

#' @title Control Plot
#' @description This function creates a control plot for the used model based on the provided data and model results.
#'
#' @param data A `data.table` containing the original ETR and yield data for the plot.
#' @param model_result A list containing the fitting results of the used model and the calculated paramters (alpha, ik...).
#' @param title A character string that specifies the title of the plot.
#' @param color A color specification for the regression line in the plot.
#' 
#' @return A plot displaying the original ETR and Yield values and the regression data. A table below the plot shows the calculated data (alpha, ik...)
#'
#' @examples
#' plot_control_eilers_peeters <- plot_control(
#'   data = pam_data,
#'   model_result = model_result_eilers_peeters,
#'   title = "ETR II - Eilers-Peeters",
#'   color = "black"
#' )
#' print(plot_control_eilers_peeters)
#' @export
plot_control <- function(
    data,
    model_result,
    title,
    color) {
  library(ggplot2)
  library(ggthemes)

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

  tbl <- plot_table(model_result, 4)

  plot <- plot_grid(
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

#' @title Write result data to csv files
#'
#' @description This function writes the raw data, regression data, and model parameters 
#' into separate CSV files.
#'
#' @param dest_dir A character string specifying the directory where CSV files 
#' will be saved.
#' @param name A character string specifying the base name for the output files.
#' @param data A data frame containing the raw input data used in the model.
#' @param model_result A list containing the model results, including parameter 
#' values and regression data.
#'
#' @details 
#' Three CSV files are created:
#' \enumerate{
#'   \item \code{name_raw_data.csv}: contains the original raw data.
#'   \item \code{name_regression_data.csv}: contains the regression data with 
#'   predictions for ETR.
#'   \item \code{name_model_result.csv}: contains the parameter values from the 
#'   model results (excluding regression data).
#' }
#' 
#' @seealso 
#' \code{\link{create_modified_model_result}}, 
#' \code{\link{plot_control}}
#'
#' @examples
#' # Usage example for write_model_result_csv
#' write_model_result_csv(
#'   dest_dir = "output",
#'   name = "eilers_peeters_experiment_001",
#'   data = raw_data,
#'   model_result = model_result_eilers_peeters
#' )
#' @export
write_model_result_csv <- function(dest_dir, name, data, model_result) {
  data_dest <- file.path(dest_dir, paste(name, "_raw_data.csv"))
  regression_data_dest <- file.path(dest_dir, paste(name, "_regression_data.csv"))
  model_result_dest <- file.path(dest_dir, paste(name, "_model_result.csv"))

  write.csv(
    data,
    file = data_dest,
    quote = TRUE,
    row.names = FALSE
  )

  write.csv(
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
      setNames(
        list(
          c(model_result[[n]])
        ),
        c(n)
      )
    )

    df <- cbind(df, NewCol = entry)
  }

  write.csv(
    df,
    file = model_result_dest,
    quote = TRUE,
    row.names = FALSE
  )
}
