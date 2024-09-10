validate_data <- function(data) {
  library(data.table)
  library(glue)

  if (is.null(data)) {
    stop("data is null")
  }

  if (!is.data.table(data)) {
    stop("data is not a valid data.table")
  }

  if (nrow(data) < 2) {
    stop("no data rows")
  }

  if (ncol(data) == 0) {
    stop("no cols in data")
  }

  if (!"ID" %in% colnames(data)) {
    stop(glue("required col 'ID' not found"))
  }

  if (!"PAR" %in% colnames(data)) {
    stop(glue("required col 'PAR' not found"))
  }

  if (!"Y.I." %in% colnames(data) && !"Y.II." %in% colnames(data)) {
    stop(glue("required col 'Y(I)' and 'Y(II)' not found"))
  }

  if (!"Action" %in% colnames(data)) {
    stop(glue("required col 'Action' not found"))
  }

  if (!"Date" %in% colnames(data)) {
    stop(glue("required col 'Date' not found"))
  }

  if (!"Time" %in% colnames(data)) {
    stop(glue("required col 'Time' not found"))
  }
}

read_pam_data <- function(
    csv_path,
    remove_recovery = TRUE,
    etr_factor = 0.84,
    p_ratio = 0.5) {
  library(data.table)
  library(glue)

  data <- read.csv(csv_path, sep = ";", dec = ".")
  data <- as.data.table(data)
  validate_data(data)

  data <- data[ID == "SP"]
  data <- data[, DateTime := as.POSIXct(paste(Date, Time, sep = " "), tz = "GMT", "%d.%m.%y %H:%M:%S")]
  data <- data[order(DateTime)]

  result <- data.table()
  lastPar <- as.numeric(0)
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    currentPar <- row$PAR
    if (!is.numeric(currentPar)) {
      stop(glue("PAR in row {i} is not numeric"))
    }

    yield_I <- row$Y.I.
    recalc_ETRI <- if (is.numeric(yield_I)) {
      calc_etr(yield_I, currentPar, etr_factor, p_ratio)
    } else {
      NA
    }

    row <- cbind(row, recalc_ETR.I. = recalc_ETRI)

    yield_II <- row$Y.II.
    recalc_ETRII <- if (is.numeric(yield_II)) {
      calc_etr(yield_II, currentPar, etr_factor, p_ratio)
    } else {
      NA
    }

    row <- cbind(row, recalc_ETR.II. = recalc_ETRII)

    if (remove_recovery && lastPar != 0 && currentPar < lastPar) {
      data <- data[!i, ]
      break
    }
    lastPar <- currentPar
    result <- rbind(result, row)
  }

  return(result)
}

calc_etr <- function(yield, par, etr_factor, p_ratio) {
  if (!is.numeric(yield)) {
    stop("yield is not numeric")
  }

  if (!is.numeric(par)) {
    stop("par is not numeric")
  }

  if (!is.numeric(etr_factor)) {
    stop("etr_factor is not numeric")
  }

  if (!is.numeric(p_ratio)) {
    stop("p_ratio is not numeric")
  }

  return(yield * par * etr_factor * p_ratio)
}





get_data_I <- function(tbl, etr_factor = 0.84, p_ratio = 0.5) {
  return(get_data(tbl, "Y.I.", "Pm.-Det.", etr_factor, p_ratio))
}

get_data_II <- function(tbl, etr_factor = 0.84, p_ratio = 0.5) {
  return(get_data(tbl, "Y.II.", "Fm-Det.", etr_factor, p_ratio))
}

get_data <- function(tbl, yield_col_name, det_action_to_use, etr_factor, p_ratio, action_to_use = "P.+F. SP") {
  if (is.null(tbl) | !is.data.table(tbl)) {
    stop("tbl paramter is not valid")
  }

  if (!eval(yield_col_name) %in% colnames(tbl)) {
    stop(glue("required col '{yield_col_name}' not found"))
  }

  if (!"DateTime" %in% colnames(tbl)) {
    stop(glue("required col 'DateTime' not found"))
  }

  if (!"filename" %in% colnames(tbl)) {
    stop(glue("required col 'filename' not found"))
  }

  if (!"PAR" %in% colnames(tbl)) {
    stop(glue("required col 'PAR' not found"))
  }

  if (!"Action" %in% colnames(tbl)) {
    stop(glue("required col 'action' not found"))
  }

  tbl <- tbl[Action == eval(action_to_use) | Action == eval(det_action_to_use), ]
  result <- data.table()
  result <- cbind(result, date_time = tbl[, DateTime])
  result <- cbind(result, file_name = tbl[, filename])
  result <- cbind(result, action = tbl[, Action])
  result <- cbind(result, par = tbl[, PAR])
  result <- cbind(result, value = tbl[, get(yield_col_name)])
  result <- result[, par := as.numeric(par)]
  result <- result[, value := as.numeric(value)]

  etr_values <- list()
  for (row_index in 1:nrow(result)) {
    row <- result[row_index]
    if (row[, action] == eval(det_action_to_use) && row[, par] != 0) {
      stop("PAR at Det. is not 0. Check raw data! ", toString(row))
    }
    etr <- as.numeric(row[, value]) * as.numeric(row[, par]) * as.numeric(etr_factor) * as.numeric(p_ratio)
    etr_values <- c(etr_values, etr)
  }

  result <- cbind(result, etr = etr_values)
  return(result)
}

plot_control_raw <- function(data, title, use_etr_I) {
  library(ggplot2)
  library(cowplot)

  validate_data(data)

  if (!is.logical(use_etr_I)) {
    stop("use_etr_I is not a valid bool")
  }

  etr_label <- expression(paste("ETR [", mu, "mol electrons"^{
    -2
  } ~ "s"^{
    -1
  } ~ "]"))
  par_label <- expression(paste("PAR [", mu, "mol photons m"^{
    -2
  } ~ "s"^{
    -1
  } ~ "]"))

  # Create a list to store individual plots
  plots <- list()

  etr_col_name <- if (use_etr_I) "recalc_ETR.I." else "recalc_ETR.II."
  if (!use_etr_I) {
    data <- data[Action != "Pm.-Det."]
  }

  # Create plot for ETR.II. by PAR and filename
  plot <- ggplot(data, aes(x = PAR, y = get(etr_col_name))) +
    geom_point() +
    labs(x = par_label, y = etr_label, title = eval(title)) +
    theme_minimal() # Adjust theme if needed

  # Add plot to the list of plots
  plots[[eval(title)]] <- plot

  # Save plots in a grid
  plot_grid <- cowplot::plot_grid(plotlist = plots, ncol = )

  # Print the plot grid to the PDF
  return(plot_grid)
}

plot_control <- function(tbl, file_name) {
  # Open PDF device to save plots
  pdf(eval(file_name))

  # Split unique filenames into groups of 6 for each page
  files <- unique(tbl$file_name)

  etr_label <- expression(paste("ETR [", mu, "mol electrons"^{
    -2
  } ~ "s"^{
    -1
  } ~ "]"))
  par_label <- expression(paste("PAR [", mu, "mol photons m"^{
    -2
  } ~ "s"^{
    -1
  } ~ "]"))

  # Loop through each group of filenames
  for (file in files) {
    # Create a list to store individual plots
    plots <- list()

    data <- tbl[file_name == eval(file), ]
    data$par <- as.numeric(data$par)
    data$etr <- as.numeric(data$etr)

    # Create plot for ETR.II. by PAR and filename
    plot <- ggplot(data, aes(x = par, y = etr)) +
      geom_point() +
      labs(x = par_label, y = etr_label, title = paste(file)) +
      theme_minimal() # Adjust theme if needed

    # Add plot to the list of plots
    plots[[file]] <- plot

    # Save plots in a grid
    plot_grid <- cowplot::plot_grid(plotlist = plots, ncol = )

    # Print the plot grid to the PDF
    print(plot_grid)
  }

  # Close the PDF device
  dev.off()
}

generate_regression_eilers_peeters <- function(tbl) {
  # Initialize lists to store filenames and coefficients
  filenames <- c()
  a_values <- c()
  b_values <- c()
  c_values <- c()

  # Loop through each unique filename
  for (filename in unique(tbl$file_name)) {
    # Subset the data for the current filename
    data_subset <- tbl[tbl$file_name == eval(filename), ]
    data_subset$par <- as.numeric(data_subset$par)
    data_subset$etr <- as.numeric(data_subset$etr)

    # TODO: remove tryCatch?????
    # Fit a nonlinear model to the data
    model <- tryCatch(
      {
        nls(etr ~ (par / (a * par^2 + b * par + c)),
          data = data_subset,
          start = list(a = 0.00004, b = 0.004, c = 5)
        ) # Initial parameter values
      },
      error = function(e) {
        stop(glue("regression failed for file '{filename}' with error {e}"))
      }
    )

    # Extract the coefficients
    coefficients <- coef(model)

    # Store the results
    filenames <- c(filenames, filename)
    a_values <- c(a_values, coefficients[["a"]])
    b_values <- c(b_values, coefficients[["b"]])
    c_values <- c(c_values, coefficients[["c"]])
  }

  # Create a data frame with filenames and coefficients
  results_df <- data.table(
    filename = filenames,
    a = a_values,
    b = b_values,
    c = c_values
  )

  # Print the results data frame
  return(results_df)
}

plot_control_eilers_peeters <- function(tbl, regression, file_name) {
  # Open PDF device to save plots
  pdf(eval(file_name))

  # Split unique filenames into groups of 6 for each page
  files <- unique(tbl$file_name)

  etr_label <- expression(paste("ETR [", mu, "mol electrons"^{
    -2
  } ~ "s"^{
    -1
  } ~ "]"))
  par_label <- expression(paste("PAR [", mu, "mol photons m"^{
    -2
  } ~ "s"^{
    -1
  } ~ "]"))

  # Loop through each group of filenames
  for (file in files) {
    # Create a list to store individual plots
    plots <- list()

    data <- tbl[file_name == eval(file), ]
    data$par <- as.numeric(data$par)
    data$etr <- as.numeric(data$etr)

    data <- cbind(data, a = regression[filename == eval(file), a])
    data <- cbind(data, b = regression[filename == eval(file), b])
    data <- cbind(data, c = regression[filename == eval(file), c])

    # Calculate predicted ETR.II. values using the regression equation
    data$prediction <- with(data, par / ((a * par^2) + (b * par) + c))

    # Create plot for ETR.II. by PAR and filename
    plot <- ggplot(data, aes(x = par, y = etr)) +
      geom_point() +
      geom_line(aes(y = prediction), color = "blue") +
      labs(x = par_label, y = etr_label, title = paste(file)) +
      theme_minimal() # Adjust theme if needed

    # Add plot to the list of plots
    plots[[file]] <- plot

    # Save plots in a grid
    plot_grid <- cowplot::plot_grid(plotlist = plots, ncol = )

    # Print the plot grid to the PDF
    print(plot_grid)
  }

  # Close the PDF device
  dev.off()
}
