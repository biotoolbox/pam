# Function for importing data from .csv files
import_data <- function(directory) {
  # Get list of .csv files
  csv_files <- list.files(directory, pattern = ".csv", full.names = TRUE)

  # Check if .csv files are found
  if (length(csv_files) == 0) {
    stop("No .csv files found in the specified directory.")
  }

  # Initialize list to store data frames
  data_list <- list()

  # Iterate over each .csv file
  for (file in csv_files) {
    # Read data from file
    data <- read.csv2(file)

    # Add 'filename' column
    data$filename <- basename(file)

    # Append data frame to list
    data_list[[file]] <- data
  }

  # Combine all data frames into one
  tbl_all <- do.call(rbind, data_list)

  return(as.data.table(tbl_all))
}

par_cleaner <- function(tbl_all) {
  if (!is.data.table(tbl_all)) {
    stop("tbl_all is not a data.table paramter")
  }

  result <- data.table()
  unique_files <- unique(unlist(tbl_all[["filename"]]))
  for (filen in unique_files) {
    file_data <- tbl_all[ID == "SP"]
    file_data <- file_data[filename == filen]
    file_data <- file_data[, DateTime := as.POSIXct(paste(Date, Time, sep = " "), tz = "GMT", "%d.%m.%y %H:%M:%S")]
    file_data <- file_data[order(DateTime)]

    lastPar <- as.numeric(0)
    for (i in 1:nrow(file_data)) {
      row <- file_data[i, ]
      row <- row[, PAR := as.numeric(PAR)]
      currentPar <- row[, PAR]
      if (lastPar != 0 & currentPar < lastPar) {
        file_data <- file_data[!i, ]
        break
      }
      lastPar <- currentPar
      result <- rbind(result, row)
    }
  }
  return(result)
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
