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
 if(!is.data.table(tbl_all)){
   stop("tbl_all is not a data.table paramter")
 }

  result = data.table()
  unique_files = unique(unlist(tbl_all[["filename"]]))
  for (filen in unique_files) {
    file_data = tbl_all[ID == "SP"]
    file_data = file_data[filename==filen]
    file_data = file_data[, DateTime := as.POSIXct(paste(Date, Time, sep = " "), tz = "GMT", "%d.%m.%y %H:%M:%S")]
    file_data = file_data[order(DateTime)]

    lastPar = as.numeric(0)
    for(i in 1:nrow(file_data)) {
      row = file_data[i, ]
      row = row[, PAR := as.numeric(PAR)]
      currentPar = row[,PAR]
      if (lastPar != 0 & currentPar < lastPar){
        file_data = file_data[!i,]
        break
      }
      lastPar = currentPar
      result = rbind(result, row)
    }


  }
  return(result)
}

get_data_I <- function(tbl, etr_factor = 0.84, p_ratio = 0.5){
  return(get_data(tbl,"Y.I.", "Pm.-Det.", etr_factor, p_ratio))
}

get_data_II <- function(tbl, etr_factor = 0.84, p_ratio = 0.5){
  return(get_data(tbl,"Y.II.", "Fm-Det.", etr_factor, p_ratio))
}

get_data <- function(tbl, yield_col_name, det_action_to_use, etr_factor, p_ratio, action_to_use = "P.+F. SP"){
  if(is.null(tbl) | !is.data.table(tbl)){
    stop("tbl paramter is not valid")
  }

  if (! eval(yield_col_name) %in% colnames(tbl)){
    stop(glue("required col '{yield_col_name}' not found"))
  }

  if (! "DateTime" %in% colnames(tbl)){
    stop(glue("required col 'DateTime' not found"))
  }

  if (! "filename" %in% colnames(tbl)){
    stop(glue("required col 'filename' not found"))
  }

  if (! "PAR" %in% colnames(tbl)){
    stop(glue("required col 'PAR' not found"))
  }

  if (! "Action" %in% colnames(tbl)){
    stop(glue("required col 'action' not found"))
  }

  tbl = tbl[Action == eval(action_to_use) | Action == eval(det_action_to_use),]
  result = data.table()
  result = cbind(result, date_time=tbl[,DateTime])
  result = cbind(result, file_name=tbl[,filename])
  result = cbind(result, action=tbl[,Action])
  result = cbind(result, par=tbl[,PAR])
  result = cbind(result, value=tbl[,get(yield_col_name)])
  result = result[, par := as.numeric(par)]
  result = result[, value := as.numeric(value)]

  etr_values = list()
  for (row_index in 1:nrow(result)) {
    row = result[row_index]
    if (row[,action] == eval(det_action_to_use) && row[,par] != 0){
      stop("PAR at Det. is not 0. Check raw data! ", toString(row))
    }
    print(etr_factor)
    print(p_ratio)
    print("================")
    etr = as.numeric(row[,value]) * as.numeric(row[,par]) * as.numeric(etr_factor) * as.numeric(p_ratio)
    etr_values = c(etr_values, etr)
  }

  print(etr_values)
  result = cbind(result, etr=etr_values)
  return(result)
}



