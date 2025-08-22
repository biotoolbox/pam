#####example for multiple files with eilers and peeters model####
install.packages("remotes")
remotes::install_github("biotoolbox/pam", subdir = "src")
library("pam")


####raw data file directory####
data_dir <- file.path(getwd(), "data")
output_dir <- file.path(getwd(), "output", "combinded")
output_path_pdf <- file.path(getwd(), "output", "eilers_peters_plot_control.pdf")
output_dir <- file.path(getwd(), "output")
reload_data_dir <- output_dir

####read_dual_pam_data()####
csv_files <-
  list.files(path = data_dir,
             pattern = "\\.csv$",
             full.names = TRUE)
data_list <- list()
for (file in csv_files) {
  data_list <- append(data_list, list(list(
    file_name = basename(file),
    data = read_dual_pam_data(file)
  )))
}

####eilers_peeters_generate_regression_ETR_II()####
results_list <- list()
for (data_entry in data_list) {
  model_result <-
    eilers_peeters_generate_regression_ETR_II(data_entry$data)
  results_list <- append(results_list, list(
    list(
      file_name = data_entry$file_name,
      data = data_entry$data,
      model_result = model_result
    )
  ))
}

####Function to create plots and save to PDF####
pdf(output_path_pdf, onefile = TRUE)
for (result_entry in results_list) {
  title <- result_entry$file_name
  data <- result_entry$data
  model_result <- result_entry$model_result
  plot <- plot_control(
    data = data,
    model_result = model_result,
    title = title,
    color = "black"
  )
  print(plot)
  cat("Processed file:", title, "\n")
}
dev.off()


####write_model_result_csv####
for (result_entry in results_list) {
  file_name <- result_entry$file_name
  data <- result_entry$data
  model_result_eilers_peeters_ETR_II <- result_entry$model_result
  write_model_result_csv(output_dir,
                         file_name,
                         data,
                         model_result_eilers_peeters_ETR_II)
  cat("Processed file:", file_name, "\n")
}

####combine_csv####
all_data <- list(
  raw_data = data.frame(),
  regression_data = data.frame(),
  model_result = data.frame()
)
for (source_dir in reload_data_dir) {
  csv_files <-
    list.files(path = source_dir,
               pattern = "\\.csv$",
               full.names = TRUE)
  for (csv_file in csv_files) {
    data <- read.csv(csv_file)
    data$file_name <- basename(csv_file)
    if (grepl("_raw_data.csv$", csv_file)) {
      all_data$raw_data <- rbind(all_data$raw_data, data)
    } else if (grepl("_regression_data.csv$", csv_file)) {
      all_data$regression_data <- rbind(all_data$regression_data, data)
    } else if (grepl("_model_result.csv$", csv_file)) {
      all_data$model_result <- rbind(all_data$model_result, data)
    }
  }
}
for (name in names(all_data)) {
  write.csv(all_data[[name]], file.path(output_dir, paste0("all_", name, ".csv")), row.names = FALSE)
}
cat("Data combined and saved to:", output_dir, "\n")

