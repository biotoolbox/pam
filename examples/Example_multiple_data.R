##### example for multiple files with eilers and peeters model####
install.packages("remotes")
remotes::install_github("biotoolbox/pam", ref = "v2", subdir = "src")
library("pam")


#### raw data file directory####
script_dir <- dirname(sys.frame(1)$ofile)
data_dir <- file.path(script_dir, "data", "bulk")
output_dir <- file.path(script_dir, "output")
dir.create(output_dir, showWarnings = FALSE)
output_path_pdf <- file.path(output_dir, "eilers_peters_plot_control.pdf")
reload_data_dir <- output_dir

#### read_dual_pam_data()####
csv_files <-
  list.files(
    path = data_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )
data_list <- list()
for (file in csv_files) {
  data_list <- append(data_list, list(list(
    file_name = basename(file),
    data = read_dual_pam_data(file)
  )))
}

#### eilers_peeters_generate_regression_ETR_II()####
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

#### Function to create plots and save to PDF####
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


#### write_model_result_csv ####
for (result_entry in results_list) {
  file_name <- result_entry$file_name
  data <- result_entry$data
  model_result_eilers_peeters_ETR_II <- result_entry$model_result
  write_model_result_csv(
    output_dir,
    file_name,
    data,
    model_result_eilers_peeters_ETR_II
  )
  cat("Processed file:", file_name, "\n")
}
