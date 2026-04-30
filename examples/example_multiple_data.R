# example for processing multiple files with the eilers and peeters model
install.packages("pam")
library("pam")

script_dir <- file.path(getwd(), "examples")
data_dir <- file.path(script_dir, "data", "bulk")
output_dir <- file.path(script_dir, "output")
dir.create(output_dir, showWarnings = FALSE)
output_path_pdf <- file.path(output_dir, "eilers_peters_plot_control.pdf")
reload_data_dir <- output_dir

pdf(output_path_pdf, onefile = TRUE)
csv_files <-
  list.files(
    path = data_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )
for (file in csv_files) {
  file_name <- basename(file)
  cat("Processing file:", file_name, "\n")

  # reading raw data csv
  intermediate_table <- read_dual_pam_data(file)

  # generating regression model result
  model_result <-
    eilers_peeters_generate_regression_ETR_II(intermediate_table)

  # generating control plot
  plot <- plot_control(
    data = intermediate_table,
    model_result = model_result,
    title = file_name,
    color = "black"
  )
  print(plot)

  # exporting intermediate table and model result
  write_model_result_csv(
    output_dir,
    file_name,
    intermediate_table,
    model_result
  )
}
dev.off()
