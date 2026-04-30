# simple example for one file with eilers and peeters model
install.packages("pam")
library("pam")
library("ggplot2")

script_dir <- file.path(getwd(), "examples")
data_path <- file.path(script_dir, "data", "20231122_01.csv")
output_dir <- file.path(script_dir, "output")
dir.create(output_dir, showWarnings = FALSE)

# reading raw data csv
data <- read_dual_pam_data(data_path)

# generating model result
model_result <- eilers_peeters_generate_regression_ETR_II(data)

# modifying model result
model_result_modified <- eilers_peeters_modified(model_result)

# generating plot
plot <- plot_control(
  data,
  model_result_modified,
  "plot_control_eilers_peeters_ETR_II_modifed_20231122_01.jpg",
  color = "blue"
)
print(plot)

# exporting plot
ggsave(
  "20231122_01.jpg",
  plot = plot,
  path = output_dir,
  width = 10,
  height = 10
)

# exporting intermediate table and model result data
write_model_result_csv(
  output_dir,
  "20231122_01.csv",
  data,
  model_result_modified
)
