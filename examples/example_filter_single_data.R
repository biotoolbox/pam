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

# filter for yield_2 values that are between 0.1 and 1
# note: this removes entire rows, so the corresponding yield_1 values
# (and all other columns) in those rows are dropped as well
data_filtered <- data[yield_2 >= 0.1 & yield_2 <= 1]

# filter for par values that are between 0 and 1000
#data_filtered <- data[par >= 0 & par <= 1000]

# generating model result
model_result <- eilers_peeters_generate_regression_ETR_II(data_filtered)

# modifying model result
model_result_modified <- eilers_peeters_modified(model_result)

# generating plot
plot <- plot_control(
  data_filtered,
  model_result_modified,
  "plot_control_eilers_peeters_ETR_II_modified_filtered_20231122_01.jpg",
  color = "blue"
)
print(plot)

# exporting plot
ggsave(
  "20231122_01_filtered.jpg",
  plot = plot,
  path = output_dir,
  width = 10,
  height = 10
)

# exporting intermediate table and model result data
write_model_result_csv(
  output_dir,
  "20231122_01_filtered.csv",
  data_filtered,
  model_result_modified
)