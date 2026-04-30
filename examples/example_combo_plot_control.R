# example for combo plot control
install.packages("pam")
library("pam")
library("ggplot2")

script_dir <- file.path(getwd(), "examples")
data_path <- file.path(script_dir, "data", "20231122_01.csv")
data <- read_dual_pam_data(data_path)
output_dir <- file.path(script_dir, "output")
dir.create(output_dir, showWarnings = FALSE)

# generating regression data
eilers_peeters <- eilers_peeters_generate_regression_ETR_II(data)
platt <- platt_generate_regression_ETR_II(data)
vollenweider <- vollenweider_generate_regression_ETR_II(data)
walsby <- walsby_generate_regression_ETR_II(data)

# modifying model results
eilers_peeters_modified <- eilers_peeters_modified(eilers_peeters)
platt_modified <- platt_modified(platt)
vollenweider_modified <- vollenweider_modified(vollenweider)
walsby_modified <- walsby_modified(walsby)

# creating combo control plot
model_results <- list(
  eilers_peeters_modified,
  platt_modified,
  vollenweider_modified,
  walsby_modified
)
name_list <- list("Eilers & Peeters", "Platt", "Vollenweider", "Walsby")
color_list <- list("blue", "red", "green", "orange")

combo_plot_control <- combo_plot_control(
  "combo_plot_control_20231122_01.csv",
  data,
  model_results,
  name_list,
  color_list
)
print(combo_plot_control)
ggsave(
  "combo_plot_control_20231122_01.jpg",
  plot = combo_plot_control,
  width = 10,
  height = 22,
  path = output_dir
)
