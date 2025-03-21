#####example for combo plot control####
install.packages("pam_1.0.0.tar.gz", repos = NULL, type = "source")
library("pam")
library("ggplot2")

####raw data file directory####
data_path <- file.path(getwd(), "data", "20231122_01.csv")
data <- read_dual_pam_data(data_path)
output_dir <- file.path(getwd(), "output")

####getting model results####
model_result_eilers_peeters_ETR_II_modified <- eilers_peeters_modified(eilers_peeters_generate_regression_ETR_II(data))
model_result_platt_ETR_II_modified <- platt_modified(platt_generate_regression_ETR_II(data))
model_result_vollenweider_ETR_II_modified <- vollenweider_modified(vollenweider_generate_regression_ETR_II(data))
model_result_walsby_ETR_II_modified <- walsby_modified(walsby_generate_regression_ETR_II(data))

####combo_plot_control####
model_results <- list(model_result_eilers_peeters_ETR_II_modified, model_result_platt_ETR_II_modified, model_result_vollenweider_ETR_II_modified, model_result_walsby_ETR_II_modified)
name_list <- list("Eilers & Peeters", "Platt", "Vollenweider", "Walsby")
color_list <- list("blue", "red", "green", "orange")

combo_plot_control <- combo_plot_control("combo_plot_control_20231122_01.csv", data, model_results, name_list, color_list)
print(combo_plot_control)
ggsave("combo_plot_control_20231122_01.jpg", plot = combo_plot_control, width = 10, height = 17, path = output_dir)
####