#####simple example for one file with eilers and peeters model####
#install library pam
install.packages("pam_1.0.0.tar.gz", repos = NULL, type = "source")
library("pam")
library("ggplot2")

####read_dual_pam_data()####
data_path <- file.path(getwd(), "data", "20231122_01.csv")
data <- read_dual_pam_data(data_path)
output_dir <- file.path(getwd(), "output")

####eilers_peeters_generate_regression_ETR_II()####
model_result_eilers_peeters_ETR_II <- eilers_peeters_generate_regression_ETR_II(data)

####eilers_peeters_modified()####
model_result_eilers_peeters_ETR_II_modified <- eilers_peeters_modified(model_result_eilers_peeters_ETR_II)

####plot_control()####
plot_control_eilers_peeters_ETRII_modifed <- plot_control(data, model_result_eilers_peeters_ETR_II_modified, "plot_control_eilers_peeters_ETRII_modifed_20231122_01.jpg", color = "blue")
print(plot_control_eilers_peeters_ETRII_modifed)
ggsave("20231122_01.jpg", plot = plot_control_eilers_peeters_ETRII_modifed, path = output_dir, width = 10, height = 10)

####write_model_result_csv####
write_model_result_csv(output_dir, "20231122_01.csv", data, model_result_eilers_peeters_ETR_II_modified)
####