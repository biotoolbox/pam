##### example for compare regression models####
# install library pam
install.packages("remotes")
remotes::install_github("biotoolbox/pam", ref = "v2", subdir = "src")
library("pam")

#### read_dual_pam_data()####
# raw data file directory
script_dir <- dirname(sys.frame(1)$ofile)
data_dir <- file.path(script_dir, "data", "bulk")

#### compare_regression_models_ETR_II####
compare_regression_models_ETR_II_result <- compare_regression_models_ETR_II(data_dir, read_dual_pam_data)
print(compare_regression_models_ETR_II_result)
####

#### This warning is expected####
# file: 20231214_10.csv processed with warning: simpleWarning in value[[3L]](cond): warning while
# calculating platt model: simpleWarning in value[[3L]](cond): failed to calculate im: warning: simpleWarning in
# log((alpha + beta)/beta): NaNs wurden erzeugt
