# example for comparing regression models
install.packages("pam")
library("pam")

script_dir <- file.path(getwd(), "examples")
data_dir <- file.path(script_dir, "data", "bulk")

result <- compare_regression_models_ETR_II(data_dir, read_dual_pam_data)
print(result)

# Those warnings are expected
# platt: failed to calculate im: warning: simpleWarning in log((alpha + beta)/beta): NaNs produced
# skipped file: 20231214_12.csv because of error: Error in value[[3L]](cond): error while calculating vollenweider model: Error in nlsModel(formula, mf, start, wts): singular gradient matrix at initial parameter estimates
# skipped file: 20231214_15.csv because of error: Error in value[[3L]](cond): error while calculating vollenweider model: Error in nlsModel(formula, mf, start, wts): singular gradient matrix at initial parameter estimates
