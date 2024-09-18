devtools::install(".")
library(pam)

csv_files <- list.files(
  file.path(getwd(), "data"),
  pattern = ".csv",
  full.names = TRUE
)


data <- read_pam_data(csv_path = csv_files[1])
data_vollenweider <- generate_regression_vollenweider(data, FALSE)
View(data_vollenweider)

