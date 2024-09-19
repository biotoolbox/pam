# List of required packages
required_packages <- c("data.table", "ggplot2", "minpack.lm", "SciViews", "glue") 

# Check if each package is installed, if not, install it
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

# Load all the packages
lapply(required_packages, library, character.only = TRUE)

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

  reg_data <- generate_regression_vollenweider(data, FALSE)
  View(reg_data)

  title <- basename(file)
    png(glue("{title}_vollenweider.png"))
    print(plot_control_vollenweider(data, reg_data, title, FALSE))
    dev.off()
 