# install.packages("../pam", repos = NULL, type="source")
# library(pam)
library(data.table)
# Set the relative path to your data directory
data_directory <- "data"

# Combine the current directory with the data directory to get the full path
full_data_directory <- file.path(getwd(), data_directory)

# Import data
tbl_all <- import_data(full_data_directory)
cleaned_data <- par_cleaner(tbl_all)
yield_I <- get_data_I(cleaned_data)
yield_II <- get_data_II(cleaned_data)
# View(yield_I)
# plot_control(yield_I, "testkekeke.pdf")
reg <- generate_regression_eilers_peeters(yield_II)
plot_control_eilers_peeters(yield_II, reg, "asdgfsdafasdfsadf.pdf")



# Initialize lists to store filenames, ETRmax values, and alpha values
filenames <- c()
ETRmax_values <- c()
alpha_values <- c()
Ik_values <- c()
Im_values <- c()

# Loop through each unique filename
for (filename in unique(tbl_abc_II$filename)) {
  # Subset the data for the current filename
  data_subset_II <- tbl_abc_II[tbl_abc_II$filename == filename, ]
  
  # Calculate ETRmax using the formula
  ETRmax <- 1 / (data_subset_II$b + 2 * sqrt(data_subset_II$a * data_subset_II$c))
  
  # Calculate alpha using the formula
  alpha <- 1 / data_subset_II$c
  
  # Calculate Ik using the formula
  Ik <- data_subset_II$c / (data_subset_II$b + 2 * sqrt(data_subset_II$a * data_subset_II$c))
  
  # Calculate Im using the formula
  Im <- sqrt(data_subset_II$c / data_subset_II$a)
  
  
  # Store the filename, ETRmax value, and alpha value
  filenames <- c(filenames, filename)
  ETRmax_values <- c(ETRmax_values, ETRmax)
  alpha_values <- c(alpha_values, alpha)
  Ik_values <- c(Ik_values, Ik)
  Im_values <- c(Im_values, Im)
}

# Create a data frame with filenames, ETRmax values, Im, Ik and alpha values
tbl_results_II <- data.frame(filename = filenames, ETRmax = ETRmax_values, alpha = alpha_values, Ik = Ik_values, Im = Im_values)
tbl_abc_II <- cbind(tbl_abc_II, Im = Im_values)