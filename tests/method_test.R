#install.packages("../pam", repos = NULL, type="source")
#library(pam)
library(data.table)
# Set the relative path to your data directory
data_directory <- "data"

# Combine the current directory with the data directory to get the full path
full_data_directory <- file.path(getwd(), data_directory)

# Import data
tbl_all <- import_data(full_data_directory)
cleaned_data = par_cleaner(tbl_all)
yield_I = get_data_I(cleaned_data)
yield_II = get_data_II(cleaned_data)
