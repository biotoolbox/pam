# install.packages("../pam", repos = NULL, type="source")
# library(pam)
# library(data.table)
# Set the relative path to your data directory
# data_directory <- "data"

# Combine the current directory with the data directory to get the full path
# full_data_directory <- file.path(getwd(), data_directory)

# Import data
# tbl_all <- import_data(full_data_directory)
# cleaned_data <- par_cleaner(tbl_all)
# yield_I <- get_data_I(cleaned_data)
# yield_II <- get_data_II(cleaned_data)
# View(yield_I)
# plot_control(yield_I, "testkekeke.pdf")
# reg <- generate_regression_eilers_peeters(yield_II)
# plot_control_eilers_peeters(yield_II, reg, "asdgfsdafasdfsadf.pdf")

library(glue)
csv_files <- list.files(file.path(getwd(), "data"), pattern = ".csv", full.names = TRUE)

# pdf("plots.pdf", onefile = TRUE)
# Iterate over each .csv file
for (file in csv_files) {
  title <- basename(file)
  png(glue("{title}.png"))
  data <- read_pam_data(file)
  # View(data)
  # print(plot_control_raw(data, basename(file), TRUE))
  reg_data <- generate_regression_eilers_peeters(data, FALSE)
  View(reg_data)
  print(plot_control_eilers_peeters(data, reg_data, title, FALSE))
  dev.off()
  stop("a")
}
