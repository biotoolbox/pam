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
for (file in csv_files) {
  title <- basename(file)
  data <- read_pam_data(file)
  png(glue("{title}_combo_plot.png"))
  print(plot_combo(title, data, TRUE))
  dev.off()
  # stop("a")
}

# print("asdfg")
# View(create_regression_data_table_eilers_peeters(csv_files, FALSE))
# write.csv(create_regression_data_table_eilers_peeters(csv_files, FALSE), "keke.csv")

test1 <- function(files) {
  # pdf("plots.pdf", onefile = TRUE)
  # Iterate over each .csv file
  for (file in csv_files) {
    title <- basename(file)
    data <- read_pam_data(file)
    # View(data)
    # print(plot_control_raw(data, basename(file), TRUE))

    reg_data_eilers_peeters <- generate_regression_eilers_peeters(data, FALSE)
    print("reg_data eilers_peeters: ")
    print(reg_data_eilers_peeters)
    View(reg_data_eilers_peeters)

    reg_data_walsby <- generate_regression_walsby(data, FALSE)
    print("reg_data_walsby: ")
    print(reg_data_walsby)

    reg_data_platt <- generate_regression_platt(data, FALSE)
    print("reg_data_platt: ")
    print(reg_data_platt)

    # View(create_regression_data_table(reg_data_eilers_peeters, reg_data_walsby, reg_data_platt))

    print(min(c(reg_data_eilers_peeters$sdiff, reg_data_walsby$sdiff, reg_data_platt$sdiff)))

    png(glue("{title}_eilers_peeters.png"))
    print(plot_control_eilers_peeters(data, reg_data_eilers_peeters, title, FALSE))
    dev.off()

    png(glue("{title}_walsby.png"))
    print(plot_control_walsby(data, reg_data_walsby, title, FALSE))
    dev.off()

    png(glue("{title}_platt.png"))
    print(plot_control_platt(data, reg_data_platt, title, FALSE))
    dev.off()

    stop("a")
  }
}
