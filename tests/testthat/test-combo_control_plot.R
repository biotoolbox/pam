test_that("test-combo_control_plot_etr_I 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")

  expect_no_error({
    data <- read_pam_data(test_data_file)

    print(
      combo_control_plot_etr_I(
        "test-combo_control_plot_etr_I 20231122_01_W3_T20_HL.csv",
        data
      )
    )
  })
})

test_that("test-combo_control_plot_etr_II 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")

  expect_no_error({
    data <- read_pam_data(test_data_file)

    print(
      combo_control_plot_etr_II(
        "test-combo_control_plot_etr_II 20231122_01_W3_T20_HL.csv",
        data
      )
    )
  })
})
