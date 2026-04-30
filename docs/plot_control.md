### plot_control()

This function creates a control plot for the used model based on the provided data and model results.

#### Parameters

- **data**: A `data.table` containing the original ETR and yield data for the plot.
- **model_result**: A list containing the fitting results of the used model and the calculated parameters (alpha, ik, etc.).
- **title**: A character string that specifies the title of the plot.
- **color**: A color specification for the regression line in the plot.

#### Return

A plot displaying the original ETR and Yield values and the regression data. A table below the plot shows the calculated data (alpha, ik, etc.).

#### Example

```r
plot_control_eilers_peeters_ETR_II <- plot_control(
  data = pam_data,
  model_result = modified_model_result_eilers_peeters_ETR_II,
  title = "eilers_peeters ETR II modified 20240925.csv",
  color = "purple"
)
print(plot_control_eilers_peeters_ETR_II)
```

![Plot](img/test-eilers_peeters_etr_II_modified_control_plot_20240925.jpg)

---

### combo_plot_control()

The `combo_plot_control()` function generates a combined plot of electron transport rate (ETR) data and regression model predictions, along with a customized table summarizing the parameters for each model.

#### Parameters

- **title**: A character string specifying the title for the plot.
- **data**: A data frame containing the raw input data for ETR and Photosynthetically Active Radiation (PAR).
- **model_results**: A list of model results, where each model result is a list containing regression data and parameters for ETR.
- **name_list**: A list of names corresponding to each model result. These names will be used in the legend and table.
- **color_list**: A list of color values for each model result. Colors are used to differentiate lines on the plot.

#### Return

A plot displaying the original ETR and Yield values and the regression data from different models. A table below the plot shows the calculated data (alpha, ik, etc.).

#### Examples

```r
test_data_file <- file.path(getwd(), "data", "dual_pam_data", "20240925.csv")
    data <- read_dual_pam_data(test_data_file)

    eilers_peeters <- eilers_peeters_modified(eilers_peeters_generate_regression_ETR_II(data))
    platt <- platt_modified(platt_generate_regression_ETR_II(data))
    walsby <- walsby_modified(walsby_generate_regression_ETR_II(data))
    vollenweider <- vollenweider_modified(vollenweider_generate_regression_ETR_II(data))

    plot <- combo_plot_control(
      "etr II test-combo_plot_control_20240925.csv",
      data,
      list(eilers_peeters, platt, walsby, vollenweider),
      list("eilers_peeters", "platt", "walsby", "vollenweider"),
      list("purple", "blue", "green", "red")
    )
```

![combo Plot](img/test_combo_plot_control_etr_II.jpg)