
### write_model_result_csv()

This function exports the raw input data, regression data, and model parameters into separate CSV files for easy access and further analysis.

#### Parameters

- **dest_dir**: A character string specifying the directory where the CSV files will be saved.
- **name**: A character string specifying the base name for the output files.
- **data**: A data frame containing the raw input data used in the model.
- **model_result**: A list containing the model results, including parameter values and regression data.

#### Details

This function creates three CSV files:

1. **`name_raw_data.csv`**: Contains the original raw data used in the model.
2. **`name_regression_data.csv`**: Contains the regression data with predictions for electron transport rate (ETR).
3. **`name_model_result.csv`**: Contains the parameter values from the model results (excluding regression data), including parameters like `alpha`, `beta`, and `etr_max`.

Each file will be named using the `name` parameter as a prefix, followed by a specific suffix for clarity.

#### Examples

```r
write_model_result_csv(
  dest_dir = "output",
  name = "eilers_peeters_experiment_001",
  data = raw_data,
  model_result = model_result_eilers_peeters
)
```