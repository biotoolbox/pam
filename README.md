# library 'pam'

## Introduction

The library ‘pam’ was developed to process PAM raw data (chlorophyll fluorometry to analyze photosystem II and dual wavelength absorbance spectrometry to analyze photosystem I), for example from the DUAL PAM of the manufacturer WALZ. 
Four different models are provided for the regression of the light curve (Vollenweider (1965), Platt (1980), Eilers and Peeters (1988) and Walsby (1997)).
To select the most suitable model for the respective data set, the models can be compared with each other. To avoid confusion in the naming of the variables and calculated factors such as ETRmax, it is possible to output both with publication-compliant naming and with homogenised naming.
Generated control plots make it possible to check each individual regression fit.

## Functions

### read_dual_pam_data()

#### Description
This function reads the original CSV file as created by the DualPAM software, processes it by calculating ETR values, and returns a cleaned dataset.
Functionality with raw data from other PAM devices cannot be guaranteed.
Individual customisation may be necessary when reading data. 

#### Parameters
- **csv_path**: A string representing the file path to the CSV file.
- **remove_recovery**: Automatic removal of recovery measurements after the actual Pi curve for an accurate regression. Default is `TRUE`.
- **etr_factor**: A numeric value used as a factor for calculating ETR. Default is `0.84`.
- **p_ratio**: A numeric value representing the ratio of PS II / PSI used in the ETR calculation formula. Default is `0.5`.  
  $$P\text{-Ratio} = \frac{\text{PPS2}}{\text{PPS1+2}}$$

#### Details
ETR values are calculated using the following formula:

$$ETR = \text{PAR} \cdot \text{ETR-Factor} \cdot \text{P-Ratio} \cdot \text{Yield}$$


The function processes the provided CSV file by:
- Reading the CSV data using `read.csv()`.
- Filtering rows where the column `ID` equals "SP".
- Combining the `Date` and `Time` columns to create a new `DateTime` column.
- Calculating the ETR values for both `Y.I.` and `Y.II.` using the function `calc_etr()`.
- Removing rows after the recovery period if `remove_recovery = TRUE`.

#### Return
A `data.table` containing the processed data with additional columns for recalculated ETR values.

#### Example
```r
data <- read_dual_pam_data("path/to/data.csv",
remove_recovery = TRUE,
etr_factor = 0.84,
p_ratio = 0.5)
```

#### References
- Heinz Walz GmbH. (2024). *DUAL-PAM-100 DUAL-PAM/F MANUAL, 5th Edition, April 2024, Chapter 7 (pp. 162-172).* Heinz Walz GmbH, Effeltrich, Germany. Available at: [DUAL-PAM-100 Manual](https://www.walz.com/files/downloads/manuals/dual-pam-100/DualPamEd05.pdf)


### vollenweider_generate_regression_ETR_I and vollenweider_generate_regression_ETR_II

This function generates a regression model based on Vollenweider (1965). Original naming conventions from the publication are used.

#### Parameters
- **data**: A `data.table` containing the input data from `read_dual_pam_data`.
- **etr_type**: A character string specifying the column name of the response variable (in this case: ETR I) to be used in the model.
- **pmax_start_value_vollenweider**: Numeric. The starting value for the parameter $$p_{max}$$ in the model. Defaults to `pmax_start_values_vollenweider_default`.
- **a_start_value_vollenweider**: Numeric. The starting value for the parameter $$a$$ in the model. Defaults to `a_start_values_vollenweider_default`.
- **alpha_start_value**: Numeric. The starting value for the parameter $$\alpha$$ in the model. Defaults to `alpha_start_values_vollenweider_default`.
- **n_start_value**: Numeric. The starting value for the parameter $$n$$ in the model. Defaults to `n_start_values_vollenweider_default`.

#### Return
A list containing the following elements:
- **etr_regression_data**: A `data.table` with the predicted values of ETR I to each PAR based on the fitted model.
- **sdiff**: The deviation between the actual and predicted ETR values.
- **pmax**: The maximum electron transport rate without photoinhibition.
- **a**: The obtained parameter $$a$$.
- **alpha**: The obtained parameter $$\alpha$$.
- **n**: The obtained parameter $$n$$.
- **popt**: The maximum electron transport rate with photoinhibition. A function computes predicted photosynthetic rates for each PAR value and tracks the maximum rate observed.
- **ik**: PAR where the transition point from light limitation to light saturation is achieved without photoinhibition, calculated as $$i_k = \frac{1}{a}$$.
- **iik**: PAR where the transition point from light limitation to light saturation is achieved with photoinhibition, calculated as $$i_{ik} = \frac{i_k \cdot popt}{pmax}$$.
- **pmax_popt_and_ik_iik_ratio**: Ratio of $$p_{max}$$ to $$popt$$ and $$i_k$$ to $$i_{ik}$$, calculated as $$pmax\_popt\_and\_ik\_iik\_ratio = \frac{i_k}{i_{ik}}$$.

#### Details
This function uses non-linear least squares fitting to estimate the parameters for the Vollenweider model, which describes the relationship between PAR and ETR. The model used is:

$$p = p_{max} \cdot \frac{a \cdot i}{\sqrt{1 + (a \cdot i)^2}} \cdot \frac{1}{\left(\sqrt{1 + (\alpha \cdot i)^2}\right)^n}$$


It is valid: $$i = PAR; p = ETR$$

#### Example
```r
result_vollenweider_ETR_II <- vollenweider_generate_regression_ETR_II(data, 
    pmax_start_value_vollenweider = 40, 
    a_start_value_vollenweider = 0.1, 
    alpha_start_value = -0.0001, 
    n_start_value = 350)
```

#### References
Vollenweider, R. A. (1965). *Calculation models of photosynthesis-depth curves and some implications regarding day rate estimates in primary production measurements*, p. 427-457. In C. R. Goldman [ed.], *Primary Productivity in Aquatic Environments*. Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.

### platt_generate_regression_ETR_I and platt_generate_regression_ETR_II

This function generates a regression model based on  Platt (1980). Original naming conventions from the publication are used.

#### Parameters
- **data**: A `data.table` containing the input data from `read_dual_pam_data`.
- **alpha_start_value_platt**: Numeric. The starting value for the parameter $$\alpha$$ in the model. Defaults to `alpha_start_value_platt_default`.
- **beta_start_value_platt**: Numeric. The starting value for the parameter $$\beta$$ in the model. Defaults to `beta_start_value_platt_default`.
- **ps_start_value_platt**: Numeric. The starting value for the parameter $$p_s$$ in the model. Defaults to `ps_start_value_platt_default`.

#### Return
A list containing the following elements:
- **etr_regression_data**: A `data.table` with the predicted values of ETR I to each PAR based on the fitted model.
- **sdiff**: The deviation between the actual and predicted ETR values.
- **ps**: The maximum electron transport rate without photoinhibition.
- **alpha**: The initial slope of the light curve.
- **beta**: The photoinhibition of the light curve.
- **pm**: The maximum electron transport rate with photoinhibition, calculated as $$pm = p_s \cdot \left(\frac{\alpha}{\alpha + \beta}\right) \cdot \left(\left(\frac{\beta}{\alpha + \beta}\right)^{\frac{\beta}{\alpha}}\right)$$.
- **ik**: PAR where the transition point from light limitation to light saturation is achieved with photoinhibition, calculated as $$ik = \frac{pm}{\alpha}$$.
- **is**: PAR where the transition point from light limitation to light saturation is achieved without photoinhibition, calculated as $$is = \frac{p_s}{\alpha}$$.
- **im**: The PAR at which the maximum electron transport rate is achieved with photoinhibition, calculated as $$im = \left(\frac{p_s}{\alpha}\right) \cdot \log\left(\frac{\alpha + \beta}{\beta}\right)$$.
- **ib**: Calculated as $$ib = \frac{p_s}{\beta}$$.

#### Details
This function uses non-linear least squares fitting to estimate the parameters for the Platt model, which describes the relationship between PAR and ETR I. The model used is:

$$p = p_s \cdot \left(1 - \frac{e^{-\alpha I}}{p_s}\right) \cdot \left(\frac{e^{-\beta I}}{p_s}\right)$$

It is valid: $$I = PAR; p = ETR$$

#### Example
```r
result_platt_ETR_II <- platt_generate_regression_ETR_II(data, 
    alpha_start_value_platt = 0.3, 
    beta_start_value_platt = 0.01, 
    ps_start_value_platt = 30)
```

#### References
Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). *Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton*. Journal of Marine Research, 38(4). Retrieved from https://elischolar.library.yale.edu/journal_of_marine_research/1525.


### eilers_peeters_generate_regression_ETR_I() and eilers_peeters_generate_regression_ETR_II()

This function generates a regression model based on  Eilers-Peeters (1988). Original naming conventions from the publication are used. All parameters are calculated taking photoinhibition into account.

#### Parameters
- **data**: A `data.table` containing the input data from `read_dual_pam_data`.
- **a_start_value**: Numeric. The starting value for the parameter $$a$$ in the model. Defaults to `a_start_values_eilers_peeters_default`.
- **b_start_value**: Numeric. The starting value for the parameter $$b$$ in the model. Defaults to `b_start_values_eilers_peeters_default`.
- **c_start_value**: Numeric. The starting value for the parameter $$c$$ in the model. Defaults to `c_start_values_eilers_peeters_default`.

#### Return
A list containing the following elements:
- **etr_regression_data**: A `data.table` with the predicted values of ETR I to each PAR based on the fitted model.
- **sdiff**: The deviation between the actual and predicted ETR values.
- **a**: The obtained parameter $$a$$.
- **b**: The obtained parameter $$b$$.
- **c**: The obtained parameter $$c$$.
- **pm**: The maximum electron transport rate, calculated as $$pm = \frac{1}{b + 2 \sqrt{a \cdot c}}$$.
- **s**: The initial slope of the light curve, calculated as $$s = \frac{1}{c}$$.
- **ik**: PAR where the transition point from light limitation to light saturation is achieved, calculated as $$ik = \frac{c}{b + 2 \sqrt{a \cdot c}}$$.
- **im**: The PAR at which the maximum electron transport rate is achieved, calculated as $$im = \sqrt{\frac{c}{a}}$$.
- **w**: The sharpness of the peak, calculated as $$w = \frac{b}{\sqrt{a \cdot c}}$$.

#### Details
This function uses non-linear least squares fitting to estimate the parameters for the Eilers-Peeters model, which describes the relationship between PAR and ETR. The model used is:

$$ p = \frac{I}{a \cdot I^2 + b \cdot I + c} $$

It is valid: $$I = PAR$$; $$p = ETR$$

#### Example
```r
result_eilers_peeters_ETR_II <- eilers_peeters_generate_regression_ETR_II(data,
a_start_value = 0.00004,
b_start_value =  0.004,
c_start_value = 5)
```

#### References
Eilers, P. H. C., & Peeters, J. C. H. (1988). *A model for the relationship between light intensity and the rate of photosynthesis in phytoplankton.* Ecological Modelling, 42(3-4), 199-215. [doi:10.1016/0304-3800(88)90057-9](https://doi.org/10.1016/0304-3800(88)90057-9).

### walsby_generate_regression_ETR_I and walsby_generate_regression_ETR_II

This function generates a regression model based on  Walsby (1997) in a modified version without the respiration term. Naming conventions from Romoth (2019) are used. ETRmax is calculated without taking photoinhibition into account.

#### Parameters
- **data**: A `data.table` containing the input data from `read_dual_pam_data`.
- **etr_max_start_value_walsby**: Numeric. The starting value for the parameter $$etr_{max}$$ in the model. Defaults to `etr_max_start_value_walsby_default`.
- **alpha_start_value_walsby**: Numeric. The starting value for the parameter $$\alpha$$ in the model. Defaults to `alpha_start_value_walsby_default`.
- **beta_start_value_walsby**: Numeric. The starting value for the parameter $$\beta$$ in the model. Defaults to `beta_start_value_walsby_default`.

#### Return
A list containing the following elements:
- **etr_regression_data**: A `data.table` with the predicted values of ETR to each PAR based on the fitted model.
- **sdiff**: The deviation between the actual and predicted ETR values.
- **etr_max**: The maximum electron transport rate without photoinhibition.
- **alpha**: The initial slope of the light curve.
- **beta**: The photoinhibition of the light curve.

#### Details
This function uses non-linear least squares fitting to estimate the parameters for the Walsby model, which describes the relationship between PAR and ETR I. The model used is:

$$ETR = etr_{max} \cdot \left(1 - e^{\left(-\frac{\alpha \cdot I}{etr_{max}}\right)}\right) + \beta \cdot I$$

It is valid: $$I = PAR$$

#### References
Walsby, A. E. (1997). Numerical integration of phytoplankton photosynthesis through time and depth in a water column. *New Phytologist*, 136(2), 189-209. https://doi.org/10.1046/j.1469-8137.1997.00736.x


Romoth, K., Nowak, P., Kempke, D., Dietrich, A., Porsche, C., & Schubert, H. (2019). Acclimation limits of *Fucus evanescens* along the salinity gradient of the southwestern Baltic Sea. *Botanica Marina*, 62(1), 1-12. https://doi.org/10.1515/bot-2018-0098

### compare_regression_models_ETR_I and compare_regression_models_ETR_II

This function compares different regression models.

#### Parameters
- **data_dir**: A character string specifying the directory where the input data files are located.

#### Return
A vector containing the total points assigned to each regression model based on their performance. Models are ranked based on the calculated deviation of the difference between observed and predicted values. Rating: 
- 1st: 3 points
- 2nd: 2 points
- 3rd: 1 point
- 4th: 0 points

#### Details
This function allows a straightforward comparison of the models: Eilers-Peeters (1988), Platt (1980), Vollenweider (1965), and Walsby (1997). The results can guide users in selecting the most appropriate model for their data. If regression is not possible for a model, no points are awarded for the file for any of the models. Start values cannot be adjusted in this function.

#### Example
```r
#raw data file directory
data_dir_compare <- file.path(getwd(), "data")

#compare regression models
compare_regression_models_ETR_II <- compare_regression_models_ETR_II(data_dir_compare)
print(compare_regression_models_ETR_II)
```

#### References
Eilers, P. H. C., & Peeters, J. C. H. (1988). *A model for the relationship between light intensity and the rate of photosynthesis in phytoplankton.* Ecological Modelling, 42(3-4), 199-215. [doi:10.1016/0304-3800(88)90057-9](https://doi.org/10.1016/0304-3800(88)90057-9).

Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). *Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton*. Journal of Marine Research, 38(4). Retrieved from https://elischolar.library.yale.edu/journal_of_marine_research/1525.

Romoth, K., Nowak, P., Kempke, D., Dietrich, A., Porsche, C., & Schubert, H. (2019). Acclimation limits of *Fucus evanescens* along the salinity gradient of the southwestern Baltic Sea. *Botanica Marina*, 62(1), 1-12. https://doi.org/10.1515/bot-2018-0098

Vollenweider, R. A. (1965). *Calculation models of photosynthesis-depth curves and some implications regarding day rate estimates in primary production measurements*, p. 427-457. In C. R. Goldman [ed.], *Primary Productivity in Aquatic Environments*. Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.

Walsby, A. E. (1997). Numerical integration of phytoplankton photosynthesis through time and depth in a water column. *New Phytologist*, 136(2), 189-209. https://doi.org/10.1046/j.1469-8137.1997.00736.x

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
  model_result = model_result_eilers_peeters_ETR_II,
  title = "ETR II - Eilers-Peeters",
  color = "black"
)
print(plot_control_eilers_peeters_ETR_II)
```

### walsby_modified()

This function adds parameters that were not originally included in the Walsby (1997) model, but were introduced by other models. 

#### Parameters

- **model_result**: A list containing the results of the model that has been validated. It should include necessary components such as ETR data, parameters like `etr_max`, `alpha`, and `beta`.

#### Return

Returns a modified model result as a list containing:
- **etr_type**: Type of ETR (e.g., ETR I or ETR II).
- **etr_regression_data**: A `data.table` with the predicted values of ETR corresponding to each PAR based on the fitted model.
- **sdiff**: The deviation between the actual and predicted ETR values.
- **a**: The maximum electron transport rate considering photoinhibition, calculated from the regression data.
- **b**: The maximum electron transport rate without photoinhibition, extracted from the model result.
- **c**: The initial slope of the light curve (alpha).
- **d**: NA
- **alpha**: The initial slope of the light curve. 
- **beta**: The photoinhibition of the light curve.. Taken directly from the model.
- **etrmax_with_photoinhibition**: The maximum ETR considering photoinhibition, calculated as follows:
  - Extracted from the row with the highest predicted ETR value in `etr_regression_data`.
- **etrmax_without_photoinhibition**: The maximum electron transport rate without photoinhibition. Taken directly from the model.
- **ik_with_photoinhibition**: The light intensity at which photosynthesis is half of the maximum, calculated as $$ ik\_with\_photoinhibition = \frac{\text{etrmax\_with\_photoinhibition}}{\alpha}$$.
- **ik_without_photoinhibition**: The light intensity at which photosynthesis is half of the maximum without considering photoinhibition, calculated as $$ ik\_without\_photoinhibition = \frac{\text{etrmax\_without\_photoinhibition}}{\alpha}$$.
- **im_with_photoinhibition**: Light intensity at ETRmax, extracted from the `etr_max_row`.
- **w**: NA
- **ib**: NA
- **etrmax_with_without_ratio**: The ratio of maximum ETR with to without photoinhibition, calculated as $$ \text{etrmax\_with\_without\_ratio} = \frac{\text{etrmax\_without\_photoinhibition}}{\text{etrmax\_with\_photoinhibition}}$$.



#### Details

This function first validates the input model result, then extracts the necessary data for ETR regression and calculates the maximum ETR with and without photoinhibition. The resulting parameters are packaged into a modified model result for further analysis.

#### Example

```r
walsby_ETR_II_modified <- walsby_modified(model_result_walsby_ETR_II)
```
## Naming overview
### Publication-accurate naming and the respective homogenisation
same        |Eilers and Peeters |Platt    |Walsby          |Vollenweider    |
|-|-|-|-|-|
|a         |a     |ps     |etr_max         |pmax      |
|b         |b     |alpha    |alpha          |a       |
|c         |c     |beta    |beta          |alpha      |
|d         |NA     |NA     |NA           |n       |
|alpha        |s     |alpha    |alpha          |NA       |
|beta        |NA     |beta    |beta          |NA       |
|etrmax_with_photoinhibition  |pm     |pm     |NA           |popt      |
|etrmax_without_photoinhibition  |NA     |ps     |etr_max         |pmax      |
|ik_with_photoinhibition   |ik     |ik     |NA           |iik      |
|ik_without_photoinhibition   |NA     |is     |NA           |ik       |
|im_with_photoinhibition   |im     |im     |NA           |NA       |
|w         |w     |NA     |NA           |NA       |
|ib         |NA     |ib     |NA           |NA       |
|etrmax_with_without_ratio   |NA     |NA     |NA           |pmax_popt_and_ik_iik_ratio |
|sdiff        |sdiff    |sdiff    |sdiff          |sdiff      |

### Publication-accurate naming and the respective homogenisation with additional calculations not included in the paper
modified

|same        |Eilers and Peeters |Platt    |Walsby          |Vollenweider    |
|-|-|-|-|-|
|sdiff        |sdiff    |sdiff    |sdiff          |sdiff      |
|a         |a     |ps     |etr_max         |pmax      |
|b         |b     |alpha    |alpha          |a       |
|c         |c     |beta    |beta          |alpha      |
|d         |NA     |NA     |NA           |n       |
|alpha        |s     |alpha    |alpha          |real_alpha     |
|beta        |NA     |beta    |beta          |NA       |
|etrmax_with_photoinhibition  |pm     |pm     |etrmax_with_photoinhibition    |popt      |
|etrmax_without_photoinhibition  |NA     |ps     |etr_max         |pmax      |
|ik_with_photoinhibition   |ik     |ik     |ik_with_photoinhibition     |iik      |
|ik_without_photoinhibition   |NA     |is     |ik_without_photoinhibition     |ik       |
|im_with_photoinhibition   |im     |im     |im_with_photoinhibition     |im_with_photoinhibition |
|w         |w     |NA     |NA           |NA       |
|ib         |NA     |ib     |NA           |NA       |
|etrmax_with_without_ratio   |NA     |ps_pm_ratio  |etr_max_etrmax_with_photoinhibition_ratio |pmax_popt_and_ik_iik_ratio |

### write_model_result_csv
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







###

## Features

### wrapper pdf

- create test for data that produces NA

## waiting

- legende für kombidiagramm das mit den vielen linien

## Test

### test all

```
library(devtools);
devtools::test();
```

### test specific file

```
library(devtools);
devtools::load_all();
library(testthat);
test_file('$$path')"
```

## Linux dependencies for devtools

Ubuntu:
libxml2-dev
libssl-dev
libcurl4-openssl-dev
libfontconfig1-dev
libharfbuzz-dev
libfribidi-dev
libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev

Debian:
libxml2-dev libssl-dev libcurl4-openssl-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libjpeg-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev

install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("minpack.lm")
install.packages("SciViews")
install.packages("ggthemes")
install.packages("gridExtra")

