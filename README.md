# library 'pam'

#### Introduction

The library ‘pam’ was developed to process PAM raw data (chlorophyll fluorometry to analyze photosystem II and dual wavelength absorbance spectrometry to analyze photosystem I), for example from the DUAL PAM of the manufacturer WALZ. 
Four different models are provided for the regression of the light curve (Vollenweider (1965), Platt (1980), Eilers and Peeters (1988) and Walsby (1997)).
To select the most suitable model for the respective data set, the models can be compared with each other. To avoid confusion in the naming of the variables and calculated factors such as ETRmax, it is possible to output both with publication-compliant naming and with homogenised naming.
Generated control plots make it possible to check each individual fit.

#### Functions

### read_dual_pam_data()

## Description
This function reads the original CSV file as created by the DualPAM software, processes it by calculating ETR values, and returns a cleaned dataset.

## Parameters
- **csv_path**: A string representing the file path to the CSV file.
- **remove_recovery**: Automatic removal of recovery measurements after the actual Pi curve for an accurate regression. Default is `TRUE`.
- **etr_factor**: A numeric value used as a factor for calculating ETR. Default is `0.84`.
- **p_ratio**: A numeric value representing the ratio of PS II / PSI used in the ETR calculation formula. Default is `0.5`.  
  $$P\text{-Ratio} = \frac{\text{PPS2}}{\text{PPS1+2}}$$

## Details
ETR values are calculated using the following formula:

$$ETR = \text{PAR} \cdot \text{ETR-Factor} \cdot \text{P-Ratio} \cdot \text{Yield}$$


The function processes the provided CSV file by:
- Reading the CSV data using `read.csv()`.
- Validating the data structure using `validate_data()`.
- Filtering rows where the column `ID` equals "SP".
- Combining the `Date` and `Time` columns to create a new `DateTime` column.
- Calculating the ETR values for both `Y.I.` and `Y.II.` using the function `calc_etr()`.
- Removing rows after the recovery period if `remove_recovery = TRUE`.

## Return
A `data.table` containing the processed data with additional columns for recalculated ETR values.

## Example
```r
data <- read_dual_pam_data("path/to/data.csv", remove_recovery = TRUE, etr_factor = 0.84, p_ratio = 0.5)
```

## References
- Heinz Walz GmbH. (2024). *DUAL-PAM-100 DUAL-PAM/F MANUAL, 5th Edition, April 2024, Chapter 7 (pp. 162-172).* Heinz Walz GmbH, Effeltrich, Germany. Available at: [DUAL-PAM-100 Manual](https://www.walz.com/files/downloads/manuals/dual-pam-100/DualPamEd05.pdf)


### eilers_peeters_generate_regression_ETR_I() and eilers_peeters_generate_regression_ETR_II()

This function generates a regression model based on the Eilers-Peeters formula. Original naming conventions from the publication are used. All parameters are calculated taking photoinhibition into account.

### Parameters
- **data**: A `data.table` containing the input data from `read_dual_pam_data`.
- **a_start_value**: Numeric. The starting value for the parameter $$a$$ in the model. Defaults to `a_start_values_eilers_peeters_default`.
- **b_start_value**: Numeric. The starting value for the parameter $$b$$ in the model. Defaults to `b_start_values_eilers_peeters_default`.
- **c_start_value**: Numeric. The starting value for the parameter $$c$$ in the model. Defaults to `c_start_values_eilers_peeters_default`.

### Return
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

### Details
This function uses non-linear least squares fitting to estimate the parameters for the Eilers-Peeters model, which describes the relationship between PAR and ETR. The model used is:

$$ p = \frac{I}{a \cdot I^2 + b \cdot I + c} $$

It is valid: $$I = PAR$$; $$p = ETR$$

## Example
```r
result_eilers_peeters_ETR_II <- eilers_peeters_generate_regression_ETR_II(data,
a_start_value = 0.00007,
b_start_value =  0.005,
c_start_value = 10)
```

## References
Eilers, P. H. C., & Peeters, J. C. H. (1988). *A model for the relationship between light intensity and the rate of photosynthesis in phytoplankton.* Ecological Modelling, 42(3-4), 199-215. [doi:10.1016/0304-3800(88)90057-9](https://doi.org/10.1016/0304-3800(88)90057-9).

### vollenweider_generate_regression_ETR_I and vollenweider_generate_regression_ETR_II

This function generates a regression model based on the Vollenweider formula. Original naming conventions from the publication are used.

## Parameters
- **data**: A `data.table` containing the input data from `read_dual_pam_data`.
- **etr_type**: A character string specifying the column name of the response variable (in this case: ETR I) to be used in the model.
- **pmax_start_value_vollenweider**: Numeric. The starting value for the parameter $$p_{max}$$ in the model. Defaults to `pmax_start_values_vollenweider_default`.
- **a_start_value_vollenweider**: Numeric. The starting value for the parameter $$a$$ in the model. Defaults to `a_start_values_vollenweider_default`.
- **alpha_start_value**: Numeric. The starting value for the parameter $$\alpha$$ in the model. Defaults to `alpha_start_values_vollenweider_default`.
- **n_start_value**: Numeric. The starting value for the parameter $$n$$ in the model. Defaults to `n_start_values_vollenweider_default`.

## Return
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

## Details
This function uses non-linear least squares fitting to estimate the parameters for the Vollenweider model, which describes the relationship between PAR and ETR. The model used is:

$$ p = p_{max} \cdot \left(\frac{a \cdot i}{\sqrt{1 + (a \cdot i)^2}} \cdot \frac{1}{\left(\sqrt{1 + (\alpha \cdot i)^2}\right)^n}\right) $$

It is valid: $$i = PAR; p = ETR$$

## Example
```r
result_vollenweider_ETR_II <- vollenweider_generate_regression(data, 
    pmax_start_value_vollenweider = 60, 
    a_start_value_vollenweider = 0.3, 
    alpha_start_value = -0.0003, 
    n_start_value = 400)
```

## References
Vollenweider, R. A. (1965). *Calculation models of photosynthesis-depth curves and some implications regarding day rate estimates in primary production measurements*, p. 427-457. In C. R. Goldman [ed.], *Primary Productivity in Aquatic Environments*. Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.








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

original

|same        |Eilers and Peeters |Platt    |Walsby          |Vollenweider    |
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
