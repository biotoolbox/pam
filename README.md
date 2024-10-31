# library 'pam'

### Introduction

The library ‘pam’ was developed to process PAM raw data (chlorophyll fluorometry to analyze photosystem II and dual wavelength absorbance spectrometry to analyze photosystem I), for example from the DUAL PAM of the manufacturer WALZ. 
Four different models are provided for the regression of the light curve (Vollenweider (1965), Platt (1980), Eilers and Peeters (1988) and Walsby (1997)).
To select the most suitable model for the respective data set, the models can be compared with each other. To avoid confusion in the naming of the variables and calculated factors such as ETRmax, it is possible to output both with publication-compliant naming and with homogenised naming.
Generated control plots make it possible to check each individual fit.

### Functions

## read_dual_pam_data()

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

$$ETR = PAR \times \text{ETR-Factor} \times P\text{-Ratio} \times Y$$

The function processes the provided CSV file by:
- Reading the CSV data using `read.csv()`.
- Validating the data structure using `validate_data()`.
- Filtering rows where the column `ID` equals "SP".
- Combining the `Date` and `Time` columns to create a new `DateTime` column.
- Calculating the ETR values for both `Y.I.` and `Y.II.` using the function `calc_etr()`.
- Removing rows after the recovery period if `remove_recovery = TRUE`.

## Return
A `data.table` containing the processed data with additional columns for recalculated ETR values.

## Examples
```r
# Example usage:
result <- read_dual_pam_data("path/to/data.csv", remove_recovery = TRUE, etr_factor = 0.84, p_ratio = 0.5)


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
