## Those functions all read raw data CSV files, compute $$ETR$$ values, and return a processed intermediate table.

#### Parameters

- **csv_path**: A string representing the file path to the CSV file.
- **etr_factor**: A numeric value used as a factor for calculating ETR. Default is `0.84`.
- **fraction_photosystem_I**: A numeric value representing the relative distribution of absorbed PAR to photosystem I used in the ETR calculation formula. Default is `0.5`.
Calculated as: $$\textit{Fraction of Photosystem I} = \frac{PPS 1}{PPS 1+2}$$
- **fraction_photosystem_II**: A numeric value representing the relative distribution of absorbed PAR to photosystem II used in the ETR calculation formula. Default is `0.5`.
Calculated as: $$\textit{Fraction of Photosystem II} = \frac{PPS 2}{PPS 1+2}$$

#### Details

ETR values are calculated using the following formula:

$$ \textit{ETR (I or II)} = PAR \cdot \textit{ETR–Factor} \cdot \textit{Fraction of Photosystem (I or II)} \cdot \textit{Yield (I or II)} $$

The function processes the provided CSV file by:

- Reading the CSV data using `read.csv()`.
- Converting the data into a `data.table`.
- Validating the raw data structure with `validate_raw_intermediate_csv()`.
- Iterating through each row to calculate ETR values for both `yield_1` and `yield_2` using `calc_etr()`.

---

### read_universal_data()

#### Description

This function reads a universal CSV file, computes $$ETR$$ values, and returns a processed intermediate table.

#### Parameters

- **csv_path**: A string representing the file path to the CSV file.
- **etr_factor**: A numeric value used as a factor for calculating ETR. Default is `0.84`.
- **fraction_photosystem_I**: A numeric value representing the relative distribution of absorbed PAR to photosystem I used in the ETR calculation formula. Default is `0.5`.
Calculated as: $$\textit{Fraction of Photosystem I} = \frac{PPS 1}{PPS 1+2}$$
- **fraction_photosystem_II**: A numeric value representing the relative distribution of absorbed PAR to photosystem II used in the ETR calculation formula. Default is `0.5`.
Calculated as: $$\textit{Fraction of Photosystem II} = \frac{PPS 2}{PPS 1+2}$$

#### Details

ETR values are calculated using the following formula:

$$ \textit{ETR (I or II)} = PAR \cdot \textit{ETR–Factor} \cdot \textit{Fraction of Photosystem (I or II)} \cdot \textit{Yield (I or II)} $$

The function processes the provided CSV file by:

- Reading the CSV data using `read.csv()`.
- Converting the data into a `data.table`.
- Validating the raw data structure with `validate_raw_intermediate_csv()`.
- Iterating through each row to calculate ETR values for both `yield_1` and `yield_2` using `calc_etr()`.

#### Return

Returning a new table containing the original `par`, `yield_1`, `yield_2`, and the calculated `etr_1` and `etr_2` columns.

#### Example

```r
data <- read_dual_pam_data("path/to/data.csv",
etr_factor = 0.84,
fraction_photosystem_I = 0.5,
fraction_photosystem_II = 0.5)
```

#### References

- Heinz Walz GmbH. (2024). *DUAL-PAM-100 DUAL-PAM/F MANUAL, 5th Edition, April 2024, Chapter 7 (pp. 162-172).* Heinz Walz GmbH, Effeltrich, Germany. Available at: [DUAL-PAM-100 Manual](https://www.walz.com/files/downloads/dualpamed05.pdf)

---

### read_dual_pam_data()

#### Description

This function reads the original CSV file as created by the [DUAL-PAM-100](https://www.walz.com/products/dual-pam-100/) software, processes it by calculating $$ETR$$ values, and returns a cleaned dataset.

#### Parameters

- **csv_path**: A string representing the file path to the CSV file.
- **remove_recovery**: Automatic removal of recovery measurements after the actual Pi curve for an accurate regression. Default is `TRUE`.
- **etr_factor**: A numeric value used as a factor for calculating ETR. Default is `0.84`.
- **fraction_photosystem_I**: A numeric value representing the relative distribution of absorbed PAR to photosystem I used in the ETR calculation formula. Default is `0.5`.
Calculated as: $$\textit{Fraction of Photosystem I} = \frac{PPS 1}{PPS 1+2}$$
- **fraction_photosystem_II**: A numeric value representing the relative distribution of absorbed PAR to photosystem II used in the ETR calculation formula. Default is `0.5`.
Calculated as: $$\textit{Fraction of Photosystem II} = \frac{PPS 2}{PPS 1+2}$$

#### Details

ETR values are calculated using the following formula:

$$ \textit{ETR (I or II)} = PAR \cdot \textit{ETR–Factor} \cdot \textit{Fraction of Photosystem (I or II)} \cdot \textit{Yield (I or II)} $$

The function processes the provided CSV file by:

- Reading the CSV data using `read.csv()` and converting it to a `data.table`.
- Validating the raw Dual-PAM data with `validate_dual_pam_data()`.
- Filtering rows where the column `ID` equals `SP`
- Combining the `Date` and `Time` columns to create a `DateTime` column and ordering the data chronologically.
- Calculating initial ETR values from `Pm.-Det.` and `Fm-Det.` rows using `calc_etr()`.
- Iterating through all rows with `Action == "P.+F. SP"` to calculate ETR values for both `Y.I.` and `Y.II.`
- Stopping at the recovery period if `remove_recovery = TRUE`.


#### Return

- Returning a table containing `par`, `yield_1`, `yield_2`, and the calculated `etr_1` and `etr_2` columns.

#### Example

```r
data <- read_dual_pam_data("path/to/data.csv",
remove_recovery = TRUE,
etr_factor = 0.84,
fraction_photosystem_I = 0.5,
fraction_photosystem_II = 0.5)
```

#### References

- Heinz Walz GmbH. (2024). *DUAL-PAM-100 DUAL-PAM/F MANUAL, 5th Edition, April 2024, Chapter 7 (pp. 162-172).* Heinz Walz GmbH, Effeltrich, Germany. Available at: [DUAL-PAM-100 Manual](https://www.walz.com/files/downloads/dualpamed05.pdf)

---

### read_dual_pam_single_channel_p700_data()

#### Description

This function reads the original CSV file as created by the [DUAL-PAM-100](https://www.walz.com/products/dual-pam-100/) software in single channel mode (P700), processes it by calculating $$ETR$$ values for Photosystem I, and returns a cleaned dataset.

#### Parameters

- **csv_path**: A string representing the file path to the CSV file.  
- **remove_recovery**: Automatic removal of recovery measurements after the actual Pi curve for an accurate regression. Default is `TRUE`.  
- **etr_factor**: A numeric value used as a factor for calculating ETR. Default is `0.84`.  
- **fraction_photosystem_I**: A numeric value representing the relative distribution of absorbed PAR to Photosystem I used in the ETR calculation formula. Default is `0.5`.  
  Calculated as: $$\textit{Fraction of Photosystem I} = \frac{PPS 1}{PPS 1+2}$$  
- **fraction_photosystem_II**: A numeric value representing the relative distribution of absorbed PAR to Photosystem II. Default is `0.5`.  
  (Must sum with Photosystem I fraction to 1.)

#### Details

ETR values for Photosystem I are calculated using the following formula:

$$ \textit{ETR (I)} = PAR \cdot \textit{ETR–Factor} \cdot \textit{Fraction of Photosystem I} \cdot \textit{Yield (I)} $$

The function processes the provided CSV file by:

- Reading the CSV data using `read.csv()` and converting it to a `data.table`.  
- Validating the raw Dual-PAM data with `validate_dual_pam_single_channel_p700_data()`.  
- Filtering rows where the column `ID` equals `SP`.  
- Combining the `Date` and `Time` columns to create a `DateTime` column and ordering the data chronologically.  
- Extracting the initial Pm.-Det. measurement at `PAR = 0` to calculate the first ETR value.  
- Iterating through all rows with `Action == "P700 SP"` to calculate ETR values for Photosystem I (`Y.I.`).  
- Stopping at the recovery period if `remove_recovery = TRUE`.  

#### Return

- Returning a table containing:
  - `par`: Photosynthetically active radiation.  
  - `yield_1`: Yield of Photosystem I.  
  - `yield_2`: `NA` (not available in single channel PS I mode).  
  - `etr_1`: Calculated ETR for Photosystem I.  
  - `etr_2`: `NA` (not available in single channel PS I mode).  

#### Example

```r
data <- read_dual_pam_single_channel_p700_data(
  "path/to/data.csv",
  remove_recovery = TRUE,
  etr_factor = 0.84,
  fraction_photosystem_I = 0.5,
  fraction_photosystem_II = 0.5
)
```

#### References

- Heinz Walz GmbH. (2024). *DUAL-PAM-100 DUAL-PAM/F MANUAL, 5th Edition, April 2024, Chapter 7 (pp. 162-172).* Heinz Walz GmbH, Effeltrich, Germany. Available at: [DUAL-PAM-100 Manual](https://www.walz.com/files/downloads/dualpamed05.pdf)
---

### read_dual_pam_single_channel_fluo_data()

#### Description

This function reads the original CSV file as created by the [DUAL-PAM-100](https://www.walz.com/products/dual-pam-100/) software in single channel mode (Fluo), processes it by calculating $$ETR$$ values for Photosystem II, and returns a cleaned dataset.

#### Parameters

- **csv_path**: A string representing the file path to the CSV file.  
- **remove_recovery**: Automatic removal of recovery measurements after the actual Pi curve for an accurate regression. Default is `TRUE`.  
- **etr_factor**: A numeric value used as a factor for calculating ETR. Default is `0.84`.  
- **fraction_photosystem_I**: A numeric value representing the relative distribution of absorbed PAR to Photosystem I. Default is `0.5`.  
- **fraction_photosystem_II**: A numeric value representing the relative distribution of absorbed PAR to Photosystem II used in the ETR calculation formula. Default is `0.5`.  
  Calculated as: $$\textit{Fraction of Photosystem II} = \frac{PPS 2}{PPS 1+2}$$  

#### Details

ETR values for Photosystem II are calculated using the following formula:

$$ \textit{ETR (II)} = PAR \cdot \textit{ETR–Factor} \cdot \textit{Fraction of Photosystem II} \cdot \textit{Yield (II)} $$

The function processes the provided CSV file by:

- Reading the CSV data using `read.csv()` and converting it to a `data.table`.  
- Validating the raw Dual-PAM data with `validate_dual_pam_single_channel_fluo_data()`.  
- Filtering rows where the column `ID` equals `SP`.  
- Combining the `Date` and `Time` columns to create a `DateTime` column and ordering the data chronologically.  
- Extracting the initial **Fm-Det.** measurement at `PAR = 0` to calculate the first ETR value.  
- Iterating through all rows with `Action == "Fluo. SP"` to calculate ETR values for Photosystem II (`Y.II.`).  
- Stopping at the recovery period if `remove_recovery = TRUE`.  

#### Return

- Returning a table containing:
  - `par`: Photosynthetically active radiation.  
  - `yield_1`: `NA` (not available in single channel Photosystem II mode).  
  - `yield_2`: Yield of Photosystem II.  
  - `etr_1`: `NA` (not available in single channel Photosystem II mode).  
  - `etr_2`: Calculated ETR for Photosystem II.  

#### Example

```r
data <- read_dual_pam_single_channel_fluo_data(
  "path/to/data.csv",
  remove_recovery = TRUE,
  etr_factor = 0.84,
  fraction_photosystem_I = 0.5,
  fraction_photosystem_II = 0.5
)
```

#### References

- Heinz Walz GmbH. (2024). *DUAL-PAM-100 DUAL-PAM/F MANUAL, 5th Edition, April 2024, Chapter 7 (pp. 162-172).* Heinz Walz GmbH, Effeltrich, Germany. Available at: [DUAL-PAM-100 Manual](https://www.walz.com/files/downloads/dualpamed05.pdf)
---

### read_junior_pam_data()

#### Description

This function reads the original CSV file from [JUNIOR-PAM](https://www.walz.com/products/junior-pam/) as created by the WinControl software, processes it by calculating $$ETR$$ values, and returns a cleaned dataset.

#### Parameters

- **csv_path**: A string representing the file path to the CSV file.
- **remove_recovery**: Automatic removal of recovery measurements after the actual Pi curve for an accurate regression. Default is `TRUE`.
- **etr_factor**: A numeric value used as a factor for calculating ETR. Default is `0.84`.
- **fraction_photosystem_I**: A numeric value representing the relative distribution of absorbed PAR to photosystem I used in the ETR calculation formula. Default is `0.5`.
Calculated as: $$\textit{Fraction of Photosystem I} = \frac{PPS 1}{PPS 1+2}$$
- **fraction_photosystem_II**: A numeric value representing the relative distribution of absorbed PAR to photosystem II used in the ETR calculation formula. Default is `0.5`.
Calculated as: $$\textit{Fraction of Photosystem II} = \frac{PPS 2}{PPS 1+2}$$

#### Details

ETR values are calculated using the following formula:

$$ \textit{ETR (II)} = PAR \cdot \textit{ETR–Factor} \cdot \textit{Fraction of Photosystem (II)} \cdot \textit{Yield (II)} $$

The function processes the provided CSV file by:

- Reading the CSV data using `read.csv()` and converting it to a `data.table`.
- Validating the raw Junior-PAM data with `validate_junior_pam_data()`.
- Renaming columns to standard names (`PAR`, `Y.II`.) if necessary.
- Filtering rows where Type equals `"FO"` or `"F"`.
- Ordering by `Time (rel/ms)` column.
- Iterating through all rows to calculate ETR values for `Y.II.` using `calc_etr()`.
- Stopping at the recovery period if `remove_recovery = TRUE`.

To ensure the file is imported correctly, please export the CSV file using the default settings:
![Plot](img/export_junior_pam.png)

#### Return

Returning a table containing `par`, `yield_1` (NA), `yield_2`, `etr_1` (NA), and `etr_2`.

#### Example

```r
data <- read_junior_pam_data("path/to/data.csv",
remove_recovery = TRUE,
etr_factor = 0.84,
fraction_photosystem_I = 0.5,
fraction_photosystem_II = 0.5)
```

#### References

- Heinz Walz GmbH. (2024). *DUAL-PAM-100 DUAL-PAM/F MANUAL, 5th Edition, April 2024, Chapter 7 (pp. 162-172).* Heinz Walz GmbH, Effeltrich, Germany. Available at: [DUAL-PAM-100 Manual](https://www.walz.com/files/downloads/dualpamed05.pdf)

---

### read_pam_2500_data()

#### Description

This function reads the original CSV file generated by the [PAM-2500](https://www.walz.com/products/pam-2500/) software, processes it by calculating $$ETR$$ values for Photosystem II, and returns a cleaned dataset.

#### Parameters

- **csv_path**: A string representing the file path to the CSV file.
- **remove_recovery**: Logical value indicating whether recovery measurements after the actual Pi curve should be removed. Default is `TRUE`.
- **etr_factor**: A numeric value used as a factor for calculating ETR. Default is `0.84`.
- **fraction_photosystem_I**: A numeric value representing the relative distribution of absorbed PAR to photosystem I. Default is `0.5`.  
  Calculated as: $$\textit{Fraction of Photosystem I} = \frac{PPS 1}{PPS 1+2}$$
- **fraction_photosystem_II**: A numeric value representing the relative distribution of absorbed PAR to photosystem II. Default is `0.5`.  
  Calculated as: $$\textit{Fraction of Photosystem II} = \frac{PPS 2}{PPS 1+2}$$


#### Details

ETR values are calculated using the following formula:

$$ \textit{ETR (II)} = PAR \cdot \textit{ETR–Factor} \cdot \textit{Fraction of Photosystem II} \cdot \textit{Yield (II)} $$

The function processes the provided CSV file by:

- Reading the CSV file using `read.csv()` with `;` as separator and converting it to a `data.table`.
- Validating the dataset using `validate_pam_2500_data()`.
- Filtering rows where the column `No.` contains numeric entries only.
- Combining the `Date` and `Time` columns into a `DateTime` column and sorting the dataset chronologically.
- Iterating through all rows to:
  - Extract `PAR` and `Y.II.` values.
  - Calculate ETR for Photosystem II using `calc_etr()`.
- Optionally stopping at the recovery phase if `remove_recovery = TRUE`, defined as a decrease in PAR values.
- Constructing a result table with calculated values.


#### Return

- A `data.table` containing the following columns:

  - `par`: Photosynthetically active radiation  
  - `yield_1`: Placeholder column (`NA`)  
  - `yield_2`: Effective quantum yield of Photosystem II  
  - `etr_1`: Placeholder column (`NA`)  
  - `etr_2`: Calculated electron transport rate for Photosystem II  


#### Example

```r
data <- read_pam_2500_data(
  "path/to/data.csv",
  remove_recovery = TRUE,
  etr_factor = 0.84,
  fraction_photosystem_I = 0.5,
  fraction_photosystem_II = 0.5
)
```

#### References

- Heinz Walz GmbH. (2024). *DUAL-PAM-100 DUAL-PAM/F MANUAL, 5th Edition, April 2024, Chapter 7 (pp. 162-172).* Heinz Walz GmbH, Effeltrich, Germany. Available at: [DUAL-PAM-100 Manual](https://www.walz.com/files/downloads/dualpamed05.pdf)
