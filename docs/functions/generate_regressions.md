# Generate regression model data

Those functions will generate regression data with the chosen model and ETR type (e.g. platt_generate_regression_ETR_II).
Original naming conventions from the publication are used.


## vollenweider_generate_regression_ETR_I() and vollenweider_generate_regression_ETR_II()

### Parameters

- **data**: A `data.table` containing the input data, processed according to the corresponding read function (e.g. `read_dual_pam_data`).
- **etr_type**: A character string specifying the column name of the response variable (ETR I or ETR II) to be used in the model.
- **pmax_start_value**: Numeric. The starting value for the parameter $$p_{max}$$ in the model. Defaults to `pmax_start_values_vollenweider_default`.
- **a_start_value**: Numeric. The starting value for the parameter $$a$$ in the model. Defaults to `a_start_values_vollenweider_default`.
- **alpha_start_value**: Numeric. The starting value for the parameter $$\alpha$$ in the model. Defaults to `alpha_start_values_vollenweider_default`.
- **n_start_value**: Numeric. The starting value for the parameter $$n$$ in the model. Defaults to `n_start_values_vollenweider_default`.

### Return

A list containing the following elements:

- **etr_regression_data**: A `data.table` with the predicted values of ETR I or ETR II to each PAR based on the fitted model.
- **residual_sum_of_squares**: Difference between observed and predicted ETR values, expressed as the sum of squared residuals.
- **root_mean_squared_error**: Difference between observed and predicted ETR values, expressed as the root mean squared error.
- **relative_root_mean_squared_error**: Difference between observed and predicted ETR values, expressed as the relative root mean squared error, normalized by the mean.
- **residual_sum_of_squares**: The deviation between the actual and predicted ETR values.
- **pmax**: The maximum electron transport rate without photoinhibition ($$p_{max}$$).
- **a**: The obtained parameter $$a$$.
- **alpha**: The obtained parameter $$\alpha$$.
- **n**: The obtained parameter $$n$$.
- **popt**: The maximum electron transport rate with photoinhibition ($$p_{opt}$$). A function computes predicted photosynthetic rates for each PAR value and tracks the maximum rate observed and is therefore modified from the original approach:

```r
  popt <- 0
      pars <- c()
      predictions <- c()
      for (p in min(data$PAR):max(data$PAR)) {
        pars <- c(pars, p)
        prediction <- pmax * (((a * p) / (sqrt(1 + (a * p)^2))) * (1 / (sqrt(1 + (alpha * p)^2)^n)))
        predictions <- c(
          predictions,
          prediction
        )

        if (prediction > popt) {
          popt <- prediction
        }
      }
```

- **ik**: PAR where the transition point from light limitation to light saturation is achieved without photoinhibition ($$I_k$$). Calculated as:

$$I_k = \\frac{1}{a}$$

- **iik**: PAR where the transition point from light limitation to light saturation is achieved with photoinhibition ($$I_k^\prime$$). Calculated as:

$$I_k^\prime = \frac{I_k \cdot p_{opt}}{p_{max}}$$

- **pmax_popt_and_ik_iik_ratio**: Ratio of $$p_{max}$$ to $$p_{opt}$$ and $$I_k$$ to $$I_k^\prime$$ ($$p_{max} / p_{opt}$$). Calculated as:

$$\\p_max\\_popt\\_and\\_ik\\_iik\\_ratio = \frac{I_k}{I_k^\prime}$$

### Details

This function uses non-linear least squares fitting to estimate the parameters for the Vollenweider model, which describes the relationship between PAR and ETR. The model used is:

$$p = p_{max} \cdot \frac{a \cdot i}{\sqrt{1 + (a \cdot i)^2}} \cdot \frac{1}{\left(\sqrt{1 + (\alpha \cdot i)^2}\right)^n}$$

It is valid: $$i = PAR; p = ETR$$

### Example

```r
result_vollenweider_ETR_II <- vollenweider_generate_regression_ETR_II(data, 
    pmax_start_value = 40, 
    a_start_value = 0.1, 
    alpha_start_value = -0.0001, 
    n_start_value = 350)
```

### References

Vollenweider, R. A. (1965). *Calculation models of photosynthesis-depth curves and some implications regarding day rate estimates in primary production measurements*, p. 427-457. In C. R. Goldman [ed.], *Primary Productivity in Aquatic Environments*. Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.



## platt_generate_regression_ETR_I() and platt_generate_regression_ETR_II()

### Parameters

- **data**: A `data.table` containing the input data from `read_dual_pam_data`.
- **alpha_start_value**: Numeric. The starting value for the parameter $$\alpha$$ in the model. Defaults to `alpha_start_value_platt_default`.
- **beta_start_value**: Numeric. The starting value for the parameter $$\beta$$ in the model. Defaults to `beta_start_value_platt_default`.
- **ps_start_value**: Numeric. The starting value for the parameter $$p_s$$ in the model. Defaults to `ps_start_value_platt_default`.

### Return

A list containing the following elements:

- **etr_regression_data**: A `data.table` with the predicted values of ETR I or ETR II to each PAR based on the fitted model.
- **residual_sum_of_squares**: Difference between observed and predicted ETR values, expressed as the sum of squared residuals.
- **root_mean_squared_error**: Difference between observed and predicted ETR values, expressed as the root mean squared error.
- **relative_root_mean_squared_error**: Difference between observed and predicted ETR values, expressed as the relative root mean squared error, normalized by the mean.
- **ps**: The maximum electron transport rate without photoinhibition ($$P_s$$).
- **alpha**: The initial slope of the light curve ($$\alpha$$).
- **beta**: The photoinhibition of the light curve ($$\beta$$).
- **pm**: The maximum electron transport rate with photoinhibition ($$P_m$$). Calculated as:

$$P_m = P_s \cdot \left(\frac{\alpha}{\alpha + \beta}\right) \cdot \left(\left(\frac{\beta}{\alpha + \beta}\right)^{\frac{\beta}{\alpha}}\right)$$

- **ik**: PAR where the transition point from light limitation to light saturation is achieved with photoinhibition ($$I_k$$). Calculated as:

$$I_k = \frac{P_m}{\alpha}$$

- **is**: PAR where the transition point from light limitation to light saturation is achieved without photoinhibition ($$I_s$$). Calculated as:

$$I_s = \frac{P_s}{\alpha}$$

- **im**: The PAR at which the maximum electron transport rate is achieved with photoinhibition ($$I_m$$). Calculated as:

$$I_m = \left(\frac{P_s}{\alpha}\right) \cdot \log\left(\frac{\alpha + \beta}{\beta}\right)$$

- **ib**: ($$I_b$$) Calculated as:

$$I_b = \frac{P_s}{\beta}$$

### Details

This function uses non-linear least squares fitting to estimate the parameters for the Platt model, which describes the relationship between PAR and ETR. The model used is:

$$P = P_s \cdot \left(1 - e^\frac{{-\alpha \cdot I}}{P_s}\right) \cdot e^\left(\frac{{-\beta \cdot I}}{P_s}\right)$$

It is valid: $$I = PAR; p = ETR$$

### Example

```r
result_platt_ETR_II <- platt_generate_regression_ETR_II(data, 
    alpha_start_value = 0.3, 
    beta_start_value = 0.01, 
    ps_start_value = 30)
```

### References

Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). *Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton*. Journal of Marine Research, 38(4). Retrieved from <https://elischolar.library.yale.edu/journal_of_marine_research/1525>.



## eilers_peeters_generate_regression_ETR_I() and eilers_peeters_generate_regression_ETR_II()

### Parameters

- **data**: A `data.table` containing the input data from `read_dual_pam_data`.
- **a_start_value**: Numeric. The starting value for the parameter $$a$$ in the model. Defaults to `a_start_values_eilers_peeters_default`.
- **b_start_value**: Numeric. The starting value for the parameter $$b$$ in the model. Defaults to `b_start_values_eilers_peeters_default`.
- **c_start_value**: Numeric. The starting value for the parameter $$c$$ in the model. Defaults to `c_start_values_eilers_peeters_default`.

### Return

A list containing the following elements:

- **etr_regression_data**: A `data.table` with the predicted values of ETR I or ETR II to each PAR based on the fitted model.
- **residual_sum_of_squares**: Difference between observed and predicted ETR values, expressed as the sum of squared residuals.
- **root_mean_squared_error**: Difference between observed and predicted ETR values, expressed as the root mean squared error.
- **relative_root_mean_squared_error**: Difference between observed and predicted ETR values, expressed as the relative root mean squared error, normalized by the mean.
- **a**: The obtained parameter $$a$$.
- **b**: The obtained parameter $$b$$.
- **c**: The obtained parameter $$c$$.
- **pm**: The maximum electron transport rate ($$p_m$$). Calculated as:

$$p_m = \frac{1}{b + 2 \sqrt{a \cdot c}}$$

- **s**: The initial slope of the light curve ($$s$$). Calculated as:

$$s = \frac{1}{c}$$

- **ik**: PAR where the transition point from light limitation to light saturation is achieved ($$I_k$$). Calculated as:

$$I_k = \frac{c}{b + 2 \sqrt{a \cdot c}}$$

- **im**: The PAR at which the maximum electron transport rate is achieved ($$I_m$$). Calculated as:

$$I_m = \sqrt{\frac{c}{a}}$$

- **w**: The sharpness of the peak ($$w$$). Calculated as:

$$w = \frac{b}{\sqrt{a \cdot c}}$$

### Details

This function uses non-linear least squares fitting to estimate the parameters for the Eilers-Peeters model, which describes the relationship between PAR and ETR. The model used is:

$$ p = \frac{I}{a \cdot I^2 + b \cdot I + c} $$

It is valid: $$I = PAR$$; $$p = ETR$$

### Example

```r
result_eilers_peeters_ETR_II <- eilers_peeters_generate_regression_ETR_II(data,
a_start_value = 0.00004,
b_start_value =  0.004,
c_start_value = 5)
```

### References

Eilers, P. H. C., & Peeters, J. C. H. (1988). *A model for the relationship between light intensity and the rate of photosynthesis in phytoplankton.* Ecological Modelling, 42(3-4), 199-215. [doi:10.1016/0304-3800(88)90057-9](https://doi.org/10.1016/0304-3800(88)90057-9).



### walsby_generate_regression_ETR_I() and walsby_generate_regression_ETR_II()

### Parameters

- **data**: A `data.table` containing the input data from `read_dual_pam_data`.
- **etr_max_start_value**: Numeric. The starting value for the parameter $$ETR_{max}$$ in the model. Defaults to `etr_max_start_value_walsby_default`.
- **alpha_start_value**: Numeric. The starting value for the parameter $$\alpha$$ in the model. Defaults to `alpha_start_value_walsby_default`.
- **beta_start_value**: Numeric. The starting value for the parameter $$\beta$$ in the model. Defaults to `beta_start_value_walsby_default`.

### Return

A list containing the following elements:

- **etr_regression_data**: A `data.table` with the predicted values of ETR I or ETR II to each PAR based on the fitted model.
- **residual_sum_of_squares**: Difference between observed and predicted ETR values, expressed as the sum of squared residuals.
- **root_mean_squared_error**: Difference between observed and predicted ETR values, expressed as the root mean squared error.
- **relative_root_mean_squared_error**: Difference between observed and predicted ETR values, expressed as the relative root mean squared error, normalized by the mean.
- **etr_max**: The maximum electron transport rate without photoinhibition ($$ETR_{max}$$).
- **alpha**: The initial slope of the light curve ($$\alpha$$).
- **beta**: The photoinhibition of the light curve ($$\beta$$).

### Details

This function uses non-linear least squares fitting to estimate the parameters for the Walsby model, which describes the relationship between PAR and ETR I. The model used is:

$$ETR = ETR_{max} \cdot \left(1 - e^{\left(-\frac{\alpha \cdot I}{ETR_{max}}\right)}\right) + \beta \cdot I$$

It is valid: $$I = PAR$$

This function generates a regression model based on  Walsby (1997) in a modified version without the respiration term.
Naming conventions from Romoth (2019) are used.
ETRmax is calculated without taking photoinhibition into account.


### References

Walsby, A. E. (1997). Numerical integration of phytoplankton photosynthesis through time and depth in a water column. *New Phytologist*, 136(2), 189-209. <https://doi.org/10.1046/j.1469-8137.1997.00736.x>

Romoth, K., Nowak, P., Kempke, D., Dietrich, A., Porsche, C., & Schubert, H. (2019). Acclimation limits of *Fucus evanescens* along the salinity gradient of the southwestern Baltic Sea. *Botanica Marina*, 62(1), 1-12. <https://doi.org/10.1515/bot-2018-0098>