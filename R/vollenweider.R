generate_regression_vollenweider <- function(data, use_etr_I) {
  library(minpack.lm)
  library(SciViews)
  validate_data(data)

  if (!is.logical(use_etr_I)) {
    stop("use_etr_I is not a valid bool")
  }

  etr_to_use <- ""
  if (use_etr_I) {
    etr_to_use <- etr_I_col_name
    data <- data[Action != "Fm-Det."]
  } else {
    etr_to_use <- etr_II_col_name
    data <- data[Action != "Pm.-Det."]
  }

  model <- nlsLM(data[[etr_to_use]] ~ ((a * PAR / sqrt(1 + (b^2) * (PAR^2))) * (1 / sqrt(1 + (c^2) * (PAR ^2)))^n),
    data = data,
    start = list(a = 0.3, b = 0.004, c = -0.0001, n = 1000)
  ) # Initial parameter values

  abc <- coef(model)
  a <- abc[["a"]]
  b <- abc[["b"]]
  c <- abc[["c"]]
  n <- abc[["n"]]
  
 pars <- c()
  predictions <- c()
  for (p in min(data$PAR):max(data$PAR)) {
    pars <- c(pars, p)
    predictions <- c(predictions, ((a * p / sqrt(1 + (b^2) * (p^2))) * (1 / sqrt(1 + (c^2) * (p ^2)))^n))
  }
  etr_regression_data <- data.table("PAR" = pars, "prediction" = predictions)

   # Calculate Iopt using the formula
  Iopt <- etr_regression_data[prediction == max(etr_regression_data$prediction) ]$PAR

# Calculate ETRmax
  etr_max <- a * Iopt / sqrt(1 + (b^2) * (Iopt^2)) * (1 / sqrt(1 + (c^2) * (Iopt ^2)))^n

  # Calculate alpha using the formula
  alpha <- a

  # Calculate Ik using the formula
  Ik <- etr_max / a

  return(list(
    etr_regression_data = etr_regression_data,
    sdiff = calculate_sdiff(data, etr_regression_data, etr_to_use),
    a = a,
    b = b,
    c = c,
    n = n,
    etr_max = etr_max,
    alpha = alpha,
    ik = Ik,
    Iopt = Iopt
  ))
}

