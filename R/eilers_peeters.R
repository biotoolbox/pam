a_start_values_eilers_peeters_default <- 0.00004
b_start_values_eilers_peeters_default <- 0.004
c_start_values_eilers_peeters_default <- 5


#' Generate Regression for ETR I using the Eilers-Peeters Model (1988)
#'
#' This function generates a regression model based on the Eilers-Peeters 
#' formula. Original naming conventions from the publication are used.
#' All parameters are calculated taking photoinhibition into account.
#'
#' @param data A `data.table` containing the input data from \link[pam]{read_pam_data}
#' @param etr_type A character string specifying the column name of the 
#' response variable (in this case: ETR I) to be used in the model.
#' @param a_start_value Numeric. The starting value for the parameter \eqn{a} 
#' in the model. Defaults to \code{a_start_values_eilers_peeters_default}.
#' @param b_start_value Numeric. The starting value for the parameter \eqn{b} 
#' in the model. Defaults to \code{b_start_values_eilers_peeters_default}.
#' @param c_start_value Numeric. The starting value for the parameter \eqn{c} 
#' in the model. Defaults to \code{c_start_values_eilers_peeters_default}.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{etr_regression_data}{A `data.table` with the predicted values of ETR I to each PAR based on the fitted model.}
#'   \item{sdiff}{The deviation between the actual and predicted ETR values.}
#'   \item{a}{The obtained parameter \eqn{a}.}
#'   \item{b}{The obtained parameter \eqn{b}.}
#'   \item{c}{The obtained parameter \eqn{c}.}
#'   \item{pm}{The maximum electron transport rate, calculated as \eqn{pm = 1 / (b + 2 * \sqrt(a * c))}.}
#'   \item{s}{The initial slope of the light curve, calculated as \eqn{s = 1 / c}.}
#'   \item{ik}{PAR where the transition point from light limitation to light saturation is achieved, calculated as \eqn{ik = c / (b + 2 \sqrt(a * c))}.}
#'   \item{im}{The PAR at which the maximum electron transport rate is achieved, calculated as \eqn{ im = \sqrt(c / a)}.}
#'   \item{w}{The sharpness of the peak, calculated as \eqn{w = b / \sqrt(a * c)}.}
#' }
#'
#' @details
#' This function uses non-linear least squares fitting to estimate the parameters 
#' for the Eilers-Peeters model, which describes the relationship between PAR and 
#' ETR. The model used is:
#'
#' \eqn{p = I / (a * I^2 + b * I + c)}
#'
#' It is valid: I = PAR; p = ETR
#'
#' @references
#' Eilers, P. H. C., & Peeters, J. C. H. (1988). A model for the relationship 
#' between light intensity and the rate of photosynthesis in phytoplankton. 
#' Ecological Modelling, 42(3-4), 199-215. \doi{10.1016/0304-3800(88)90057-9}.

#'
#' @seealso \code{\link{nlsLM}}, \code{\link{minpack.lm}}
#' @importFrom minpack.lm nlsLM
#' @importFrom data.table data.table
#' @export
generate_regression_eilers_peeters_ETR_I <- function(
    data,
    a_start_value = a_start_values_eilers_peeters_default,
    b_start_value = b_start_values_eilers_peeters_default,
    c_start_value = c_start_values_eilers_peeters_default) {
  return(generate_regression_eilers_peeters_internal(
    data,
    etr_I_type,
    a_start_value,
    b_start_value,
    c_start_value
  ))
}

#' Generate Regression for ETR II using the Eilers-Peeters Model (1988)
#'
#' This function generates a regression model based on the Eilers-Peeters 
#' formula. Original naming conventions from the publication are used.
#' All parameters are calculated taking photoinhibition into account.
#'
#' @param data A `data.table` containing the input data from \link[pam]{read_pam_data}
#' @param etr_type A character string specifying the column name of the 
#' response variable (in this case: ETR II) to be used in the model.
#' @param a_start_value Numeric. The starting value for the parameter \eqn{a} 
#' in the model. Defaults to \code{a_start_values_eilers_peeters_default}.
#' @param b_start_value Numeric. The starting value for the parameter \eqn{b} 
#' in the model. Defaults to \code{b_start_values_eilers_peeters_default}.
#' @param c_start_value Numeric. The starting value for the parameter \eqn{c} 
#' in the model. Defaults to \code{c_start_values_eilers_peeters_default}.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{etr_regression_data}{A `data.table` with the predicted values of ETR II to each PAR based on the fitted model.}
#'   \item{sdiff}{The deviation between the actual and predicted ETR values.}
#'   \item{a}{The obtained parameter \eqn{a}.}
#'   \item{b}{The obtained parameter \eqn{b}.}
#'   \item{c}{The obtained parameter \eqn{c}.}
#'   \item{pm}{The maximum electron transport rate, calculated as \eqn{pm = 1 / (b + 2 * \sqrt(a * c))}.}
#'   \item{s}{The initial slope of the light curve, calculated as \eqn{s = 1 / c}.}
#'   \item{ik}{PAR where the transition point from light limitation to light saturation is achieved, calculated as \eqn{ik = c / (b + 2 \sqrt(a * c))}.}
#'   \item{im}{The PAR at which the maximum electron transport rate is achieved, calculated as \eqn{ im = \sqrt(c / a)}.}
#'   \item{w}{The sharpness of the peak, calculated as \eqn{w = b / \sqrt(a * c)}.}
#' }
#'
#' @details
#' This function uses non-linear least squares fitting to estimate the parameters 
#' for the Eilers-Peeters model, which describes the relationship between PAR and 
#' ETR. The model used is:
#'
#' \eqn{p = I / (a * I^2 + b * I + c)}
#'
#' It is valid: I = PAR; p = ETR
#'
#' @references
#' Eilers, P. H. C., & Peeters, J. C. H. (1988). A model for the relationship 
#' between light intensity and the rate of photosynthesis in phytoplankton. 
#' Ecological Modelling, 42(3-4), 199-215. \doi{10.1016/0304-3800(88)90057-9}.
#'
#' @seealso \code{\link{nlsLM}}, \code{\link{minpack.lm}}
#' @importFrom minpack.lm nlsLM
#' @importFrom data.table data.table
#' @export
generate_regression_eilers_peeters_ETR_II <- function(
    data,
    a_start_value = a_start_values_eilers_peeters_default,
    b_start_value = b_start_values_eilers_peeters_default,
    c_start_value = c_start_values_eilers_peeters_default) {
  return(generate_regression_eilers_peeters_internal(
    data,
    etr_II_type,
    a_start_value,
    b_start_value,
    c_start_value
  ))
}


generate_regression_eilers_peeters_internal <- function(
    data,
    etr_type,
    a_start_value = a_start_values_eilers_peeters_default,
    b_start_value = b_start_values_eilers_peeters_default,
    c_start_value = c_start_values_eilers_peeters_default) {
  library(minpack.lm)
  library(data.table)

  tryCatch(
    {
      validate_data(data)
      validate_etr_type(etr_type)

      if (!is.numeric(a_start_value)) {
        stop("a start value is not a valid number")
      }
      if (!is.numeric(b_start_value)) {
        stop("b start value is not a valid number")
      }
      if (!is.numeric(c_start_value)) {
        stop("c start value is not a valid number")
      }

      data <- remove_det_row_by_etr(data, etr_type)

      model <- nlsLM(data[[etr_type]] ~ (PAR / ((a * PAR^2) + (b * PAR) + c)),
        data = data,
        start = list(a = a_start_value, b = b_start_value, c = c_start_value),
        control = nls.lm.control(maxiter = 1000)
      )

      abc <- coef(model)
      a <- abc[["a"]]
      b <- abc[["b"]]
      c <- abc[["c"]]

      pm <- NA_real_
      tryCatch(
        {
          pm <- 1 / (b + 2 * sqrt(a * c))
        },
        warning = function(w) {
          message("failed to calculate etr_max: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate etr_max: Error:", e)
        }
      )

      s <- NA_real_
      tryCatch(
        {
          s <- 1 / c
        },
        warning = function(w) {
          message("failed to calculate alpha: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate alpha: Error:", e)
        }
      )

      ik <- NA_real_
      tryCatch(
        {
          ik <- c / (b + 2 * sqrt(a * c))
        },
        warning = function(w) {
          message("failed to calculate Ik: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate Ik: Error:", e)
        }
      )

      im <- NA_real_
      tryCatch(
        {
          im <- sqrt(c / a)
        },
        warning = function(w) {
          message("failed to calculate Im: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate Im: Error:", e)
        }
      )
      w <- NA_real_
      tryCatch(
        {
          w <- b / sqrt(a * c)
        },
        warning = function(w) {
          message("failed to calculate w: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate w: Error:", e)
        }
      )

      pars <- c()
      predictions <- c()
      for (p in min(data$PAR):max(data$PAR)) {
        pars <- c(pars, p)
        predictions <- c(predictions, p / ((a * p^2) + (b * p) + c))
      }
      etr_regression_data <- create_regression_data(pars, predictions)

      sdiff <- NA_real_
      tryCatch(
        {
          sdiff <- calculate_sdiff(data, etr_regression_data, etr_type)
        },
        warning = function(w) {
          message("failed to calculate sdiff: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate  sdiff: Error:", e)
        }
      )

      return(list(
        etr_regression_data = etr_regression_data,
        sdiff = sdiff,
        a = a,
        b = b,
        c = c,
        pm = pm,
        s = s,
        ik = ik,
        im = im,
        w = w
      ))
    },
    warning = function(w) {
      stop("Warning while calculating eilers peeters model: ", w)
    },
    error = function(e) {
      stop("Error while calculating eilers peeters model: ", e)
    }
  )
}

#' @title Control Plot for Eilers-Peeters Model (1988)
#' @description This function creates a plot for the Eilers-Peeters Model based on the provided data and model results.
#'
#' @param data A `data.table` containing the original ETR and yield data for the plot.
#' @param model_result A list containing the fitting results of the Eilers-Peeters Model and the calculated paramters (alpha, ik...).
#' @param etr_type A character string describing the ETR type (ETR I or ETR II).
#' @param title A character string that specifies the title of the plot.
#'
#' @return A plot displaying the original ETR and Yield values and the regression data. A table below the plot shows the calculated data (alpha, ik...)
#'
#' @references
#' Eilers, P. H. C., & Peeters, J. C. H. (1988). A model for the relationship 
#' between light intensity and the rate of photosynthesis in phytoplankton. 
#' Ecological Modelling, 42(3-4), 199-215. \doi{10.1016/0304-3800(88)90057-9}.
#' 
#' @export
plot_control_eilers_peeters <- function(data, model_result, etr_type, title) {
  # validate model result
  values <- c(
    as.character(round(model_result[["sdiff"]], 3)),
    as.character(round(model_result[["a"]], 7)),
    as.character(round(model_result[["b"]], 6)),
    as.character(round(model_result[["c"]], 6)),
    as.character(round(model_result[["pm"]], 3)),
    as.character(round(model_result[["s"]], 3)),
    as.character(round(model_result[["ik"]], 3)),
    as.character(round(model_result[["im"]], 3)),
    as.character(round(model_result[["w"]], 3))
  )

  params <- data.frame(
    Parameter = c("sdiff", "a", "b", "c", "pm", "s", "ik", "im", "w"),
    Value = unlist(values)
  )

  return(
    plot_control(
      data,
      model_result,
      etr_type,
      title,
      color_eilers_peeters,
      params
    )
  )
}
