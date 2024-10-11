walsby_default_start_value_etr_max <- 100
walsby_default_start_value_alpha <- 0.4
walsby_default_start_value_beta <- -0.01

#' Generate Regression for ETR I using the Walsby Model (1997) modified by Romoth (2019)
#'
#' This function generates a regression model based on the Walsby Model
#' formula in a modified version without respiration term.
#' Naming conventions from Romoth (2019) are used. ETRmax is calculated without
#' taking photoinhibition into account.
#'
#' @param data A `data.table` containing the input data from \link[pam]{read_pam_data}
#' @param etr_type A character string specifying the column name of the
#' response variable (in this case: ETR I) to be used in the model.
#' @param etr_max_start_value_walsby Numeric. The starting value for the parameter \eqn{etr_max}
#' in the model. Defaults to \code{etr_max_start_value_walsby_default}.
#' @param alpha_start_value_walsby Numeric. The starting value for the parameter \eqn{alpha}
#' in the model. Defaults to \code{alpha_start_value_walsby_default}.
#' @param beta_start_value_platt Numeric. The starting value for the parameter \eqn{beta}
#' in the model. Defaults to \code{beta_start_value_walsby_default}.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{etr_regression_data}{A `data.table` with the predicted values of ETR I to each PAR based on the fitted model.}
#'   \item{sdiff}{The deviation between the actual and predicted ETR values.}
#'   \item{etr_max}{The maximum electron transport rate without photoinhibition.}
#'   \item{alpha}{The initial slope of the light curve.}
#'   \item{beta}{The photoinhibition of the light curve.}
#' }
#'
#' @details
#' This function uses non-linear least squares fitting to estimate the parameters
#' for the Platt model, which describes the relationship between PAR and
#' ETR I. The model used is:
#'
#' \eqn{ETR = etr_max * (1 - e^((-alpha * I) / etr_max)) + beta * I}
#'
#' It is valid: I = PAR
#'
#' @references
#' Walsby, A. E. (1997). Numerical integration of phytoplankton photosynthesis
#' through time and depth in a water column. *Journal of Plankton Research*,
#' 19(3), 487-502. https://doi.org/10.1093/plankt/19.3.487
#'
#' Romoth, K., Nowak, P., Kempke, D., Dietrich, A., Porsche, C., & Schubert, H. (2019).
#' Acclimation limits of *Fucus evanescens* along the salinity gradient of the
#' southwestern Baltic Sea. *Botanica Marina*, 62(1), 1-12. https://doi.org/10.1515/bot-2018-0098
#'
#'
#' @seealso \code{\link{nlsLM}}, \code{\link{minpack.lm}}
#' @importFrom minpack.lm nlsLM
#' @importFrom data.table data.table
#' @export
walsby_generate_regression_ETR_I <- function(
    data,
    etr_max_start_value = walsby_default_start_value_etr_max,
    alpha_start_value = walsby_default_start_value_alpha,
    beta_start_value = walsby_default_start_value_alpha) {
  return(
    walsby_generate_regression_internal(
      data,
      etr_I_type,
      etr_max_start_value,
      alpha_start_value,
      beta_start_value
    )
  )
}

#' Generate Regression for ETR II using the Walsby Model (1997) modified by Romoth (2019)
#'
#' This function generates a regression model based on the Walsby Model
#' formula in a modified version without respiration term.
#' Naming conventions from Romoth (2019) are used. ETRmax is calculated without
#' taking photoinhibition into account.
#'
#' @param data A `data.table` containing the input data from \link[pam]{read_pam_data}
#' @param etr_type A character string specifying the column name of the
#' response variable (in this case: ETR II) to be used in the model.
#' @param etr_max_start_value_walsby Numeric. The starting value for the parameter \eqn{etr_max}
#' in the model. Defaults to \code{etr_max_start_value_walsby_default}.
#' @param alpha_start_value_walsby Numeric. The starting value for the parameter \eqn{alpha}
#' in the model. Defaults to \code{alpha_start_value_walsby_default}.
#' @param beta_start_value_platt Numeric. The starting value for the parameter \eqn{beta}
#' in the model. Defaults to \code{beta_start_value_walsby_default}.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{etr_regression_data}{A `data.table` with the predicted values of ETR II to each PAR based on the fitted model.}
#'   \item{sdiff}{The deviation between the actual and predicted ETR values.}
#'   \item{etr_max}{The maximum electron transport rate without photoinhibition.}
#'   \item{alpha}{The initial slope of the light curve.}
#'   \item{beta}{The photoinhibition of the light curve.}
#' }
#'
#' @details
#' This function uses non-linear least squares fitting to estimate the parameters
#' for the Platt model, which describes the relationship between PAR and
#' ETR I. The model used is:
#'
#' \eqn{ETR = etr_max * (1 - e^((-alpha * I) / etr_max)) + beta * I}
#'
#' It is valid: I = PAR
#'
#' @references
#' Walsby, A. E. (1997). Numerical integration of phytoplankton photosynthesis
#' through time and depth in a water column. *Journal of Plankton Research*,
#' 19(3), 487-502. https://doi.org/10.1093/plankt/19.3.487
#'
#' Romoth, K., Nowak, P., Kempke, D., Dietrich, A., Porsche, C., & Schubert, H. (2019).
#' Acclimation limits of *Fucus evanescens* along the salinity gradient of the
#' southwestern Baltic Sea. *Botanica Marina*, 62(1), 1-12. https://doi.org/10.1515/bot-2018-0098
#'
#'
#' @seealso \code{\link{nlsLM}}, \code{\link{minpack.lm}}
#' @importFrom minpack.lm nlsLM
#' @importFrom data.table data.table
#' @export
walsby_generate_regression_ETR_II <- function(
    data,
    etr_max_start_value = walsby_default_start_value_etr_max,
    alpha_start_value = walsby_default_start_value_alpha,
    beta_start_value = walsby_default_start_value_beta) {
  return(walsby_generate_regression_internal(
    data,
    etr_II_type,
    etr_max_start_value,
    alpha_start_value,
    beta_start_value
  ))
}

walsby_generate_regression_internal <- function(
    data,
    etr_type,
    etr_max_start_value = walsby_default_start_value_etr_max,
    alpha_start_value = walsby_default_start_value_alpha,
    beta_start_value = walsby_default_start_value_beta) {
  library(data.table)
  library(minpack.lm)
  library(SciViews)

  tryCatch(
    {
      validate_data(data)
      validate_etr_type(etr_type)

      if (!is.numeric(etr_max_start_value)) {
        stop("etr max start value is not a valid number")
      }
      if (!is.numeric(alpha_start_value)) {
        stop("alpha start value is not a valid number")
      }
      if (!is.numeric(beta_start_value)) {
        stop("beta start value is not a valid number")
      }

      data <- remove_det_row_by_etr(data, etr_type)

      model <- nlsLM(data[[etr_type]] ~ etr_max * (1 - exp((-alpha * PAR) / etr_max)) + beta * PAR,
        data = data,
        start = list(
          etr_max = etr_max_start_value,
          alpha = alpha_start_value,
          beta = beta_start_value
        ),
        control = nls.control(maxiter = 1000)
      )

      abc <- coef(model)
      etr_max <- abc[["etr_max"]]
      alpha <- abc[["alpha"]]
      beta <- abc[["beta"]]

      pars <- c()
      predictions <- c()
      for (p in min(data$PAR):max(data$PAR)) {
        pars <- c(pars, p)
        predictions <- c(predictions, etr_max * (1 - exp((-alpha * p) / etr_max)) + beta * p)
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

      result <- list(
        etr_type = etr_type,
        etr_regression_data = etr_regression_data,
        sdiff = sdiff,
        etr_max = etr_max,
        alpha = alpha,
        beta = beta
      )
      validate_model_result(result)
      return(result)
    },
    warning = function(w) {
      stop("Warning while calculating walsby model: ", w)
    },
    error = function(e) {
      stop("Error while calculating walsby model: ", e)
    }
  )
}

walsby_modified <- function(model_result) {
  validate_model_result(model_result)

  etr_regression_data <- get_etr_regression_data_from_model_result(model_result)
  etr_max_row <- etr_regression_data[etr_regression_data[[prediction_name]] == max(etr_regression_data[[prediction_name]]), ]
  etrmax_with_photoinhibition <- etr_max_row[[prediction_name]]
  im_with_photoinhibition <- etr_max_row[[PAR_name]]

  result <- create_modified_model_result(
    etr_type = get_etr_type_from_model_result(model_result),
    etr_regression_data = get_etr_regression_data_from_model_result(model_result),
    sdiff = get_sdiff_from_model_result(model_result),
    a = model_result[["etr_max"]],
    b = model_result[["alpha"]],
    c = model_result[["beta"]],
    d = NA_real_,
    alpha = model_result[["alpha"]],
    beta = model_result[["beta"]],
    etrmax_with_photoinhibition = etrmax_with_photoinhibition,
    etrmax_without_photoinhibition = model_result[["etr_max"]],
    ik_with_photoinhibition = etrmax_with_photoinhibition / model_result[["alpha"]],
    ik_without_photoinhibition = model_result[["etr_max"]] / model_result[["alpha"]],
    im_with_photoinhibition = im_with_photoinhibition,
    w = NA_real_,
    ib = NA_real_,
    etrmax_with_without_ratio = model_result[["etr_max"]] / etrmax_with_photoinhibition
  )

  return(result)
}
