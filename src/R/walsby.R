#' Default start value
#' @export
walsby_default_start_value_etr_max <- 49.70366

#' Default start value
#' @export
walsby_default_start_value_alpha <- 0.1228559

#' Default start value
#' @export
walsby_default_start_value_beta <- -0.0008944076

#' Walsby Regression for ETR I
#'
#' Fits a modified Walsby (1997) regression model without the respiration term, using Romoth (2019) naming conventions.
#' Calculates \eqn{ETR_{max}} without accounting for photoinhibition.
#'
#' @param data A \code{data.table} from read function (e.g.\code{read_dual_pam_data}).
#' @param etr_max_start_value Numeric. Initial value for \eqn{ETR_{max}}. Default: \code{etr_max_start_value_walsby_default}.
#' @param alpha_start_value Numeric. Initial value for \eqn{\alpha}. Default: \code{alpha_start_value_walsby_default}.
#' @param beta_start_value Numeric. Initial value for \eqn{\beta}. Default: \code{beta_start_value_walsby_default}.
#'
#' @details
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#walsby_generate_regression_etr_i-and-walsby_generate_regression_etr_ii}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{etr_regression_data}: Predicted ETR values.
#'   \item \code{residual_sum_of_squares}: Difference between observed and predicted ETR values, expressed as the sum of squared residuals.
#'   \item \code{root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the root mean squared error.
#'   \item \code{relative_root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the relative root mean squared error, normalized by the mean.
#'   \item \code{etr_max}: Maximum ETR (\eqn{ETR_{max}}).
#'   \item \code{alpha}: Initial slope (\eqn{\alpha}).
#'   \item \code{beta}: Photoinhibition factor (\eqn{\beta}).
#' }
#'
#' @references{
#'   Walsby, A. E. (1997). Numerical integration of phytoplankton photosynthesis through time and depth in a water column.
#'   \emph{New Phytologist}, 136(2), 189-209. Available at: \doi{10.1046/j.1469-8137.1997.00736.x}
#'
#' Romoth, K., Nowak, P., Kempke, D., Dietrich, A., Porsche, C., & Schubert, H. (2019).
#'   Acclimation limits of \emph{Fucus evanescens} along the salinity gradient of the southwestern Baltic Sea.
#'   \emph{Botanica Marina}, 62(1), 1-12. Available at: \doi{10.1515/bot-2018-0098}
#' }
#'
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' result <- walsby_generate_regression_ETR_I(data)
#'
#' @export
walsby_generate_regression_ETR_I <- function(
    data,
    etr_max_start_value = walsby_default_start_value_etr_max,
    alpha_start_value = walsby_default_start_value_alpha,
    beta_start_value = walsby_default_start_value_alpha) {
  return(
    walsby_generate_regression_internal(
      data,
      etr_1_type,
      etr_max_start_value,
      alpha_start_value,
      beta_start_value
    )
  )
}

#' Walsby Regression for ETR II
#'
#' Fits a modified Walsby (1997) regression model without the respiration term, using Romoth (2019) naming conventions.
#' Calculates \eqn{ETR_{max}} without accounting for photoinhibition.
#'
#' @param data A \code{data.table} from read function (e.g.\code{read_dual_pam_data}).
#' @param etr_max_start_value Numeric. Initial value for \eqn{ETR_{max}}. Default: \code{etr_max_start_value_walsby_default}.
#' @param alpha_start_value Numeric. Initial value for \eqn{\alpha}. Default: \code{alpha_start_value_walsby_default}.
#' @param beta_start_value Numeric. Initial value for \eqn{\beta}. Default: \code{beta_start_value_walsby_default}.
#'
#' @details
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#walsby_generate_regression_etr_i-and-walsby_generate_regression_etr_ii}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{etr_regression_data}: Predicted ETR values.
#'   \item \code{residual_sum_of_squares}: Difference between observed and predicted ETR values, expressed as the sum of squared residuals.
#'   \item \code{root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the root mean squared error.
#'   \item \code{relative_root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the relative root mean squared error, normalized by the mean.
#'   \item \code{etr_max}: Maximum ETR (\eqn{ETR_{max}}).
#'   \item \code{alpha}: Initial slope (\eqn{\alpha}).
#'   \item \code{beta}: Photoinhibition factor (\eqn{\beta}).
#' }
#'
#' @references{
#'   Walsby, A. E. (1997). Numerical integration of phytoplankton photosynthesis through time and depth in a water column.
#'   \emph{New Phytologist}, 136(2), 189-209. Available at: \doi{10.1046/j.1469-8137.1997.00736.x}
#'
#' Romoth, K., Nowak, P., Kempke, D., Dietrich, A., Porsche, C., & Schubert, H. (2019).
#'   Acclimation limits of \emph{Fucus evanescens} along the salinity gradient of the southwestern Baltic Sea.
#'   \emph{Botanica Marina}, 62(1), 1-12. Available at: \doi{10.1515/bot-2018-0098}
#' }
#'
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' result <- walsby_generate_regression_ETR_II(data)
#'
#' @export
walsby_generate_regression_ETR_II <- function(
    data,
    etr_max_start_value = walsby_default_start_value_etr_max,
    alpha_start_value = walsby_default_start_value_alpha,
    beta_start_value = walsby_default_start_value_beta) {
  return(walsby_generate_regression_internal(
    data,
    etr_2_type,
    etr_max_start_value,
    alpha_start_value,
    beta_start_value
  ))
}

walsby_message <- function(msg) {
  if (is.character(msg) == FALSE) {
    message("walsby problem")
  } else {
    message("walsby: ", msg)
  }
}

walsby_generate_regression_internal <- function(
    data,
    etr_type,
    etr_max_start_value = walsby_default_start_value_etr_max,
    alpha_start_value = walsby_default_start_value_alpha,
    beta_start_value = walsby_default_start_value_beta) {
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

      model <- minpack.lm::nlsLM(data[[etr_type]] ~ etr_max * (1 - exp((-alpha * par) / etr_max)) + beta * par,
        data = data,
        start = list(
          etr_max = etr_max_start_value,
          alpha = alpha_start_value,
          beta = beta_start_value
        ),
        control = stats::nls.control(maxiter = 1000)
      )

      residual_sum_of_squares <- model$m$deviance()

      abc <- stats::coef(model)
      etr_max <- abc[["etr_max"]]
      alpha <- abc[["alpha"]]
      beta <- abc[["beta"]]

      pars <- c()
      predictions <- c()
      for (p in min(data$par):max(data$par)) {
        pars <- c(pars, p)
        predictions <- c(predictions, etr_max * (1 - exp((-alpha * p) / etr_max)) + beta * p)
      }

      etr_regression_data <- create_regression_data(pars, predictions)

      measured_predicted_etr_par_data <- get_etr_data_for_par_values(data, etr_regression_data, etr_type)

      root_mean_squared_error <- root_mean_squared_error(measured_predicted_etr_par_data)

      relative_root_mean_squared_error <- relative_root_mean_squared_error(measured_predicted_etr_par_data)

      result <- list(
        etr_type = etr_type,
        etr_regression_data = etr_regression_data,
        residual_sum_of_squares = residual_sum_of_squares,
        root_mean_squared_error = root_mean_squared_error,
        relative_root_mean_squared_error = relative_root_mean_squared_error,
        etr_max = etr_max,
        alpha = alpha,
        beta = beta
      )
      validate_model_result(result)
      return(result)
    },
    warning = function(w) {
      warning("warning while calculating walsby model: ", w)
    },
    error = function(e) {
      stop("error while calculating walsby model: ", e)
    }
  )
}

#' Walsby Model Modification
#'
#' Enhances the Walsby (1997) model by adding parameters from other models and standardizing parameter names.
#'
#' @param model_result A list containing the model result (e.g. from walsby_generate_regression_ETR_II()).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{etr_type}: ETR Type based on the model result.
#'   \item \code{etr_regression_data}: Regression data with ETR predictions based on the fitted model.
#'   \item \code{residual_sum_of_squares}: Difference between observed and predicted ETR values, expressed as the sum of squared residuals.
#'   \item \code{root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the root mean squared error.
#'   \item \code{relative_root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the relative root mean squared error, normalized by the mean.
#'   \item \code{a}: Obtained parameter \code{a}, equal to \code{etrmax_without_photoinhibition}.
#'   \item \code{b}: Obtained parameter \code{b}, equal to \code{alpha}.
#'   \item \code{c}: Obtained parameter \code{c}, equal to \code{beta}.
#'   \item \code{d}: Not available, set to \code{NA_real_}.
#'   \item \code{alpha}: The initial slope of the light curve, transferred unchanged as \code{alpha}.
#'   \item \code{beta}: The photoinhibition of the light curve, transferred unchanged as \code{beta}.
#'   \item \code{etrmax_with_photoinhibition}: The maximum electron transport rate with photoinhibition.
#'   \item \code{etrmax_without_photoinhibition}: The maximum electron transport rate without photoinhibition, transferred as \code{etr_max}.
#'   \item \code{ik_with_photoinhibition}: PAR where the transition point from light limitation to light saturation is achieved with photoinhibition.
#'   \item \code{ik_without_photoinhibition}: PAR where the transition point from light limitation to light saturation is achieved without photoinhibition.
#'   \item \code{im_with_photoinhibition}: PAR at the maximum ETR with photoinhibition.
#'   \item \code{w}: Not available, set to \code{NA_real_}.
#'   \item \code{ib}: Not available, set to \code{NA_real_}.
#'   \item \code{etrmax_with_without_ratio}: Ratio of \code{etrmax_with_photoinhibition} to \code{etrmax_without_photoinhibition}.
#' }
#'
#' @details
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#walsby_modified}
#'
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' result <- walsby_generate_regression_ETR_II(data)
#' modified_result <- walsby_modified(result)
#'
#' @export
walsby_modified <- function(model_result) {
  validate_model_result(model_result)

  etr_regression_data <- get_etr_regression_data_from_model_result(model_result)
  etr_max_row <- etr_regression_data[etr_regression_data[[prediction_name]] == max(etr_regression_data[[prediction_name]]), ]
  etrmax_with_photoinhibition <- etr_max_row[[prediction_name]]
  im_with_photoinhibition <- etr_max_row[[PAR_name]]

  result <- create_modified_model_result(
    etr_type = get_etr_type_from_model_result(model_result),
    etr_regression_data = get_etr_regression_data_from_model_result(model_result),
    residual_sum_of_squares = get_sdiff_from_model_result(model_result),
    model_result[["root_mean_squared_error"]],
    model_result[["relative_root_mean_squared_error"]],
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
