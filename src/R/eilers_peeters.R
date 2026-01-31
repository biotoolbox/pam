#' Default start value
#' @export
eilers_peeters_default_start_value_a <- 1.550175e-06

#' Default start value
#' @export
eilers_peeters_default_start_value_b <- 0.01419034

#' Default start value
#' @export
eilers_peeters_default_start_value_c <- 7.012012

#' Eilers-Peeters Regression for  ETR I
#'
#' Fits a regression model for ETR I based on Eilers-Peeters (1988), considering photoinhibition.
#'
#' @param data A \code{data.table} from read function (e.g.\code{read_dual_pam_data}).
#' @param a_start_value Numeric. Starting value for \eqn{a}. Default: \code{a_start_values_eilers_peeters_default}.
#' @param b_start_value Numeric. Starting value for \eqn{b}. Default: \code{b_start_values_eilers_peeters_default}.
#' @param c_start_value Numeric. Starting value for \eqn{c}. Default: \code{c_start_values_eilers_peeters_default}.
#'
#' @details
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#eilers_peeters_generate_regression_etr_i-and-eilers_peeters_generate_regression_etr_ii}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{etr_regression_data}: Predicted ETR values.
#'   \item \code{residual_sum_of_squares}: Difference between observed and predicted ETR values, expressed as the sum of squared residuals.
#'   \item \code{root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the root mean squared error.
#'   \item \code{relative_root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the relative root mean squared error, normalized by the mean.
#'   \item \code{a}, \code{b}, \code{c}: Fitted parameters.
#'   \item \code{pm}: Maximum ETR (\eqn{p_m}).
#'   \item \code{s}: Initial slope (\eqn{s}).
#'   \item \code{ik}: Transition point from light limitation to light saturation (\eqn{I_k}).
#'   \item \code{im}: PAR at maximum ETR (\eqn{I_m}).
#'   \item \code{w}: Peak sharpness (\eqn{w}).
#' }
#'
#' @references{
#'   Eilers, P. H. C., & Peeters, J. C. H. (1988). \emph{A model for the relationship between light intensity and the rate of photosynthesis in phytoplankton.}
#'   Ecological Modelling, 42(3-4), 199-215. Available at: \doi{10.1016/0304-3800(88)90057-9}
#' }
#'
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' result <- eilers_peeters_generate_regression_ETR_I(data)
#'
#' @export
eilers_peeters_generate_regression_ETR_I <- function(
    data,
    a_start_value = eilers_peeters_default_start_value_a,
    b_start_value = eilers_peeters_default_start_value_b,
    c_start_value = eilers_peeters_default_start_value_c) {
  return(eilers_peeters_generate_regression_internal(
    data,
    etr_1_type,
    a_start_value,
    b_start_value,
    c_start_value
  ))
}

#' Eilers-Peeters Regression for  ETR II
#'
#' Fits a regression model for ETR II based on Eilers-Peeters (1988), considering photoinhibition.
#'
#' @param data A \code{data.table} from from read function (e.g.\code{read_dual_pam_data}).
#' @param a_start_value Numeric. Starting value for \eqn{a}. Default: \code{a_start_values_eilers_peeters_default}.
#' @param b_start_value Numeric. Starting value for \eqn{b}. Default: \code{b_start_values_eilers_peeters_default}.
#' @param c_start_value Numeric. Starting value for \eqn{c}. Default: \code{c_start_values_eilers_peeters_default}.
#'
#' @details
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#eilers_peeters_generate_regression_etr_i-and-eilers_peeters_generate_regression_etr_ii}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{etr_regression_data}: Predicted ETR values.
#'   \item \code{residual_sum_of_squares}: Difference between observed and predicted ETR values, expressed as the sum of squared residuals.
#'   \item \code{root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the root mean squared error.
#'   \item \code{relative_root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the relative root mean squared error, normalized by the mean.
#'   \item \code{a}, \code{b}, \code{c}: Fitted parameters.
#'   \item \code{pm}: Maximum ETR (\eqn{p_m}).
#'   \item \code{s}: Initial slope (\eqn{s}).
#'   \item \code{ik}: Transition point from light limitation to light saturation (\eqn{I_k}).
#'   \item \code{im}: PAR at maximum ETR (\eqn{I_m}).
#'   \item \code{w}: Peak sharpness (\eqn{w}).
#' }
#'
#' @references{
#'   Eilers, P. H. C., & Peeters, J. C. H. (1988). \emph{A model for the relationship between light intensity and the rate of photosynthesis in phytoplankton.}
#'   Ecological Modelling, 42(3-4), 199-215. Available at: \doi{10.1016/0304-3800(88)90057-9}
#' }
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' result <- eilers_peeters_generate_regression_ETR_II(data)
#'
#' @export
eilers_peeters_generate_regression_ETR_II <- function(
    data,
    a_start_value = eilers_peeters_default_start_value_a,
    b_start_value = eilers_peeters_default_start_value_b,
    c_start_value = eilers_peeters_default_start_value_c) {
  return(eilers_peeters_generate_regression_internal(
    data,
    etr_2_type,
    a_start_value,
    b_start_value,
    c_start_value
  ))
}

eilers_peeters_message <- function(msg) {
  if (is.character(msg) == FALSE) {
    message("eilers peeters problem")
  } else {
    message("eilers peeters: ", msg)
  }
}

eilers_peeters_generate_regression_internal <- function(
    data,
    etr_type,
    a_start_value = eilers_peeters_default_start_value_a,
    b_start_value = eilers_peeters_default_start_value_b,
    c_start_value = eilers_peeters_default_start_value_c) {
  tryCatch(
    {
      validate_data(data)
      validate_etr_type(etr_type)

      if (!is.numeric(a_start_value)) {
        stop("eilers peeters: a start value is not a valid number")
      }
      if (!is.numeric(b_start_value)) {
        stop("eilers peeters: b start value is not a valid number")
      }
      if (!is.numeric(c_start_value)) {
        stop("eilers peeters: c start value is not a valid number")
      }

      model <- minpack.lm::nlsLM(data[[etr_type]] ~ (par / ((a * par^2) + (b * par) + c)),
        data = data,
        start = list(a = a_start_value, b = b_start_value, c = c_start_value),
        control = minpack.lm::nls.lm.control(maxiter = 1000)
      )

      residual_sum_of_squares <- model$m$deviance()

      abc <- stats::coef(model)
      a <- abc[["a"]]
      b <- abc[["b"]]
      c <- abc[["c"]]

      pm <- NA_real_
      tryCatch(
        {
          pm <- 1 / (b + 2 * sqrt(a * c))
        },
        warning = function(w) {
          eilers_peeters_message(paste("failed to calculate pm: warning:", w))
        },
        error = function(e) {
          eilers_peeters_message(paste("failed to calculate pm: error:", w))
        }
      )

      s <- NA_real_
      tryCatch(
        {
          s <- 1 / c
        },
        warning = function(w) {
          eilers_peeters_message(paste("failed to calculate s: warning:", w))
        },
        error = function(e) {
          eilers_peeters_message(paste("failed to calculate s: error:", w))
        }
      )

      ik <- NA_real_
      tryCatch(
        {
          ik <- c / (b + 2 * sqrt(a * c))
        },
        warning = function(w) {
          eilers_peeters_message(paste("failed to calculate ik: warning:", w))
        },
        error = function(e) {
          eilers_peeters_message(paste("failed to calculate ik: error:", w))
        }
      )

      im <- NA_real_
      tryCatch(
        {
          im <- sqrt(c / a)
        },
        warning = function(w) {
          eilers_peeters_message(paste("failed to calculate im: warning:", w))
        },
        error = function(e) {
          eilers_peeters_message(paste("failed to calculate im: error:", w))
        }
      )

      w <- NA_real_
      tryCatch(
        {
          w <- b / sqrt(a * c)
        },
        warning = function(w) {
          eilers_peeters_message(paste("failed to calculate w: warning:", w))
        },
        error = function(e) {
          eilers_peeters_message(paste("failed to calculate w: error:", w))
        }
      )

      pars <- c()
      predictions <- c()
      for (p in min(data$par):max(data$par)) {
        pars <- c(pars, p)
        predictions <- c(predictions, p / ((a * p^2) + (b * p) + c))
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
        a = a,
        b = b,
        c = c,
        pm = pm,
        s = s,
        ik = ik,
        im = im,
        w = w
      )

      validate_model_result(result)
      return(result)
    },
    warning = function(w) {
      warning("warning while calculating eilers peeters model: ", w)
    },
    error = function(e) {
      stop("error while calculating eilers peeters model: ", e)
    }
  )
}

#' Eilers & Peeters Model Modification
#'
#' This function enhances the Eilers and Peeters (1988) model by adding parameters not originally included in the model, which were introduced by other models. It also renames parameters to a standardized naming convention used across all models.
#'
#' @param model_result A list containing the model result (e.g. from eilers_peeters_generate_regression_ETR_II()).
#'
#' @return A modified model result as a list with the following elements:
#' \itemize{
#'   \item \code{etr_type}: ETR Type based on the model result.
#'   \item \code{etr_regression_data}: Regression data with ETR predictions based on the fitted model.
#'   \item \code{residual_sum_of_squares}: Difference between observed and predicted ETR values, expressed as the sum of squared residuals.
#'   \item \code{root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the root mean squared error.
#'   \item \code{relative_root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the relative root mean squared error, normalized by the mean.
#'   \item \code{a}: The obtained parameter \code{a}.
#'   \item \code{b}: The obtained parameter \code{b}.
#'   \item \code{c}: The obtained parameter \code{c}.
#'   \item \code{d}: Not available, set to \code{NA_real_}.
#'   \item \code{alpha}: The initial slope of the light curve, transferred unchanged as \code{s}.
#'   \item \code{beta}: Not available, set to \code{NA_real_}.
#'   \item \code{etrmax_with_photoinhibition}: The maximum electron transport rate with photoinhibition, transferred as \code{pm}.
#'   \item \code{etrmax_without_photoinhibition}: Not available, set to \code{NA_real_}.
#'   \item \code{ik_with_photoinhibition}: PAR where the transition point from light limitation to light saturation is achieved with photoinhibition, transferred as \code{ik}.
#'   \item \code{ik_without_photoinhibition}: Not available, set to \code{NA_real_}.
#'   \item \code{im_with_photoinhibition}: The PAR at which the maximum electron transport rate is achieved with photoinhibition, transferred as \code{im}.
#'   \item \code{w}: The sharpness of the peak, transferred as \code{w}.
#'   \item \code{ib}: Not available, set to \code{NA_real_}.
#'   \item \code{etrmax_with_without_ratio}: Not available, set to \code{NA_real_}.
#' }
#'
#' @details
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#eilers_peeters_modified}
#'
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' result <- eilers_peeters_generate_regression_ETR_II(data)
#' modified_result <- eilers_peeters_modified(result)
#'
#' @export
eilers_peeters_modified <- function(model_result) {
  validate_model_result(model_result)
  result <- create_modified_model_result(
    get_etr_type_from_model_result(model_result),
    get_etr_regression_data_from_model_result(model_result),
    get_sdiff_from_model_result(model_result),
    model_result[["root_mean_squared_error"]],
    model_result[["relative_root_mean_squared_error"]],
    model_result[["a"]],
    model_result[["b"]],
    model_result[["c"]],
    NA_real_,
    model_result[["s"]],
    NA_real_,
    model_result[["pm"]],
    NA_real_,
    model_result[["ik"]],
    NA_real_,
    model_result[["im"]],
    model_result[["w"]],
    NA_real_,
    NA_real_
  )

  return(result)
}
