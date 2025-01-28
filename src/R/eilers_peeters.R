#' Default start value
#' @export
eilers_peeters_default_start_value_a <- 0.00004

#' Default start value
#' @export
eilers_peeters_default_start_value_b <- 0.004

#' Default start value
#' @export
eilers_peeters_default_start_value_c <- 5

#' Eilers-Peeters Regression for  ETR I
#'
#' Fits a regression model for ETR I based on Eilers-Peeters (1988), considering photoinhibition.
#'
#' @param data A \code{data.table} from \code{read_dual_pam_data}.
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
#'   \item \code{sdiff}: Deviation between actual and predicted values.
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
    etr_I_type,
    a_start_value,
    b_start_value,
    c_start_value
  ))
}

#' Eilers-Peeters Regression for  ETR II
#'
#' Fits a regression model for ETR II based on Eilers-Peeters (1988), considering photoinhibition.
#'
#' @param data A \code{data.table} from \code{read_dual_pam_data}.
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
#'   \item \code{sdiff}: Deviation between actual and predicted values.
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
    etr_II_type,
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

      data <- remove_det_row_by_etr(data, etr_type)

      model <- minpack.lm::nlsLM(data[[etr_type]] ~ (PAR / ((a * PAR^2) + (b * PAR) + c)),
        data = data,
        start = list(a = a_start_value, b = b_start_value, c = c_start_value),
        control = minpack.lm::nls.lm.control(maxiter = 1000)
      )

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
          eilers_peeters_message(paste("failed to calculate sdiff: warning:", w))
        },
        error = function(e) {
          eilers_peeters_message(paste("failed to calculate sdiff: error:", w))
        }
      )

      result <- list(
        etr_type = etr_type,
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
#' @param model_result A list containing the results of the model, including parameters such as \code{a}, \code{b}, \code{c}, \code{s}, \code{pm}, \code{ik}, \code{im}, and \code{w}.
#'
#' @return A modified model result as a list with the following elements:
#' \itemize{
#'   \item \code{etr_type}: ETR Type based on the model result.
#'   \item \code{etr_regression_data}: Regression data with ETR predictions based on the fitted model.
#'   \item \code{sdiff}: The difference between observed and predicted ETR values.
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
