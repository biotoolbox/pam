#' Default start value
#' @export
vollenweider_default_start_value_pmax <- 50

#' Default start value
#' @export
vollenweider_default_start_value_a <- 0.002

#' Default start value
#' @export
vollenweider_default_start_value_alpha <- 0.00001

#' Default start value
#' @export
vollenweider_default_start_value_n <- 100

#' Vollenweider Regression for ETR I
#'
#' Fits the Vollenweider (1965) regression model using original naming conventions from the publication.
#'
#' @param data A \code{data.table} from read function (e.g.\code{read_dual_pam_data}).
#' @param pmax_start_value Numeric. Initial value for \eqn{p_{max}}. Default: \code{pmax_start_values_vollenweider_default}.
#' @param a_start_value Numeric. Initial value for \eqn{a}. Default: \code{a_start_values_vollenweider_default}.
#' @param alpha_start_value Numeric. Initial value for \eqn{\alpha}. Default: \code{alpha_start_values_vollenweider_default}.
#' @param n_start_value Numeric. Initial value for \eqn{n}. Default: \code{n_start_values_vollenweider_default}.
#'
#' @details
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#vollenweider_generate_regression_etr_i-and-vollenweider_generate_regression_etr_ii}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{etr_regression_data}: Predicted ETR values.
#'   \item \code{residual_sum_of_squares}: Difference between observed and predicted ETR values, expressed as the sum of squared residuals.
#'   \item \code{root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the root mean squared error.
#'   \item \code{relative_root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the relative root mean squared error, normalized by the mean.
#'   \item \code{pmax}: Maximum electron transport rate (\eqn{p_{max}}).
#'   \item \code{a}: Parameter \eqn{a}.
#'   \item \code{alpha}: Parameter \eqn{\alpha}.
#'   \item \code{n}: Parameter \eqn{n}.
#'   \item \code{popt}: Maximum electron transport rate with photoinhibition (\eqn{p_{opt}}).
#'   \item \code{ik}: Transition point from light limitation to light saturation without photoinhibition (\eqn{I_k}).
#'   \item \code{iik}: Transition point from light limitation to light saturation with photoinhibition (\eqn{I_k^\prime}).
#'   \item \code{pmax_popt_and_ik_iik_ratio}: Ratio of \eqn{p_{max}} to \eqn{p_{opt}} and \eqn{I_k} to \eqn{I_k^\prime}.
#' }
#'
#' @references{
#'   Vollenweider, R. A. (1965). \emph{Calculation models of photosynthesis-depth curves and some implications regarding day rate estimates in primary production measurements},
#'   p. 427-457. In C. R. Goldman [ed.], \emph{Primary Productivity in Aquatic Environments}. Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.
#'
#' }
#'
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' result <- vollenweider_generate_regression_ETR_I(data)
#'
#' @export
vollenweider_generate_regression_ETR_I <- function(
    data,
    pmax_start_value = vollenweider_default_start_value_a,
    a_start_value = vollenweider_default_start_value_a,
    alpha_start_value = vollenweider_default_start_value_alpha,
    n_start_value = vollenweider_default_start_value_n) {
  return(vollenweider_generate_regression_internal(
    data,
    etr_1_type,
    pmax_start_value,
    a_start_value,
    alpha_start_value,
    n_start_value
  ))
}

#' Vollenweider Regression for ETR II
#'
#' Fits the Vollenweider (1965) regression model using original naming conventions from the publication.
#'
#' @param data A \code{data.table} from read function (e.g.\code{read_dual_pam_data}).
#' @param pmax_start_value Numeric. Initial value for \eqn{p_{max}}. Default: \code{pmax_start_values_vollenweider_default}.
#' @param a_start_value Numeric. Initial value for \eqn{a}. Default: \code{a_start_values_vollenweider_default}.
#' @param alpha_start_value Numeric. Initial value for \eqn{\alpha}. Default: \code{alpha_start_values_vollenweider_default}.
#' @param n_start_value Numeric. Initial value for \eqn{n}. Default: \code{n_start_values_vollenweider_default}.
#'
#' @details
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#vollenweider_generate_regression_etr_i-and-vollenweider_generate_regression_etr_ii}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{etr_regression_data}: Predicted ETR values.
#'   \item \code{residual_sum_of_squares}: Difference between observed and predicted ETR values, expressed as the sum of squared residuals.
#'   \item \code{root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the root mean squared error.
#'   \item \code{relative_root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the relative root mean squared error, normalized by the mean.
#'   \item \code{pmax}: Maximum electron transport rate (\eqn{p_{max}}).
#'   \item \code{a}: Parameter \eqn{a}.
#'   \item \code{alpha}: Parameter \eqn{\alpha}.
#'   \item \code{n}: Parameter \eqn{n}.
#'   \item \code{popt}: Maximum electron transport rate with photoinhibition (\eqn{p_{opt}}).
#'   \item \code{ik}: Transition point from light limitation to light saturation without photoinhibition (\eqn{I_k}).
#'   \item \code{iik}: Transition point from light limitation to light saturation with photoinhibition (\eqn{I_k^\prime}).
#'   \item \code{pmax_popt_and_ik_iik_ratio}: Ratio of \eqn{p_{max}} to \eqn{p_{opt}} and \eqn{I_k} to \eqn{I_k^\prime}.
#' }
#'
#' @references{
#'   Vollenweider, R. A. (1965). \emph{Calculation models of photosynthesis-depth curves and some implications regarding day rate estimates in primary production measurements},
#'   p. 427-457. In C. R. Goldman [ed.], \emph{Primary Productivity in Aquatic Environments}. Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.
#' }
#'
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' result <- vollenweider_generate_regression_ETR_II(data)
#'
#' @export
vollenweider_generate_regression_ETR_II <- function(
    data,
    pmax_start_value = vollenweider_default_start_value_pmax,
    a_start_value = vollenweider_default_start_value_a,
    alpha_start_value = vollenweider_default_start_value_alpha,
    n_start_value = vollenweider_default_start_value_n) {
  return(vollenweider_generate_regression_internal(
    data,
    etr_2_type,
    pmax_start_value,
    a_start_value,
    alpha_start_value,
    n_start_value
  ))
}

vollenweider_message <- function(msg) {
  if (is.character(msg) == FALSE) {
    message("vollenweider problem")
  } else {
    message("vollenweider: ", msg)
  }
}

vollenweider_generate_regression_internal <- function(
    data,
    etr_type,
    pmax_start_value = vollenweider_default_start_value_pmax,
    a_start_value = vollenweider_default_start_value_a,
    alpha_start_value = vollenweider_default_start_value_alpha,
    n_start_value = vollenweider_default_start_value_n) {
  tryCatch(
    {
      validate_etr_type(etr_type)
      validate_data(data)

      if (!is.numeric(pmax_start_value)) {
        stop("pmax start value is not a valid number")
      }
      if (!is.numeric(a_start_value)) {
        stop("a start value is not a valid number")
      }
      if (!is.numeric(alpha_start_value)) {
        stop("alpha start value is not a valid number")
      }
      if (!is.numeric(n_start_value)) {
        stop("n start value is not a valid number")
      }

      model <- minpack.lm::nlsLM(
        data[[etr_type]] ~
          pmax * (((a * par) / (sqrt(1 + (a * par)^2))) * (1 / (sqrt(1 + (alpha * par)^2)^n))),
        data = data,
        start = list(
          pmax = pmax_start_value,
          a = a_start_value,
          alpha = alpha_start_value,
          n = n_start_value
        ),
        control = stats::nls.control(maxiter = 1000)
      )

      residual_sum_of_squares <- model$m$deviance()

      abc <- stats::coef(model)
      pmax <- abc[["pmax"]]
      a <- abc[["a"]]
      alpha <- abc[["alpha"]]
      n <- abc[["n"]]

      ik <- NA_real_
      tryCatch(
        {
          ik <- 1 / a
        },
        warning = function(w) {
          vollenweider_message(paste("failed to calculate ik: warning:", w))
        },
        error = function(e) {
          vollenweider_message(paste("failed to calculate ik: error:", e))
        }
      )

      popt <- 0
      pars <- c()
      predictions <- c()
      for (p in min(data$par):max(data$par)) {
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
      etr_regression_data <- create_regression_data(pars, predictions)

      measured_predicted_etr_par_data <- get_etr_data_for_par_values(data, etr_regression_data, etr_type)

      root_mean_squared_error <- root_mean_squared_error(measured_predicted_etr_par_data)

      relative_root_mean_squared_error <- relative_root_mean_squared_error(measured_predicted_etr_par_data)

      iik <- NA_real_
      tryCatch(
        {
          iik <- (ik * popt) / pmax
        },
        warning = function(w) {
          vollenweider_message(paste("failed to calculate iik: warning:", w))
        },
        error = function(e) {
          vollenweider_message(paste("failed to calculate iik: error:", e))
        }
      )

      pmax_popt_and_ik_iik_ratio <- NA_real_
      tryCatch(
        {
          pmax_popt_and_ik_iik_ratio <- ik / iik
        },
        warning = function(w) {
          vollenweider_message(paste("failed to calculate pmax_popt_and_ik_iik_ratio: warning:", w))
        },
        error = function(e) {
          vollenweider_message(paste("failed to calculate pmax_popt_and_ik_iik_ratio: error:", e))
        }
      )

      result <- list(
        etr_type = etr_type,
        etr_regression_data = etr_regression_data,
        residual_sum_of_squares = residual_sum_of_squares,
        root_mean_squared_error = root_mean_squared_error,
        relative_root_mean_squared_error = relative_root_mean_squared_error,
        pmax = pmax,
        a = a,
        alpha = alpha,
        n = n,
        ik = ik,
        popt = popt,
        iik = iik,
        pmax_popt_and_ik_iik_ratio = pmax_popt_and_ik_iik_ratio
      )
      validate_model_result(result)
      return(result)
    },
    warning = function(w) {
      warning("warning while calculating vollenweider model: ", w)
    },
    error = function(e) {
      stop("error while calculating vollenweider model: ", e)
    }
  )
}

#' Vollenweider Model Modification
#'
#' This function adds parameters that were not originally included in the Vollenweider (1965) model, but were introduced by other models, and renames the parameters to a standardized one for all models.
#'
#' @param model_result A list containing the model result (e.g. from vollenweider_generate_regression_ETR_II()).
#'
#' @return A modified model result as a list containing the following elements:
#' \itemize{
#'   \item \code{etr_type}: ETR Type based on the model result.
#'   \item \code{etr_regression_data}: Regression data with ETR predictions based on the fitted model.
#'   \item \code{residual_sum_of_squares}: Difference between observed and predicted ETR values, expressed as the sum of squared residuals.
#'   \item \code{root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the root mean squared error.
#'   \item \code{relative_root_mean_squared_error}: Difference between observed and predicted ETR values, expressed as the relative root mean squared error, normalized by the mean.
#'   \item \code{a}: Obtained parameter \code{a}, here equal to \code{etrmax_without_photoinhibition}.
#'   \item \code{b}: Obtained parameter \code{b}, transferred as \code{a}.
#'   \item \code{c}: Obtained parameter \code{c}, here transferred as \code{alpha}.
#'   \item \code{d}: Obtained parameter \code{d}, here transferred as \code{n}.
#'   \item \code{alpha}: The initial slope of the light curve.
#'   \item \code{beta}: Not available, here set to \code{NA_real_}.
#'   \item \code{etrmax_with_photoinhibition}: The maximum electron transport rate with photoinhibition, transferred as \code{popt}.
#'   \item \code{etrmax_without_photoinhibition}: The maximum electron transport rate without photoinhibition, transferred as \code{pmax}.
#'   \item \code{ik_with_photoinhibition}: PAR where the transition point from light limitation to light saturation is achieved taking photoinhibition into account, transferred as \code{iik}.
#'   \item \code{ik_without_photoinhibition}: PAR where the transition point from light limitation to light saturation is achieved not taking photoinhibition into account, transferred as \code{ik}.
#'   \item \code{im_with_photoinhibition}: The PAR at which the maximum electron transport rate is achieved by taking photoinhibition into account, determined using the regression data from the model.
#'   \item \code{w}: Not available, here set to \code{NA_real_}.
#'   \item \code{ib}: Transferred unchanged as \code{ib}.
#'   \item \code{etrmax_with_without_ratio}: Ratio of \code{etrmax_with_photoinhibition} to \code{etrmax_without_photoinhibition} and \code{ik_with_photoinhibition} to \code{ik_without_photoinhibition}.
#' }
#'
#' @details
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#vollenweider_modified}
#'
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' result <- vollenweider_generate_regression_ETR_II(data)
#' modified_result <- vollenweider_modified(result)
#'
#' @export
vollenweider_modified <- function(model_result) {
  validate_model_result(model_result)

  etr_regression_data <- get_etr_regression_data_from_model_result(model_result)
  im_with_photoinhibition <- etr_regression_data[etr_regression_data[[prediction_name]] == max(etr_regression_data[[prediction_name]]), ][[PAR_name]]

  result <- create_modified_model_result(
    etr_type = get_etr_type_from_model_result(model_result),
    etr_regression_data = get_etr_regression_data_from_model_result(model_result),
    residual_sum_of_squares = get_sdiff_from_model_result(model_result),
    model_result[["root_mean_squared_error"]],
    model_result[["relative_root_mean_squared_error"]],
    a = model_result[["pmax"]],
    b = model_result[["a"]],
    c = model_result[["alpha"]],
    d = model_result[["n"]],
    alpha = model_result[["popt"]] / model_result[["iik"]],
    beta = NA_real_,
    etrmax_with_photoinhibition = model_result[["popt"]],
    etrmax_without_photoinhibition = model_result[["pmax"]],
    ik_with_photoinhibition = model_result[["iik"]],
    ik_without_photoinhibition = model_result[["ik"]],
    im_with_photoinhibition = im_with_photoinhibition,
    w = NA_real_,
    ib = NA_real_,
    etrmax_with_without_ratio = model_result[["pmax_popt_and_ik_iik_ratio"]]
  )

  return(result)
}
