pmax_start_value_vollenweider_default <- 40
a_start_value_vollenweider_default <- 0.1
alpha_start_value_vollenweider_default <- -0.0001
n_start_value_vollenweider_default <- 350

#' Generate Regression for ETR I using the Vollenweider Model (1965)
#'
#' This function generates a regression model based on the Vollenweider
#' formula. Original naming conventions from the publication are used.
#'
#' @param data A `data.table` containing the input data from \link[pam]{read_pam_data}
#' @param etr_type A character string specifying the column name of the 
#' response variable (in this case: ETR I) to be used in the model.
#' @param pmax_start_value_vollenweider Numeric. The starting value for the parameter \eqn{pmax} 
#' in the model. Defaults to \code{pmax_start_values_vollenweider_default}.
#' @param a_start_value_vollenweider Numeric. The starting value for the parameter \eqn{a} 
#' in the model. Defaults to \code{a_start_values_vollenweider_default}.
#' @param alpha_start_value Numeric. The starting value for the parameter \eqn{alpha} 
#' in the model. Defaults to \code{alpha_start_values_vollenweider_default}.
#' #' @param n_start_value Numeric. The starting value for the parameter \eqn{n} 
#' in the model. Defaults to \code{n_start_values_vollenweider_default}.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{etr_regression_data}{A `data.table` with the predicted values of ETR I to each PAR based on the fitted model.}
#'   \item{sdiff}{The deviation between the actual and predicted ETR values.}
#'   \item{pmax}{The maximum electron transport rate without photoinhibition.}
#'   \item{a}{The obtained parameter \eqn{a}.}
#'   \item{alpha}{The obtained parameter \eqn{alpha}.}
#'   \item{n}}{The obtained parameter \eqn{n}.}
#'   \item{popt}{The maximum electron transport rate with photoinhibition. A function computes predicted photosynthetic rates for each PAR value and tracks the maximum rate observed. This eliminates the need to determine the partitioning factor for each value of n in advance.}
#'   \item{ik}{PAR where the transition point from light limitation to light saturation is achieved without photoinhibition, calculated as \eqn(ik = 1 / a)}
#'   \item{iik}{PAR where the transition point from light limitation to light saturation is achieved with photoinhibition, calculated as \eqn(iik = (ik * popt) / pmax)}
#'   \item{pmax_popt_and_ik_iik_ratio}{Ratio of pmax to popt and ik to iik, calculated as \eqn(pmax_popt_and_ik_iik_ratio = ik / iik)}

#'
#' @details
#' This function uses non-linear least squares fitting to estimate the parameters 
#' for the Vollenweider model, which describes the relationship between PAR and 
#' ETR. The model used is:
#'
#' \eqn{p = pmax * (((a * i) / (sqrt(1 + (a * i)^2))) * (1 / (sqrt(1 + (alpha * i)^2)^n))}
#'
#' It is valid: i = PAR; p = ETR
#'
#' @references
#' Vollenweider, R. A. (1965). Calculation models of photosynthesis-depth curves 
#' and some implications regarding day rate estimates in primary production measurements, 
#' p. 427-457. In C. R. Goldman [ed.], *Primary Productivity in Aquatic Environments*. 
#' Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.
#'
#' @seealso \code{\link{nlsLM}}, \code{\link{minpack.lm}}
#' @importFrom minpack.lm nlsLM
#' @importFrom data.table data.table
#' @export
generate_regression_vollenweider_ETR_I <- function(
    data,
    pmax_start_value = a_start_value_vollenweider_default,
    a_start_value = a_start_value_vollenweider_default,
    alpha_start_value = alpha_start_value_vollenweider_default,
    n_start_value = n_start_value_vollenweider_default) {
  return(generate_regression_vollenweider_internal(
    data,
    etr_I_type,
    pmax_start_value,
    a_start_value,
    alpha_start_value,
    n_start_value
  ))
}

#' Generate Regression for ETR II using the Vollenweider Model (1965)
#'
#' This function generates a regression model based on the Vollenweider
#' formula. Original naming conventions from the publication are used.
#'
#' @param data A `data.table` containing the input data from \link[pam]{read_pam_data}
#' @param etr_type A character string specifying the column name of the 
#' response variable (in this case: ETR II) to be used in the model.
#' @param pmax_start_value_vollenweider Numeric. The starting value for the parameter \eqn{pmax} 
#' in the model. Defaults to \code{pmax_start_values_vollenweider_default}.
#' @param a_start_value_vollenweider Numeric. The starting value for the parameter \eqn{a} 
#' in the model. Defaults to \code{a_start_values_vollenweider_default}.
#' @param alpha_start_value Numeric. The starting value for the parameter \eqn{alpha} 
#' in the model. Defaults to \code{alpha_start_values_vollenweider_default}.
#' #' @param n_start_value Numeric. The starting value for the parameter \eqn{n} 
#' in the model. Defaults to \code{n_start_values_vollenweider_default}.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{etr_regression_data}{A `data.table` with the predicted values of ETR II to each PAR based on the fitted model.}
#'   \item{sdiff}{The deviation between the actual and predicted ETR values.}
#'   \item{pmax}{The maximum electron transport rate without photoinhibition.}
#'   \item{a}{The obtained parameter \eqn{a}.}
#'   \item{alpha}{The obtained parameter \eqn{alpha}.}
#'   \item{n}}{The obtained parameter \eqn{n}.}
#'   \item{popt}{The maximum electron transport rate with photoinhibition. A function computes predicted photosynthetic rates for each PAR value and tracks the maximum rate observed. This eliminates the need to determine the partitioning factor for each value of n in advance.}
#'   \item{ik}{PAR where the transition point from light limitation to light saturation is achieved without photoinhibition, calculated as \eqn(ik = 1 / a)}
#'   \item{iik}{PAR where the transition point from light limitation to light saturation is achieved with photoinhibition, calculated as \eqn(iik = (ik * popt) / pmax)}
#'   \item{pmax_popt_and_ik_iik_ratio}{Ratio of pmax to popt and ik to iik, calculated as \eqn(pmax_popt_and_ik_iik_ratio = ik / iik)}

#'
#' @details
#' This function uses non-linear least squares fitting to estimate the parameters 
#' for the Vollenweider model, which describes the relationship between PAR and 
#' ETR. The model used is:
#'
#' \eqn{p = pmax * (((a * i) / (sqrt(1 + (a * i)^2))) * (1 / (sqrt(1 + (alpha * i)^2)^n))}
#'
#' It is valid: i = PAR; p = ETR
#'
#' @references
#' Vollenweider, R. A. (1965). Calculation models of photosynthesis-depth curves 
#' and some implications regarding day rate estimates in primary production measurements, 
#' p. 427-457. In C. R. Goldman [ed.], *Primary Productivity in Aquatic Environments*. 
#' Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.
#'
#' @seealso \code{\link{nlsLM}}, \code{\link{minpack.lm}}
#' @importFrom minpack.lm nlsLM
#' @importFrom data.table data.table
#' @export
generate_regression_vollenweider_ETR_II <- function(
    data,
    pmax_start_value = pmax_start_value_vollenweider_default,
    a_start_value = a_start_value_vollenweider_default,
    alpha_start_value = alpha_start_value_vollenweider_default,
    n_start_value = n_start_value_vollenweider_default) {
  return(generate_regression_vollenweider_internal(
    data,
    etr_II_type,
    pmax_start_value,
    a_start_value,
    alpha_start_value,
    n_start_value
  ))
}

generate_regression_vollenweider_internal <- function(
    data,
    etr_type,
    pmax_start_value = pmax_start_value_vollenweider_default,
    a_start_value = a_start_value_vollenweider_default,
    alpha_start_value = alpha_start_value_vollenweider_default,
    n_start_value = n_start_value_vollenweider_default) {
  library(data.table)
  library(minpack.lm)

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

      data <- remove_det_row_by_etr(data, etr_type)

      model <- nlsLM(
        data[[etr_type]] ~
          pmax * (((a * PAR) / (sqrt(1 + (a * PAR)^2))) * (1 / (sqrt(1 + (alpha * PAR)^2)^n))),
        data = data,
        start = list(
          pmax = pmax_start_value,
          a = a_start_value,
          alpha = alpha_start_value,
          n = n_start_value
        ),
        control = nls.control(maxiter = 1000)
      )

      abc <- coef(model)
      pmax <- abc[["pmax"]]
      a <- abc[["a"]]
      alpha <- abc[["alpha"]]
      n <- abc[["n"]]
      ik <- 1 / a

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
      etr_regression_data <- create_regression_data(pars, predictions)

      iik <- (ik * popt) / pmax
      pmax_popt_and_ik_iik_ratio <- ik / iik

      sdiff <- NA_real_
      tryCatch(
        {
          sdiff <- calculate_sdiff(data, etr_regression_data, etr_type)
        },
        warning = function(w) {
          message("failed to calculate sdiff: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate sdiff: Error:", e)
        }
      )

      return(list(
        etr_regression_data = etr_regression_data,
        sdiff = sdiff,
        pmax = pmax,
        a = a,
        alpha = alpha,
        n = n,
        ik = ik,
        popt = popt,
        iik = iik,
        pmax_popt_and_ik_iik_ratio = pmax_popt_and_ik_iik_ratio
      ))
    },
    warning = function(w) {
      stop("Warning while calculating vollenweider model: ", w)
    },
    error = function(e) {
      stop("Error while calculating vollenweider model: ", e)
    }
  )
}

#' @title Control Plot for Vollenweider Model (1965)
#' @description This function creates a plot for the Vollenweider Model based on the provided data and model results.
#'
#' @param data A `data.table` containing the original ETR and yield data for the plot.
#' @param model_result A list containing the fitting results of the Vollenweider Model and the calculated paramters (alpha, ik...).
#' @param etr_type A character string describing the ETR type (ETR I or ETR II).
#' @param title A character string that specifies the title of the plot.
#'
#' @return A plot displaying the original ETR and Yield values and the regression data. A table below the plot shows the calculated data (alpha, ik...)
#'
#' @references
#' Vollenweider, R. A. (1965). Calculation models of photosynthesis-depth curves 
#' and some implications regarding day rate estimates in primary production measurements, 
#' p. 427-457. In C. R. Goldman [ed.], *Primary Productivity in Aquatic Environments*. 
#' Mem. Ist. Ital. Idrobiol., 18 Suppl., University of California Press, Berkeley.
#' 
#' @export
plot_control_vollenweider <- function(data, model_result, etr_type, title) {
  # validate model result
  values <- c(
    as.character(round(model_result[["sdiff"]], 3)),
    as.character(round(model_result[["pmax"]], 7)),
    as.character(round(model_result[["a"]], 6)),
    as.character(round(model_result[["alpha"]], 6)),
    as.character(round(model_result[["n"]], 3)),
    as.character(round(model_result[["ik"]], 3)),
    as.character(round(model_result[["popt"]], 3)),
    as.character(round(model_result[["iik"]], 3)),
    as.character(round(model_result[["pmax_popt_and_ik_iik_ratio"]], 3))
  )

  params <- data.frame(
    Parameter = c(
      "sdiff",
      "pmax",
      "a",
      "alpha",
      "n",
      "ik",
      "popt",
      "iik",
      "pmax_popt_and_ik_iik_ratio"
    ),
    Value = unlist(values)
  )

  return(
    plot_control(
      data,
      model_result,
      etr_type,
      title,
      color_vollenweider,
      params
    )
  )
}
