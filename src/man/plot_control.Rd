% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{plot_control}
\alias{plot_control}
\title{Plot Control}
\usage{
plot_control(data, model_result, title, color = "black")
}
\arguments{
\item{data}{A `data.table` containing the original ETR and yield data for the plot.}

\item{model_result}{A list containing the fitting results of the used model and the calculated paramters (alpha, ik...).}

\item{title}{A character string that specifies the title of the plot.}

\item{color}{A color specification for the regression line in the plot.}
}
\value{
A plot displaying the original ETR and Yield values and the regression data. A table below the plot shows the calculated data (alpha, ik...)
}
\description{
This function creates a control plot for the used model based on the provided data and model results.
}
\details{
A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#plot_control}
}
\examples{
path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
data <- read_dual_pam_data(path)

result <- eilers_peeters_generate_regression_ETR_I(data)
plot_control(data, result, "Control Plot")

}
