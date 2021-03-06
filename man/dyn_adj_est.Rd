% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dynadj.R
\name{dyn_adj_est}
\alias{dyn_adj_est}
\title{Estimate Models for Dynamic Adjustment Analysis}
\usage{
dyn_adj_est(y, xreg, h, p, ...)
}
\arguments{
\item{y}{A vector. If the user estimates the first regression described
below, \code{y} represents headline inflation or
core inflation measure if the regression is the second. The choice depends if interest is in
\eqn{\lambda_h} or in \eqn{\lambda^*_h}.}

\item{xreg}{A vector. Infation or core inflation depending of the \code{y} choice.}

\item{h}{An integer. The horizon of prediction.}

\item{p}{An integer to specify the lag p.}

\item{...}{Additional parameter to pass to the function
\code{\link{lags}} for specifying if it is recursive or not.}
}
\value{
A list where each element of it contains the following:
\item{data}{A tibble with the data used in fitting the model.}
\item{model}{A \code{lm} object.}
\item{n_obs}{Number of observations of \code{y}.}
\item{h}{Horizon used in direct estimation.}
}
\description{
Use \code{dyn_adj_est} to estimate several models for dynamic
adjustment analysis.
}
\details{
For \eqn{\pi} being the healine inflation and \eqn{\pi^*}
a core inflation measure, two specifications of a regression model can be estimated through this
function:
\enumerate{
\item \eqn{\pi_{t + h} - \pi_t =
 a_0 + \lambda_h(\pi_t - \pi^*_t) + \sum_{i = 1}^pa_i\pi_{t - i} +
 e_{t + h}},
 \item  \eqn{\pi^*_{t + h} - \pi^*_t =
 a^*_0 + \lambda^*_h(\pi_t - \pi^*_t) + \sum_{i = 1}^pa^*_i\pi^*_{t - i} +
 e^*_{t + h}}.
}
}
\examples{
inf_head <- coreinf_br[["ipca"]]
inf_corems <- coreinf_br[["ipcams"]]
dyn_adj_est(inf_head, inf_corems, 2, 2)
dyn_adj_est(inf_corems, inf_head, 2, 2)
}
\seealso{
\code{\link{lags}, \link{comb_regr}}
}
