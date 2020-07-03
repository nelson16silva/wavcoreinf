#' Bias
#'
#' Estimates a regression model of
#' \code{y} (healine inflation, \eqn{\pi})
#' in \code{x} (core inflation, \eqn{\pi^*}),
#' \itemize{
#' \item \eqn{\pi_{it} = a + b\pi^*_{it} + e_{it}},
#' } and
#' gives statistics for \eqn{a, b, \overline{R}^2} and F-test \eqn{H0:
#' a = 0, b = 1}. \code{no_bias} is useful to find out
#' whether or not the bias in a core inflation measure (\eqn{\pi^*}) is
#' significant.
#'
#' @param y A numeric vector or \code{ts} (inflation, \eqn{\pi})
#' @param x A numeric vector or \code{ts }(core inflation, \eqn{\pi^*})
#'
#' @return A (1 x 4) tibble.
#'
#' @seealso \code{\link{no_bias2}}
#'
#' @export
#'
#' @examples
#' inf_head <- coreinf_br[["ipca"]]
#' inf_corems <- coreinf_br[["ipcams"]]
#' no_bias(inf_head, inf_corems)
no_bias <- function(y, x) {
  model <- stats::lm(y ~ x)
  ht <- car::linearHypothesis(model, c("(Intercept) = 0", "x = 1"))
  df <- tibble::tibble(a = stats::coef(model)[1],
               b = stats::coef(model)[2],
               `Teste-F` = ht$`Pr(>F)`[2],
               R2 = summary(model)$adj.r.squared)
  df
}

#' Bias p-value
#'
#' Extracts p-value from the model estimated by the function
#' \code{\link{no_bias}}. If the mean of \code{y} is equal of  \code{x}, p-value is
#' expected to be higher than 0.05. In this circuntance, bias of
#' the core inflation measure is not significant. See \code{\link{no_bias}}
#' for more information.
#'
#'
#' @inheritParams no_bias
#'
#' @return A numeric vector of length one.
#' @export
#' @seealso \code{\link{no_bias}}
#' @examples
#' inf_head <- coreinf_br[["ipca"]]
#' inf_corems <- coreinf_br[["ipcams"]]
#' no_bias2(inf_head, inf_corems)
no_bias2 <- function(y, x) {
  IPCA <- y
  model <- stats::lm(IPCA ~ x)
  ht <- car::linearHypothesis(model, c("(Intercept) = 0", "x = 1"))
  ht$`Pr(>F)`[2]
}
