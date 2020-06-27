#' Lag Index Combination
#'
#' Generates a data frame whose rows correspond to indexes showing the initial and final
#' lag to be used in a regression as finite distributed lag model with two explanatory
#' variables, for example. This function is useful as an intermediate step for constructing lag variables. See details.
#'
#' @param p Integer, maximum lag \code{p}.
#' @param q Integer, maximum lag \code{q}.
#' @param recursive Logical, \code{FALSE} or \code{TRUE} (see details).
#'
#' @details Condiser the following regression:
#' \itemize{
#' \item \eqn{y_{i,t} = a_0 + y_{i,t-pstart} + ... + y_{i,t-pend} + x_{i,t-qstart} + ... + x_{i,t-qend} + e_{i,t}}.
#' }
#' \code{lags} constructs a data frame with indexes \eqn{pstar}, \eqn{pend},
#' \eqn{qstart} and \eqn{qend}. This is an intermediate step for obtaining
#' the variables to estimate a lot of possible models.
#' If \code{recursive = FALSE}, both \eqn{pstart} and \eqn{qstart} are always = 1
#' and \eqn{pend} and \eqn{qend} grows from one to \code{p} and \code{q}, respectively.
#' If \code{recursive = TRUE}, on the other hand, \eqn{pstart} and \eqn{qstart}
#' also grows from one to \code{p} and \code{q}. This last case is
#' appropriated if one wants to construct variables for
#' estimating models with more combinations of lags.
#' The cost of this option is that it is time-consuming in
#' terms of computation.
#'
#'
#'
#' @return Data frame
#'
#' @export
#' @examples
#' lags(2, 1, recursive = FALSE)
lags <- function(p, q, recursive = FALSE) {
  lag_df <- data.frame(
    pstart = rep(1:p, each = p),
    pend = 1:p,
    qstart = rep(1:q, each = p^2 * q),
    qend = rep(1:q, each = p^2)
  )
  if (recursive == TRUE) {
    lag_df %>% dplyr::filter(
      lag_df$pstart <= lag_df$pend &
        lag_df$qstart <= lag_df$qend
    )
  } else {
    lag_df %>% dplyr::filter(
      lag_df$pstart == 1 &
        lag_df$qstart == 1
    )
  }
}
