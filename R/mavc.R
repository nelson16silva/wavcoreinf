#' Centered Moving Avarage
#'
#' \code{mavc} calculates the \code{k}-period centered moving avarage of a vector or time series.
#' When \code{k} is even the series length is increased by one and the two
#' endpoints are weighted by 0.5.
#'
#' @param x Numeric vector or a time series.
#' @param k Integer, window length.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' coreinf_br %>%
#' dplyr::select(-date) %>%
#' purrr::map_df(~mavc(.x, 25)) %>%
#' tibble::add_column(date = coreinf_br$date, .before = 1)
mavc <- function(x, k) {
  n <- length(x)
  kleft <- (k - 1) / 2
  kright <- kleft
  out <- vector("double", n)
  if (k %% 2 == 1) {
    for (i in seq_along(x)) {
      if (i <= kleft) {
        out[i] <- sum(x[1:(i + kright)]) / length(1:(i + kright))
      } else if (i < (n - kright)) {
        out[i] <- sum(x[(i - kleft):(i + kright)]) / length((i - kleft):(i + kright))
      } else {
        out[i] <- sum(x[(i - kleft):n]) / length((i - kleft):n)
      }
    }
  } else {
    kodd <- k + 1
    koddleft <- k / 2
    koddright <- koddleft
    for (i in seq_along(x)) {
      if (i == 1) {
        out[i] <- sum(x[1:(i + koddright - 1)], 0.5 * x[(i + koddright)]) / (koddright + 0.5)
      } else if (i <= koddleft) {
        out[i] <- sum(x[1:((i + koddright) - 1)], 0.5 * x[(i + koddright)]) / (length(1:(i + koddright)) - 0.5)
      } else if (i <= (n - koddright)) {
        out[i] <- sum(0.5 * x[(i - koddleft)], x[(i - koddleft + 1):(i + koddright - 1)], 0.5 * x[(i + koddright)]) / k
      } else if (i < n) {
        out[i] <- sum(0.5 * x[(i - koddleft)], x[(i - koddleft + 1):n]) / (length((i - koddright):n) - 0.5)
      } else {
        out[i] <- sum(0.5 * x[(i - koddleft)], x[(i - koddleft + 1):n]) / (koddleft + 0.5)
      }
    }
  }
  out
}
