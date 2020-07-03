#' Determine Scale Factor Parameter "a" for Laplace Distribution
#'
#' The function \code{prior_a} determines the parameter
#' \code{a} of the laplace distribution that is used in
#' \link[=ebayesthresh.wavelet]{empirical
#' Bayes thresholding approach}. Estimation  is based on
#' minimizing the avarage of forecast error.
#'
#'
#' @param interval A vector containing the end-points of the interval to be searched for the minimum. See: \code{stats::\link[stats]{optimize}}.
#' @param tol The desired accuracy. See: \code{stats::\link[stats]{optimize}}.
#' @param wtfun A wavelet transform function: \code{waveslim::\link[waveslim]{dwt}} or \code{waveslim::\link[waveslim]{modwt}}.
#' @param wtfunlist A named list of parameters to pass to the function \code{\link[=dwt]{dwt}} or \code{\link[=modwt]{modwt}}.
#' @inheritParams model_error
#' @inheritParams wav_smooth
#' @param ... Additional parameters to pass to the function \code{\link{smooth_wavelet}}.
#'
#'
#' @return A numeric vector of length one.
#' @export
#' @importFrom stats optimise
#'
#' @examples
#' pq <- lags(2, 1)
#' ipca <- coreinf_br$ipca
#' prior_a(
#'   x = ipca, h = 2, k = 15,
#'   lags = pq, interval = c(0.1, 3),
#'   tol = 0.2, wtfun = "dwt",
#'   wtfunlist = list(
#'     wf = "haar", n.levels = 4,
#'     boundary = "reflection"
#'   ),
#'   vscale = "independet", threshrule = "mean"
#' )
prior_a <- function(x, h, k, lags, interval = c(0.1, 5), tol = 0.01, wtfun = "dwt", wtfunlist = list(), ...) {
  a_constructor <- function(a) {
    mean(purrr::map_dbl(1:h, function(h) {
      list <- list(
        k = k, y = x, lags = lags, h = h,
        wtfun = wtfun, wtfunlist = wtfunlist,
        thfun = "ebayesthresh.wavelet", a = a, ...
      )
      do.call(model_error_wave_single, list)
    }))
  }
  optimise(a_constructor, interval, tol = tol)$minimum
}


#' Determine 	Hyperparameters for Bayesian Wavelet Thresholding
#'
#' The function \code{prior_alpha_beta} determines both the parameters
#' \code{alpha} and \code{beta} that are used
#' in  \link[=BAYES.THR]{bayesian wavelet thresholding
#' of noisy data}. Estimation  is based on
#' minimizing the avarage of forecast error.
#'
#'
#' @inheritParams prior_a
#' @param par Initial values for the parameters to be optimized over.
#' @param lower Bounds on the variables for the "L-BFGS-B" method. See \code{stats::\link[stats]{optim}}.
#' @param upper Bounds on the variables for the "L-BFGS-B" method. See  \code{stats::\link[stats]{optim}}.
#' @param control A list of control parameters. See \code{stats::\link[stats]{optim}}.
#' @param wtfunlist A named list of parameters to pass to function \code{wavethresh::\link[wavethresh]{wd}}.
#' @param ... Additional parameters to pass to function \code{\link{smooth_wavelet}}.
#' Do not pass the argument \code{policy} used in function
#' \code{wavethresh::\link[wavethresh]{threshold.wd}}.
#'
#' @return A numeric vector of length two.
#' @export
#'
#' @examples
#'
#' # Example 1
#'
#' pq <- lags(1, 1)
#' ipca <- coreinf_br$ipca
#' prior_alpha_beta(
#' x = ipca, h = 2, k = 5, lags = pq,
#' lower = c(0.2, 0.5),
#' wtfunlist = list(type = "wavelet"),
#' type = "hard"
#' )
#'
#' # Example 2
#'
#' \dontrun{
#'
#' # Including the best priors for denoising
#'
#' wthr_wd <- list(
#' filter.number = 1,
#' family = c("DaubExPhase"),
#' type = c("wavelet", "station")
#' )
#'
#' # Note: initial alpha and beta must be included such
#' # that the appropriate structure of wthr_args is maitained
#' # after the inclusion of estimated priors
#'
#' wthr_thr <- list(
#'   type = c("soft"),
#'   alpha = 0.5,
#'   beta = 1
#' )
#'
#' wthr_args <- wav_args_wthr(wthr_wd, wthr_thr, 3)
#'
#' # Best priors estimation and inclusion in wthr_args
#'
#' args_tbl2 <- wthr_args$args_tbl %>%
#' dplyr::mutate(ab = purrr::pmap(., ~purrr::possibly(prior_alpha_beta, NA)(
#'   ipca, 2, 4, pq,
#'   lower = c(0.4, 0.6), upper = c(0.6, 1.1),
#'   control = list(reltol = 0.01),
#'   wtfunlist = list(..1, ..2, ..3),
#'   type = ..4, lr = ..7)),
#'   alpha = purrr::map_dbl(ab, ~`[`(.x, 1)),
#'   beta = purrr::map_dbl(ab, ~`[`(.x, 2))) %>%
#'   dplyr::select(-ab)
#'
#' wthr_args[[1]] <- args_tbl2
#'
#' # Denoising after including estimated priors
#'
#' wav_smooth(ipca, wthr_args)
#' }
#'
prior_alpha_beta <- function(x, h, k, lags,
                             par = c(0.5, 1),
                             lower = c(0, 0),
                             upper = c(3, 3),
                             control = list(),
                             wtfunlist = list(), ...) {
  ab_constructor <- function(ab) {
    mean(purrr::map_dbl(1:h, function(.h) {
      list <- list(
        k = k, y = x, lags = lags, h = .h,
        wtfun = "wd", wtfunlist = wtfunlist,
        thfun = "threshold", alpha = ab[1], beta = ab[2],
        policy = "BayesThresh", ...
      )
      do.call(model_error_wave_single, list)
    }))
  }
  stats::optim(par,
    ab_constructor,
    lower = lower, upper = upper,
    method = "L-BFGS-B"
  )$par
}
