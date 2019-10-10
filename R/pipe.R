## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Wavelet transform (decomposition)
#'
#' See \code{wavethresh::\link[wavethresh]{wd}} for details.
#'
#' @name wd
#' @keywords internal
#' @export
#' @importFrom wavethresh wd
NULL

#' (Inverse) Maximal Overlap Discrete Wavelet Transform
#'
#' See \code{waveslim::\link[waveslim]{modwt}} for details.
#'
#' @name modwt
#' @keywords internal
#' @export
#' @importFrom waveslim modwt
NULL


#' Discrete Wavelet Transform (DWT)
#'
#' See \code{waveslim::\link[waveslim]{dwt}} for details.
#'
#' @name dwt
#' @keywords internal
#' @export
#' @importFrom waveslim dwt
NULL


#' Empirical Bayes thresholding on the levels of a wavelet transform
#'
#' See \code{EbayesThresh::\link[EbayesThresh]{ebayesthresh.wavelet}} for details.
#' @name ebayesthresh.wavelet
#' @keywords internal
#' @export
#' @importFrom EbayesThresh ebayesthresh.wavelet
NULL

#' Bayesian wavelet thresholding
#'
#' See \code{::wavethresh\link[wavethresh]{BAYES.THR}} for details.
#' @name BAYES.THR
#' @keywords internal
#' @export
#' @importFrom wavethresh BAYES.THR
NULL
