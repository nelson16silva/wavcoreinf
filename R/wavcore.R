#' Make Wavelet Arguments
#'
#' This function is used internally by other functions. There
#' are specific methods for each wavelet-based signal
#' estimation procedure: \code{\link{wav_args_wshr}},
#' \code{\link{wav_args_ebthr}},
#' \code{\link{wav_args_wthr}}.
#'
#' @param list A list of wavelet arguments.
#'
#' @return data frame
#'
#' @export
#' @seealso \code{\link{wav_args_wshr}}, \code{\link{wav_args_ebthr}},
#' \code{\link{wav_args_wthr}}
#' @examples
#' # wmtsa arguments for using with wmtsa package
#' wshr_wmtsa <- list(
#'   wavelet = c("haar", "d4", "d6", "d8", "s8"),
#'   n.level = 1:4,
#'   shrink.fun = c("hard", "soft", "mid"),
#'   thresh.fun = c("universal", "minimax", "adaptive"),
#'   xform = c("dwt", "modwt"),
#'   reflect = c(TRUE, FALSE)
#' )
#' wav_args(wshr_wmtsa)
wav_args <- function(list) {
  if (length(list) == 1) {
    data.frame(list, stringsAsFactors = FALSE)
  }
  else {
    leng <- purrr::map_dbl(list, length)
    for (i in 2:length(list)) {
      list[[i]] <- rep(list[[i]], each = prod(leng[1:(i - 1)]))
    }
    data.frame(list, stringsAsFactors = FALSE)
  }
}

#' Wavelet Shrinkage Arguments
#'
#' A tibble with combinations of some possible arguments
#' that can be passed to get wavelet-based signal
#' estimation from the function \code{wmtsa::\link[wmtsa]{wavShrink}}.
#'
#' @param wtlist A named list of wavelet arguments for getting
#' wavelet-based signal estimation from \code{wmtsa::\link[wmtsa]{wavShrink}}.
#'
#' @return S3 objetc of class args_wshr. This object is a list of
#' the following element:
#' \describe{
#' \item{args_tbl}{A tibble with arguments to get wavevel-based
#' signal estimation from \cr \code{wmtsa::\link[wmtsa]{wavShrink}}}.
#' }
#' @export
#' @seealso \code{\link{wav_smooth}}
#'
#' @examples
#' # wmtsa arguments for using with wmtsa package
#' wshr_wmtsa <- list(
#'   wavelet = c("haar", "d4", "d6", "d8", "s8"),
#'   n.level = 1:4,
#'   shrink.fun = c("hard", "soft", "mid"),
#'   thresh.fun = c("universal", "minimax", "adaptive"),
#'   xform = c("dwt", "modwt"),
#'   reflect = c(TRUE, FALSE)
#' )
#' wav_args_wshr(wshr_wmtsa)
wav_args_wshr <- function(wtlist) {
  args_tbl <- tibble::as.tibble(wav_args(wtlist))
  args_list <- list(args_tbl = args_tbl)
  attr(args_list, "class") <- "args_wshr"
  args_list
}

#' Wavelet EbayesThresh Arguments
#'
#' The main goal of this function is to generate a tibble
#' with combinations of some possible arguments that can be passed to
#' get wavelet-based signal estimation from the function \cr
#' \code{EbayesThresh::\link[EbayesThresh]{ebayesthresh.wavelet}}.
#'
#' @param xform A named list of wavelet transform methods. Types can be \code{"dwt"} and \code{"modwt"}.
#' @param wtlist A named list of arguments to use in the wavelet
#' decomposition, excluding the input vector \code{x}, performed by the
#' functions \code{waveslim::\link[waveslim]{dwt}} or \code{waveslim::\link[waveslim]{modwt}}.
#' @param ebthrlist A named list of arguments that can be passed to the function \cr
#' \code{EbayesThresh::\link[EbayesThresh]{ebayesthresh.wavelet}} excluding
#' \code{xtr}.
#'
#' @return S3 objetc of class args_ebthr. This object is a list of
#' the following elements:
#' \describe{
#' \item{args_tbl}{A tibble with arguments to get wavevel-based
#' signal estimation from \cr \code{EbayesThresh::\link[EbayesThresh]{ebayesthresh.wavelet}}}.
#' \item{nwt_args}{A integer vector of length two. The first
#' element is the number of arguments to pass to the function
#'  \code{waveslim::\link[waveslim]{dwt}} or \code{waveslim::\link[waveslim]{modwt}}. The
#'  second element is the number of arguments to the  \code{EbayesThresh::\link[EbayesThresh]{ebayesthresh.wavelet}}}.
#' }
#' @export
#' @seealso \code{\link{wav_smooth}}
#' @examples
#'
#' # Wavelet transform
#'
#' ebthr_xform <- list(wt = c("dwt", "modwt"))
#'
#' # Wavelet filter (package waveslim)
#'
#' ebthr_wt <- list(
#'   wf = c("haar", "d4", "d6", "la8"),
#'   n.levels = 1:4,
#'   boundary = c("periodic", "reflection")
#' )
#'
#' # Some arguments that can be passed to EbayesThresh::ebayesthresh.wavelet
#'
#' ebthr_ebwav <- list(
#'   vscale = c("level", "independet"),
#'   a = 0.5,
#'   prior = c("laplace", "cauchy"),
#'   threshrule = c("median", "mean", "soft", "hard")
#' )
#'
#' ebthr_args <- wav_args_ebthr(ebthr_xform, ebthr_wt, ebthr_ebwav)
wav_args_ebthr <- function(xform, wtlist, ebthrlist) {
  wtlist <- c(xform, wtlist)
  n1 <- length(wtlist)
  args_list <- c(wtlist, ebthrlist)
  n <- length(args_list)
  args_tbl <- tibble::as.tibble(wav_args(args_list))
  args_list <- list(args_tbl = args_tbl, nwt_args = c(n1, n))
  attr(args_list, "class") <- "args_ebthr"
  args_list
}

#' Wavelet Wavethresh Arguments
#'
#' The main goal of this function is to generate a tibble
#' with combinations of some possible arguments that can be passed to
#' get wavelet-based signal estimation from the function
#' \code{wavethresh::\link[wavethresh]{threshold}}.
#'
#'
#' @param wdlist A named list of arguments for wavelet transform by using
#' \code{wavethresh::\link[wavethresh]{wd}}, excluding the
#' \code{data} argument.
#' @param thrlist A named list of arguments that can be used in function
#' \code{wavethresh::\link[wavethresh]{threshold}}, excluding the \code{wd} argument.
#' @param lr An integer vector which determines the minimum scale
#'  level that is thresholded. The maximum scale level is given by
#'  \code{wavethresh::\link[wavethresh]{nlevelsWT}(wd)-1}. For example,
#'  \code{lr[1] = 3} is equivalent to \code{levels = 3:(nlevelsWT(wd) - 1)}
#'  in \code{wavethresh::\link[wavethresh]{threshold.wd}}.
#'
#' @return S3 objetc of class args_wthr. This object is a list of
#' the following elements:
#' \describe{
#' \item{args_tbl}{A tibble with arguments to get wavevel-based
#' signal estimation from \cr \code{wavethresh::\link[wavethresh]{threshold}}.}
#' \item{nwt_args}{A integer vector of length two. The first
#' element is the number of arguments to pass to the function
#'  \code{wavethresh::\link[wavethresh]{wd}}, excluding \code{data}. The
#'  second element is the number of arguments to the
#'  \code{wavethresh::\link[wavethresh]{threshold}} function, excluding \code{wd}.}
#' }
#' @export
#' @seealso \code{\link{wav_smooth}}
#'
#' @examples
#' wthr_wd <- list(
#'   filter.number = 1:4,
#'   family = c("DaubLeAsymm", "DaubExPhase"),
#'   type = c("wavelet", "station"),
#'   bc = c("periodic", "symmetric")
#' )
#'
#' wthr_thr <- list(
#'   type = c("soft", "hard"),
#'   policy = c(
#'     "universal",
#'     "sure",
#'     "BayesThresh", "cv"
#'   ),
#'   boundary = c(TRUE, FALSE)
#' )
#'
#' wthr_args <- wav_args_wthr(wthr_wd, wthr_thr, 3:5)
wav_args_wthr <- function(wdlist, thrlist, lr) {
  n1 <- length(wdlist)
  args_list <- c(wdlist, thrlist, list(lr = lr))
  n <- length(args_list)
  args_tbl <- tibble::as.tibble(wav_args(args_list))
  args_list <- list(args_tbl = args_tbl, nwt_args = c(n1, n))
  attr(args_list, "class") <- "args_wthr"
  args_list
}

#' Wavelet Smooth
#'
#' This functional is a generic function that computes
#' wavelet-based signals by mapping diferent arguments
#' that can be used directly or indirectly in functions \code{wmta::\link[wmtsa]{wavShrink}}, \cr
#' \code{EbayesThresh::\link[EbayesThresh]{ebayesthresh.wavelet}} and
#' \code{wavethresh::\link[wavethresh]{threshold}}. Each estimated signal
#' is a wavelet core inflation measure. \code{wav_smooth}
#' calls \code{\link{smooth_wavelet}} for the estimation of the wavelet-based signal.
#'
#' @param x A vector or a time series containing the data (headline inflation).
#' @param y An object of class args_wshr, args_ebthr or args_wthr. Those
#' objects can be obtained through functions \code{\link{wav_args_ebthr}},
#' \code{\link{wav_args_wshr}} and \code{\link{wav_args_wthr}}.
#'
#' @return A tibble containing the estimated wavelet-based signal
#' (\strong{x_sm} column). Arguments
#' that are not presented in the tibble are the default ones of the respective
#' function that generated the signal (wavelet core inflation).
#' @export
#' @seealso \code{\link{smooth_wavelet}}, \code{\link{wav_args_ebthr}},
#' \code{\link{wav_args_wshr}},
#' \code{\link{wav_args_wthr}}.
#'
#' @examples
#' wshr_obj <- wav_args_wshr(list(
#'   wavelet = c("haar", "d4", "d6", "d8", "s8"),
#'   n.level = 1:4
#' ))
#'
#' inf_head <- coreinf_br[["ipca"]]
#'
#' wav_smooth(inf_head, wshr_obj)
wav_smooth <- function(x, y) {
  UseMethod("wav_smooth", y)
}

#' @export
wav_smooth.args_wshr <- function(x, y) {
  args_tbl <- y[[1]]
  args_tbl %>%
    dplyr::mutate(
      x_sm =
        purrr::pmap(args_tbl, function(...) {
          args <- list(...)
          args$x <- x
          xsm <- do.call(purrr::possibly(wmtsa::wavShrink, NULL), args)
        })
    )
}

#' @export
wav_smooth.args_ebthr <- function(x, y) {
  args_tbl <- y[[1]]
  n1 <- y[[2]][1]
  n <- y[[2]][2]
  args_tbl %>%
    dplyr::mutate(
      x_sm =
        purrr::pmap(args_tbl, function(...) {
          lx <- length(x)
          xform <- list(...)[[1]][[1]]
          if (xform == "dwt") {
            x <- WiSEBoot::padVector(x, pad.direction = "rear", replaceLinearTrend = TRUE)[[1]]
          }
          args1 <- list(...)[2:n1]
          args1$x <- x
          if (xform == "modwt") {
            xtr <- do.call(waveslim::modwt, args1)
          }
          else {
            xtr <- do.call(waveslim::dwt, args1)
          }
          args2 <- list(...)[(n1 + 1):n]
          args2$xtr <- xtr
          if (xform == "modwt") {
            xsm <- waveslim::imodwt(do.call(EbayesThresh::ebayesthresh.wavelet, args2))
          }
          else {
            xsm <- waveslim::idwt(do.call(EbayesThresh::ebayesthresh.wavelet, args2))[1:lx]
          }
        })
    )
}

#' @export
wav_smooth.args_wthr <- function(x, y) {
  args_tbl <- y[[1]]
  n1 <- y[[2]][1]
  n <- y[[2]][2]
  args_tbl %>%
    dplyr::mutate(
      x_sm =
        purrr::pmap(args_tbl, function(...) {
          lx <- length(x)
          x <- WiSEBoot::padVector(x, pad.direction = "rear", replaceLinearTrend = TRUE)[[1]]
          args1 <- list(...)[1:n1]
          args1$data <- x
          xwd <- do.call(purrr::possibly(wavethresh::wd, NULL), args1)
          args2_aux <- list(...)[(n1 + 1):n]
          if (!is.null(xwd)) {
            lr <- args2_aux[["lr"]]
            args2_aux$levels <- lr:(wavethresh::nlevelsWT(xwd) - 1)
            args2_aux$lr <- NULL
          }
          if (!is.null(args1$type) & !is.null(args2_aux$type.1)) {
            args2_aux$type <- args2_aux$type.1
            args2_aux$type.1 <- NULL
          }
          args2 <- c(list(wd = xwd), args2_aux)
          xtr <- do.call(purrr::possibly(wavethresh::threshold, NULL), args2)
          if (is.null(xwd$type)) {
            xsm <- NULL
          } else if (xwd$type == "wavelet") {
            xsm <- do.call(purrr::possibly(wavethresh::wr, NULL), list(xtr))[1:lx]
          }
          else {
            xsm_aux <- do.call(purrr::possibly(wavethresh::convert, NULL), list(xtr))
            xsm <- do.call(purrr::possibly(wavethresh::AvBasis, NULL), list(xsm_aux))[1:lx]
          }
        })
    )
}
