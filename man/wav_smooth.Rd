% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wavcore.R
\name{wav_smooth}
\alias{wav_smooth}
\title{Wavelet Smooth}
\usage{
wav_smooth(x, y)
}
\arguments{
\item{x}{A vector or a time series containing the data (headline inflation).}

\item{y}{An object of class args_wshr, args_ebthr or args_wthr. Those
objects can be obtained through functions \code{\link{wav_args_ebthr}},
\code{\link{wav_args_wshr}} and \code{\link{wav_args_wthr}}.}
}
\value{
A tibble containing the estimated wavelet-based signal
(\strong{x_sm} column). Arguments
that are not presented in the tibble are the default ones of the respective
function that generated the signal (wavelet core inflation).
}
\description{
This functional is a generic function that computes
wavelet-based signals by mapping diferent arguments
that can be used directly or indirectly in functions \code{wmta::\link[wmtsa]{wavShrink}}, \cr
\code{EbayesThresh::\link[EbayesThresh]{ebayesthresh.wavelet}} and
\code{wavethresh::\link[wavethresh]{threshold}}. Each estimated signal
is a wavelet core inflation measure. \code{wav_smooth}
calls \code{\link{smooth_wavelet}} for the estimation of the wavelet-based signal.
}
\examples{
wshr_obj <- wav_args_wshr(list(
  wavelet = c("haar", "d4", "d6", "d8", "s8"),
  n.level = 1:4
))

inf_head <- coreinf_br[["ipca"]]

wav_smooth(inf_head, wshr_obj)
}
\seealso{
\code{\link{smooth_wavelet}}, \code{\link{wav_args_ebthr}},
\code{\link{wav_args_wshr}},
\code{\link{wav_args_wthr}}.
}
