% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wavcore.R
\name{wav_args}
\alias{wav_args}
\title{Make Wavelet Arguments}
\usage{
wav_args(list)
}
\arguments{
\item{list}{A list of wavelet arguments.}
}
\value{
data frame
}
\description{
This function is used internally by other functions. There
are specific methods for each wavelet-based signal
estimation procedure: \code{\link{wav_args_wshr}},
\code{\link{wav_args_ebthr}},
\code{\link{wav_args_wthr}}.
}
\examples{
# wmtsa arguments for using with wmtsa package
wshr_wmtsa <- list(
  wavelet = c("haar", "d4", "d6", "d8", "s8"),
  n.level = 1:4,
  shrink.fun = c("hard", "soft", "mid"),
  thresh.fun = c("universal", "minimax", "adaptive"),
  xform = c("dwt", "modwt"),
  reflect = c(TRUE, FALSE)
)
wav_args(wshr_wmtsa)
}
\seealso{
\code{\link{wav_args_wshr}}, \code{\link{wav_args_ebthr}},
\code{\link{wav_args_wthr}}
}
