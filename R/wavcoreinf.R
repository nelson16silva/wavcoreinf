#' wavcoreinf: estimation and evaluation of wavelet core infaltion measures
#'
#' \code{wavcoreinf} provides a flexible estimation and
#' evaluation of wavelet core inflation from wavelet-based signal
#' estimation methods. Functionals are developed to facilitate
#' the estimation of several specifications permitting
#' the selection of the best one according to specific criteria
#' established in the core inflation literature.
#'
#' @section Wavelet-based signal estimation:
#'
#' The wavelet methods for signal estimation are those
#' available in three \code{R packages}:
#' \itemize{
#' \item \code{wmtsa}
#' \item \code{EbayesThresh}
#' \item \code{wavethresh}
#' }
#' The specific methods are explained on the help page of
#' the functions: \code{wmtsa::\link[wmtsa]{wavShrink}}, \cr
#' \code{EbayesThresh::\link[EbayesThresh]{ebayesthresh.wavelet}} and
#' \code{wavethresh::\link[wavethresh]{threshold.wd}}
#'
#' The textbooks by Percival and Walden (2000)
#' and Nason (2008) explain in more details these methods.
#'
#' @section Core inflation:
#'
#' Certain criteria that a good core inflation measure should satisfy
#' and which are analyzed in this package include the following:
#'
#' \itemize{
#' \item Standard desviaton
#' \item Bias
#' \item Trend representation
#' \item Dynamic adjustment
#' \item Forecasting
#' }
#'
#' Examples using wavelet methods to construct core inflation
#' measure can be viewed in the works of Baqaee (2010)
#' and Dowd and Loh (2011). A general discussion
#' of core inflation measures for the Brazilian
#' case is available in Silva Filho and Figueiredo (2011).
#'
#' @references
#' Baqaee, David (2010). Using wavelets to measure core inflation: The case of New Zealand,
#' \emph{The North American Journal of Economics and Finance},
#' Volume 21, Issue 3,
#' Pages 241-255.
#'
#' Dowd, K., Cotter, J., & Loh, L. (2011).
#' U.S. core inflation: a wavelet analysis.
#' \emph{Macroeconomic Dynamics}, 15(4), 513-536.
#'
#' Nason, G.P. (2008). \emph{Wavelet Methods in
#' Statistics with R} (1 ed.). Springer Publishing
#' Company, Incorporated.
#'
#'  Percival, D., & Walden, A. (2000).
#'  \emph{Wavelet Methods for Time Series Analysis}
#'  (Cambridge Series in Statistical and Probabilistic
#'   Mathematics). Cambridge: Cambridge University Press.
#'
#' Silva Filho, Tito N. Teixeira da, &
#' Figueiredo, Francisco Marcos R. (2011).
#' Has core inflation been doing a good job in Brazil?.
#' \emph{Revista Brasileira de Economia}, 65(2), 207-233.
"_PACKAGE"
