#' Brazilian Inflation and Core Inflation Data
#'
#' A dataset containing the variation of the Brazilian headline Consumer Price Index (CPI)
#' along with core inflation measures.
#'
#' @format An object of class (inherits from tbl, data.frame) with 302 rows and 9 columns.
#' Monthly \% variation from 1994-07-01 to 2019-08-01.
#'
#' \describe{
#' \item{ipca}{\emph{Broad National Consumer Price Index}. \code{ipca}
#' represents the variation in consumer price index. It
#' is the reference for the inflation targeting system
#' implemented in June 1999.}
#'
#' \item{ipcams}{\emph{Broad national consumer price index - Core IPCA trimmed means smoothed}.
#' Excludes items whose monthly change is higher than the 80th
#' percentile or bellow the 20th percentile with smoothing of items
#' with low frequency variations.}
#'
#' \item{ipcama}{\emph{Broad national consumer price index - Core IPCA trimmed means non smoothed}. The same
#' as ipcams but without smoothing.}
#'
#' \item{ipcaex0}{\emph{Broad national consumer price index - Core exclusion - EX0}. Excludes
#' food at home and administered goods from ipca inflation.}
#'
#' \item{ipcaex1}{\emph{Broad national consumer price index - Core exclusion - EX1}. Excludes
#' 10 of the 16 items in the food at home group as well as domestic
#' fuels and vehicle fuels.}
#'
#' \item{ipcadp}{\emph{Broad national consumer price index - Core IPCA - Double Weight}. Adjusts
#' original weigths of each item according to its relative volatility,
#' which reduces the weight of components with more
#' volatile inflation.}
#'
#' \item{ipcaex2}{\emph{Broad national consumer price index - Core exclusion - EX2}. Considers
#' only the underlying items of the services, industrial goods, and
#' food away from home groups.}
#'
#' \item{ipcaex3}{\emph{Broad national consumer price index - Core exclusion - EX3}. Considers
#' only the underlying items of the services and industrial goods groups.}
#' }
#' @source Series can be obtained from function \code{\link{getsgs}} or by codes: 433, 11427, 16121,  27838,  27839,  11426,  4466, 16122
#' in the \cr
#' \href{https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries}{Time Series Management System}
#'
#' @references Central Bank of Brazil (2018). New core inflation measures.
#' Inflation Report, june 2018.  \cr
#' \href{https://www.bcb.gov.br/content/publications/inflationreportboxes/RI201806B5I-ri201806b5i.pdf}{Inflation report boxes: New core inflation measures / 2018-06}
#'
#' Tito N. T da Silva Filho and Francisco M. R. Figueiredo (2014). Revisiting
#' the Core Inflation Measures of the Banco Central do Brasil.
#' Working Papers Series 356, Central Bank of Brazil, Research Department. \cr
#' \url{https://www.bcb.gov.br/pec/wps/port/TD356.pdf}
#'
"coreinf_br"

