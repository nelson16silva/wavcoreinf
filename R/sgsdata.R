#' Extract Data from SGS Database
#'
#' Extract data from Central Bank of Brazil statistical database \href{https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries}{Time Series Management System (SGS)}.
#'
#' @param namex String vector with the series names.
#' @param numberx Integer vector representing the code of the series in SGS database.
#'
#' @return Tibble
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' # 433  ipca (cpi)
#' # 11427 ex0 (core inflation)
#' n <- c(433, 11427)
#' x <- c("ipca", "ex0")
#' # Sys.setenv(no_proxy = "bcb.gov.br") # configuration for internal bcb connection
#' inf <- getsgs(x, n)
#' }
getsgs <- function(namex, numberx) {
  http1 <- "http://api.bcb.gov.br/dados/serie/bcdata.sgs."
  http2 <- "/dados?formato=json&dataFinal=1"
  inf.list <- purrr::map(numberx, ~ jsonlite::fromJSON(paste(http1, .x, http2, sep = "")))
  inf.data.frame <- purrr::reduce(inf.list, dplyr::full_join, by = "data")
  inf <- inf.data.frame[-1] %>%
    tibble::as.tibble() %>%
    dplyr::mutate_all(as.numeric) %>%
    tibble::add_column(data = inf.data.frame[, 1], .before = 1)
  names(inf) <- c("data", namex)
  inf
}
