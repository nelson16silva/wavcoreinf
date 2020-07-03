#' Map RMSE and MAE by Combining Two Data Frames
#'
#' \code{rmse_mae} is a functional to compute root mean square error and mean absolute
#' error for all variables in a data frame with respect to all variables in
#' another data frame. The number of rows of both data
#' frames must be equal.
#'
#'
#' @param dfa A data frame of numeric vectors.
#' @param dfb A data frame of numeric vectors. The number
#' of rows of \code{dfb} must be equal to \code{dfa}.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' # Measuring how far wavelet core inflation measures are
#' # from moving averages of headline inflation.
#'
#' inf_head <- coreinf_br[["ipca"]]
#'
#' mavc_order <- c(13, 25, 37)
#' names(mavc_order) <- paste0("mavc", mavc_order)
#' df_mavc <- purrr::map_df(mavc_order, ~mavc(x = inf_head, k = .))
#'
#' corewav1 <- wmtsa::wavShrink(inf_head)
#' corewav2 <- wmtsa::wavShrink(inf_head, wavelet = "haar")
#' df_core <- tibble::tibble(corewav1, corewav2)
#'
#' rmse_mae(df_core, df_mavc)
rmse_mae <- function(dfa, dfb) {
  if(is.data.frame(dfa) == FALSE) {
    dfa <- as.data.frame(dfa)}
  if(is.data.frame(dfb) == FALSE) {
    dfb <- as.data.frame(dfb)}
  n_row <- nrow(dfa)
  n_col_dfa <- length(dfa)
  names_dfa <- names(dfa)
  names_dfb <- names(dfb)
  names <- paste0(names_dfa, "_", rep(names_dfb, each = n_col_dfa))
  m_rmse <- matrix(nrow = n_row, ncol = length(names))
  m_mae <- matrix(nrow = n_row, ncol = length(names))
  colnames(m_rmse) <- paste0(names, "_rmse")
  colnames(m_mae) <- paste0(names, "_mae")
  for (j in seq_along(dfb)) {
    for (i in seq_along(dfa)) {
      mindex <- n_col_dfa * (j - 1) + i
      m_rmse[, mindex] <- (dfa[[i]] - dfb[[j]]) ^ 2
      m_mae[, mindex] <- abs(dfa[[i]] - dfb[[j]])
    }
  }
  rmse_aux <- tibble::as.tibble(m_rmse)
  mae_aux <- tibble::as.tibble(m_mae)
  rmse <- purrr::map(rmse_aux, ~sqrt(mean(.x , na.rm = TRUE)))
  mae <- purrr::map(mae_aux, mean, na.rm = TRUE)
  rmse_mae <- data.frame(cbind(rmse, mae))
  return(rmse_mae)
}
