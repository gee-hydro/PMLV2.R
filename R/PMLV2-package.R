#' @import magrittr
#' @import rtrend
#' @import nctools
#' @importFrom data.table fread fwrite as.data.table data.table
#' @importFrom hydroTools cal_es cal_lambda cal_slope cal_U2 W2mm
#' @importFrom dplyr as_tibble mutate across
#' @importFrom stats setNames
#' @importFrom utils str
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {

  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      c(
        ".", ".SD", ".N", "..vars",
        "R2", "NSE", "KGE", "R", "RMSE", "Bias_perc", "n_sim", "site", 
        "GPPobs", "ETobs", "GPP", "ET", "GPP_DT", "GPP_NT",
        "site", "IGBP", "yobs", "ysim", 
        "where"
      )
    )
  }
}
