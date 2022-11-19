#' Transform parameters to GEE
#'
#' @description
#' parameter transfer scheme:
#'
#' - DNF(4) = EBF(2)
#' - CSH(7) = OSH(8)
#' - CNV(15) = CRO(13)
#' - `14` (Urban and Built-Up)           = 6  (mixed forest)
#' - `17` (Barren or Sparsely Vegetated) = 11 (grassland)
#'
#' @param file The file path of parameter, in the csv format
#'
#' @export
#' @examples
#' \dontrun{
#' parameters_2GEE("data-raw/PMLV2_parameters_20181202_110828.csv")
#' }
parameters_2GEE <- function(file) {
  x <- parameters_toR(file)[, -1]
  str <- character()
  for (i in seq_along(x)) {
    var <- names(x)[i]
    fmt <- paste(rep("%.3f", 6), collapse = ", ") %>%
      rep(3) %>%
      paste(collapse = ",\n\t\t")
    fmti <- sprintf("var %s_raw = ee.List([%s]);", var, fmt)
    str[i] <- do.call(sprintf, c(fmti, as.list(x[[i]])))
  }
  paste(str, collapse = "\n") %T>% cat() %>% writeLines("clipboard")
}

#' @rdname parameters_2GEE
#' @export
parameters_2R <- function(file) {
  x <- fread(file)
  mat <- x[, 2:(ncol(x) - 1)] %>%
    as.matrix() %>%
    set_rownames(x$IGBPs)

  # for leuning D0 = 0.7
  missing_D0 <- 0.7 # considering the average value
  mat[c(4, 7, 15, 14, 17), ] <- mat[c(2, 8, 13, 6, 11), ]
  mat[c(1, 16, 18), ] <- matrix(c(0, 0, 0, 0, missing_D0, 0.6, 0.7, 0, 0, 1, 4),
    nrow = 3, ncol = ncol(mat), byrow = T
  )

  d <- as.data.table(mat) %>%
    set_colnames(c("Alpha", "Thelta", "m", "Am_25", "D0", "kQ", "kA", "S_sls", "fER0", "VPDmin", "VPDmax"))
  cbind(IGBPname = IGBPnames_005, d, gsx = gsx_raw, hc = hc_raw)
}
