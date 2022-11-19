#' PML_yearly_LargeGrid2
#'
#' @export
PML_yearly_LargeGrid2 <- function(data, LC, Year, return.Es = FALSE) {
  ## for all kinds of veg
  d_CO2 = d_CO2[date >= "2000-02-26"]
  CO2s <- d_CO2[year == Year, CO2]
  dates <- d_CO2[year == Year, date]
  
  # rm UNC，是为了跟PML的参数顺序保持一致
  I_del <- 18 # UNC
  IGBPcodes <- IGBPcodes_005[-I_del] + 1 
  areas_ratio <- rowSums(LC[, -I_del])

  res <- rep(list(NULL), length(IGBPcodes)) %>% set_names(names(IGBPcodes))
  for (IGBPcode in IGBPcodes) {
    # runningId(IGBPcode)
    param <- options_param[IGBPcode, ]
    ans <- PML(data, param,
      CO2 = CO2s, ratio = LC[, IGBPcode],
      IGBPcode = IGBPcode, scale = "regional", return.Es = return.Es
    )
    res[[IGBPcode]] <- ans
  }
  l <- purrr::transpose(res)
  res <- purrr::transpose(res) %>% list_sum()
  r <- map(res, divide_by, areas_ratio)
  r
}
