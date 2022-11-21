#' PML_region
#' 
#' Running one year each time.
#' 
#' @importFrom terra time rast
#' @export
PML_region <- function(f_mete, f_lc, outfile = NULL, poly = NULL, ..., 
  overwrite = TRUE, verbose = FALSE) 
{
  ## 01: load data
  s = rast(f_mete)
  dim = dim(s)[2:1]
  dates = s["LAI"] %>% time()
  range = as.vector(ext(s))
  year = year(dates[1]) # one year each time

  if (!is.null(poly)) {
    r_temp = s["Pa"][[1]] %>% mask(poly)
    I_grid = r_temp %>% rast_array() %>% c() %>% which.notna()
  }

  l_forcing = map(varnames(s) %>% set_names(., .), ~ s[.x] %>%
    rast_array() %>%
    array_3dTo2d(I_grid))

  r_lc = rast(f_lc)  
  mat_LC = rast_array(r_lc) %>% array_3dTo2d(I_grid) %>% set_colnames(names(r_lc))

  ## 02: run model
  res <- .PML_region(l_forcing, LC, year, return.Es = FALSE, , verbose = verbose)
  res %<>% add_Es(frame = 3) %>% add_ETsum()
  data = res %>% map(~ array_2dTo3d(.x, I_grid, dim))

  if (!is.null(outfile)) {
    ncwrite(data, outfile, range = range, dates = dates, overwrite = TRUE)
  } else data
}

# ' @param 
#' @importFrom data.table year
.PML_region <- function(l_mete, mat_LC, Year, ..., verbose = TRUE, return.Es = FALSE) {
  ## for all kinds of veg
  d_CO2 <- d_CO2[date >= "2000-02-26"]

  CO2s <- d_CO2[year == Year, CO2]
  dates <- d_CO2[year == Year, date]

  # rm UNC，是为了跟PML的参数顺序保持一致
  igbpnames = IGBPnames_006[-1] # rm UNC
  inds_lc = match(igbpnames, colnames(mat_LC))
  areas_ratio <- rowSums(mat_LC[, inds_lc])   # ! caution here

  res <- rep(list(NULL), length(igbpnames)) %>% set_names(igbpnames)

  for (i in seq_along(igbpnames)) {
    igbp = igbpnames[i]
    if (verbose) runningId(i)

    par <- options_param[IGBPname == igbp, ]
    res[[i]] <- PML(l_mete, par, CO2 = CO2s, ratio = mat_LC[, igbp], ...,
      IGBPname = igbp, scale = "regional", return.Es = return.Es
    )
  }
  # weighted average among different IGBPs
  res <- purrr::transpose(res) %>% list_sum()
  r <- map(res, divide_by, areas_ratio)
  r
}
