bands <- c("LAI", "emiss", "albedo", "qc", "Tmax", "Tmin", "Tavg", "q", "Rln", "Rs", "Pa", "U2", "Prcp", "CO2")
nband <- length(bands)

tif_range <- function(file) {
  info <- rgdal::GDALinfo(file, returnStats = FALSE)[1:7] %>% as.list()
  lon_range <- with(info, c(ll.x, ll.x + columns * res.x))
  lat_range <- with(info, c(ll.y, ll.y + rows * res.y))
  c(lon_range, lat_range)
}

#' tidy_forcing_yearly
#' @param l_static NULL
tidy_forcing_yearly <- function(file, l_static = NULL,
                                write2nc = FALSE, overwrite = FALSE) {
  x <- rgdal::readGDAL(file, silent = TRUE)
  grps <- 1:46 %>% set_names(1:46)

  lst <- lapply(grps, function(i) {
    ind <- seq((i - 1) * nband + 1, i * nband)
    data <- x@data[, ind] %>%
      set_names(bands) %>%
      .[, -c(4, 14)] # rm `qc` and `CO2`
    # replace static bands if provided
    if (!is.null(l_static)) {
      data <- cbind(l_static[[i]][, 1:3], data[, -(1:3)])
    }
    data
  })

  if (write2nc) {
    outfile <- gsub(".tif$", ".nc", file)

    if (!file.exists(outfile) || overwrite) {
      range <- tif_range(file)
      lst <- purrr::transpose(lst) %>% map(~ do.call(cbind, .) %>% spdata_array(range))
      year <- stringr::str_extract(basename(file), "\\d{4}") %>% as.numeric()
      dates <- get_modis_date(year)

      fprintf("writing %s ...\n", basename(outfile))
      ncwrite(lst, outfile, compression = 1, range = range, dates = dates)
    }
  } else {
    lst
  }
}
