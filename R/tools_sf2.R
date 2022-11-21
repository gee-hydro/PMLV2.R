rast_array <- function(r) {
  if (is.character(r)) {
    r %<>% check_rast()
  }
  aperm(as.array(r), c(2, 1, 3)) %>% flipud()
}

flipud <- function(x, ...) {
  I <- ncol(x):1
  ndim <- length(dim(x))
  if (ndim == 2) {
    x[, I]
  } else if (ndim == 3) {
    x[, I, ]
  }
}

check_rast <- function(r, type = "raster") {
  fun <- switch(type,
    raster = raster::raster,
    terra = terra::rast
  )
  if (is.character(r) || (type == "raster" && !raster::is.raster(r)) ||
    (type == "terra" && !terra::is.terra(r))) {
    r %<>% fun()
  }
  r
}
