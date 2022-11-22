#' @importFrom terra as.array
rast_array <- function(r) {
  if (is.character(r)) {
    r %<>% rast()
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
