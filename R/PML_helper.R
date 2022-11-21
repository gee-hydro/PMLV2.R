# 需要每个站点单独计算
add_fvalSoil <- function(prcp, Es_eq, frame = 3, sites = NULL) {
  # sites = NULL
  if (!is.null(sites)) {
    # site should be in the order
    d <- data.table(site = sites, prcp, Es_eq)
    fval_soil <- d[, movmean2(prcp, frame, 0) / movmean2(Es_eq, frame, 0), .(site)]$V1
  } else {
    if (is.matrix(prcp)) {
      # Ei只占不到10%，因此前期土壤含水量可以用Prcp/Es_eq代替Pi/Es_eq
      # 每年的前frame-1个时间段中，frame变小；8-day尺度 应该影响不大
      fval_soil <- movmean_2d(prcp, frame, 0) / movmean_2d(Es_eq, frame, 0)
    } else {
      fval_soil <- movmean2(prcp, frame, 0) / movmean2(Es_eq, frame, 0)
    }
  }
  # fval_soil = movmean(prcp, [frame, 0], 'omitNA')  / movmean(Es_eq, [frame, 0], 'omitNA');
  clamp(fval_soil, lims = c(0, 1))
}

add_Es <- function(l, frame = 3) {
  fval_soil <- add_fvalSoil(l$Pi, l$Es_eq, frame)
  l$Es <- fval_soil * l$Es_eq
  l
}

#' @export
add_ETsum <- function(l) {
  l$Ec <- l$Ecr + l$Eca
  l$ET <- l$Ec + l$Ei + l$Es
  l
}

nc_add_Es <- function(file, frame = 3, grid_type = "mat") {
  # ! bug might exist here
  l <- ncread(file, c("Pi", "Es_eq")) # , ntime = -1, grid_type = grid_type

  fval_soil <- add_fvalSoil(l$data$Pi, l$data$Es_eq, frame)
  Es <- fval_soil * l$data$Es_eq
  if (!is.null(grid_type)) {
    range <- nc_range(file)$range
    Es <- spdata_array(Es, range, flip = FALSE)
  }
  ncwrite(list(Es = Es), file,
    # dates = dates,
    verbose = TRUE, overwrite = FALSE
  )
}
