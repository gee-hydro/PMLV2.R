test_that("PML_region works", {
  library(terra)

  i = 1
  indir <- system.file("PMLV2_forcing_NorthChina/MCD12Q1_006_IGBP/", package = "PMLV2")
  fs_lc <- dir(indir, "*.tif", full.names = TRUE)

  indir <- system.file("PMLV2_forcing_NorthChina/mete_GLDASV21/", package = "PMLV2")
  fs_mete <- dir(indir, "*.nc", full.names = TRUE)

  f_lc <- fs_lc[i]
  f_mete <- fs_mete[i]

  fout = "PMLV2_2019.nc"
  PML_region(f_mete, f_lc, fout)

  r <- rast(fout)
  # check variable names
  vars_out = c("GPP", "Ec", "Ei", "Ecr", "Eca", "Ga", "Gc", "Pi", "Es_eq", "Es", "ET")
  expect_equal(varnames(r), vars_out)

  # check data
  dates_orgin = as.Date(time(rast(f_mete)["LAI"]))
  expect_equal(dates_orgin, time(r["ET"]))

  file.remove(fout)
})

