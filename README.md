
# Penman-Monteith-Leuning Evapotranspiration (ET) Version2 in R

> Dongdong Kong, CUG

<!-- badges: start -->
[![R-CMD-check](https://github.com/gee-hydro/PML.R/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gee-hydro/PML.R/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/gee-hydro/PML.R/branch/master/graph/badge.svg)](https://app.codecov.io/gh/gee-hydro/PML.R)
<!-- [![CRAN](http://www.r-pkg.org/badges/version/PMLV2)](https://cran.r-project.org/package=PMLV2) -->
<!-- [![total](http://cranlogs.r-pkg.org/badges/grand-total/PMLV2)](https://www.rpackages.io/package/PMLV2) -->
<!-- [![monthly](http://cranlogs.r-pkg.org/badges/PMLV2)](https://www.rpackages.io/package/PMLV2) -->
<!-- badges: end -->


## TODO

- [x] 以NorthChina为输入，测试PMLV2的核心代码；
  
- [ ] 植被驱动数据，等待GEE跑完Albedo，然后进行线性插值、历史平均插值处理；

- [ ] 气象驱动数据，从GEE截取，tiff转nc；

- [ ] 模型参数率定，截取通量站驱动数据，重新率定模型参数（ERA5L, GLDASv2.1, GLDASv2.2, CFSV2, MERRA2）；

## Installation

You can install the development version of `PMLV2` like so:

``` r
remotes::install_github("gee-hydro/PMLV2")

pak::pkg_install(c("phenofit"))
pak::pkg_install(c("rpkgs/rfluxnet", "rpkgs/nctools"))
```

## Example

See the following instruction:

<https://gee-hydro.github.io/PML.R/articles/model_forcing.html>

<https://gee-hydro.github.io/PML.R/articles/run_model.html>


## Validation ET

- Eddy covariance flux

   <https://fluxnet.org/data/fluxnet2015-dataset/fullset-data-product>

- Basin-scale water balance
  
  $$E_{wb} = P - R - \Delta S + RES_s$$

  $$E_{wb} ≈ P - R - \Delta S$$

  1. GRDC streamflow, <https://portal.grdc.bafg.de/applications/public.html?publicuser=PublicUser#dataDownload/Stations>
  2. USGS streamflow, <https://github.com/DOI-USGS/dataRetrieval/>

- Atmosphere moisture balance

  $$E_{atm} = P + Div + \Delta W - RES_w$$

  $$E_{atm} ≈ P + Div$$
  
  1. ERA5 monthly, <https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=form>


## References

1. Zhang, Y., Kong, D., Gan, R., Chiew, F. H. S., McVicar, T. R., Zhang, Q., & Yang, Y. (2019). Coupled estimation of 500 m and 8-day resolution global evapotranspiration and gross primary production in 2002–2017. Remote Sensing of Environment, 222, 165–182. <https://doi.org/10.1016/j.rse.2018.12.031>

2. Kong, D., Zhang, Y., Gu, X., & Wang, D. (2019). A robust method for reconstructing global MODIS EVI time series on the Google Earth Engine. ISPRS Journal of Photogrammetry and Remote Sensing, 155, 13–24. <https://doi.org/10.1016/j.isprsjprs.2019.06.014>
