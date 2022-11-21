
# Penman-Monteith-Leuning Evapotranspiration V2 in R

> Dongdong Kong, CUG

<!-- badges: start -->
[![R-CMD-check](https://github.com/gee-hydro/pml_california/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gee-hydro/pml_california/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/gee-hydro/pml_california/branch/master/graph/badge.svg)](https://app.codecov.io/gh/gee-hydro/pml_california)
<!-- [![CRAN](http://www.r-pkg.org/badges/version/PMLV2)](https://cran.r-project.org/package=PMLV2) -->
<!-- [![total](http://cranlogs.r-pkg.org/badges/grand-total/PMLV2)](https://www.rpackages.io/package/PMLV2) -->
<!-- [![monthly](http://cranlogs.r-pkg.org/badges/PMLV2)](https://www.rpackages.io/package/PMLV2) -->
<!-- badges: end -->


## TODO

- [ ] 以NorthChina为输入，测试PMLV2的核心代码；

- [ ] 截取通量站驱动数据，重新率定模型参数（Options: ERA5L, GLDASv2.1, GLDASv2.2, CFSV2, MERRA2）；
  
- [ ] 等待GEE跑完Albedo，然后进行插值处理；

- [ ] 从GEE截取驱动数据，tiff转nc；


## Installation

You can install the development version of `PMLV2` like so:

``` r
remotes::install_github("gee-hydro/PMLV2")
```

## Example

``` r
library(PMLV2)
## basic example code
```

## References

1. Zhang, Y., Kong, D., Gan, R., Chiew, F. H. S., McVicar, T. R., Zhang, Q., & Yang, Y. (2019). Coupled estimation of 500 m and 8-day resolution global evapotranspiration and gross primary production in 2002–2017. Remote Sensing of Environment, 222, 165–182. <https://doi.org/10.1016/j.rse.2018.12.031>

2. Kong, D., Zhang, Y., Gu, X., & Wang, D. (2019). A robust method for reconstructing global MODIS EVI time series on the Google Earth Engine. ISPRS Journal of Photogrammetry and Remote Sensing, 155, 13–24. <https://doi.org/10.1016/j.isprsjprs.2019.06.014>
