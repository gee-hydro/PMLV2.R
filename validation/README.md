## Validation of remotely sensed ET products using a mass balance approach

Here, the GAGES-II dataset is leveraged to solve for long-term mean annual ET as:

ET = P - Q

P is mean annual precipitation from PRISM, and Q is mean annual streamflow from USGS gages (both calculated in GAGES-II characteristics). 
This ET from mass balance is then compared to ET as modeled from remotely sensed data. Files in the directory include:

* Get_basins_for_PML_verification.ipynb: a Jupyter notebook that imports GAGES-II data 
(not included in this repository at the moment because it's really big; Falcone, 2011) and identifies minimally disturbed basins in the study area
* PML_validation_sitelist.zip: A shapefile of basin outlines including basin area and long-term mean annual precipitation, 
runoff, and ET estimated from mass balance.
* PML_validation_runoff.ipynb: a Colab notebook that compares long-term mean ET from PML-V2, MODIS ET, and ET as estimated from mass balance
* query_runoff.ipynb: a Colab notebook that gives an example using hydrofunctions to import USGS streamflow data for one site

References

Falcone, James A. GAGES-II: Geospatial attributes of gages for evaluating streamflow. US Geological Survey, 2011. https://pubs.er.usgs.gov/publication/70046617

USGS streamflow data: https://waterdata.usgs.gov/nwis/rt
