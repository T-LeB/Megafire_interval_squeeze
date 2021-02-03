# Megafire_interval_squeeze

Repository for code for the manuscript "Megafire-induced interval squeeze threatens vegetation at landscape scales"

Software used during development:

All scripts were constructed in R 4.0.3.
The raster calculations require a local installation of GDAL and Python. I used which OSGeo4W which includes GDAL 3.0.3 and Python 3.7.0.
Packages used in this project include the following and their dependencies.
•	tmap 3.2
•	maptools 1.0-2
•	tmaptools 3.1
•	gdalUtils 2.0.3.2
•	rgdal 1.5-18
•	raster 3.3-13
•	rasterVis 0.48
•	tidyverse 1.3.0
•	sf 0.9-6
•	sp 1.4-4

Resources required: 
The majority of these scripts can be run locally in R on a laptop. However, gdal calculations need to be run through a shell via gdalUtils functions. The customised gdal_calc_py function (megafire_interval_squeeze_code-0a-fn-gdal_calc_py.R) will run for 2-5 minutes per calculation, and when looped over ~60 or so rasters can take up to 8 hours to complete. The cross-tabulation of fire history rasters and vegetation types in megafire_interval_squeeze_code-03-results_cross_tabulation cannot be run locally except possibly on high end machines. To run this I had to use the UNSW Katana shared computational cluster and parrallelise the task across 16 CPU's adn 64Gb of memory this generally ran quite quickly i.e. minutes rather than hours.
