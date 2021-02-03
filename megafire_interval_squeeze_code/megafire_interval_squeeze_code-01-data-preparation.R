#### Code for paper "Megafire-induced interval squeeze threatens vegetation at landscape scales" ####
#### megafire_interval_squeeze_code-00-fn-gdal_calc_py ####

#### Preparing Raw datasets
## This code specifices the datasets that form the basis of the analysis,
## the main function of this script is to reproject the fireyear raster layers 
## to the same extent, projection and 8 bit data-type as the Vegetation Map.
## It then reclassifies each fireyear as a binary raster with burnt = 0 and 
## unburnt = 1.

## Code developed by Tom Le Breton (t.lebreton@unsw.edu.au)

## Required packages
library(tidyverse)
library(gdalUtils)



#### 1. Reading Veg formation and minimum interval layers ####
## Datasets: 
## - NSW Keith Class distribution raster - Bushfire Hub Cloudstor
## - Fire Interval Recomendations - Kenny et al 2004 - Guidelines for ecologically sustainable fire management
##    - Vegetation formations in this document more or less align exactly with Keith Formations where theyve
##      deviated i've used the closest available match to get a minimum interval. For formations that are
##      recommended to avoid fire i've assigned a minimum interval of 103 years for now
## The vegetation types/classes that make up this layer were reclassified to their minimum fire intervals in ArcGIS 
## and exported as an 8bit raster.

vegClass_8bit <- raster("Data/raw/Veg Formations/vegClass_jan19_clip_rc_8bit.tif")


#### 2. Reprojected Fireyear raster layers ####  
## Produced by Trent Penman for bushfire hub. Using years 1951-2019
## Each layer represents a fireyear i.e. July-June
## with the year date being the start of the fire year so July 2002- June 2003 = 2002
## First step is to compile into a stack and reproject layers to the same projection as
## veg layers.

#first import all files in a single folder as a list 
rastlist <- list.files(path = "Data/raw/fireyeartifs/", pattern='.tif', all.files=TRUE, full.names=FALSE)

## Use Gdalwarp to reproject the fireyear layers to the same extent, 
## resolution and projection as vegclass. And the use gdaltranslate to 
## reclassify the layers as binary where burnt = 0 and unburnt = 1
## takes 5-6 minutes per raster or 5-6 hours for 70 or so. 
## only needs to run once.

## Use gdalwarp as described here https://www.ecologi.st/post/big-spatial-data/

## first set some key values 
src_dir <- "Data/raw/fireyeartifs/"
dest_dir <- "Data/processed/binary_fireyears/"
target_extent <- c(816000,-4220000,2086305,-3170000)  ## xmin,ymin,xmas,ymax extent values from vegclass


## The following for loop first uses gdalwarp to create a reprojected compressed version of the raw 
## fireyears file and stores it in the folder fireyears. THen using gdal_translate reclassifies the 
## fire years layer into a binary version and stores it in binary_fireyears folder

for (i in rastlist){
  gdalwarp(srcfile = paste0("Data/raw/fireyeartifs/", i),
           dstfile = paste0("Data/processed/fireyears/", str_replace(i, pattern = "fireyear", replacement = "rp_fireyear")),
           t_srs = crs(vegClass_8bit), #set crs
           te = c(816000,-4220000,2086305,-3170000), ## xmin,ymin,xmas,ymax extent values from vegclass
           tr = res(vegClass_8bit), # set resolution
           r = "near", # resample method = nearest neighbour
           wt = "Float32", # input datatype
           ot = "Byte", # output datatype == signed 8 bit integer 0-255
           co = c("COMPRESS=LZW","predictor=2"),
           output_Raster=TRUE,
           overwrite=TRUE,verbose=TRUE)
  gdal_translate(
    src_dataset = paste0("Data/processed/fireyears/", str_replace(i, pattern = "fireyear", replacement = "rp_fireyear")),
    dst_dataset = paste0("Data/processed/binary_fireyears/", str_replace(i, pattern = "fireyear", replacement = "binary_")),
    scale = c(0,255,1,0), # reclassifies data such that burnt = 0, unburnt = 1 
    co = c("COMPRESS=LZW","predictor=2","pixeltype=SIGNEDBYTE"), #signedbyte converts the datatype to unsigned 8 bit integer so it can have -ve values too
    output_Raster=TRUE,
    overwrite=TRUE,
    verbose=TRUE)
}