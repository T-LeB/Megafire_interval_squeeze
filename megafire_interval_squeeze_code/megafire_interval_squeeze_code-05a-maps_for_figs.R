#### Code for paper "Megafire-induced interval squeeze threatens vegetation at landscape scales" ####
#### megafire_interval_squeeze_code-05b-plots_for_figs ####
##
## This script takes the raster layers produced in "megafire_interval_squeeze_code-02-raster_calculations"
## and creates the final maps used for the manuscript.
## 
## The script outputs two maps, 
##  1. Fig. 1a. Exent of 2019-2020 fires across eastern New South Wales including
##            areas burnt above MI, below MI, below MI at least once and in 2019-2020
##      
##  2. Fig. 2a. Mapped frequency of fires below minimum interval from 1960-2020
##
##  Note: Throughout this code files have a suffix MIp3 referring to "minimum interval plus three"
##        which is the final minimum interval used for this project based on Kenny et al 2004's fire
##        intervals plus 3 years to account for seed bank accumulation as per Kenny 2013.
##
## Code developed by Tom Le Breton (t.lebreton@unsw.edu.au)

## Required Packages
library(tidyverse)
library(gdalUtils)
library(sp)
library(sf)
library(rgdal)
library(maptools)
library(tmap)
library(tmaptools)
library(rasterVis)

## This code also requires an installation of gdal and python.
## I used OSGeo4W which includes GDAL 3.0.3 and  Python 3.7.0

####################################
#### Creating mappable rasters ####
###################################

## First need to create desired mapping rasters and assign no data values to rasters we want to map  
## so that we can stack multiple layers.

## load gdal_calc_py function - This needs to be a modified version which includes
## the NoDataValue variable - see gdal_calc_py script for details.

source("megafire_interval_squeeze_code-00-fn-gdal_calc_py.R")

## First create a deratified version of the vegetation type map to use to remove NA and non-Native areas from other rasters for mapping
gdal_translate(
  src_dataset = paste0("Data/raw/Veg Formations/vegClass_jan19_clip_rc_8bit.tif"),
  dst_dataset = paste0("Data/raw/Veg Formations/veg_class_derat.tif"),
  co = c("COMPRESS=LZW","predictor=2"), #signedbyte converts the datatype to unsigned 8 bit integer so it can have -ve values too
  output_Raster=TRUE,
  overwrite=TRUE,
  verbose=TRUE)

## Fig 1. Layer 1. Footprint of 2019/20 fires - masks out the areas not burnt burnt by 2019-2020 fires
## input_fileA is not used in the calculation at all, gdal_calc_py just requires two inputs to run
gdal_calc_py(input_fileA = "Data/raw/Veg Formations/veg_class_derat.tif",
            input_fileB = "D:/PhD/2_NSW_Fire_Season/Data/processed/fireyears/rp_fireyear2019.tif",
            calculation = "B", 
            nodatavalue = 0,
            output_file = "D:/PhD/2_NSW_Fire_Season/Data/processed/fireyears/mapping_rp_fireyear2019.tif")

## Fig 1. Layer 2. Native areas "squeezed" by 2019-2020 fires
## Identify areas of native vegetation burnt below Minimum interval in 2019/20 that had also been burnt below minimum  
## interval previously and masks out 0's
## 2 steps

  ## Step 1. Identify areas burnt below minimum interval in 2019-2020 and also previously
  gdal_calc(input_fileA = "Data/processed/prev_below_MI/count_below_MIp3/count_below_MIp3_2018.tif",
            input_fileB = "Data/processed/prev_below_MI/prev_below_MIp3/prev_below_MIp3_2019.tif",
            calculation = "A*B",
            nodatavalue = 0,
            output_file = "D:/PhD/2_NSW_Fire_Season/Data/processed/prev_below_MI/squeezed_by_2019_MIp3.tif")

  ## Step 2. Remove non-native and NA veg types.
  gdal_calc(input_fileA = "Data/raw/Veg Formations/veg_class_derat.tif",
            input_fileB = "D:/PhD/2_NSW_Fire_Season/Data/processed/prev_below_MI/squeezed_by_2019_MIp3.tif",
            calculation = "(B*(A!=1))*(A!=82)",
            nodatavalue = 0,
            output_file = "D:/PhD/2_NSW_Fire_Season/Data/processed/prev_below_MI/squeezed_by_2019_MIp3_native.tif")

## Fig 1. Layer 3. Burnt below Minimum interval 2019-2020  
## Takes the prev_below layer for 2019 which shows the areas burnt below minimum interval in 2019-2020
## and removes non-native vegetation types and masks out 0's

gdal_calc(input_fileA = "Data/raw/Veg Formations/veg_class_derat.tif",
          input_fileB = "Data/processed/prev_below_MI/prev_below_MIp3/prev_below_MIp3_2019.tif",
          calculation = "(B*(A!=1))*(A!=82)", # masks out the non-native and NA veg types
          nodatavalue = 0,
          output_file = "D:/PhD/2_NSW_Fire_Season/Data/processed/prev_below_MI/masked_bMIp3_2019.tif")

## Fig 2. Layer 1. 
## Times native vegetation burnt below minimum interval by 2019/20 with 0's masked out
gdal_calc(input_fileA = "Data/raw/Veg Formations/veg_class_derat.tif",
          input_fileB = "Data/processed/prev_below_MI/count_below_MIp3/count_below_MIp3_2019.tif",
          calculation = "(B*(A!=1))*(A!=82)", # masks out the non-native and NA veg types
          nodatavalue = 0,
          output_file = "D:/PhD/2_NSW_Fire_Season/Data/processed/prev_below_MI/repeat_bMIp3_by_2019_native.tif")


#########################################################
## Shared Assets - Australian borders and bounding boxes.
#########################################################

## Pull Australian and NSW borders from 'raster' package
Australia <- getData("GADM",country="AUS",level=0) 

NSW <- getData("GADM",country="AUS",level=1)
NSW <- Aus[Aus$NAME_1 == "New South Wales",]

## Manually set bounding box values encompasses the majority of the fire affected portion of the state
border_bbox <- st_bbox(NSW)
border_bbox[1] <- 147.500
border_bbox[2] <- -37.4
border_bbox[3] <- 153.7
border_bbox[4] <- -28.3

## Base layer map of Australia cropped to the bounding box
aus <- tm_shape(Australia,bbox = border_bbox) +
  tm_fill(palette = "#e0e0e0") +
  tm_borders()#+
tm_grid(lines = FALSE,
        labels.inside.frame = FALSE,
        labels.show = TRUE,
        labels.col = "white",
        labels.format = list(fun = function(x){paste0(x/x)}),
        ticks = TRUE)

## Inset map
nsw_region = st_bbox(border_bbox,
                     crs = st_crs(Australia)) %>% 
  st_as_sfc()

inset_map = tm_shape(Australia, bbox = c(xmin = 112.1 , ymin = -43.9 , xmax = 154, ymax = -10.45)) + 
  tm_polygons(palette = "#e0e0e0") +
  tm_shape(nsw_region) + 
  tm_borders(lwd = 3) +
  tm_layout(bg.color = "transparent",
            frame = FALSE)

## Export the inset map seperately to be put into the main figure in Adobe Illustrator or some other program
tmap_save(tm = inset_map, filename = "Data/results/summaries/Figures/fig2_inset.svg", dpi = 72)

#################################################################################################################
##  1. Fig. 1a. Exent of 2019-2020 fires across eastern New South Wales including
##            areas burnt above MI, below MI, below MI at least once and in 2019-2020

bmi_2019 <- raster("D:/PhD/2_NSW_Fire_Season/Data/processed/prev_below_MI/masked_bMIp3_2019.tif")
squeezed <- raster("D:/PhD/2_NSW_Fire_Season/Data/processed/prev_below_MI/squeezed_by_2019_MIp3_native.tif")
footprint <- raster("Data/processed/fireyears/mapping_rp_fireyear2019.tif")

## Bounding box specific to the raster layers for this map.
raster_bbox <-st_bbox(squeezed) 
raster_bbox[1] <- 1380000
raster_bbox[4] <- -3215000

## Layer 1. 2019-2020 Fire extent/footprint
fp <- tm_shape(footprint, bbox = raster_bbox) + 
  tm_raster(breaks = c(0,245,255),
            palette = c("white","#969696"),
            labels = c("", "2019/20 burnt area"),
            title = "",
            legend.show = FALSE) +
  tm_layout(NA) 

## Layer 2. Area burnt below minimum interval during the 2019-2020 fires
bmi <- tm_shape(bmi_2019, bbox = raster_bbox) +
  tm_raster(palette = c('#fed976'),
            title = "",
            labels = c("Burnt below MI 2019/20"),
            style = "cont") +
  tm_layout(legend.show = FALSE)

## Layer 3. Area burnt below minimum interval durting the 2019-2020 fires
##  and at least once previously.
sq <- tm_shape(squeezed,bbox = raster_bbox) + 
  tm_raster(palette = c('#fd8d3c'),
            title = "",
            labels = c("Consecutive fires below MI"),
            style = "cont") +
  tm_layout(legend.show = FALSE)

## Layer 1 legend
fp_legend <- tm_shape(footprint, bbox = raster_bbox) + 
  tm_raster(breaks = c(0,245,255),
            palette = c("white","#969696"),
            labels = c("", "2019/20 burnt area"),
            title = "",
            legend.show = TRUE) +
  tm_layout(NA,
            legend.title.size = 2,
            legend.text.size = 2,
            legend.bg.color = "white",
            legend.bg.alpha = 0,
            #legend.position = c("RIGHT", "BOTTOM"),
            legend.only = TRUE) 

## Layer 2 legend
bmi_legend <- tm_shape(bmi_2019, bbox = raster_bbox) +
  tm_raster(palette = c('#fed976'),
            #palette = "plasma",
            title = "",
            labels = c("Below MI"),
            style = "cont",
            legend.is.portrait = TRUE) +
  tm_layout(NA,
            legend.title.size = 2,
            legend.text.size = 2,
            legend.bg.color = "white",
            legend.bg.alpha = 0,
            #legend.position = c("RIGHT", "BOTTOM"),
            legend.only = TRUE)

## Layer 3 legend
sq_legend <- sq <- tm_shape(squeezed,bbox = raster_bbox) + 
  tm_raster(palette = c('#fd8d3c'),
            #palette = "plasma",
            title = "",
            labels = c("Ongoing FIS"),
            style = "cont",
            legend.is.portrait = TRUE) +
  tm_layout(NA,
            legend.title.size = 2,
            legend.text.size = 2,
            legend.bg.color = "white",
            legend.bg.alpha = 0,
            #legend.position = c("RIGHT", "BOTTOM"),
            legend.only = TRUE) 

## Main Map
## Putting all the layers together and exporting the map.
fig_1 <- aus+fp+bmi+sq + tm_scale_bar(breaks = c(0, 100), text.size = 1, color.light = "black",  position = c("LEFT","BOTTOM"))

tmap_save(tm = fig_1, filename = "Data/results/summaries/Figures/fig1_inset_mip3.png", 
          insets_tm = inset_map,insets_vp = viewport(x = 0, y = 1.12, width = 0.35, height = 0.45, just=c("left", "top")), 
          height = 7.5, units = "in", dpi = 300, outer.margins = c(0,0,0,0))

## Exporting the legends seperately to be put into the main figure in Adobe Illustrator or some other program
tmap_save(tm = fp_legend, filename = "Data/results/summaries/Figures/fig1_legend_fp.png", dpi = 300)

tmap_save(tm = sq_legend, filename = "Data/results/summaries/Figures/fig1_legend_sq.png", dpi = 300)

tmap_save(tm = bmi_legend, filename = "Data/results/summaries/Figures/fig1_legend_bmi.png", dpi = 300)

#################################################################################################################################

### 2.  Fig. 2a. Mapped frequency of fires below minimum interval from 1960-2020

count <- raster("D:/PhD/2_NSW_Fire_Season/Data/processed/prev_below_MI/repeat_bMIp3_by_2019_native.tif")

## Bounding box specific to the raster layers for this map.
raster_bbox <-st_bbox(count) 
raster_bbox[1] <- 1380000
raster_bbox[4] <- -3215000

## Layer 1. Frequency of fires below minimum interval from 1960-2020
ct <- tm_shape(count,bbox = raster_bbox) + 
  tm_raster(breaks = c(1,2,3,4,5,6,7,8,9),
            palette = c('#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026'),
            #palette = "plasma",
            title = "",
            labels = c("1"," "," "," ","5"," "," "," ","9"),
            style = "cont",
            legend.is.portrait = TRUE) +
  tm_layout(NA,
            legend.title.size = 1,
            legend.text.size = 1,
            legend.bg.color = "white",
            legend.bg.alpha = 0,
            legend.position = c("RIGHT", "BOTTOM"),
            legend.show = FALSE)

## Layer 1. legend
legend <- tm_shape(count) + 
  tm_raster(breaks = c(1,2,3,4,5,6,7,8,9),
            palette = c('#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026'),
            #palette = "plasma",
            title = "",
            labels = c("1"," "," "," ","5"," "," "," ","9"),
            style = "cont",
            legend.is.portrait = TRUE) +
  tm_layout(NA,
            legend.title.size = 2,
            legend.text.size = 2,
            legend.bg.color = "white",
            legend.bg.alpha = 0,
            frame = FALSE,
            legend.only = TRUE) 

## Main Map
## Putting all the layers together and exporting the map.
fig_2 <- aus+ct + tm_scale_bar(breaks = c(0, 100), text.size = 1, color.light = "black",  position = c("RIGHT","BOTTOM"))

tmap_save(tm = fig_2, filename = "Data/results/summaries/Figures/fig2_inset_mip3.png", 
          insets_tm = inset_map,insets_vp = viewport(x = 0, y = 1.12, width = 0.35, height = 0.45, just=c("left", "top")), 
          height = 7.5, units = "in", dpi = 300, outer.margins = c(0,0,0,0))

## Exporting the legends seperately to be put into the main figure in Adobe Illustrator or some other program
tmap_save(tm = legend, filename = "Data/results/summaries/Figures/fig2_legend.svg", dpi = 300)

