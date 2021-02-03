#### Code for paper "Megafire-induced interval squeeze threatens vegetation at landscape scales" ####
#### megafire_interval_squeeze_code-02-raster_calculations ####

#### Raster calculations for creating time since fire, fire interval and summary raster layers ####
## This script contains code to run raster calculations on the binary fire year layers created in 
## megafire_interval_squeeze_code-01-data_preparation. The resulting layers are the raw data that
## form the results for this project.
## There are five stages in this script which produce unique output layers.
##  1. Time since fire (tsf)
##      A base raster is created with the same dimensions and specifications as the binary fireyear
##      rasters with all pixels = -128. This layer is then multiplied by the first binary 
##      (burnt = 0 unburnt =1) fireyear layer (i.e. fireyear_1950) and that fire year is then added to
##      the multiplied raster. Such that any cells burnt in the year 1950 are equal to 0 indicating the
##      time since fire is 0 years. Unburnt cells are equal to -127 indicating that they are yet to be burnt.
##      This process is then repeated with the 1951 time since fire raster taking the place of the base 
##      layer and being multiplied and added to 1952 any pixels that were burnt in 1950 but not in 1952 will 
##      now have a value of 1, pixels burnt in 1952 will have a value of 0 and never burnt cells a value of -126
##      This process is repeated for each consecutive pair of rasters until time since fire for all 
##      years has been calculated. At each year positive numbers indicate time since fire, negative values
##      indicate pixels that have never been burnt in the dataset and 0 values indicate cells that were burnt
##      in that year.
##  2. Fire Intervals
##      This calculation creates a raster layer in which the time since fire value is only visible for 
##      pixels burnt in that year i.e. the fire interval is the time since fire at the time it was burnt.
##      To do this year n is essentially masked by year n+1 to remove all pixels not burnt during year n+1. 
##      so 1951 would be masked by 1952. The values in each pixel are more accurately the time since fire
##      in burnt pixels for the previous year (i.e. if a pixel was burnt (=0) in 1951 the "fire interval" in 1952 will
##      also be 0 even though a year has passed) this is corrected and accounted for in later steps but i have left the
##      code as i have orginally used it even though it could be corrected in this step.
##  3. Areas have previously been burnt below MI
##      This calculation takes each fire_interval raster and compares it with the minimum intervals for veg types
##      to identify which pixels were burnt below minimum interval in that year. Because the fire intervals are 
##      out by one year (see above) 1 is added to every pixel in the fire interval layers in this step. The formula
##      first masks out any negative values (B*(B>=0)), then multiplies the resulting raster by a raster containing 
##      only values below minimum intervals ((B+1)<A) and divides the product of the two by the original fire 
##      interval raster to return a binary raster with pixels burnt below minimum interval equal to 1.
##  4. Count of times burnt Below MI 
##      Exclude first 10 years of data 1950-1959 to reduce the influence of any artefacts stemming from the 
##      lack of fire history before 1950.
##      This step requires creating the count_below_MIp3 folder before hand and manually adding the prev_
##      file into the folder and renaming it as count_below_MIp3_1960. To form the first "output" file to 
##      be added together with the 1st input file i.e. prev_below_MIp3_1961.
##      Because the gdal_cacl_py function is limited in the number of rasters it can handle at once this step adds
##      each successive year to a raster containing the sum of the last n years. 
##  5. Identify areas burnt below Minimum interval in 2019/20 that had also been burnt below minimum interval previously 
##      This step creates a single raster layer which masks the count_below MIp3 of 2018 by the prevoiously burnt below MIp3 (prev_below_mip3)
##      of 2019 to identify which areas were burnt below minimum interval at the time of the 2019 fires that had also been previously been 
##      burnt below minimum interval at least once.
##
## Code developed by Tom Le Breton (t.lebreton@unsw.edu.au)


## Required packages
library(tidyverse)
library(gdalUtils)


## This code also requires an installation of gdal and python.
## I used OSGeo4W which includes GDAL 3.0.3 and  Python 3.7.0

## load gdal_calc_py function

source("megafire_interval_squeeze_code-00-fn-gdal_calc_py.R")


## 1. Time Since Fire rasters
## Base layer same dims as Fireyears, all cells = -100
## Converts fireyears to 1 for unburnt and 0 for burnt
## Each binary fireyear layer multiplies the baselayer or its most recent equivalent then adds itself to it so 
## that once cells are burnt they become 0 and start accumulating positive TSF values.

## Read in binary fireyears and tsf files
bin_list <- list.files(path = "Data/processed/binary_fireyears/", pattern='.tif', all.files=TRUE, full.names=TRUE)

## Create Baselayer with all cells = -100, This will immediately be multiplied and added
## with the 1951 layer to create the TSF_base which will be updated everytime the for loop runs

gdal_calc_py(input_fileA = "D:/PhD/2_NSW_Fire_Season/Data/processed/binary_fireyears/binary_1951.tif",
              input_fileB = "D:/PhD/2_NSW_Fire_Season/Data/processed/binary_fireyears/binary_1951.tif",
              calculation = "(A-B)-128",
              output_file = "D:/PhD/2_NSW_Fire_Season/Data/processed/time_since_fire/base_TSF.tif")

# This loop runs from 2:69 because the first year 1951 is created prior to its initiation

for (i in 2:69){
  tsf_list <-list.files(path = "Data/processed/time_since_fire/", pattern='.tif', all.files=TRUE, full.names=TRUE)
  gdal_calc_py(input_fileA = paste0("D:/PhD/2_NSW_Fire_Season/", bin_list[[i]]),
                input_fileB = paste0("D:/PhD/2_NSW_Fire_Season/", tsf_list[[i]]),
                calculation = "(B*A)+A",
                output_file = paste0("D:/PhD/2_NSW_Fire_Season/", 
                                    str_replace(bin_list[[i]], pattern = "binary_fireyears/binary", replacement = "time_since_fire/tsf"))
  )
  
}

#### 2. Burn Interval layer ####
## This layer only shows the fire intervals i.e. time between fires in each cell burnt for that year

## Read in the tsf and fireyear files as lists

fireyear_list <- list.files(path = "Data/processed/fireyears/", pattern='.tif', all.files=TRUE, full.names=TRUE)

tsf_list <- list.files(path = "Data/processed/time_since_fire/", pattern='.tif', all.files=TRUE, full.names=TRUE)[-1] # drops the base layer from the list

## Because it gets sent to the windows shell the division sign / gets converted to a \ wont be read properly
## multiplying by 0.004 to get from 250 to 1 instead. 
## The calculation has 3 steps
## 1. converts the fireyear layer into a binary layer where 0 = unburnt and 1 equals burnt
## 2. multiplies the TSF layer so that all values in the unburnt area are converted to 0, at this point 0 represents both unbunt areas AND areas that have just been burnt i.e. TSF <=1
## 3. adds a version of the fireyear layer where -1 = unburnt and 0 = burnt so that the final layer has -1 = unburnt, 0 = just burnt, -60 = never burnt and all positive values = TSF

for (i in 1:69){
  gdal_calc_py(input_fileA = paste0("D:/PhD/2_NSW_Fire_Season/", fireyear_list[[i+1]]),
                input_fileB = paste0("D:/PhD/2_NSW_Fire_Season/", tsf_list[[i]]),
                calculation = "((A*0.004)*B)+((A*0.004)-1)",
                output_file = paste0("D:/PhD/2_NSW_Fire_Season/Data/processed/fire_intervals/", 
                                    str_replace(tsf_list[[i+1]], pattern = "Data/processed/time_since_fire/tsf", replacement = "fire_interval"))
  )
  
}

#### 3. Areas have been burnt below MI
## first create a version of the veg formation interval layer that is compatible with gdal
## this uses the veg class map reclassified to the minimum intervals for each class in ARCGIS
## it has to be deratified through gda_translate to remove the reference table so it is compatible
## with the gdal_calc_py function.
## then calculate whether each pixel in each year was burnt below minimum interval in that year
## The out put files are named prev_below_MIp3 which stands for previously burnt below minimum interval
## and p3 stands for plus 3 which represents the 3 additional years added onto the original minimum intervals
## to account for seed bank accumulation times (see Kenny 2013).

## To then get the areas burnt below minimum interval previously which were also burnt in 2019 
## use the 2019 below minimum interval (prev_below_MI_2019) as a mask over the count_below_MI for 2018
## dont use the count_below_MI for 2019 as it will just be exactly the same essentially masking itself.

gdal_translate(
  src_dataset = paste0("Data/raw/Veg Formations/vegClass_jan19_8bit_MIp3.tif"), 
  dst_dataset = paste0("Data/raw/Veg Formations/veg_form_MIp3_derat.tif"),
  co = c("COMPRESS=LZW","predictor=2"), #signedbyte converts the datatype to unsigned 8 bit integer so it can have -ve values too
  output_Raster=TRUE,
  overwrite=TRUE,
  verbose=TRUE)

for (i in 1:68){
  input_list <-list.files(path = "Data/processed/fire_intervals/", pattern='.tif', all.files=TRUE, full.names=TRUE)
  gdal_calc(input_fileA = paste0("D:/PhD/2_NSW_Fire_Season/Data/raw/Veg Formations/veg_form_MIp3_derat.tif"),
            input_fileB = paste0("D:/PhD/2_NSW_Fire_Season/", input_list[[i]]),
            calculation = "(B*(B>=0))*((B+1)<A)/B", # this removes -ves, then identifies and retains all values for B (fire interval corrected with +1) that are below A (MI)
            output_file = paste0("D:/PhD/2_NSW_Fire_Season/Data/processed/prev_below_MI/prev_below_MIp3/", 
                                 str_replace(input_list[[i]], pattern = "Data/processed/fire_intervals/fire_interval",
                                             replacement = "prev_below_MIp3")))
  
}  

#### 4. Count of times burnt Below MI 
## Exclude first 10 years of data 1950-1959 to reduce the influence of any artefacts stemming from the 
## lack of fire history before 1950.
## Because the gdal_cacl_py function is limited in the number of rasters it can handle at once this step adds
## each successive year to a raster containing the sum of the last n years. 
## This step requires creating the count_below_MIp3 folder before hand and manually adding the prev_
## file into the folder and renaming it as count_below_MIp3_1960. To form the first "output" file to 
## be added together with the 1st input file i.e. prev_below_MIp3_1961. 

for (i in 9:59){
  input_list <- list.files(path = "Data/processed/prev_below_MI/prev_below_MIp3/", pattern='.tif', all.files=TRUE, full.names=TRUE)
  output_list <- list.files(path = "Data/processed/prev_below_MI/count_below_MIp3/", pattern='.tif', all.files=TRUE, full.names=TRUE)
  gdal_calc(input_fileA = paste0("D:/PhD/2_NSW_Fire_Season/", output_list[[i]]),
            input_fileB = paste0("D:/PhD/2_NSW_Fire_Season/", input_list[[i+10]]),
            calculation = "A+B", 
            output_file = paste0("D:/PhD/2_NSW_Fire_Season/Data/processed/prev_below_MI/count_below_MIp3/", 
                                 str_replace(input_list[[i+10]], pattern = "Data/processed/prev_below_MI/prev_below_MIp3/prev_below_MIp3",
                                             replacement = "count_below_MIp3")))
  
} 

#### 5. Identify areas burnt below Minimum interval in 2019/20 that had also been burnt below minimum interval previously 
## This step creates a single raster layer which masks the count_below MIp3 of 2018 by the prevoiously burnt below MIp3 (prev_below_mip3)
## of 2019 to identify which areas were burnt below minimum interval at the time of the 2019 fires that had also been previously been 
## burnt below minimum interval at least once.


gdal_calc(input_fileA = "Data/processed/prev_below_MI/count_below_MIp3/count_below_MIp3_2018.tif",
          input_fileB = "Data/processed/prev_below_MI/prev_below_MIp3/prev_below_MIp3_2019.tif",
          calculation = "A*B", # masks out the areas not burnt below MI in 2019 gives all areas squeezed by 2019 fires
          output_file = "D:/PhD/2_NSW_Fire_Season/Data/processed/prev_below_MI/squeezed_by_2019_MIp3.tif")