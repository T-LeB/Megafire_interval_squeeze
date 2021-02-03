#### Code for paper "Megafire-induced interval squeeze threatens vegetation at landscape scales" ####
#### megafire_interval_squeeze_code-03-results_cross_tabulation ####
##
## This script takes dfferent products of the past script "megafire_interval_squeeze_code-02-raster_calculations"
## and cross tabulates them with the vegeetation classification map to identify how many pixels per vegetation
## class have been affected by four different variables related to the fires. 
## Because of the number of times this has to be repeated (up to 60) and the large number of pixels per raster
## this stage was run in UNSW's super comuting system katana which distributes calculations across nodes to run
## in parrallel. Each of the four code chunks are parrallelised and are normally stored in their own script to 
## be called through a shell and run on katana. This code will not run on most normal individual computers.
## To make the large rasters maneagable each one is split into chunks and blocks are crosstabulated against 
## the corresponding vegetation map blocks and compiled in a frequency table. 
## Resources required for each of these were 
##  number of cpus = 16
##  memory = 64 gb
##  time = up to 12 hours


## Code developed by Tom Le Breton (t.lebreton@unsw.edu.au)


## Required packages
library(dplyr)
library(readr)
library(stringr)
library(raster)
library(parallel)

#### 1. Total area burnt during the 2019-2020 fires

bs <- blockSize(raster("/srv/scratch/z3525067/interval_squeeze/Data/raw/veg_class/vegClass_jan19_8bit.tif"))

freq_table <- data.frame(matrix(numeric(), ncol = 2, nrow = 0),  
                         integer(),
                         stringsAsFactors = FALSE) %>% 
  setNames(nm = c("layer.1", "layer.2", "Freq"))


r <- mclapply(as.list(seq(1,bs$n,1)), FUN = function (x){
  target <- getValuesBlock(raster("/srv/scratch/z3525067/interval_squeeze/Data/processed/prev_below_MI/rp_fireyear2019.tif"),row = bs$row[x],nrows = bs$nrows[x], format = 'matrix')  
  target_veg <- getValuesBlock(raster("/srv/scratch/z3525067/interval_squeeze/Data/raw/veg_class/vegClass_jan19_8bit.tif"),row = bs$row[x],nrows = bs$nrows[x], format = 'matrix')
  
  temp_table <- crosstab(x = raster(target),
                         y = raster(target_veg),
                         long = TRUE) 
  
  return(temp_table)
},
mc.cores = detectCores())

freq_table <- bind_rows(freq_table, r)


write_csv(freq_table, "/srv/scratch/z3525067/interval_squeeze/Data/results/total_area_burnt_2019.csv")

#### 2. Change in area burnt before 2019-2020 fires i.e. 2018 and after i.e. 2019
rastlist <- list.files(path = "/srv/scratch/z3525067/interval_squeeze/Data/processed/prev_below_MI/count_below_MIp3/", pattern='.tif', all.files=TRUE, full.names=TRUE)

bs <- blockSize(raster("/srv/scratch/z3525067/interval_squeeze/Data/raw/veg_class/vegClass_jan19_8bit.tif"))

freq_table <- data.frame(matrix(numeric(), ncol = 2, nrow = 0),  
                         integer(),
                         character(),
                         stringsAsFactors = FALSE) %>% 
  setNames(nm = c("layer.1", "layer.2", "Freq", "year"))

for(i in 1:2){
  
  fire_interval <- rastlist[i]
  
  r <- mclapply(as.list(seq(1,bs$n,1)), FUN = function (x){
    target <- getValuesBlock(raster(fire_interval),row = bs$row[x],nrows = bs$nrows[x], format = 'matrix')  
    target_veg <- getValuesBlock(raster("/srv/scratch/z3525067/interval_squeeze/Data/raw/veg_class/vegClass_jan19_8bit.tif"),row = bs$row[x],nrows = bs$nrows[x], format = 'matrix')
    
    temp_table <- crosstab(x = raster(target),
                           y = raster(target_veg),
                           long = TRUE) %>% 
      mutate(year =  str_replace(fire_interval, pattern = "/srv/scratch/z3525067/interval_squeeze/Data/processed/prev_below_MI/count_below_MIp3/count_below_MIp3", replacement = ""))%>% 
      mutate(year =  str_replace(year, pattern = ".tif", replacement = ""))
    
    return(temp_table)
  },
  mc.cores = detectCores())
  
  freq_table <- bind_rows(freq_table, r)
}

write_csv(freq_table, "/srv/scratch/z3525067/interval_squeeze/Data/results/change_in_area_MIp3.csv")

#### 3. Areas burnt below minimum interval in 2019-2020 and also at least once previously
bs <- blockSize(raster("/srv/scratch/z3525067/interval_squeeze/Data/raw/veg_class/vegClass_jan19_8bit.tif"))

freq_table <- data.frame(matrix(numeric(), ncol = 2, nrow = 0),  
                         integer(),
                         character(),
                         stringsAsFactors = FALSE) %>% 
  setNames(nm = c("layer.1", "layer.2", "Freq", "year"))


r <- mclapply(as.list(seq(1,bs$n,1)), FUN = function (x){
  target <- getValuesBlock(raster("/srv/scratch/z3525067/interval_squeeze/Data/processed/prev_below_MI/squeezed_by_2019_MIp3.tif"),row = bs$row[x],nrows = bs$nrows[x], format = 'matrix')  
  target_veg <- getValuesBlock(raster("/srv/scratch/z3525067/interval_squeeze/Data/raw/veg_class/vegClass_jan19_8bit.tif"),row = bs$row[x],nrows = bs$nrows[x], format = 'matrix')
  
  temp_table <- crosstab(x = raster(target),
                         y = raster(target_veg),
                         long = TRUE) %>% 
    mutate(year =  str_replace("/srv/scratch/z3525067/interval_squeeze/Data/processed/prev_below_MI/squeezed_by_2019.tif", pattern = "/srv/scratch/z3525067/interval_squeeze/Data/processed/prev_below_MI/squeezed_by_2019.tif", replacement = ""))%>% 
    mutate(year =  str_replace(year, pattern = ".tif", replacement = "")) 
  
  return(temp_table)
},
mc.cores = detectCores())

freq_table <- bind_rows(freq_table, r)


write_csv(freq_table, "/srv/scratch/z3525067/interval_squeeze/Data/results/squeezed_by_2019_crosstab_MIp3.csv")

#### 4. Area burnt below minimum interval in 2019-2020
rastlist <- list.files(path = "/srv/scratch/z3525067/interval_squeeze/Data/processed/prev_below_MI/area_below_MIp3/", pattern='.tif', all.files=TRUE, full.names=TRUE)

bs <- blockSize(raster("/srv/scratch/z3525067/interval_squeeze/Data/raw/veg_class/vegClass_jan19_8bit.tif"))

freq_table <- data.frame(matrix(numeric(), ncol = 2, nrow = 0),  
                         integer(),
                         character(),
                         stringsAsFactors = FALSE) %>% 
  setNames(nm = c("layer.1", "layer.2", "Freq", "year"))

for(i in 1){
  
  fire_interval <- rastlist[i]
  
  r <- mclapply(as.list(seq(1,bs$n,1)), FUN = function (x){
    target <- getValuesBlock(raster(fire_interval),row = bs$row[x],nrows = bs$nrows[x], format = 'matrix')  
    target_veg <- getValuesBlock(raster("/srv/scratch/z3525067/interval_squeeze/Data/raw/veg_class/vegClass_jan19_8bit.tif"),row = bs$row[x],nrows = bs$nrows[x], format = 'matrix')
    
    temp_table <- crosstab(x = raster(target),
                           y = raster(target_veg),
                           long = TRUE) %>% 
      mutate(year =  str_replace(fire_interval, pattern = "/srv/scratch/z3525067/interval_squeeze/Data/processed/prev_below_MI/area_below_MIp3/prev_below_MI_", replacement = ""))%>% 
      mutate(year =  str_replace(year, pattern = ".tif", replacement = "")) 
    
    return(temp_table)
  },
  mc.cores = detectCores())
  
  freq_table <- bind_rows(freq_table, r)
}

write_csv(freq_table, "/srv/scratch/z3525067/interval_squeeze/Data/results/area_below_MI_2019_crosstab_MIp3.csv")
