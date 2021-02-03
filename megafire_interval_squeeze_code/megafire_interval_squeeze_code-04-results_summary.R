#### Code for paper "Megafire-induced interval squeeze threatens vegetation at landscape scales" ####
#### megafire_interval_squeeze_code-04-results_summary ####
##
## This script takes the raw outputs of the cross-tabulated data created previously and
## summarises them to calculate the proportion of the state, vegetation formations and 
## vegetation classes that have been burnt below minimum interval once or more and also
## by the 2019-2020 fires.
## 
## The script outputs three seperate summary tables, some of which have areas that overlap
## but its constructed in this way for figure construction at the next stage and to maximise
## readability of each set of results. They are:
##  1. area_burnt_below_MI_2019-2020
##      Summarises the area across the state, veg formaions and classes that were burnt below
##      minimum interval (MI) in 2019-2020.
##  2. area_squeezed_by_2019-2020
##      Summarises the area across the state, veg formaions and classes that were burnt below
##      minimum interval at least once prior to 2019-2020 and again in 2019-2020 fire season.
##      Squeezed here is shorthand for the above description and doesnt necessarily mean
##      the areas "squeezed" here are experiencing interval squeeze, more that they are at
##      risk of it.
##  3. change_in_area_bMI_2018-2019
##      Summarises the area across the state, veg formaions and classes that were burnt below
##      minimum interval at least once, and more than once, before the 2019-2020 fires
##      from 2960-2018 inclusive and after the 2019-2020 fires from 1960-2019-2020 inclusive.
##
## Code developed by Tom Le Breton (t.lebreton@unsw.edu.au)

## Required packages
library(tidyverse)

#### Read in general data used across different summaries
## 0a. State total burnt area 2019
## 0b. Veg lookup table to match Value with its veg class and formation 
##      This is used to to cross reference with ID numbers ("Value") in cross-tabulations
##      contains minimum intervals based on Kenny 2013 which reccomends an additional 3 years 
##      on top of the minimum intervals reccommended by Kenny 2004 to account for seed bank 
##      accumulation - represented by p3 as shorthand for plus 3 in various filenames to 
##      distinguish from versions based on Kenny 2004.


## 0a. State total

raw_total <- read_csv("Data/results/katana_results/total_area_burnt_2019.csv",
                      col_names = c("burnt","Value","count"))

summary_total<- raw_total %>% 
  group_by(Value, burnt) %>% 
  summarise(class_total_burnt = sum(count)) %>%  # gets the total class area in pixels burnt and unburnt in 2019
  ungroup() %>%
  group_by(burnt) %>% 
  mutate(state_total_burnt = sum(class_total_burnt)) %>% # gets the total state area in pixels burnt and unburnt in 2019
  ungroup() %>% 
  filter(burnt > 0) %>% 
  dplyr::select(Value, class_total_burnt, state_total_burnt) 

## 0b. Vetation formation/class lookup table 

veg_lookup <- read_csv("D:/PhD/2_NSW_Fire_Season/Data/raw/Veg Formations/vegClassLookup_seedbankIntervals.csv") %>% 
  ## Need to combine candidate native-grasslands and native grasslands, make the assumption for now that they are native grasslands.
  mutate(vegForm = replace(vegForm, vegForm == "Candidate Native Grasslands", "Grasslands"), ## Assign Candidate native grasslands to formation grasslands but retain as a veg class
         MinFireInt = replace(MinFireInt, MinFireInt == 999, NA), ## reassign MI for Non-Native's to NA
         MinFireInt = replace(MinFireInt, MinFireInt == 0, NA)) ## change NA MI from 0 to NA, doesnt really matter as only including this to get a count of total area burnt for now

raw <- read_csv("Data/results/katana_results/area_below_MI_2019_crosstab_MIp3.csv",
                col_names = c("burnt_bMI","Value","count","year")) %>% 
  dplyr::select(burnt_bMI,Value,count) %>% 
  mutate(burnt_bMI = ifelse(Value %in%  c(1,82) & burnt_bMI == 1, 0, burnt_bMI)) #reset non-native and NA forms to 0 for Below MI becuase theyshouldnt have any because they shouldnt have any intervals

#### 1. area_burnt_below_MI_2019-2020

summary <- raw %>% 
  group_by(Value, burnt_bMI) %>% 
  summarise(count_sum = sum(count)) %>%   # for each year how many cells within in each veg class (Value) fall into each interval category.
  left_join(veg_lookup, by = 'Value') %>% 
  left_join(summary_total, by = 'Value') %>%
  ## Do a little tidying before getting started calculating new values, remove NA's for areas not burnt 
  mutate(class_total_burnt = replace_na(class_total_burnt, 0),
         state_total_burnt = replace_na(state_total_burnt,first(.$state_total_burnt))) %>% 
  ## Calculate the total area of each formation by taking the size of each vegclass ($Count) and then get area bunnt and propotion for class and form
  group_by(vegForm) %>% 
  mutate(form_pixel_count = sum(unique(Count)), ## Sums the "Count" field from the veg_lookup table to get the size of each formation in pixels, "Count" is the pixel count for each vegetation class
         form_total_burnt = sum(unique(class_total_burnt)), ## total area of each formation burnt in pixels in 2019
         prop_form_burnt = round((form_total_burnt/form_pixel_count)*100, digits = 3)) %>% ## % of formation burnt in 2019
  ungroup() %>% 
  group_by(vegClass) %>% 
  mutate(prop_class_burnt = round((class_total_burnt/Count)*100,digits = 3)) %>% ## % class burnt in 2019
  ## remove unburnt rows exccept for classes with now area burnt below MI 
  arrange(desc(burnt_bMI), .by_group = TRUE) %>% 
  filter(row_number()==1) %>%
  ungroup() %>% 
  ##calculate total area  with burns below MI
  group_by(burnt_bMI) %>% 
  mutate(state_total_bMI = ifelse(burnt_bMI == 1, sum(count_sum), 0), ## total area across the state burnt below MI regardless of form or class
         prop_bMI_2019 = (state_total_bMI/state_total_burnt)*100) %>% ## % of state burnt below MI
  ungroup() %>% 
  ##calculate prportionof each calss and fomration burnt below MI - needs to happen after uburnt areas have been removed
  group_by(vegForm, burnt_bMI) %>%
  mutate(below_mi_perForm = ifelse(burnt_bMI == 1, sum(count_sum), 0), ## Area burnt below minimum interval per formation
         prop_form_bMI = round((below_mi_perForm/form_pixel_count)*100, digits = 3), # % formation burnt below BMI
         prop_total_bMI_perForm = round((below_mi_perForm/state_total_bMI)*100, digits = 3)) %>% # % the states total area burnt below MI accounted for by each formation
  ungroup() %>% 
  group_by(vegClass, burnt_bMI) %>%
  mutate(below_mi_perClass = ifelse(burnt_bMI == 1, sum(count_sum), 0),# Area burnt below minimum interval per class
         prop_class_bMI = round((below_mi_perClass/Count)*100, digits = 3), # % class burnt below BMI
         prop_total_bMI_perClass = round((below_mi_perClass/state_total_bMI)*100, digits = 3)) %>% # % the states total area burnt below MI accounted for by each class
  ungroup() %>% 
  mutate(below_mi_ha_form = (below_mi_perForm*225)/10000, ## Multiplies number of pixels burnt below minimum interval by pixel area 15m*15m = 225, and converts to ha
         below_mi_ha_class = (below_mi_perClass*225)/10000) %>% 
  dplyr::select(vegForm,
                vegClass,
                form_pixel_count,
                Count,
                MinFireInt,
                state_total_burnt,
                state_total_bMI,
                prop_bMI_2019,
                form_total_burnt,
                below_mi_ha_form,
                prop_form_burnt,
                below_mi_perForm, 
                prop_form_bMI,
                prop_total_bMI_perForm,
                class_total_burnt,
                below_mi_ha_class,
                prop_class_burnt,
                below_mi_perClass,
                prop_class_bMI,
                prop_total_bMI_perClass) %>% 
  rename(class_pixel_count = Count)

write_csv(summary, "Data/results/summaries/area_burnt_below_MI_2019-2020.csv")

##  2. area_squeezed_by_2019-2020

raw_sqzd <- read_csv("Data/results/katana_results/squeezed_by_2019_crosstab_MIp3.csv",
                     col_names = c("n_bMI","Value","count")) %>% 
  mutate(n_bMI = ifelse(Value %in%  c(1,82) & n_bMI >= 1, 0, n_bMI)) #reset non-native and NA forms to 0 for Below MI becuase they shouldnt have any because they shouldnt have any intervals


summary_sqzd <- raw_sqzd %>% 
  group_by(Value, n_bMI) %>% 
  summarise(count_sum = sum(count)) %>%  # for each year how many cells within in each veg class (Value) fall into each interval category.
  left_join(veg_lookup, by = 'Value') %>% 
  left_join(summary_total, by = 'Value') %>%
  ## Do a little tidying before getting started calculating new values, remove NA's for areas not burnt 
  mutate(class_total_burnt = replace_na(class_total_burnt, 0),
         state_total_burnt = replace_na(state_total_burnt,first(.$state_total_burnt))) %>% 
  #filter(burnt > 0) %>% 
  ## Calculate the total area burnt of each formation by taking the size of each vegclass ($Count)
  group_by(vegForm) %>% 
  mutate(form_pixel_count = sum(unique(Count)), ## formation size
         form_total_burnt = sum(unique(class_total_burnt)), ## total area of each formation burnt in 2019
         prop_form_burnt = round((form_total_burnt/form_pixel_count)*100, digits = 3)) %>% ## % of formation burnt in 2019
  ungroup() %>% 
  group_by(vegClass) %>% 
  mutate(prop_class_burnt = round((class_total_burnt/Count)*100,digits = 3)) %>% ## % class burnt in 2019
  ## remove unburnt rows exccept for classes with now area burnt below MI 
  filter(if(n()>1){n_bMI > 0} else {n_bMI == 0}) %>%
  ungroup() %>%
  ##calculate total area  squeezed across state, first create new column to seperate squeezed from unsqueezed more easily
  mutate(squeezed = n_bMI >0) %>% 
  mutate(state_total_sqzd = sum(count_sum[squeezed == TRUE]), ## total area across the state squeezed regardless of form or class
         prop_sqzd_2019 = (state_total_sqzd/state_total_burnt)*100) %>% ## % of state squeezed
  ## Calculate area squeezed per formation, as a proportion of the formation, proportion of formation area burnt and proportion of total area squeezed across the state
  group_by(vegForm) %>% 
  mutate(sqzd_perForm = sum(count_sum[squeezed == TRUE]), ## Area burnt for each formation that had previously been burnt below MI at least once
         prop_sqzd_perForm = round((sqzd_perForm/form_pixel_count)*100, digits = 3), ## proportion of formation area squeezed
         prop_sqzd_perForm_burnt = round((sqzd_perForm/form_total_burnt)*100, digits = 3), ## proportion of burnt area squeezed per formation
         prop_state_sqzd_perForm = round((sqzd_perForm/state_total_sqzd)*100, digits = 3)) %>% ## proportion of state area squeezed per formation
  ungroup() %>% 
  ## Same for vegClass
  group_by(vegClass) %>%
  mutate(sqzd_perClass = sum(count_sum[squeezed == TRUE]), ## Area burnt for each Class that had previously been burnt below MI at least once
         prop_sqzd_perClass = round((sqzd_perClass/Count)*100, digits = 3), ## proportion of Class area squeezed
         prop_sqzd_perClass_burnt = round((sqzd_perClass/class_total_burnt)*100, digits = 3), ## proportion of burnt area squeezed per Class
         prop_state_sqzd_perClass = round((sqzd_perClass/state_total_sqzd)*100, digits = 3)) %>% ## proportion of state area squeezed per Class
  ungroup() %>% 
  mutate(squzd_area_ha_form = (sqzd_perForm*225)/10000,## Multiplies number of pixels burnt below minimum interval by pixel area 15m*15m = 225, and converts to ha
         squzd_area_ha_class = (sqzd_perClass*225)/10000) %>% 
  mutate(class_prop_per_freq = ifelse(n_bMI > 0, round((count_sum/sqzd_perClass)*100, digits = 3),0), ## proportion of squeezed area burnt n times per class
         form_prop_per_freq = ifelse(n_bMI > 0,round((count_sum/sqzd_perForm)*100, digits = 3),0)) %>% ## proportion of squeezed area burnt n times per formation
  dplyr::select(vegForm,
                vegClass,
                n_bMI,
                form_pixel_count,
                Count,
                MinFireInt,
                state_total_burnt,
                state_total_sqzd,
                prop_sqzd_2019,
                form_total_burnt,
                prop_form_burnt,
                sqzd_perForm,
                squzd_area_ha_form,
                prop_sqzd_perForm,
                prop_sqzd_perForm_burnt,
                prop_state_sqzd_perForm,
                form_prop_per_freq,
                class_total_burnt,
                prop_class_burnt,
                sqzd_perClass,
                squzd_area_ha_class,
                prop_sqzd_perClass,
                prop_sqzd_perClass_burnt,
                prop_state_sqzd_perClass,
                class_prop_per_freq) %>% 
  rename(class_pixel_count = Count)

write_csv(summary_sqzd, "Data/results/summaries/area_squeezed_by_2019-2020.csv")

##  3. change_in_area_bMI_2018-2019

raw_change <- read_csv("Data/results/katana_results/change_in_area_MIp3.csv",
                       col_names = c("n_bMI","Value","count", "year")) %>% 
  mutate(n_bMI = ifelse(Value %in%  c(1,82) & n_bMI >= 1, 0, n_bMI), #reset non-native and NA forms to 0 for Below MI becuase they shouldnt have any because they shouldnt have any intervals
         year = str_remove(year, "/srv/scratch/z3525067/interval_squeeze/Data/processed/prev_below_MI/count_below_MIp3//count_below_MIp3_"))

summary_change<- raw_change %>% 
  group_by(year, Value, n_bMI) %>% 
  summarise(count_sum = sum(count)) %>%  # for each year how many cells within in each veg class (Value) fall into each interval category.
  left_join(veg_lookup, by = 'Value') %>% 
  group_by(year, vegForm) %>% 
  mutate(form_pixel_count = sum(unique(Count))) %>%  ## formation size
  ungroup() %>% 
  ## remove unburnt rows exccept for classes with no area burnt below MI 
  filter(if(n()>1){n_bMI > 0} else {n_bMI == 0}) %>%
  ungroup() %>%
  ## calculate total area burnt below minimum interval at least once (n_bMI > 0) and more than once (n_bMI >1).
  ## Shorthand for the former is gt_0 (greater than 0) and  for the latter gt_1 (greater than 1)
  ## first create new column to seperate squeezed from unsqueezed more easily
  mutate(n_bMI_gt_0 = n_bMI > 0,
         n_bMI_gt_1 = n_bMI > 1) %>% 
  group_by(year) %>% 
  mutate(state_total_gt_0 = sum(count_sum[n_bMI_gt_0 == TRUE]),
         state_total_gt_1 = sum(count_sum[n_bMI_gt_1 == TRUE])) %>% 
  ungroup() %>% 
  mutate(state_change = state_total_gt_0 - state_total_gt_0[year == 2018], ## change in pixels burnt under MI at least once before and after 2019
         state_pcnt_inrease = round((state_change/state_total_gt_0[year == 2018])*100, digits = 3),## % increase from 2018 to 2019
         state_change_gt_1 = state_total_gt_1 - state_total_gt_1[year == 2018], ## change in pixels burnt under MI more than once before and after 2019
         state_pcnt_inrease_gt_1 = round((state_change_gt_1/state_total_gt_1[year == 2018])*100, digits = 3)) %>%  
  ## Calculate area gt_0 per formation, as a proportion of the formation, proportion of formation area burnt and proportion of total area squeezed across the state
  group_by(year,vegForm) %>% 
  mutate(gt_0_perForm = sum(count_sum[n_bMI_gt_0 == TRUE]), ## Area burnt for each formation that had previously been burnt below MI at least once
         prop_gt_0_perForm = round((gt_0_perForm/form_pixel_count)*100, digits = 3), ## proportion of formation area squeezed
         prop_state_gt_0_perForm = round((gt_0_perForm/state_total_gt_0)*100, digits = 3),
         gt_1_perForm = sum(count_sum[n_bMI_gt_1 == TRUE]), ## Area burnt for each formation that had previously been burnt below MI at least once
         prop_gt_1_perForm = round((gt_1_perForm/form_pixel_count)*100, digits = 3), ## proportion of formation area squeezed
         prop_state_gt_1_perForm = round((gt_1_perForm/state_total_gt_1)*100, digits = 3)) %>% ## proportion of state area squeezed per formation
  ungroup() %>% 
  ## Same for vegClass
  group_by(year,vegClass) %>%
  mutate(gt_0_perClass = sum(count_sum[n_bMI_gt_0 == TRUE]), ## Area burnt for each Class that had previously been burnt below MI at least once
         prop_gt_0_perClass = round((gt_0_perClass/Count)*100, digits = 3), ## proportion of Class area squeezed
         prop_state_gt_0_perClass = round((gt_0_perClass/state_total_gt_0)*100, digits = 3)) %>% ## proportion of state area squeezed per Class
  ungroup() %>% 
  group_by(vegForm) %>% 
  mutate(form_change = gt_0_perForm - gt_0_perForm[year == 2018], ## change in pxels burnt under MI at least once from 2018-2019
         form_prop_change = round((form_change/form_pixel_count)*100, digits = 3), ## change above in terms of the % of formation
         form_pcnt_inrease = round((form_change/gt_0_perForm[year == 2018])*100, digits = 3),
         form_change_gt_1 = gt_1_perForm - gt_1_perForm[year == 2018], ## change in pxels burnt under MI more than once from 2018-2019
         form_prop_change_gt_1 = round((form_change_gt_1/form_pixel_count)*100, digits = 3), ## change above in terms of the % of formation
         form_pcnt_inrease_gt_1 = round((form_change_gt_1/gt_1_perForm[year == 2018])*100, digits = 3)) %>%  ## % increase from 2018 to 2019
  ungroup() %>% 
  group_by(vegClass) %>% 
  mutate(class_change = gt_0_perClass - gt_0_perClass[year == 2018], ## change in pxels burnt under MI at least once from 2018-2019
         class_prop_change = round((class_change/Count)*100, digits = 3), ## change above in terms of the % of formation
         class_pcnt_inrease = round((class_change/gt_0_perClass[year == 2018])*100, digits = 3)) %>% ## % increase from 2018 to 2019
  ungroup() %>% 
  mutate(form_change_ha = (form_change*225)/10000,
         class_change_ha = (class_change*225)/10000,
         state_total_gt_0_ha = (state_total_gt_0*225)/10000,
         state_change_ha = (state_change*225)/10000) %>% 
  group_by(vegClass, year) %>% 
  filter(row_number()==1) %>%
  ungroup() %>% 
  dplyr::select(year,
                vegForm,
                vegClass,
                form_pixel_count,
                Count,
                MinFireInt,
                state_total_gt_0,
                state_total_gt_0_ha,
                state_change,
                state_change_ha,
                state_pcnt_inrease,
                gt_0_perForm,
                prop_gt_0_perForm,
                prop_state_gt_0_perForm,
                form_change,
                form_change_ha,
                form_prop_change,
                form_pcnt_inrease,
                state_total_gt_1,
                state_change_gt_1,
                state_pcnt_inrease_gt_1,
                gt_1_perForm,
                prop_gt_1_perForm,
                prop_state_gt_1_perForm,
                form_change_gt_1,
                form_prop_change_gt_1,
                form_pcnt_inrease_gt_1,
                gt_0_perClass,
                prop_gt_0_perClass,
                prop_state_gt_0_perClass,
                class_change,
                class_change_ha,
                class_prop_change,
                class_pcnt_inrease) %>% 
  rename(class_pixel_count = Count)

write_csv(summary_change, "Data/results/summaries/change_in_area_bMI_2018-2019.csv")
