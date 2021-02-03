#### Code for paper "Megafire-induced interval squeeze threatens vegetation at landscape scales" ####
#### megafire_interval_squeeze_code-04-results_summary ####
##
## This script takes the summaries produced in "megafire_interval_squeeze_code-04-results_summary"
## and creates the final plots used for the manuscript.
## 
## The script outputs seven plots, 
##  1. Fig. 1b. Proportion of vegetation types impacted by 2019-2020 fires
##      
##  2. Fig. 1c. Proportion of rainfroest classes impacted by 2019-2020 fires
##      
##  3. Fig. 1d. Proportion of wet sclerophyll forest classes impacted by 2019-2020 fires
##  
##  4. Fig. 2i. Percentage of rainforests burnt below minimum interval at least once before and after 2019
##      
##  5. Fig. 2ii. Percentage of heathlands burnt below minimum interval at least once before and after 2019
##      
##  6. Fig. 2iii. Percentage of wet sclerophyll forests burnt below minimum interval at least once before and after 2019
##  
##  7. Fig. 2iv. Percentage of alpine complex burnt below minimum interval at least once before and after 2019

## Code developed by Tom Le Breton (t.lebreton@unsw.edu.au)

## Required packages
library(tidyverse)

## Need to 
#   relable figures in correct order 
##  make sure for figure 3 which is based on summary 3 that the terms sqzd and FIS are changed with gt_o and gt_1 respectively

##  1. Fig. 1b. Proportion of vegetation types impacted by 2019-2020 fires

summary_bmi <- read_csv("Data/results/summaries/area_burnt_below_MI_2019-2020.csv")
summary_sqzd <- read_csv("Data/results/summaries/area_squeezed_by_2019-2020.csv")

fig_1b_all <- summary_sqzd %>% 
  dplyr::select(vegForm, prop_sqzd_perForm) %>% 
  left_join(summary_bmi, by = "vegForm") %>% 
  dplyr::select(vegForm, prop_form_burnt, prop_form_bMI, prop_sqzd_perForm) %>% 
  group_by(vegForm) %>% 
  filter(row_number()==1) %>% 
  pivot_longer(cols = c(prop_form_burnt, prop_form_bMI, prop_sqzd_perForm), names_to = "prop") %>% 
  filter(!vegForm %in% c(NA, "Non-native", "Semi-arid woodlands","Saline wetlands","Arid shrublands")) %>%
  ggplot(aes(x = factor(vegForm, level = sort(unique(.$vegForm),decreasing = FALSE)), 
             y = value, 
             fill = factor(prop, levels = c("prop_form_burnt", "prop_form_bMI", "prop_sqzd_perForm")))) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  scale_x_discrete(labels = c("alp","dsf", "fow", "fww", "gl", "gw", "hth", "rf", "wsf")) +
  scale_fill_manual(values=c('#969696','#fed976','#fd8d3c')) +
  theme_minimal()+
  ylab("Percent of area (%)") + xlab("Vegetation Formation")+
  theme(legend.position = 'none')+
  theme(axis.line = element_line(colour = "#525252"),
        axis.ticks = element_line(colour = "#525252"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA))

ggsave(file="Data/results/summaries/Figures/Fig1_b_all.svg", width=5, height=2.4, dpi=300, units = "in")

    
##  2. Fig. 1c. Proportion of rainfroest classes impacted by 2019-2020 fires

fig_1c <- summary_sqzd %>% 
  filter(vegForm %in% c("Rainforests")) %>%
  mutate(vegClass = str_remove(vegClass, pattern = "Rainforests")) %>% 
  ggplot(aes(x = factor(vegClass, level = sort(unique(.$vegClass),decreasing = FALSE)), y = prop_sqzd_perClass, group = 1)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
  scale_x_discrete(labels = c("ct","dry", "lit", "nwt", "swt", "str", "wvt")) +
  theme_minimal()+
  ylab("Percent of area (%)") + xlab("")+
  theme(legend.text = element_blank(), legend.title = element_blank())+
  theme(axis.line = element_line(colour = "#525252"),
        axis.ticks = element_line(colour = "#525252"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) +
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA))


fig_1c

ggsave(file="Data/results/summaries/Figures/Fig1_c_rf_all.svg", width=5, height=2.4, dpi=300, units = "in")

     
##  3. Fig. 1d. Proportion of wet sclerophyll forest classes impacted by 2019-2020 fires

fig_1d <- summary_sqzd %>% 
  filter(vegForm %in% c("Wet sclerophyll forests")) %>%
  mutate(vegClass = str_remove(vegClass, pattern = "Wet Sclerophyll Forests")) %>% 
  ggplot(aes(x = factor(vegClass, level = sort(unique(.$vegClass),decreasing = FALSE)), y = prop_sqzd_perClass, group = 1)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
  scale_x_discrete(labels = c("mon","nco","nes","nhl","ntl","sco","ses","sll","stl")) +
  theme_minimal()+
  ylab("Percent of area (%)") + xlab("")+
  theme(legend.text = element_blank(), legend.title = element_blank())+
  theme(axis.line = element_line(colour = "#525252"),
        axis.ticks = element_line(colour = "#525252"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA))


fig_1d

ggsave(file="Data/results/summaries/Figures/Fig1_d_wsf_all.svg", width=5, height=2.4, dpi=300, units = "in")

##  4-7. Figs. 2i-iv. Percentage of veg types burnt below minimum interval at least once before and after 2019
## These four figures are produced from a single loop which iterates over the list of desired vegetation types.

summary_change <- read_csv("Data/results/summaries/change_in_area_bMI_2018-2019.csv")


for (i in c("Rainforests", "Heathlands", "Wet sclerophyll forests", "Alpine complex")){
  
  fig_3 <- summary_change %>% 
    filter(vegForm == i) %>% 
    ggplot(aes(x = as.factor(year), y = prop_gt_0_perForm)) +
    geom_bar(stat = 'identity', position = position_dodge(), fill = "#e31a1c") +
    scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
    theme_minimal()+
    ylab(element_blank()) + xlab(element_blank())+
    theme(legend.text = element_text(size = 10), legend.title = element_blank())+
    theme(axis.line = element_line(colour=  "#525252"),
          axis.ticks = element_line(colour=  "#525252")) +
    theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10))+
    theme(panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA))
  
  fig_3
  
  ggsave(file=paste0("Data/results/summaries/Figures/Fig2_",i,".svg"), width=1.2, height=1.75, dpi=300, units = "in")
}