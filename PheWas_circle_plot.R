### Script for creating a circle plot for PheWas data

## Load packages & data
library(circlize)
library(ComplexHeatmap)
library(tidyverse)

raw_data <- read.csv("data_file.csv", header = TRUE) 
#note: data_file in this example has the following columns: category (phenotype group), 
#title (name of trait), strata1:4_est (effect estimate for that strata), 
#strata1:4_P (P-value for that strata's estimate)

## Data wrangling to account for positive and negative effect sizes in plotting
data_with_effect <- raw_data %>% 
  #when effect estimate < 0, multiply P-value*-1
  #when effect estimate > 0, multiply P-value*1
  mutate(strata1_effect = case_when(strata1_est < 0 ~ strata1_P*-1,
                                 strata1_est > 0 ~ strata1_P*1,
                                 strata1_est == 0 ~ strata1_P)) %>% 
  mutate(strata2_effect = case_when(strata2_est < 0 ~ strata2_P*-1,
                                    strata2_est > 0 ~ strata2_P*1,
                                    strata2_est == 0 ~ strata2_P)) %>% 
  mutate(strata3_effect = case_when(strata3_est < 0 ~ strata3_P*-1,
                                    strata3_est > 0 ~ strata3_P*1,
                                    strata3_est == 0 ~ strata3_P)) %>% 
  mutate(strata4_effect = case_when(strata4_est < 0 ~ strata4_P*-1,
                                  strata4_est > 0 ~ strata4_P*1,
                                  strata4_est == 0 ~ strata4_P))


## Convert data to matrix form (circlize package only accepts matrices)
formatted_data <- data_with_effect %>% 
  #select colums that incorporate pos/neg effects
  select(strata1_effect, strata2_effect, strata3_effect, strata4_effect) 

#put into matrix form (this is the data matrix)
formatted_mat <- as.matrix(formatted_data) 

#select trait titles 
titles <- data_with_effect %>% 
  select(title) 

#put into matrix form (this is the titles matrix)
titles_mat <- as.matrix(titles)

#set row.names for the data matrix to be the trait titles
row.names(formatted_mat) <- titles_mat

#create splits (i.e. categories) for the trait groups
splits <- data_with_effect %>% 
  select(category) 

#put into matrix form (this is the splits matrix)
splits <- as.matrix(splits)

#set levels for each split group
splits = factor(splits, levels = c("Circulatory system", "Endocrine/metabolic", "Genitourinary",
                                   "Hematopoietic", "Musculoskeletal", "Neurological", "Respiratory", "Sense organs")) 

## Plotting
#set the color scale
col_list <- colorRamp2(breaks = c(-1, -0.05, 0.05, 1), c("#c7e3f1", "#53b4ec", "#7bb765", "#e1efa7")) 
#initialize circos object
circos.par(gap.degree = 10, points.overflow.warning = F)
  #note: gap.degree = space between groups
#create heatmap, give the function the data matrix, the splits matrix, and the color list
circos.heatmap(formatted_mat, split = splits, col = col_list, show.sector.labels = TRUE,
               bg.border = "grey", rownames.side = "inside") #if you don't want trait groups, remove splits=splits
#create a legend 
lgd_pvals = Legend(at = c(-1, -0.05, 0.05, 1), col_fun = col_list, 
                   title_position = "topleft", title = "PRS p-values", 
                   labels = c("1", "< 0.05", "< 0.05", "1")) 
#draw the legend on the plot
draw(lgd_pvals,  x = unit(10, "mm"), y = unit(10, "mm"), just = c("left", "bottom"))
#clear the plotting space (note: this is very important!)
circos.clear()


