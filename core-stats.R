##############################################
############# Hillsborough, NH ###############
# Red Spruce Growth & Precipitation Analysis #
############### 1970 - 2014 ##################
##############################################

# Preparing environment

library(tidyverse)
library(readxl)
library(infer)

# Reading in the data 

global1 <- read_csv('./raw-data/2117690.csv')
global2 <- read_csv('./raw-data/2117694.csv')

global1 <- global1 %>% select(!DSNW)
global <- rbind(global1, global2)

temp1 <- global %>% filter(DATE == 2006 & DT00 == 4)

temp2 <- global %>% filter(DATE == 1982) %>% .[6,]

# Appending the second data set (2000 to 2014) to the first (1970 to 2000)

global <- global %>% filter(!is.na(PRCP)) %>% filter(NAME != "EDWARD MACDOWELL LAKE, NH US")

global <- global %>% filter(!is.na(SNOW)) %>% filter(!is.na(TAVG)) %>% arrange(DATE)

# Filtering out NA values/other dead ends with the data

# Whoops, we were missing 2006!

global <- global %>% arrange(DATE)

global <- global %>% filter(!NAME %in% c("PETERBORO 2 S, NH US", "MANCHESTER, NH US", "FRANCESTOWN, NH US"))

# Temp is one Petersboro date from 1976 that I need to preserve 

global <- global %>% rbind(temp) %>% rbind(temp2) %>% arrange(DATE)

# Big cahuna culling to get one data point per year
# Established pecking order based on proximity to Greenfield, NH

global <- global[c(1,4,5,8,11,13,15:17,19:23,26,28,30,33,35,39,42,45,50,54,60,65,68,71,75,80,89,90,92:95,97,99,102,106,109,112,115,119),] %>% 
  rbind(temp1) %>% arrange(DATE)

# Yeehaw, it's ready for action! Let's add in some core data.

# This is red spruce T8's core data, analyzed with ImageJ

t8 <- read_csv('./raw-data/t8-core.csv')
t8 <- t8$Length # 59
T8 <- rev(t8[1:45])

# T7 Red Spruce

t7 <- read_csv('./raw-data/t7-core.csv')
t7 <- t7$Length # 61
T7 <- rev(t7[1:45])

# T6 Red Spruce

t6 <- read_csv('./raw-data/t6-core.csv')
t6 <- t6$Length
T6 <- rev(t6[1:45])

# T5 Red Spruce

t5 <- read_csv('./raw-data/t5-core.csv')
t5 <- t5$Length
T5 <- rev(t5[1:45])

# T4 Red Spruce

t4 <- read_csv('./raw-data/t4-core.csv')
t4 <- t4$Length
T4 <- rev(t4[1:45])

# Combine core data with global climate statistics, reversing to align timelines

spruce <- cbind(global, T4, T5, T6, T7, T8)

# A standardized all these to 44, which was the length of the time interval
# 1970-2014.

spruced <- spruce %>% gather("tree", "length", 11:15)

# Making data taller for plotting purposes

############################################################################

## Data Analysis: Look at this GRAPH

# Gauging annual growth across all trees from 1970 to 2014

spruced %>%
  ggplot(aes(DATE, length, color = tree)) + geom_point() +
  theme_classic() +
  labs(
    x = "Year",
    y = "Growth (mm)",
    title = "Red Spruce Annual Growth",
    subtitle = "1970-2014"
  ) +
  scale_color_discrete(name = "Tree ID")

# Box plots of trees' growth

spruced %>%
  ggplot(aes(tree, length, fill = tree)) + geom_boxplot() +
  theme_classic() +
  labs(
    x = "Tree",
    y = "Growth (mm)"
  ) + scale_fill_manual(values=c("#66FFBC", "#5DE8D2", "#74EFFF", "#5DB3E8", "#669DFF")) +
  theme(legend.position = "none")

# Intriguing... with lower temps, trees are generally growing more.

spruced %>% 
  ggplot(aes(TAVG, length, color = tree)) + geom_point()

cor(spruced$TAVG, spruced$length)

# -.2897, eh!

# General positive trend we are viewing b/w snow and length.

spruced %>%
  filter(!is.na(SNOW)) %>%
  ggplot(aes(SNOW, length, color = tree)) + geom_point()

# Wildly enough, precipitation really doesn't seem to be that impactful.

spruced %>% 
  ggplot(aes(PRCP, length, color = tree)) + geom_point()

cor(spruced$PRCP, spruced$length)

# -0.002781147 oof!

# Interesting to observe almost a bell -- some rain is good, but not way too much

spruced %>% 
  ggplot(aes(DP10, length, color = tree)) + geom_point()

# Recording/comparing elevation

elev <- tibble(m = c(500, 542, 558, 616, 654), tree = c(4:8))







