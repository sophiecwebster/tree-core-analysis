##############################################
############# Hillsborough, NH ###############
# Red Spruce Growth & Precipitation Analysis #
############### 1970 - 2014 ##################
##############################################

# Preparing environment

library(tidyverse)
library(readxl)
library(infer)
library(broom)
library(gt)
library(ggpubr)

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

# This is red spruce T8's core data, analyzed with ImageJ software

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
    y = "Growth (mm)",
    title = "Annual Growth Distributions in Red Spruce Trees",
    subtitle = "1970-2014"
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

elev <- tibble(m = c(500, 542, 558, 616, 654), tree = c("T4", "T5", "T6", "T7", "T8"))

####################
# Regression Hours #
####################

spruced %>%
  lm(length ~ PRCP, data = .) %>%
  augment()

l_and_temp <- spruced %>%
  lm(length ~ TAVG, data = .) 

l_and_temp <- spruce %>%
  lm(T4 ~ TAVG, data = .) %>%
  augment()


# 

spruced %>% 
      ggplot(aes(TAVG, length, color = tree)) + geom_point() +
  labs(
    x = "Annual Average Temperature (F)",
    y = "Growth (mm)",
    title = "Annual Average Temperature versus \nAnnual Red Spruce Growth"
  ) +
  theme_minimal() +
  geom_smooth(method = "lm", se = F) + 
  scale_color_discrete(name = "Tree ID")

ggsave(filename = '~/Desktop/OEB 55/growthvtemp.jpg', plot = last_plot())

# facet wrapping instead

spruced %>% 
  ggplot(aes(TAVG, length, color=tree)) + geom_point() +
  labs(
    x = "Annual Average Temperature (F)",
    y = "Growth (mm)",
    title = "Annual Average Temperature versus Annual Red Spruce Growth"
  ) +
  theme_minimal() +
  facet_wrap(~ tree) +
  geom_smooth(method = "lm", se = F) + 
  scale_color_discrete(name = "Tree ID") +
  stat_cor(label.x = 45, label.y = 7.7) +
  stat_regline_equation(label.x = 45, label.y = 6.7) +
  theme(legend.position = "none")
  

################
# Elevation Table

elev %>%
  gt() %>%
  tab_header(
    title = "Elevation of Sampled Red Spruce Trees",
    subtitle="Pack Monadnock, NH"
  ) %>%
  cols_label(
    tree = "Tree ID",
    m = "Elevation (m)"
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  cols_move_to_start(
    columns = vars(tree)
    )

# Function for calculating R^2 and p value

pval <- function(x, y, data) {
  temp <- lm(y ~ x, data = data)
  return(glance(temp)[,c(1,5)])
}

reg_stats <- tibble(tree = c("T4", "T5", "T6", "T7", "T8"), rsquared = c(0.356, 0.0181, 0.225, 0.226, 0.0244), pval = c(0.0000150, .378, .000989, .000967, .305))


meany <- spruced %>%
  group_by(tree) %>%
  summarize(avg_growth = mean(length))

meany <- full_join(meany, elev, by = "tree")

meany %>%
  ggplot(aes(m, avg_growth)) + geom_point(color = "forestgreen") + geom_smooth(se = F, method = "lm", color = "forestgreen") + 
  stat_regline_equation(label.x = 600, label.y = 3) + stat_cor(label.x = 600, label.y = 2.8) +
  theme_minimal() +
  labs(
    title = "Elevation versus Average Annual Growth",
    subtitle = "Red Spruces, 1970-2014"
  )


## Monthly Stuff
################################

# load other data! do it Sophie!
# don't forget to rbind

second <- yo %>%
  filter(!is.na(TAVG))

library(lubridate)

g00d_temps <- second %>% filter(month(.$DATE) %in% c(5,6,7,8,9))

temps_time <- g00d_temps %>%
  group_by(year(DATE)) %>%
  summarize(avg_t = mean(TAVG))

temps_time <- temps_time %>% rename(DATE = `year(DATE)`)

temps_time %>%
  ggplot(aes(year, avg_t)) + geom_point(color = "cadetblue3") +
  labs(
    title = "Average Spring & Summer Temperatures",
    subtitle = "(May-September)",
    x = "Year",
    y = "Average Temperature (F)"
  ) +
  geom_smooth(method = "lm", se = F, color = "cadetblue3") +
  theme_minimal() +
  stat_cor() + 
  stat_regline_equation(label.y = 67.5)

ggsave('~/Desktop/OEB 55/spring-temps.jpg', plot = last_plot())

yoho <- full_join(spruced, temps_time, by = "DATE")

yoho %>% 
  ggplot(aes(avg_t, length, color=tree)) + geom_point(aes(alpha = 0.25)) +
  labs(
    x = "Annual Average Temperature (F)",
    y = "Growth (mm)",
    title = "Spring/Summer Average Temperature versus Annual Red Spruce Growth",
    subtitle = "May-September"
  ) +
  theme_minimal() +
  facet_wrap(~ tree) +
  geom_smooth(method = "lm", se = F) + 
  scale_color_discrete(name = "Tree ID") +
  stat_cor(label.x = 63, label.y = 7.7) +
  stat_regline_equation(label.x = 63, label.y = 6.7) +
  theme(legend.position = "none")

#ggsave('~/Desktop/OEB 55/springz.jpg', last_plot())

yoho %>% 
  ggplot(aes(avg_t, length, color = tree)) + geom_point() +
  labs(
    x = "Annual Average Temperature (F)",
    y = "Growth (mm)",
    title = "Annual Average Temperature versus \nAnnual Red Spruce Growth"
  ) +
  theme_minimal() +
  geom_smooth(method = "lm", se = F) + 
  scale_color_discrete(name = "Tree ID")

