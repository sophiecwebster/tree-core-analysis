############################################
############ Hillsborough, NH ##############
### Tree Core and Precipitation Analysis ###
############## 1973 - 2014 #################
############################################

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

#global <- global[c(1,4,5,8,11,13:16,18:20),]

