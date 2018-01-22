library(readr)
library(tidyr)
library(dplyr)
library(stringr)

state_ts <- read_csv("../data/State_time_series.csv") 
# subtract year
state_ts$Date <- year(state_ts$Date)

# select certain column, reshape dataframe, calculate the mean within a year
state_ts <- state_ts %>% 
      select("year" = "Date", "region" = RegionName, ZHVI_1bedroom,ZHVI_2bedroom,ZHVI_3bedroom, ZHVI_4bedroom,ZHVI_5BedroomOrMore, ZHVI_CondoCoop, ZHVI_SingleFamilyResidence, ZHVI_MiddleTier, ZHVI_TopTier, ZHVI_BottomTier) %>% 
      gather(key = "type", value = "price", 3:12) %>% 
      group_by(year, region, type ) %>% 
      summarise(price = round(mean(price, na.rm =T),2)) 

# change the name of sampleso in type
state_ts$type <- str_sub(state_ts$type, start = 6, end = -1L)
state_ts$type <- str_replace(state_ts$type, "SingleFamilyResidence", "House")


write_csv(state_ts, path = "../data/clean_state.csv")

