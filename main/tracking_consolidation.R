library(data.table)
library(ggplot2)
library(readr)
library(magrittr)
library(lest)
library(forcats)
library(arrow)

tracking <- (fread('data/tracking_week_1.csv')) %>% 
            rbind(fread('data/tracking_week_2.csv')) %>% 
            rbind(fread('data/tracking_week_3.csv')) %>% 
            rbind(fread('data/tracking_week_4.csv')) %>% 
            rbind(fread('data/tracking_week_5.csv')) %>% 
            rbind(fread('data/tracking_week_6.csv')) %>% 
            rbind(fread('data/tracking_week_7.csv')) %>% 
            rbind(fread('data/tracking_week_8.csv')) %>% 
            rbind(fread('data/tracking_week_9.csv'))


write_parquet(tracking,'data/tracking.parquet')
  