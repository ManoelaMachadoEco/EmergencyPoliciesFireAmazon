# Manoela 17/11/2020,
# then 11th Jan 2021! Adding 2020 data

## Script to join the complete dataset with all years , adding ImmedRegions! 

rm(list=ls())
require("tidyverse")
require("dplyr")
require("sf")
require("ggplot2")
require("lubridate")
require("plotly")
require("formattable")
require("stringr")

#### Load VIIRS data separate each year ####
## 2012 ####
VIIRS_LCC_LT_PA_StMun_ImmedReg_2012_sf <- read_sf('~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/VIIRS_LCC_LT_PA_StMun_ImmedReg_2012_sf.shp')
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_2012_sf)
unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_2012_sf$LCC_nam) # deforestation in 2012 and 2011

VIIRS_LCC_LT_PA_StMun_ImmedReg_2012_sf$LandCoverClass <- NA
VIIRS_LCC_LT_PA_StMun_ImmedReg_2012_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2012_sf$LCC_nam%in%c('Forest Loss in 2011')] <- 'Deforested PreviousYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2012_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2012_sf$LCC_nam%in%c('Forest Loss in 2012')] <- 'Deforested SameYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2012_sf$LandCoverClass[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2012_sf$LandCoverClass)] <- 
  VIIRS_LCC_LT_PA_StMun_ImmedReg_2012_sf$LCC_nam[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2012_sf$LandCoverClass)]

## 2013 ####
VIIRS_LCC_LT_PA_StMun_ImmedReg_2013_sf <- read_sf('~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/VIIRS_LCC_LT_PA_StMun_ImmedReg_2013_sf.shp')
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_2013_sf) 

VIIRS_LCC_LT_PA_StMun_ImmedReg_2013_sf$LandCoverClass <- NA
VIIRS_LCC_LT_PA_StMun_ImmedReg_2013_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2013_sf$LCC_nam%in%c('Forest Loss in 2012')] <- 'Deforested PreviousYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2013_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2013_sf$LCC_nam%in%c('Forest Loss in 2013')] <- 'Deforested SameYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2013_sf$LandCoverClass[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2013_sf$LandCoverClass)] <- 
  VIIRS_LCC_LT_PA_StMun_ImmedReg_2013_sf$LCC_nam[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2013_sf$LandCoverClass)]

## 2014 ####
VIIRS_LCC_LT_PA_StMun_ImmedReg_2014_sf <- read_sf('~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/VIIRS_LCC_LT_PA_StMun_ImmedReg_2014_sf.shp')
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_2014_sf) 

VIIRS_LCC_LT_PA_StMun_ImmedReg_2014_sf$LandCoverClass <- NA
VIIRS_LCC_LT_PA_StMun_ImmedReg_2014_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2014_sf$LCC_nam%in%c('Forest Loss in 2013')] <- 'Deforested PreviousYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2014_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2014_sf$LCC_nam%in%c('Forest Loss in 2014')] <- 'Deforested SameYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2014_sf$LandCoverClass[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2014_sf$LandCoverClass)] <- 
  VIIRS_LCC_LT_PA_StMun_ImmedReg_2014_sf$LCC_nam[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2014_sf$LandCoverClass)]

## 2015 ####
VIIRS_LCC_LT_PA_StMun_ImmedReg_2015_sf <- read_sf('~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/VIIRS_LCC_LT_PA_StMun_ImmedReg_2015_sf.shp')
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_2015_sf) 

VIIRS_LCC_LT_PA_StMun_ImmedReg_2015_sf$LandCoverClass <- NA
VIIRS_LCC_LT_PA_StMun_ImmedReg_2015_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2015_sf$LCC_nam%in%c('Forest Loss in 2014')] <- 'Deforested PreviousYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2015_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2015_sf$LCC_nam%in%c('Forest Loss in 2015')] <- 'Deforested SameYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2015_sf$LandCoverClass[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2015_sf$LandCoverClass)] <- 
  VIIRS_LCC_LT_PA_StMun_ImmedReg_2015_sf$LCC_nam[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2015_sf$LandCoverClass)]

## 2016 ####
VIIRS_LCC_LT_PA_StMun_ImmedReg_2016_sf <- read_sf('~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/VIIRS_LCC_LT_PA_StMun_ImmedReg_2016_sf.shp')
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_2016_sf) 

VIIRS_LCC_LT_PA_StMun_ImmedReg_2016_sf$LandCoverClass <- NA
VIIRS_LCC_LT_PA_StMun_ImmedReg_2016_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2016_sf$LCC_nam%in%c('Forest Loss in 2015')] <- 'Deforested PreviousYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2016_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2016_sf$LCC_nam%in%c('Forest Loss in 2016')] <- 'Deforested SameYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2016_sf$LandCoverClass[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2016_sf$LandCoverClass)] <- 
  VIIRS_LCC_LT_PA_StMun_ImmedReg_2016_sf$LCC_nam[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2016_sf$LandCoverClass)]

## 2017 ####
VIIRS_LCC_LT_PA_StMun_ImmedReg_2017_sf <- read_sf('~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/VIIRS_LCC_LT_PA_StMun_ImmedReg_2017_sf.shp')
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_2017_sf) 

VIIRS_LCC_LT_PA_StMun_ImmedReg_2017_sf$LandCoverClass <- NA
VIIRS_LCC_LT_PA_StMun_ImmedReg_2017_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2017_sf$LCC_nam%in%c('Forest Loss in 2016')] <- 'Deforested PreviousYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2017_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2017_sf$LCC_nam%in%c('Forest Loss in 2017')] <- 'Deforested SameYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2017_sf$LandCoverClass[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2017_sf$LandCoverClass)] <- 
  VIIRS_LCC_LT_PA_StMun_ImmedReg_2017_sf$LCC_nam[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2017_sf$LandCoverClass)]

## 2018 ####
VIIRS_LCC_LT_PA_StMun_ImmedReg_2018_sf <- read_sf('~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/VIIRS_LCC_LT_PA_StMun_ImmedReg_2018_sf.shp')
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_2018_sf) 

VIIRS_LCC_LT_PA_StMun_ImmedReg_2018_sf$LandCoverClass <- NA
VIIRS_LCC_LT_PA_StMun_ImmedReg_2018_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2018_sf$LCC_nam%in%c('Forest Loss in 2017')] <- 'Deforested PreviousYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2018_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2018_sf$LCC_nam%in%c('Forest Loss in 2018')] <- 'Deforested SameYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2018_sf$LandCoverClass[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2018_sf$LandCoverClass)] <- 
  VIIRS_LCC_LT_PA_StMun_ImmedReg_2018_sf$LCC_nam[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2018_sf$LandCoverClass)]

## 2019 ####
VIIRS_LCC_LT_PA_StMun_ImmedReg_2019_sf <- read_sf('~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/VIIRS_LCC_LT_PA_StMun_ImmedReg_2019_sf.shp')
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_2019_sf) 

VIIRS_LCC_LT_PA_StMun_ImmedReg_2019_sf$LandCoverClass <- NA
VIIRS_LCC_LT_PA_StMun_ImmedReg_2019_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2019_sf$LCC_nam%in%c('Forest Loss in 2018')] <- 'Deforested PreviousYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2019_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2019_sf$LCC_nam%in%c('Forest Loss in 2019')] <- 'Deforested SameYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2019_sf$LandCoverClass[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2019_sf$LandCoverClass)] <- 
VIIRS_LCC_LT_PA_StMun_ImmedReg_2019_sf$LCC_nam[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2019_sf$LandCoverClass)]
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_2019_sf)

## Adding 2020 data on 8th Jan 2021 ####
VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf <- read_sf('~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf.shp')
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf) 
VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf$LandCoverClass <- NA
VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf$LCC_nam%in%c('Forest Loss in 2019')] <- 'Deforested PreviousYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf$LandCoverClass[VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf$LCC_nam%in%c('Forest Loss in 2020')] <- 'Deforested SameYear'
VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf$LandCoverClass[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf$LandCoverClass)] <- 
  VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf$LCC_nam[is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf$LandCoverClass)]
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf)
## remove columns "DAYNIGH"
VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf <- VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf %>% select(-'DAYNIGH') 


#### rbind VIIRS all years #####
VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears <- rbind(VIIRS_LCC_LT_PA_StMun_ImmedReg_2012_sf,
                                                 VIIRS_LCC_LT_PA_StMun_ImmedReg_2013_sf,
                                                 VIIRS_LCC_LT_PA_StMun_ImmedReg_2014_sf,
                                                 VIIRS_LCC_LT_PA_StMun_ImmedReg_2015_sf,
                                                 VIIRS_LCC_LT_PA_StMun_ImmedReg_2016_sf,
                                                 VIIRS_LCC_LT_PA_StMun_ImmedReg_2017_sf, 
                                                 VIIRS_LCC_LT_PA_StMun_ImmedReg_2018_sf,
                                                 VIIRS_LCC_LT_PA_StMun_ImmedReg_2019_sf,
                                                 VIIRS_LCC_LT_PA_StMun_ImmedReg_2020_sf)

glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears)
dim(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears) # 4623207
unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$year)

## Organizing time variables  ####
## I need to create a variable with day month and 1000 as year
## try and get rid of the hour part of the date variables that Posix creates

# as.character
unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DATE)

# as.Date
VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DATEasDate <- as.Date(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DATE)
head(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears)

## Date with 1000 as the year
VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DateComMil <- paste0('1000', '-', format(as.Date(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DATEasDate), format="%m-%d"))
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears) # no NAs:
VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears %>% filter(is.na(DateComMil))

## Date with 1000 as.Date
VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DateComMilAsDate <- as.Date(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DateComMil, format = "%Y-%m-%d")
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears) ## all the NAs are of 9th February! 411

# rename vars 
VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears <- VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears %>% rename(DayMonthYear = DateComMil) 
VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears <- VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears %>% mutate(DayMonthYearTIME = DateComMilAsDate) # rename didn't want to work here
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears)

## same as before the correction of removing hours from time objects
class(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYear)
range(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYear)
tail(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYear,10)

# as date
class(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYearTIME)
summary(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears)

### As tibble ####
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears)
VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb <- as_tibble(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears) # %>% select(-geometry) # dont remove if i want to sf plot later
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb)

#### Rename var meso regions and immediate regions ####
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb)
VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb <- VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb %>% rename(NM_MesoRegion = nm_rgnt)
VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb <- VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb %>% rename(NM_ImmedRegion = nome_rg)

#### Create var Date to match CHIRPS dataset ####
VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb <- VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb %>% mutate(Date = paste0(year,monthNo,day)) 
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb)

# basic checks
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb) # 4623228 without 2020 data
unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb$LandCoverClass)
# NAs in LCC
length(which(is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb$LandCoverClass))) # 497 (497/8=62 NAs per year, not too bad) --> this was before adding 2020
# NAs in States
length(which(is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb$NM_ESTA))) # 207
# NAs in Meso Regions
length(which(is.na(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb$NM_MesoRegion))) # 213

# VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb <- VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb %>% filter(!is.na(NM_ESTA))
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb)

## save ####
class(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb)
save(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb,
      file='~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb.RData')


###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***#####
rm(list=ls()) 
load('~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb.RData')
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb)


####  add immediate regions chirps #######
## adding 2020 on 8th of Jan 2021 --> problem is that CHIRPS hasn't updated their data with December 2020
## so currently (on 11th Jan 2021, this dataset does NOT have precipitation data for december 2021)
Year2020_mean_daily_precip_ImmedRegion <- read_csv('~/Oxford/FireAmazon2019_GCRF/DataFromGEE/Phase6_MBcoll5/CHIRPS_mean_daily_ImmedRegions_allYears/Year2020_mean_daily_precip_ImmedReg.csv')
head(Year2020_mean_daily_precip_ImmedRegion)
range(Year2020_mean_daily_precip_ImmedRegion$`system:index`)
Year2019_mean_daily_precip_ImmedRegion <- read_csv('~/Oxford/FireAmazon2019_GCRF/DataFromGEE/Phase6_MBcoll5/CHIRPS_mean_daily_ImmedRegions_allYears/Year2019_mean_daily_precip_ImmedReg.csv')
head(Year2019_mean_daily_precip_ImmedRegion)
range(Year2019_mean_daily_precip_ImmedRegion$`system:index`)
Year2018_mean_daily_precip_ImmedRegion <- read_csv('~/Oxford/FireAmazon2019_GCRF/DataFromGEE/Phase6_MBcoll5/CHIRPS_mean_daily_ImmedRegions_allYears/Year2018_mean_daily_precip_ImmedReg.csv')
head(Year2018_mean_daily_precip_ImmedRegion)
range(Year2018_mean_daily_precip_ImmedRegion$`system:index`)
Year2017_mean_daily_precip_ImmedRegion <- read_csv('~/Oxford/FireAmazon2019_GCRF/DataFromGEE/Phase6_MBcoll5/CHIRPS_mean_daily_ImmedRegions_allYears/Year2017_mean_daily_precip_ImmedReg.csv')
head(Year2017_mean_daily_precip_ImmedRegion)
range(Year2017_mean_daily_precip_ImmedRegion$`system:index`)
Year2016_mean_daily_precip_ImmedRegion <- read_csv('~/Oxford/FireAmazon2019_GCRF/DataFromGEE/Phase6_MBcoll5/CHIRPS_mean_daily_ImmedRegions_allYears/Year2016_mean_daily_precip_ImmedReg.csv')
head(Year2016_mean_daily_precip_ImmedRegion)
range(Year2016_mean_daily_precip_ImmedRegion$`system:index`)
Year2015_mean_daily_precip_ImmedRegion <- read_csv('~/Oxford/FireAmazon2019_GCRF/DataFromGEE/Phase6_MBcoll5/CHIRPS_mean_daily_ImmedRegions_allYears/Year2015_mean_daily_precip_ImmedReg.csv')
head(Year2015_mean_daily_precip_ImmedRegion)
range(Year2015_mean_daily_precip_ImmedRegion$`system:index`)
Year2014_mean_daily_precip_ImmedRegion <- read_csv('~/Oxford/FireAmazon2019_GCRF/DataFromGEE/Phase6_MBcoll5/CHIRPS_mean_daily_ImmedRegions_allYears/Year2014_mean_daily_precip_ImmedReg.csv')
head(Year2014_mean_daily_precip_ImmedRegion)
range(Year2014_mean_daily_precip_ImmedRegion$`system:index`)
Year2013_mean_daily_precip_ImmedRegion <- read_csv('~/Oxford/FireAmazon2019_GCRF/DataFromGEE/Phase6_MBcoll5/CHIRPS_mean_daily_ImmedRegions_allYears/Year2013_mean_daily_precip_ImmedReg.csv')
head(Year2013_mean_daily_precip_ImmedRegion)
range(Year2013_mean_daily_precip_ImmedRegion$`system:index`)
Year2012_mean_daily_precip_ImmedRegion <- read_csv('~/Oxford/FireAmazon2019_GCRF/DataFromGEE/Phase6_MBcoll5/CHIRPS_mean_daily_ImmedRegions_allYears/Year2012_mean_daily_precip_ImmedReg.csv')
head(Year2012_mean_daily_precip_ImmedRegion)
range(Year2012_mean_daily_precip_ImmedRegion$`system:index`)

## rbind all years meso regions ####
Mean_daily_precip_ImmedRegions_AllYears <- rbind(Year2012_mean_daily_precip_ImmedRegion, Year2013_mean_daily_precip_ImmedRegion,
                                                 Year2014_mean_daily_precip_ImmedRegion, Year2015_mean_daily_precip_ImmedRegion, 
                                                 Year2016_mean_daily_precip_ImmedRegion, Year2017_mean_daily_precip_ImmedRegion, 
                                                 Year2018_mean_daily_precip_ImmedRegion, Year2019_mean_daily_precip_ImmedRegion,
                                                 Year2020_mean_daily_precip_ImmedRegion)
summary(Mean_daily_precip_ImmedRegions_AllYears)
glimpse(Mean_daily_precip_ImmedRegions_AllYears)
range(Mean_daily_precip_ImmedRegions_AllYears$`system:index`) ##  Jan 2012 until Nov 2020 

## rename var mean
Mean_daily_precip_ImmedRegions_AllYears <- Mean_daily_precip_ImmedRegions_AllYears %>% 
                                           rename(PrecipImmedRegion = mean) %>% 
                                           rename(ImmedRegion = nome_rgi)
head(Mean_daily_precip_ImmedRegions_AllYears)

## remove separate years ####
rm(Year2012_mean_daily_precip_ImmedRegion, Year2013_mean_daily_precip_ImmedRegion,
   Year2014_mean_daily_precip_ImmedRegion, Year2015_mean_daily_precip_ImmedRegion, 
   Year2016_mean_daily_precip_ImmedRegion, Year2017_mean_daily_precip_ImmedRegion, 
   Year2018_mean_daily_precip_ImmedRegion, Year2019_mean_daily_precip_ImmedRegion)


## Correct var 'date' system ####
head(Mean_daily_precip_ImmedRegions_AllYears)
Mean_daily_precip_ImmedRegions_AllYears <- Mean_daily_precip_ImmedRegions_AllYears %>% 
  separate(`system:index`, sep = '_', into=c('Date','del')) %>% 
  select(-c(del,.geo))
head(Mean_daily_precip_ImmedRegions_AllYears,11)
unique(Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)


## Correct acentos Immediate Regions ####
sort(unique(Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion))
head(Mean_daily_precip_ImmedRegions_AllYears,29)
## grep
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion <- NA
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('gua Boa', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Agua Boa'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Aragua', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Araguaina'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Araguatins', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Araguatins' # to correct above
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('ail', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Acailandia'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Bel', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Belem'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Bragan', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Braganca'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Brasil', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Brasileia'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Camet', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Cameta'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Capit', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Capitao Poco'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Caracara', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Caracarai'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('ceres', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Caceres'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Eirunep', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Eirunepe'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Guara', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Guarai' ## then peixoto...
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Ji-Paran', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Ji-Parana'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Ju', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Juina'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Juara', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Juara' # to correct above
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('brea', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Labrea'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Macap', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Macapa'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Manicor', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Manicore'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Marab', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Maraba'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Oriximin', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Oriximina'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('so do Tocantins', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Paraiso do Tocantins'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Peixoto de Azevedo - Guarant', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Peixoto de Azevedo - Guaranta do Norte'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Reden', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Redencao'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Rorain', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Rorainopolis'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Santa In', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Santa Ines'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Santar', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Santarem'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('o Gabriel da Cachoeira', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Sao Gabriel da Cachoeira'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('o Lu', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Sao Luis'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Tangar', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Tangara da Serra'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Tarauac', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Tarauaca'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Tef', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Tefe'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('lix do Xingu', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Tucuma - Sao Felix do Xingu'
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[grep('Tucuru', Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion)] <- 'Tucurui'
## checks:
Mean_daily_precip_ImmedRegions_AllYears %>% filter(NM_ImmedRegion == 'Tocantinopolis')
unique(Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion)
## names that are alright
Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion[is.na(Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion)] <- Mean_daily_precip_ImmedRegions_AllYears$ImmedRegion[is.na(Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion)]

Mean_daily_precip_ImmedRegions_AllYears[1:85,4]
sort(unique(Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion))

## create variable state for each meso region
Mean_daily_precip_ImmedRegions_AllYears$NM_ESTADO <- NA
Mean_daily_precip_ImmedRegions_AllYears$NM_ESTADO[
  Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion%in%c('Cruzeiro do Sul','Tarauaca','Sena Madureira',
                                                              'Rio Branco','Brasileia')] <- 'ACRE'
Mean_daily_precip_ImmedRegions_AllYears$NM_ESTADO[
  Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion%in%c('Laranjal do Jari','Macapa','Oiapoque',
                                                              'Porto Grande')] <- 'AMAPA'
Mean_daily_precip_ImmedRegions_AllYears$NM_ESTADO[
  Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion%in%c('Sao Gabriel da Cachoeira','Tefe','Tabatinga',
                                                              'Coari','Manacapuru','Labrea', 'Manaus','Parintins',
                                                              'Itacoatiara','Manicore','Eirunepe')] <- 'AMAZONAS'
Mean_daily_precip_ImmedRegions_AllYears$NM_ESTADO[
  Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion%in%c('Governador Nunes Freire', 'Cururupu','Pinheiro',
                                                              'Viana','Itapecuru Mirim','Sao Luis','Barreirinhas',
                                                              'Tutoia - Araioses','Chapadinha','Imperatriz','Santa Ines',
                                                              'Bacabal','Pedreiras','Barra do Corda','Acailandia')] <- 'MARANHAO'
Mean_daily_precip_ImmedRegions_AllYears$NM_ESTADO[
  Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion%in%c('Juina','Alta Floresta','Peixoto de Azevedo - Guaranta do Norte',
                                                              'Confresa - Vila Rica','Sinop','Agua Boa','Sorriso','Diamantino',
                                                              'Tangara da Serra','Mirassol D\'oeste','Sinop','Cuiaba','Caceres',
                                                              'Rondonopolis','Jaciara','Primavera do Leste','Barra do Garcas',
                                                              'Pontes e Lacerda - Comodoro','Juara')] <- 'MATO GROSSO'
Mean_daily_precip_ImmedRegions_AllYears$NM_ESTADO[
  Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion%in%c('Altamira','Santarem','Breves','Belem','Castanhal',
                                                              'Maraba','Redencao','Oriximina','Almeirim - Porto de Moz',
                                                              'Abaetetuba','Soure-Salvaterra','Abaetuba','Capanema',
                                                              'Braganca','Cameta','Capitao Poco','Paragominas','Tucurui',
                                                              'Parauapebas','Xinguara','Tucuma - Sao Felix do Xingu','Itaituba')] <- 'PARA'
Mean_daily_precip_ImmedRegions_AllYears$NM_ESTADO[
  Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion%in%c('Porto Velho','Ariquemes','Ji-Parana','Jaru','Cacoal','Vilhena')] <- 'RONDONIA'
Mean_daily_precip_ImmedRegions_AllYears$NM_ESTADO[
  Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion%in%c('Pacaraima','Boa Vista','Rorainopolis','Caracarai')] <- 'RORAIMA'
Mean_daily_precip_ImmedRegions_AllYears$NM_ESTADO[
  Mean_daily_precip_ImmedRegions_AllYears$NM_ImmedRegion%in%c('Araguatins','Araguaina','Palmas','Guarai','Paraiso do Tocantins',
                                                              'Miracema do Tocantins','Colinas do Tocantins')] <- 'TOCANTINS'

Mean_daily_precip_ImmedRegions_AllYears %>% filter(is.na(NM_ESTADO))
Mean_daily_precip_ImmedRegions_AllYears %>% filter(is.na(NM_ImmedRegion))

### para conferir os nomes IBGE ####
# IBGE_ImmedRegions <- read_csv('~/Oxford/FireAmazon2019_GCRF/DataFromGEE/Phase6_MBcoll5/CHIRPS_mean_daily_ImmedRegions_allYears/regioes_geograficas_composicao_por_municipios_2017_20180911.csv')
# head(IBGE_ImmedRegions)
# sort(unique(IBGE_ImmedRegions$nome_rgint))
# sort(unique(Mean_daily_precip_States_AllYears$NM_ESTADO))

####
glimpse(Mean_daily_precip_ImmedRegions_AllYears)
Mean_daily_precip_ImmedRegions_AllYears <- Mean_daily_precip_ImmedRegions_AllYears %>% select(Date, PrecipImmedRegion, NM_ImmedRegion, NM_ESTADO)
head(Mean_daily_precip_ImmedRegions_AllYears)


## Join VIIRS dataset with CHIRPS MESO REGIONS by 'Date' and 'NM_ImmedRegion' #####
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb)
glimpse(Mean_daily_precip_ImmedRegions_AllYears)

VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb <- VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears_tb %>% 
                                                     left_join(Mean_daily_precip_ImmedRegions_AllYears, by = c("NM_ImmedRegion", "Date"))

glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb)

## is na
VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb %>% filter(is.na(NM_ImmedRegion)) %>% glimpse # 256 NAs



## save ####
save(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb, file='~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb.RData')


###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###***###
###*
###*## old:####
###*
###*# #### Create a variable Drought Year #####
# VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DroughtYear <- NA
# VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DroughtYear[VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$year
#                                                   %in%c('2012','2013','2014','2016','2017','2018','2019')] <- 'NonDrought'
# VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DroughtYear[VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$year
#                                                   %in%c('2015')] <- 'Drought'
# VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DroughtYear <- as.factor(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DroughtYear)
# glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears)

# 
# #### Create variable 'Day_Month'####
# ## fictional year for analysis bc 'acq_date' carries the time info, i need all years in the same time frame for GAMs 
# # as character
# VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears <- VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears %>% mutate(DayMonthYearCh = paste0(day,'/',monthNo,'/',year)) 
# class(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYearCh)
# 
# # as Date
# VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYearChAsDate <- as.Date(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYearCh, format = "%d/%m/%Y")
# class(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYearChAsDate)
# 
# # as Numeric
# VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYearChAsDateAsNumeric <- as.numeric(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYearChAsDate)
# class(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYearChAsDateAsNumeric)
# 
# ##
# as.POSIXct(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYearCh, format = "%d/%m/%Y",tz = "GMT")
# 
# VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMontYearChPosix <- as.POSIXct(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYearCh, format = "%d/%m/%Y",tz = "GMT")
# class(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMontYearChPosix)
# 
# VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMontYearChPosixNum <- as.numeric(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMontYearChPosix)*1000 # to mimic what GEE did 
# head(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMontYearChPosixNum)
# 
# VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMontYearChPosixNumMilli <- ymd("1970-01-01")+seconds((VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMontYearChPosixNum)/(1000)) # to undo what GEE would've done
# head(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMontYearChPosixNumMilli)
# range(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMontYearChPosixNumMilli)
# 
# VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYear <- paste0(format(as.Date(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMontYearChPosixNumMilli), format="%d/%m"), '/', '1000')
# VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYearTIME <- as.POSIXct(VIIRS_LCC_LT_PA_StMun_ImmedReg_allYears$DayMonthYear, format = "%d/%m/%Y",tz = "GMT")