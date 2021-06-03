### Manoela 
# 22 March 2021, yes, unbelievable! 
# same analysis, adding se = TRUE in the predict function


## Deforestation FIRE
## Script to run analysis of fire count over time --> daily
#
rm(list=ls())
## load packages  #####
require("tidyverse")
require("dplyr")
require("sf")
require("ggplot2")
require("lubridate")
require("plotly")
require("formattable")
require("nlme")
require("mgcv")
require("tidymv")
require('TTR')
require('tstools')
require('forecast')
require('tseries')
require('zoo')
require('TSstudio')
require('scales')
require('grid')
require('gridExtra')
require('ggplotify')
require("ggpubr") # devtools::install_github("kassambara/ggpubr")

## Data LCC separate ####
ColGraphClassesYears <- c("green4", "#FF7F00","#6A3D9A", "gold1","skyblue2","#E31A1C","#f531c1", "black", 'red', 'blue')

## Deforestation ####
load('~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/Dats_FireCount_forAnalysis/datDeforestation_FireCountDailyImmedRegion.RData')
head(datDeforestation_FireCountDailyImmedRegion) # 
unique(datDeforestation_FireCountDailyImmedRegion$year)

# remove 2021 (only Jan 1st)
datDeforestation_FireCountDailyImmedRegion <- datDeforestation_FireCountDailyImmedRegion %>% filter(year != "2021")

## as numeric DayMonthYearTIME
datDeforestation_FireCountDailyImmedRegion$DayMonthYearTIMEnum <- as.numeric(datDeforestation_FireCountDailyImmedRegion$DayMonthYearTIME)

## Remove immediate regions ####
unique(datDeforestation_FireCountDailyImmedRegion$NM_ImmedRegion)
# datDeforestation_FireCountDailyImmedRegion %>% filter(NM_ImmedRegion %in% c('Caceres')) %>% dim 
datDeforestation_FireCountDailyImmedRegion <- datDeforestation_FireCountDailyImmedRegion %>% 
  filter(!NM_ImmedRegion %in% c('Barreirinhas','Itapecuru Mirim','Pedreiras','Barra do Corda','Imperatriz','Araguatins',
                                'Araguaina','Colinas do Tocantins','Guarai','Miracema do Tocantins','Paraiso do Tocantins',
                                'Primavera do Leste','Sorriso','Diamantino','Tangara da Serra','Caceres'))
length(unique(datDeforestation_FireCountDailyImmedRegion$NM_ImmedRegion)) # 68

glimpse(datDeforestation_FireCountDailyImmedRegion)
## check plot per immediate region 
gDeforestationFireAllImmedRegionAllYears <- ggplot(datDeforestation_FireCountDailyImmedRegion) +
  geom_point(aes(y= DailyFireCountImmedRegion, x= DayMonthYearTIME, color= year), alpha=0.6) +
  geom_smooth(aes(y= DailyFireCountImmedRegion, x= DayMonthYearTIME, color=year), span=0.6) +
  facet_wrap(~NM_ImmedRegion, scale='free_y') +
  labs(title='Fire count in Deforestation') + theme_bw() +
  scale_color_manual(values=ColGraphClassesYears) +
  geom_segment(aes(x = as.Date('1000-08-24'), y = 0, xend = as.Date('1000-08-24'), yend = 500), 
               colour = "darkgrey", linetype='dashed') +
  geom_segment(aes(x = as.Date('1000-10-24'), y = 0, xend = as.Date('1000-10-24'), yend = 500), 
               colour = "darkgrey", linetype='dashed') 
#gDeforestationFireAllImmedRegionAllYears


## Creating full date posix  ####
head(datDeforestation_FireCountDailyImmedRegion)

# Full date as character
datDeforestation_FireCountDailyImmedRegion <- datDeforestation_FireCountDailyImmedRegion %>% 
  separate(DayMonthYearTIME, into = c('del', 'month', 'day'), remove = FALSE) %>% select(-del) %>% 
  mutate(MonthYear = paste0(month, '-', year))  %>% 
  mutate(FullDate = paste0(year, '-', month, '-', day)) 

summary(datDeforestation_FireCountDailyImmedRegion)
datDeforestation_FireCountDailyImmedRegion %>% filter(is.na(day)) ## probably 29 Feb?
#
datDeforestation_FireCountDailyImmedRegion$FullDateAsDate <- as.Date(datDeforestation_FireCountDailyImmedRegion$FullDate, format = "%Y-%m-%d")
range(datDeforestation_FireCountDailyImmedRegion$FullDateAsDate, na.rm=T)
summary(datDeforestation_FireCountDailyImmedRegion) # 20/Jan/2012 until 31/Dec/2020
## 

datDeforestation_FireCountDailyImmedRegion <- datDeforestation_FireCountDailyImmedRegion %>% arrange(FullDateAsDate)
head(datDeforestation_FireCountDailyImmedRegion)
####

### create variable days 365 for Predicts #####  
## create DayMonthYearTIMEnum that starts in 01 01 1000 and ends on 31 12 1000 --> calculate this and set this var instead of stretching the existing dates
### 
df_for_dates <- NULL
df_for_dates$Date <- seq(as.Date("1000-01-01"), as.Date("1000-12-31"), "days")
df_for_dates$DateAsDate <- as.Date(df_for_dates$Date)
df_for_dates$DateAsDateNum <- as.numeric(df_for_dates$DateAsDate)
df_for_dates <- as.data.frame(df_for_dates)
head(df_for_dates)


## Immediate Regions separately ####
mr <- sort(unique(datDeforestation_FireCountDailyImmedRegion$NM_ImmedRegion))
mr

################ Analysis #######################

## Part 1: Model daily fire count across a year in each meso region using data from 2012 to 2018 as repetition, considering daily precipitation of each meso region
## Part 2 for 2019: To use real observed precipitation of 2019 to predict ‘a year’ from this model, for each immediate region.
## Part 2 for 2020: To use real observed precipitation of 2020 to predict ‘a year’ from this model, for each immediate region.
## Part 3 (new for daily diff): calculate observed minus expected fire daily


# dat without 2019 and 2020 for models ####
datDeforestation_FireCountDailyImmedRegion_No2019 <- datDeforestation_FireCountDailyImmedRegion %>% filter(year != '2019' & year != '2020')
unique(datDeforestation_FireCountDailyImmedRegion_No2019$year)

## dat only 2019 ####
datDeforestation_FireCountDailyImmedRegion_Only2019 <- datDeforestation_FireCountDailyImmedRegion %>% filter(year == '2019')
unique(datDeforestation_FireCountDailyImmedRegion_Only2019$year)

## dat only 2020 ####
datDeforestation_FireCountDailyImmedRegion_Only2020 <- datDeforestation_FireCountDailyImmedRegion %>% filter(year == '2020')
unique(datDeforestation_FireCountDailyImmedRegion_Only2020$year)


## PART 1 ####

# to run this smoothly:
# remove from mr problematic regions to run separately while correcting the problems
# so remove: 16, 23, 33, 34, 49, 50, 60, 66 --> problems with MA. 
# and: 24, 58
# Porto Grande has 0 data in 2020. 

mrSub <- mr[-c(16,23,33,34,49,50,60,66,24,58)] ## this mrSub runs beautifully 
bli <- mr[c(16,23,33,34,49,50,60,66,24,58)] # encrenqueiros 


i<-10
mr[i]
bli[i]
# mrSub[i]
#df_Daily_Deforestation_FIRE <- NULL
#for(i in 1:length(mrSub)) {  # 
  ## Dat NO 2019 mr[i]
  datDeforestationMesoRegion_No2019 <- datDeforestation_FireCountDailyImmedRegion_No2019 %>% filter(NM_ImmedRegion == bli[i])
  ## Correcting crazy weird curve in early 2019 and 2020:
  # Problem with Eirunepe: delete data before April in 2018 in Eirunepe
  # datDeforestationMesoRegion_No2019 <- datDeforestationMesoRegion_No2019 %>% filter(month != '02')
  # Problem with Sena Madureira: delete data before May
  datDeforestationMesoRegion_No2019 <- datDeforestationMesoRegion_No2019 %>% filter(!month %in% c('02','03','04'))
  #
  gdatDeforestationMesoRegion_No2019 <- ggplot(datDeforestationMesoRegion_No2019, aes(y=DailyFireCountImmedRegion, x=DayMonthYearTIME, color=year)) + 
    geom_point() + geom_smooth() + scale_color_manual(values=ColGraphClassesYears) + facet_wrap(~NM_ImmedRegion)
  # MODEL ## ModDeforestation_MR : from 2012 to 2018 
  ModDeforestation_MR <- gamm(DailyFireCountImmedRegion ~ s(DayMonthYearTIMEnum, k=8) + s(PrecipImmedRegion), 
                              random = list(year=~1),
                              family = "poisson",
                              data = datDeforestationMesoRegion_No2019)
  ## PREDICT from this model (2012-2018) ####
  newdatModDeforestation_MR_MeanPrecip <- expand.grid(DayMonthYearTIMEnum = df_for_dates$DateAsDateNum,
                                                      PrecipImmedRegion = mean(datDeforestationMesoRegion_No2019$PrecipImmedRegion))
  newdatModDeforestation_MR_MeanPrecip$DayMonthYearTIME <- as.Date(newdatModDeforestation_MR_MeanPrecip$DayMonthYearTIMEnum, format="%Y-%m-%d")
  newdatModDeforestation_MR_MeanPrecip$predsModDeforestation_MR <- predict(ModDeforestation_MR$gam, newdatModDeforestation_MR_MeanPrecip,level=0, type='response') 
  head(newdatModDeforestation_MR_MeanPrecip)
  ## new: adding SE to calculate CI upper and lower
  newdatModDeforestation_MR_MeanPrecip$predsModDeforestation_MR_se <- predict(ModDeforestation_MR$gam, newdatModDeforestation_MR_MeanPrecip, level=0, type='response', se.fit=TRUE)$se.fit 
  head(newdatModDeforestation_MR_MeanPrecip)
  # CI: 
  newdatModDeforestation_MR_MeanPrecip <- newdatModDeforestation_MR_MeanPrecip %>% 
    mutate(upper_predsModDeforestation_MR = predsModDeforestation_MR + (2 * predsModDeforestation_MR_se)) %>% # upr <- p$fit + (2 * p$se.fit) & lwr <- p$fit - (2 * p$se.fit)
    mutate(lower_predsModDeforestation_MR = predsModDeforestation_MR - (2 * predsModDeforestation_MR_se))
  head(newdatModDeforestation_MR_MeanPrecip)
  ## PLOT predict from this model
  gPredsModDeforestation_MR_2012_2018 <- ggplot(datDeforestationMesoRegion_No2019) +
    geom_point(aes(y=DailyFireCountImmedRegion, x=DayMonthYearTIME), alpha=0.3) + # raw data 2012 to 2018
    geom_line(data= newdatModDeforestation_MR_MeanPrecip, aes(y=predsModDeforestation_MR, x=DayMonthYearTIME), colour='black', size=1.5) +
    # CI
    geom_ribbon(data= newdatModDeforestation_MR_MeanPrecip, aes(ymax=upper_predsModDeforestation_MR, ymin=lower_predsModDeforestation_MR, x=DayMonthYearTIME), fill='purple', alpha=0.5) +
    labs(title = paste0('Deforestation fire in ',bli[i],'\nBlack: raw data and predicted 2012-2018')) + theme(plot.title = element_text(vjust = -20))
  gPredsModDeforestation_MR_2012_2018
  ##
  ## PART 2 for 2019 ####
  ## Dat ONLY 2019 mr[1]
  datDeforestationMesoRegion_Only2019 <- datDeforestation_FireCountDailyImmedRegion_Only2019 %>% filter(NM_ImmedRegion == bli[i])
  ## precipitation in mr[i] only in 2019 ==   ## should have 365 observations, usually has less
  Precip2019 <- as_tibble(datDeforestationMesoRegion_Only2019) %>% select(DayMonthYearTIME, PrecipImmedRegion) %>% 
    mutate(Date = DayMonthYearTIME)
  ## get moving average
  Precip2019 %>% filter(is.na(PrecipImmedRegion)) ## no NAs
  Precip2019$MA10 <- runMean(Precip2019$PrecipImmedRegion, n = 10, cumulative = FALSE) ## 10
  Precip2019 %>% filter(is.na(MA10)) ## NAs in the first 10 obs
  # rename and select
  Precip2019 <- Precip2019 %>% select(-PrecipImmedRegion) %>% rename(PrecipImmedRegion = MA10) 
  ## PREDICT with real 2019 precipitation data 
  newdatModDeforestation_MR_Precip2019 <- as_tibble(newdatModDeforestation_MR_MeanPrecip) %>% 
    select(-c(PrecipImmedRegion, predsModDeforestation_MR, predsModDeforestation_MR_se, upper_predsModDeforestation_MR, lower_predsModDeforestation_MR))
  # redundant?:
  #newdatModDeforestation_MR_Precip2019$DayMonthYearTIME <- as.Date(newdatModDeforestation_MR_Precip2019$DayMonthYearTIMEnum, format="%Y-%m-%d")
  newdatModDeforestation_MR_Precip2019 <- newdatModDeforestation_MR_Precip2019 %>% 
    mutate(Date = DayMonthYearTIME)
  ## join "newdatModDeforestation_MR_Precip2019" from model with real precipitation data of 2019 
  ## this left join of a dat with 365 obs with another dat with less obs creates the NAs I need so I can fill them with values
  newdatModDeforestation_MR_Precip2019 <- left_join(newdatModDeforestation_MR_Precip2019, Precip2019, by = 'Date') %>% select(-DayMonthYearTIME.y) %>% 
    rename(DayMonthYearTIME=DayMonthYearTIME.x)
  ##
  ## Dealing with NAs in the precip data (NAs in the chirps dataset)
  newdatModDeforestation_MR_Precip2019 %>% filter(is.na(PrecipImmedRegion))
  newdatModDeforestation_MR_Precip2019$PrecipImmedRegion <- round(na.approx(newdatModDeforestation_MR_Precip2019$PrecipImmedRegion, rule=2),3)  
  head(newdatModDeforestation_MR_Precip2019)
  newdatModDeforestation_MR_Precip2019 %>% filter(is.na(PrecipImmedRegion))
  ## Predict: 
  newdatModDeforestation_MR_Precip2019$predsModDeforestation_MR <- predict(ModDeforestation_MR$gam, newdatModDeforestation_MR_Precip2019, level=0, type='response') 
  head(newdatModDeforestation_MR_Precip2019)
  ## new: adding SE to calculate CI upper and lower
  newdatModDeforestation_MR_Precip2019$predsModDeforestation_MR_se <- predict(ModDeforestation_MR$gam, newdatModDeforestation_MR_Precip2019, level=0, type='response', se.fit=TRUE)$se.fit 
  head(newdatModDeforestation_MR_Precip2019)
  # CI: 
  newdatModDeforestation_MR_Precip2019 <- newdatModDeforestation_MR_Precip2019 %>% 
    mutate(upper_predsModDeforestation_MR = predsModDeforestation_MR + (2 * predsModDeforestation_MR_se)) %>% # upr <- p$fit + (2 * p$se.fit) & lwr <- p$fit - (2 * p$se.fit)
    mutate(lower_predsModDeforestation_MR = predsModDeforestation_MR - (2 * predsModDeforestation_MR_se))
  head(newdatModDeforestation_MR_Precip2019)
  #
  ## PLOT predict from this model predicted for 2019 (moving average precipitation)
  gPredsModDeforestation_MR_Precip2019 <- ggplot(newdatModDeforestation_MR_Precip2019) + 
    geom_point(data= datDeforestationMesoRegion_No2019, aes(y=DailyFireCountImmedRegion, x=DayMonthYearTIME), alpha=0.3) + # raw data 2012 to 2018
    geom_line(aes(y=predsModDeforestation_MR, x=DayMonthYearTIME), colour='blue', size=2) +
    # CI
    geom_ribbon(aes(ymax=upper_predsModDeforestation_MR, ymin=lower_predsModDeforestation_MR, x=DayMonthYearTIME), fill='purple', alpha=0.5) +
    labs(title = paste0('Deforestation fire in ',bli[i],'\nBlue: predicted from model 2012-2018 using real precipitation of 2019')) + 
    theme(plot.title = element_text(vjust = -20))
  #gPredsModDeforestation_MR_Precip2019 ## should have no interruptions in the blue line
  ##
  ##
  gAllPartsAllYear <- ggplot(datDeforestationMesoRegion_Only2019) + 
    geom_point(data=datDeforestationMesoRegion_No2019, aes(y=DailyFireCountImmedRegion, x=DayMonthYearTIME), alpha=0.2) + # raw data 2012-2018
    geom_line(data= newdatModDeforestation_MR_MeanPrecip, aes(y=predsModDeforestation_MR, x=DayMonthYearTIME), colour='black', size=1.5) + # predicted 2012-2018
    geom_ribbon(data=newdatModDeforestation_MR_MeanPrecip, aes(ymin=lower_predsModDeforestation_MR, ymax=upper_predsModDeforestation_MR, x=DayMonthYearTIME), fill='purple', alpha=0.5) +
    geom_point(aes(y=DailyFireCountImmedRegion, x=DayMonthYearTIME), alpha=1, colour='firebrick') + # raw data 2019
    geom_line(aes(y=DailyFireCountImmedRegion, x=DayMonthYearTIME), alpha=0.7, colour='firebrick') + # raw data 2019
    geom_line(data=newdatModDeforestation_MR_Precip2019, aes(y=predsModDeforestation_MR, x=DayMonthYearTIME), colour='blue', size=2) +
    geom_ribbon(data=newdatModDeforestation_MR_Precip2019, aes(ymin=lower_predsModDeforestation_MR, ymax=upper_predsModDeforestation_MR, x=DayMonthYearTIME), fill='purple', alpha=0.5) + 
    # GLO period
    geom_segment(aes(x = as.Date('1000-08-23'), y = 0, xend = as.Date('1000-08-23'), yend = 200), colour = "black", linetype='dashed') +
    geom_segment(aes(x = as.Date('1000-10-28'), y = 0, xend = as.Date('1000-10-28'), yend = 200), colour = "black", linetype='dashed') +
    labs(title = paste0('Deforestation fire in ',bli[i],'\nBlack: raw data and predicted 2012-2018\nBlue: predicted from model 2012-2018 using real precipitation of 2019\nRed: raw data 2019')) + 
    theme(plot.title = element_text(vjust = -20)) 
  #gAllPartsAllYear
  ##
  ## PART 2 for 2020 ####
  ## Dat ONLY 2020 mr[1] 
  datDeforestationMesoRegion_Only2020 <- datDeforestation_FireCountDailyImmedRegion_Only2020 %>% filter(NM_ImmedRegion == bli[i])
  ## precipitation in mr[i] only in 2020 ==   ## should have 365 observations, but CHIRPS hasn't updated December 2020 yet (11th Jan today)
  Precip2020 <- as_tibble(datDeforestationMesoRegion_Only2020) %>% select(DayMonthYearTIME, PrecipImmedRegion) %>% 
    mutate(Date = DayMonthYearTIME)
  ## get moving average
  Precip2020 %>% filter(is.na(PrecipImmedRegion)) ## SHOULD HAVE no NAs! FOR NOW I'M DELETING THE NAS
  ## TEMPORARY STUFF ! UNTIL chirps IS UPDATED WITH THE WHOLE OF 2020 !!!!!  
  Precip2020 <- Precip2020 %>% filter(!is.na(PrecipImmedRegion))
  # 
  Precip2020$MA10 <- runMean(Precip2020$PrecipImmedRegion, n = 10, cumulative = FALSE) ## this gives a problem in some of them, need to use a smaller number
  Precip2020 %>% filter(is.na(MA10)) ## NAs in the first 10 obs
  # rename and select
  Precip2020 <- Precip2020 %>% select(-PrecipImmedRegion) %>% rename(PrecipImmedRegion = MA10) 
  ## PREDICT with real 2020 precipitation data 
  newdatModDeforestation_MR_Precip2020 <- as_tibble(newdatModDeforestation_MR_MeanPrecip) %>% 
    select(-c(PrecipImmedRegion, predsModDeforestation_MR, predsModDeforestation_MR_se, upper_predsModDeforestation_MR, lower_predsModDeforestation_MR)) %>% 
    mutate(Date = DayMonthYearTIME)
  ## join "newdatModDeforestation_MR_Precip2020" from model with real precipitation data of 2020 
  ## this left join of a dat with 365 obs with another dat with less obs creates the NAs I need so I can fill them with values
  newdatModDeforestation_MR_Precip2020 <- left_join(newdatModDeforestation_MR_Precip2020, Precip2020, by = 'Date') %>% select(-DayMonthYearTIME.y) %>% 
    rename(DayMonthYearTIME=DayMonthYearTIME.x)
  ##
  ## Dealing with NAs in the precip data (NAs in the chirps dataset)
  newdatModDeforestation_MR_Precip2020 %>% filter(is.na(PrecipImmedRegion))
  newdatModDeforestation_MR_Precip2020$PrecipImmedRegion <- round(na.approx(newdatModDeforestation_MR_Precip2020$PrecipImmedRegion, rule=2),3)  
  head(newdatModDeforestation_MR_Precip2020)
  newdatModDeforestation_MR_Precip2020 %>% filter(is.na(PrecipImmedRegion))
  ## Predict: 
  newdatModDeforestation_MR_Precip2020$predsModDeforestation_MR <- predict(ModDeforestation_MR$gam, newdatModDeforestation_MR_Precip2020, level=0, type='response') 
  head(newdatModDeforestation_MR_Precip2020)
  ## new: adding SE to calculate CI upper and lower
  newdatModDeforestation_MR_Precip2020$predsModDeforestation_MR_se <- predict(ModDeforestation_MR$gam, newdatModDeforestation_MR_Precip2020, level=0, type='response', se.fit=TRUE)$se.fit 
  head(newdatModDeforestation_MR_Precip2020)
  # CI: 
  newdatModDeforestation_MR_Precip2020 <- newdatModDeforestation_MR_Precip2020 %>% 
    mutate(upper_predsModDeforestation_MR = predsModDeforestation_MR + (2 * predsModDeforestation_MR_se)) %>% # upr <- p$fit + (2 * p$se.fit) & lwr <- p$fit - (2 * p$se.fit)
    mutate(lower_predsModDeforestation_MR = predsModDeforestation_MR - (2 * predsModDeforestation_MR_se))
  head(newdatModDeforestation_MR_Precip2020)
  ## PLOT predict from this model predicted for 2020 (moving average precipitation)
  gPredsModDeforestation_MR_Precip2020 <- ggplot(newdatModDeforestation_MR_Precip2020) + 
    geom_point(data= datDeforestationMesoRegion_No2019, aes(y=DailyFireCountImmedRegion, x=DayMonthYearTIME), alpha=0.3) + # raw data 2012 to 2018
    geom_line(aes(y=predsModDeforestation_MR, x=DayMonthYearTIME), colour='blue', size=2) +
    geom_ribbon(aes(ymax=upper_predsModDeforestation_MR, ymin=lower_predsModDeforestation_MR, x=DayMonthYearTIME), fill='purple',alpha=0.5) +
    labs(title = paste0('Deforestation fire in ',bli[i],'\nBlue: predicted from model 2012-2018 using real precipitation of 2020')) + 
    theme(plot.title = element_text(vjust = -20))
  #gPredsModDeforestation_MR_Precip2020 ## should have no interruptions in the blue line
  ##
  ##
  gAllPartsAllYear2020 <- ggplot(datDeforestationMesoRegion_Only2020) + 
    geom_point(data=datDeforestationMesoRegion_No2019, aes(y=DailyFireCountImmedRegion, x=DayMonthYearTIME), alpha=0.2) + # raw data 2012-2018
    geom_line(data= newdatModDeforestation_MR_MeanPrecip, aes(y=predsModDeforestation_MR, x=DayMonthYearTIME), colour='black', size=1.5) + # predicted 2012-2018
    geom_ribbon(data= newdatModDeforestation_MR_MeanPrecip, aes(ymax=upper_predsModDeforestation_MR, ymin=lower_predsModDeforestation_MR, x=DayMonthYearTIME), fill='purple',alpha=0.5) +
    geom_point(aes(y=DailyFireCountImmedRegion, x=DayMonthYearTIME), alpha=1, colour='firebrick') + # raw data 2020
    geom_line(aes(y=DailyFireCountImmedRegion, x=DayMonthYearTIME), alpha=0.7, colour='firebrick') + # raw data 2020
    geom_line(data=newdatModDeforestation_MR_Precip2020, aes(y=predsModDeforestation_MR, x=DayMonthYearTIME), colour='blue', size=2) +
    geom_ribbon(data= newdatModDeforestation_MR_Precip2020, aes(ymax=upper_predsModDeforestation_MR, ymin=lower_predsModDeforestation_MR, x=DayMonthYearTIME), fill='purple',alpha=0.5) +
    # GLO period
    geom_segment(aes(x = as.Date('1000-08-23'), y = 0, xend = as.Date('1000-08-23'), yend = 200), colour = "black", linetype='dashed') +
    geom_segment(aes(x = as.Date('1000-10-28'), y = 0, xend = as.Date('1000-10-28'), yend = 200), colour = "black", linetype='dashed') +
    labs(title = paste0('Deforestation fire in ',bli[i],'\nBlack: raw data and predicted 2012-2018\nBlue: predicted from model 2012-2018 using real precipitation of 2020\nRed: raw data 2020')) + 
    theme(plot.title = element_text(vjust = -20)) 
  #gAllPartsAllYear2020
  ##
  ## 2019 and 2020
  gridExtra::grid.arrange(gAllPartsAllYear, gAllPartsAllYear2020, ncol=2)
  ##
  ##
  ## PART 3 ####
  ## new for daily diff: calculate observed minus expected fire daily 
  ## Rename var 'Precip' bc it's got the same name but diff info:
  #### 2019: ####
  datDeforestationMesoRegion_Only2019 <- datDeforestationMesoRegion_Only2019 %>% rename(PrecipImmedRegion_2019 = "PrecipImmedRegion")
  # newdat with predicted values
  newdatModDeforestation_MR_Precip2019 <- newdatModDeforestation_MR_Precip2019 %>% rename(PrecipImmedRegion_Predict = "PrecipImmedRegion") %>% 
    mutate(NM_ImmedRegion = bli[i])
  # join them (new dat has 365 obs, so it needs to go full. I'll fill in the NAs after this) -> have a think if NAs should be filled or left as NAs
  ## think: if data is missing from observed fire that day in that place it's because there was no fire then! ## so replace these NAs with zero!
  ##
  dat_with_Observed_and_Expected_MR_2019 <- newdatModDeforestation_MR_Precip2019 %>% 
    left_join(datDeforestationMesoRegion_Only2019, by = c("DayMonthYearTIMEnum", "DayMonthYearTIME", "NM_ImmedRegion"))
  summary(dat_with_Observed_and_Expected_MR_2019) 
  unique(dat_with_Observed_and_Expected_MR_2019$NM_ESTA)
  ## NAs in DailyFireCountImmedRegion --> should be zero :
  dat_with_Observed_and_Expected_MR_2019$DailyFireCountImmedRegion[is.na(dat_with_Observed_and_Expected_MR_2019$DailyFireCountImmedRegion)] <- "0"
  dat_with_Observed_and_Expected_MR_2019$DailyFireCountImmedRegion <- as.numeric(dat_with_Observed_and_Expected_MR_2019$DailyFireCountImmedRegion)
  ## NAs in year --> should be 2019:
  dat_with_Observed_and_Expected_MR_2019$year[is.na(dat_with_Observed_and_Expected_MR_2019$year)] <- "2019"
  ## NAs in NM Estado --> should be the same as the only state info of each immediate region
  dat_with_Observed_and_Expected_MR_2019$NM_ESTA[is.na(dat_with_Observed_and_Expected_MR_2019$NM_ESTA)] <- dplyr::first(na.omit(dat_with_Observed_and_Expected_MR_2019$NM_ESTA))
  summary(dat_with_Observed_and_Expected_MR_2019)
  ## NAs in date
  dat_with_Observed_and_Expected_MR_2019 <- dat_with_Observed_and_Expected_MR_2019 %>% separate(Date, into=c('mil','mes','dia'), sep="-", remove=FALSE) %>% 
    mutate(DataCompleta = paste0(year,"-",mes,"-",dia)) %>%
    mutate(DataCompletaAsDate = as.Date(DataCompleta)) %>% 
    select(-c(FullDateAsDate, FullDate, MonthYear, Month, day, month))
  summary(dat_with_Observed_and_Expected_MR_2019)
  ##
  ## Calculate the difference : observed - expected
  dat_with_Observed_and_Expected_MR_2019 <- dat_with_Observed_and_Expected_MR_2019 %>% mutate(DailyDiff = DailyFireCountImmedRegion - predsModDeforestation_MR) 
  ## 
  glimpse(dat_with_Observed_and_Expected_MR_2019)
  ## plot with FullDatePosix as x axis to keep the year and connect 2019 with 2020
  gDailyDiff_2019 <- ggplot(dat_with_Observed_and_Expected_MR_2019) +
    geom_point(aes(y=DailyFireCountImmedRegion, x=DataCompletaAsDate), colour="firebrick") +
    geom_line(aes(y=DailyFireCountImmedRegion, x=DataCompletaAsDate), colour="firebrick") +
    geom_point(aes(y=predsModDeforestation_MR, x=DataCompletaAsDate), colour="blue") +
    geom_line(aes(y=predsModDeforestation_MR, x=DataCompletaAsDate), colour="blue") +
    geom_point(aes(y=DailyDiff, x=DataCompletaAsDate), colour= "purple") +
    geom_line(aes(y=DailyDiff, x=DataCompletaAsDate), colour= "purple")
  gDailyDiff_2019
  ##
  # colnames(dat_with_Observed_and_Expected_MR_2019)
  # summary(dat_with_Observed_and_Expected_MR_2019)
  subsetDat2019 <- dat_with_Observed_and_Expected_MR_2019 %>% select(c("DataCompleta","DataCompletaAsDate","predsModDeforestation_MR",
                                                                       "upper_predsModDeforestation_MR","lower_predsModDeforestation_MR",
                                                                       "NM_ImmedRegion","DailyFireCountImmedRegion","NM_ESTA","DailyDiff"))
  subsetDat2019 <- subsetDat2019 %>% mutate(year=as.factor("2019"))
  ##
  # df_Daily_Deforestation_FIRE <- NULL
  df_Daily_Deforestation_FIRE <- rbind(df_Daily_Deforestation_FIRE, subsetDat2019)
  # summary(df_Daily_Deforestation_FIRE)
  ##
  #### 2020: ####
  datDeforestationMesoRegion_Only2020 <- datDeforestationMesoRegion_Only2020 %>% rename(PrecipImmedRegion_2020 = "PrecipImmedRegion")
  # newdat with predicted values
  newdatModDeforestation_MR_Precip2020 <- newdatModDeforestation_MR_Precip2020 %>% rename(PrecipImmedRegion_Predict = "PrecipImmedRegion") %>% 
    mutate(NM_ImmedRegion = bli[i])
  # join them (new dat has 365 obs, so it needs to go full. I'll fill in the NAs after this) -> have a think if NAs should be filled or left as NAs
  ## think: if data is missing from observed fire that day in that place it's because there was no fire then! ## so replace these NAs with zero!
  ##
  dat_with_Observed_and_Expected_MR_2020 <- newdatModDeforestation_MR_Precip2020 %>% 
    left_join(datDeforestationMesoRegion_Only2020, by = c("DayMonthYearTIMEnum", "DayMonthYearTIME", "NM_ImmedRegion"))
  summary(dat_with_Observed_and_Expected_MR_2020)   
  ## NAs in DailyFireCountImmedRegion --> should be zero :
  dat_with_Observed_and_Expected_MR_2020$DailyFireCountImmedRegion[is.na(dat_with_Observed_and_Expected_MR_2020$DailyFireCountImmedRegion)] <- "0"
  dat_with_Observed_and_Expected_MR_2020$DailyFireCountImmedRegion <- as.numeric(dat_with_Observed_and_Expected_MR_2020$DailyFireCountImmedRegion)
  ## NAs in year --> should be 2020:
  dat_with_Observed_and_Expected_MR_2020$year[is.na(dat_with_Observed_and_Expected_MR_2020$year)] <- "2020"
  ## NAs in NM Estado --> should be the same as the only state info of each immediate region
  dat_with_Observed_and_Expected_MR_2020$NM_ESTA[is.na(dat_with_Observed_and_Expected_MR_2020$NM_ESTA)] <- dplyr::first(na.omit(dat_with_Observed_and_Expected_MR_2020$NM_ESTA))
  summary(dat_with_Observed_and_Expected_MR_2020)
  ## NAs in date
  dat_with_Observed_and_Expected_MR_2020 <- dat_with_Observed_and_Expected_MR_2020 %>% separate(Date, into=c('mil','mes','dia'), sep="-", remove=FALSE) %>% 
    mutate(DataCompleta = paste0(year,"-",mes,"-",dia)) %>%
    mutate(DataCompletaAsDate = as.Date(DataCompleta)) %>% 
    select(-c(FullDateAsDate, FullDate, MonthYear, Month, day, month))
  summary(dat_with_Observed_and_Expected_MR_2020)
  ##
  ## Calculate the difference : observed - expected
  dat_with_Observed_and_Expected_MR_2020 <- dat_with_Observed_and_Expected_MR_2020 %>% mutate(DailyDiff = DailyFireCountImmedRegion - predsModDeforestation_MR) 
  ## 
  glimpse(dat_with_Observed_and_Expected_MR_2020)
  ## plot with FullDatePosix as x axis to keep the year and connect 2019 with 2020
  gDailyDiff_2020 <- ggplot(dat_with_Observed_and_Expected_MR_2020) +
    geom_point(aes(y=DailyFireCountImmedRegion, x=DataCompletaAsDate), colour="firebrick") +
    geom_line(aes(y=DailyFireCountImmedRegion, x=DataCompletaAsDate), colour="firebrick") +
    geom_point(aes(y=predsModDeforestation_MR, x=DataCompletaAsDate), colour="blue") +
    geom_line(aes(y=predsModDeforestation_MR, x=DataCompletaAsDate), colour="blue") +
    geom_point(aes(y=DailyDiff, x=DataCompletaAsDate), colour= "purple") +
    geom_line(aes(y=DailyDiff, x=DataCompletaAsDate), colour= "purple")
  # gDailyDiff_2020
  # gridExtra::grid.arrange(gDailyDiff_2019, gDailyDiff_2020, ncol=2)
  ##
  ##
  # dat_with_Observed_and_Expected_MR_2020
  subsetDat2020 <- dat_with_Observed_and_Expected_MR_2020 %>% select(c("DataCompleta","DataCompletaAsDate","predsModDeforestation_MR",
                                                                       "upper_predsModDeforestation_MR", "lower_predsModDeforestation_MR",
                                                                       "NM_ImmedRegion","DailyFireCountImmedRegion","NM_ESTA","DailyDiff"))
  subsetDat2020 <- subsetDat2020 %>% mutate(year=as.factor("2020"))
  ##
  df_Daily_Deforestation_FIRE <- rbind(df_Daily_Deforestation_FIRE, subsetDat2020)
  glimpse(df_Daily_Deforestation_FIRE)
  summary(df_Daily_Deforestation_FIRE)
#}


length(unique(df_Daily_Deforestation_FIRE$NM_ImmedRegion)) # 58 saem do loop. agora 68
#
##### 
## which Immediate Regions have a problem with MA = 10 :
# mr : 16 (after cameta), 23 (after cruzeiro do sul), 33 34 (after labrea), 49 50 (after pontes e lacerda), 60 (after sinop), 66 (after tucurui)
## i gotta manually skip some, so type from: 17, 24, 35, 51, 61 and 67 


######
## then create bli and go changing the number for MA

## original, number of immediate regions
bla <- unique(datDeforestation_FireCountDailyImmedRegion$NM_ImmedRegion) # 68!
## number of immediate regions in the df after the loop
ble <- unique(df_Daily_Deforestation_FIRE$NM_ImmedRegion) # 61
## what the damn hell?
bli <- setdiff(bla, ble)
bli # 


#####
glimpse(df_Daily_Deforestation_FIRE)
summary(df_Daily_Deforestation_FIRE)
df_Daily_Deforestation_FIRE %>% filter(is.na(NM_ESTA))
df_Daily_Deforestation_FIRE$DailyFireCountImmedRegion <- as.numeric(df_Daily_Deforestation_FIRE$DailyFireCountImmedRegion)
length(unique(df_Daily_Deforestation_FIRE$NM_ImmedRegion)) # 68 (yay)

df_Daily_Deforestation_FIRE %>% 
  ggplot(.) +
  #geom_point(aes(y=predsModDeforestation_MR, x=DataCompletaAsDate), colour='black') +
  geom_line(aes(y=predsModDeforestation_MR, x=DataCompletaAsDate), colour='black') +
  #geom_point(aes(y=DailyFireCountImmedRegion, x=DataCompletaAsDate), colour='firebrick', alpha=0.5) +
  geom_line(aes(y=DailyFireCountImmedRegion, x=DataCompletaAsDate), colour='firebrick', alpha=0.5) +
  #geom_point(aes(y=DailyDiff, x=DataCompletaAsDate), colour='purple', alpha=0.5) +
  geom_line(aes(y=DailyDiff, x=DataCompletaAsDate), colour='purple', alpha=0.5) #+
# facet_wrap(~year)


## getting summed values for the whole amazon

head(df_Daily_Deforestation_FIRE)
(df_Daily_Deforestation_FIRE) %>% filter(NM_ImmedRegion == "Porto Grande" & year == 2020)

summary(df_Daily_Deforestation_FIRE$DailyFireCountImmedRegion)
dim(df_Daily_Deforestation_FIRE) # 49643

range(df_Daily_Deforestation_FIRE$DataCompletaAsDate)


hist(df_Daily_Deforestation_FIRE$predsModDeforestation_MR,30)
hist(df_Daily_Deforestation_FIRE$DailyFireCountImmedRegion,30)

df_Daily_Deforestation_FIRE %>% group_by(year, DataCompletaAsDate) %>% 
  #filter(NM_ImmedRegion != "Porto Grande") %>% 
  summarise(ExpectedFire = sum(predsModDeforestation_MR),
            ObservedFire = sum(DailyFireCountImmedRegion, na.rm=T),
            UpperExpected = sum(upper_predsModDeforestation_MR),
            LowerExpected = sum(lower_predsModDeforestation_MR)) %>%#
  ggplot(.) +
  geom_ribbon(aes(ymax=UpperExpected, ymin=LowerExpected, x=DataCompletaAsDate), fill='purple', alpha=0.5) +
  geom_point(aes(y=ExpectedFire, x=DataCompletaAsDate), colour='darkgrey') + 
  geom_line(aes(y=ExpectedFire, x=DataCompletaAsDate), colour='darkgrey') + 
  geom_point(aes(y=ObservedFire, x=DataCompletaAsDate), colour='firebrick') +
  geom_line(aes(y=ObservedFire, x=DataCompletaAsDate), colour='firebrick') 





# ## salvo sem porto grande em 2020, apenas em 2019.

## save with a new name, now with CI! (22nd march 2021)
df_Daily_Deforestation_FIRE_CI <- df_Daily_Deforestation_FIRE



save(df_Daily_Deforestation_FIRE_CI, file="~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/FromAnalysis/with_ImmediateRegions/With2020Data/df_Daily_Deforestation_FIRE_CI.RData")
# load("~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/FromAnalysis/with_ImmediateRegions/With2020Data/df_Daily_Deforestation_FIRE.RData")
#





#
# ## old #####
# datDeforestation_FireCountDailyImmedRegion$FullDatePosix <- as.POSIXct(datDeforestation_FireCountDailyImmedRegion$FullDate, format = "%Y-%m-%d",tz = "GMT")
# 
# range(datDeforestation_FireCountDailyImmedRegion$FullDatePosix, na.rm=T) # 2012 to 2019
# unique(datDeforestation_FireCountDailyImmedRegion$FullDatePosix)
# 
# 
# datDeforestation_FireCountDailyImmedRegion <- datDeforestation_FireCountDailyImmedRegion %>% arrange(FullDatePosix)
# glimpse(datDeforestation_FireCountDailyImmedRegion)
# datDeforestation_FireCountDailyImmedRegion %>% filter(is.na(FullDatePosix)) ## yes, there are NAs =/


# #####
# newdatModDeforestation_MR_MeanPrecip <- expand.grid(DayMonthYearTIMEnum = seq(min(datDeforestationMesoRegion_No2019$DayMonthYearTIMEnum, na.rm = T),
#                                                                         max(datDeforestationMesoRegion_No2019$DayMonthYearTIMEnum, na.rm = T), length.out = 365),
#                                               PrecipImmedRegion = mean(datDeforestationMesoRegion_No2019$PrecipImmedRegion))
# ###
# newdatModDeforestation_MR_MeanPrecip$DayMonthYearTIME <- as.POSIXct(as.numeric(newdatModDeforestation_MR_MeanPrecip$DayMonthYearTIMEnum+1e-6), 
#                                                              origin = "1970-01-01", tz="GMT", '%Y-%m-%d')
# 
# Precip2019 <- as_tibble(datDeforestationMesoRegion_Only2019) %>% select(DayMonthYearTIME, PrecipImmedRegion) %>% 
#   separate(DayMonthYearTIME, sep = ' ', into = c('Date', 'Time'), remove= FALSE) %>% select(-Time)
# ###
# newdatModDeforestation_MR_Precip2019$DayMonthYearTIME <- as.POSIXct(as.numeric(newdatModDeforestation_MR_Precip2019$DayMonthYearTIMEnum+1e-6), 
#                                                              origin = "1970-01-01", tz="GMT", '%Y-%m-%d')
# newdatModDeforestation_MR_Precip2019 <- newdatModDeforestation_MR_Precip2019 %>% 
#   separate(DayMonthYearTIME, sep=' ', into=c('Date', 'Time'), remove=F) %>% select(-Time)
# 

###

# ## create the real var for time 
# df_Daily_Deforestation_FIRE <- df_Daily_Deforestation_FIRE %>% separate(Date, into=c('mil', 'mes', 'dia')) %>% 
#   mutate(AnoMesDia = paste0(year,'-', mes,'-', dia))
# df_Daily_Deforestation_FIRE$AnoMesDiaPosix <- as.POSIXct(df_Daily_Deforestation_FIRE$AnoMesDia, format = "%Y-%m-%d", tz = "GMT")

## mean, sum?