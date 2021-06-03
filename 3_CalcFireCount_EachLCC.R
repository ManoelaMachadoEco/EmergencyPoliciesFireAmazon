# Manoela 19 November 2020

## Script to calculate basic stats (fire count) of each LCC and LT for analysis
## Using VIIRS + LCC + LT + PA +STMun + Immediate regions + CHIRPS of immediate regions 

rm(list=ls())
require("tidyverse")
require("dplyr")
require("sf")
require("ggplot2")
require("lubridate")
require("plotly")
require("formattable")

load('~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb.RData')
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb)

## colours for graphs
ColGraphClassesYears <- c("green4", "#FF7F00","#6A3D9A", "gold1","skyblue2","#E31A1C","#f531c1", "black", 'red', 'blue')

# ## check plot map ####
# VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb_asSF <- st_as_sf(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb)
# head(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb_asSF)
# VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb_asSF %>% filter(year == '2019') %>% ggplot(.) + geom_sf(aes(colour=NM_ImmedRegion))


#### Calculate basic stats for each LCC ####
# as factor
VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$NM_ImmedRegion <- as.factor(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$NM_ImmedRegion)
VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$NM_ESTA <- as.factor(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$NM_ESTA)
VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$year <- as.factor(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$year)
VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$LandCoverClass <- as.factor(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$LandCoverClass)
# VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$DroughtYear <- as.factor(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$DroughtYear)

## remove NAs
VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb <- VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb %>% filter(!is.na(NM_ImmedRegion)) # 256 NAs

## check porto grande
VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb %>% filter(NM_ImmedRegion == 'Porto Grande')


#### FOREST (PF+SF) ####
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb)
unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$LandCoverClass)
unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$NM_ImmedRegion)
##
datForest <-VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb %>% 
  dplyr::filter(LandCoverClass%in%c('Forest Formation','Secondary Forest'))
glimpse(datForest)

## group by Immediate Region #
datForest %>% group_by(year, NM_ImmedRegion, DayMonthYearTIME) %>% 
  ggplot(.) + geom_histogram(aes(DayMonthYearTIME, fill=year)) + 
  facet_wrap(~NM_ESTA, scales = "free_y") # 

datForest %>% filter(NM_ImmedRegion == 'Porto Grande' & year == '2019')


## within each year, get daily count of fire in each ImmedRegion
datForest_FireCountDailyImmedRegion <- datForest %>% group_by(year, NM_ImmedRegion, DayMonthYearTIME) %>% 
  summarise(DailyFireCountImmedRegion = n(),
            #DroughtYear = unique(DroughtYear),
            PrecipImmedRegion = unique(PrecipImmedRegion),
            Month = unique(Month),
            NM_ESTA = unique(NM_ESTA)) 

glimpse(datForest_FireCountDailyImmedRegion)

gforest <- ggplot(datForest_FireCountDailyImmedRegion) +
  geom_point(aes(y= DailyFireCountImmedRegion, x= DayMonthYearTIME, color= year), alpha=0.6) +
  geom_smooth(aes(y= DailyFireCountImmedRegion, x= DayMonthYearTIME, color=year), span=0.6) +
  # facet_wrap(~NM_ImmedRegion, scale='free_y') + # some problems here
  labs(title='Fire count in forest') + theme_bw() +
  scale_color_manual(values=ColGraphClassesYears) +
  geom_segment(aes(x = as.Date('1000-08-24'), y = 0, xend = as.Date('1000-08-24'), yend = 1000), colour = "darkgrey", linetype='dashed') +
  geom_segment(aes(x = as.Date('1000-10-24'), y = 0, xend = as.Date('1000-10-24'), yend = 1000), colour = "darkgrey", linetype='dashed') 
gforest

## save Forest dat ##
save(datForest_FireCountDailyImmedRegion, file='~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/Dats_FireCount_forAnalysis/datForest_FireCountDailyImmedRegion.RData')


#### PASTURE ####
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb)
unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$LandCoverClass)
unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$NM_ImmedRegion)
##
datPasture <-VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb %>% 
  dplyr::filter(LandCoverClass%in%c('Pasture'))
glimpse(datPasture)


## group by Immediate Region #
datPasture %>% group_by(year, NM_ImmedRegion, DayMonthYearTIME) %>% 
  ggplot(.) + geom_histogram(aes(DayMonthYearTIME, fill=year)) + 
  facet_wrap(~NM_ESTA, scales = "free_y") # NM_ImmedRegion

## within each year, get daily count of fire in each ImmedRegion
datPasture_FireCountDailyImmedRegion <- datPasture %>% group_by(year, NM_ImmedRegion, DayMonthYearTIME) %>% 
  summarise(DailyFireCountImmedRegion = n(),
            #DroughtYear = unique(DroughtYear),
            PrecipImmedRegion = unique(PrecipImmedRegion),
            Month = unique(Month),
            NM_ESTA = unique(NM_ESTA)) 

glimpse(datPasture_FireCountDailyImmedRegion)

gpasture <- ggplot(datPasture_FireCountDailyImmedRegion) +
  geom_point(aes(y= DailyFireCountImmedRegion, x= DayMonthYearTIME, color= year), alpha=0.6) +
  geom_smooth(aes(y= DailyFireCountImmedRegion, x= DayMonthYearTIME, color=year), span=0.6) +
  #facet_wrap(~NM_ImmedRegion, scale='free_y') + # some problems here
  labs(title='Fire count in pasture') + theme_bw() +
  scale_color_manual(values=ColGraphClassesYears) +
  geom_segment(aes(x = as.Date('1000-08-24'), y = 0, xend = as.Date('1000-08-24'), yend = 1000), colour = "darkgrey", linetype='dashed') +
  geom_segment(aes(x = as.Date('1000-10-24'), y = 0, xend = as.Date('1000-10-24'), yend = 1000), colour = "darkgrey", linetype='dashed') 
gpasture

## save Pasture dat ##
save(datPasture_FireCountDailyImmedRegion, file='~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/Dats_FireCount_forAnalysis/datPasture_FireCountDailyImmedRegion.RData')


#### DEFORESTATION  ####
glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb)
unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$LandCoverClass)
unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$NM_ImmedRegion)
##
datDeforestation <-VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb %>% 
  dplyr::filter(LandCoverClass%in%c('Deforested SameYear', 'Deforested PreviousYear'))
glimpse(datDeforestation)

datDeforestation %>% filter(NM_ImmedRegion == 'Porto Grande' & year == '2020') # no data

## group by Immediate Region #
datDeforestation %>% group_by(year, NM_ImmedRegion, DayMonthYearTIME) %>% 
  ggplot(.) + geom_histogram(aes(DayMonthYearTIME, fill=year)) + facet_wrap(~NM_ESTA, scales = "free_y") # 

## within each year, get daily count of fire in each ImmedRegion
datDeforestation_FireCountDailyImmedRegion <- datDeforestation %>% group_by(year, NM_ImmedRegion, DayMonthYearTIME) %>% 
  summarise(DailyFireCountImmedRegion = n(),
            # DroughtYear = unique(DroughtYear),
            PrecipImmedRegion = unique(PrecipImmedRegion),
            Month = unique(Month),
            NM_ESTA = unique(NM_ESTA)) 

glimpse(datDeforestation_FireCountDailyImmedRegion)

gdefo <- ggplot(datDeforestation_FireCountDailyImmedRegion) +
  geom_point(aes(y= DailyFireCountImmedRegion, x= DayMonthYearTIME, color= year), alpha=0.6) +
  geom_smooth(aes(y= DailyFireCountImmedRegion, x= DayMonthYearTIME, color=year), span=0.6) +
  #facet_wrap(~NM_ImmedRegion, scale='free_y') + # some problems here
  labs(title='Fire count in Deforestation') + theme_bw() +
  scale_color_manual(values=ColGraphClassesYears) +
  geom_segment(aes(x = as.Date('1000-08-24'), y = 0, xend = as.Date('1000-08-24'), yend = 1000), colour = "darkgrey", linetype='dashed') +
  geom_segment(aes(x = as.Date('1000-10-24'), y = 0, xend = as.Date('1000-10-24'), yend = 1000), colour = "darkgrey", linetype='dashed') 
gdefo

## save Deforestation dat ##
save(datDeforestation_FireCountDailyImmedRegion, file='~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/Dats_FireCount_forAnalysis/datDeforestation_FireCountDailyImmedRegion.RData')
##


gridExtra::grid.arrange(gforest, gpasture, gdefo, ncol=3)

# #### Non Designated Lands  ####
# glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb)
# unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$LandCoverClass)
# unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$LT_name)
# unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$NM_ImmedRegion)
# ##
# datNonDesigLands <-VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb %>% 
#   dplyr::filter(LT_name%in%c('Non-Designated Lands'))
# glimpse(datNonDesigLands)
# 
# ## group by Immediate Region #
# datNonDesigLands %>% group_by(year, NM_ImmedRegion, DayMonthYearTIME) %>% 
#   ggplot(.) + geom_histogram(aes(DayMontYearChPosixNumMilli, fill=year)) + facet_wrap(~NM_ESTA, scales = "free_y") # NM_ImmedRegion
# 
# ## within each year, get daily count of fire in each ImmedRegion
# datNonDesigLands_FireCountDailyImmedRegion <- datNonDesigLands %>% group_by(year, NM_ImmedRegion, DayMonthYearTIME) %>% 
#   summarise(DailyFireCountImmedRegion = n(),
#             DroughtYear = unique(DroughtYear),
#             PrecipImmedRegion = unique(PrecipImmedRegion),
#             Month = unique(Month),
#             NM_ESTA = unique(NM_ESTA)) 
# 
# glimpse(datNonDesigLands_FireCountDailyImmedRegion)
# 
# ggplot(datNonDesigLands_FireCountDailyImmedRegion) +
#   geom_point(aes(y= DailyFireCountImmedRegion, x= DayMonthYearTIME, color= year), alpha=0.6) +
#   geom_smooth(aes(y= DailyFireCountImmedRegion, x= DayMonthYearTIME, color=year), span=0.6) +
#   #facet_wrap(~NM_ImmedRegion, scale='free_y') + # some problems here
#   labs(title='Fire count in NonDesigLands') + theme_bw() +
#   scale_color_manual(values=ColGraphClassesYears) +
#   geom_segment(aes(x = as.POSIXct('1000-08-24'), y = 0, xend = as.POSIXct('1000-08-24'), yend = 1000), colour = "darkgrey", linetype='dashed') +
#   geom_segment(aes(x = as.POSIXct('1000-10-24'), y = 0, xend = as.POSIXct('1000-10-24'), yend = 1000), colour = "darkgrey", linetype='dashed') 
# 
# 
# ## save NonDesigLands dat ##
# save(datNonDesigLands_FireCountDailyImmedRegion, file='~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/Dats_FireCount_forAnalysis/datNonDesigLands_FireCountDailyImmedRegion.RData')
# 
# ##
# 
# #### Private Farms  ####
# glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb)
# unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$LandCoverClass)
# unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$LT_name)
# unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$NM_ImmedRegion)
# ##
# datPrivateFarms <-VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb %>% 
#   dplyr::filter(LT_name%in%c('Private Farms INCRA', 'Private Farms CAR'))
# glimpse(datPrivateFarms)
# 
# ## group by Immediate Region #
# datPrivateFarms %>% group_by(year, NM_ImmedRegion, DayMonthYearTIME) %>% 
#   ggplot(.) + geom_histogram(aes(DayMontYearChPosixNumMilli, fill=year)) + facet_wrap(~NM_ESTA, scales = "free_y") # NM_ImmedRegion
# 
# ## within each year, get daily count of fire in each ImmedRegion
# datPrivateFarms_FireCountDailyImmedRegion <- datPrivateFarms %>% group_by(year, NM_ImmedRegion, DayMonthYearTIME) %>% 
#   summarise(DailyFireCountImmedRegion = n(),
#             DroughtYear = unique(DroughtYear),
#             PrecipImmedRegion = unique(PrecipImmedRegion),
#             Month = unique(Month),
#             NM_ESTA = unique(NM_ESTA)) 
# 
# glimpse(datPrivateFarms_FireCountDailyImmedRegion)
# 
# ggplot(datPrivateFarms_FireCountDailyImmedRegion) +
#   geom_point(aes(y= DailyFireCountImmedRegion, x= DayMonthYearTIME, color= year), alpha=0.6) +
#   geom_smooth(aes(y= DailyFireCountImmedRegion, x= DayMonthYearTIME, color=year), span=0.6) +
#   #facet_wrap(~NM_ImmedRegion, scale='free_y') + # some problems here
#   labs(title='Fire count in PrivateFarms') + theme_bw() +
#   scale_color_manual(values=ColGraphClassesYears) +
#   geom_segment(aes(x = as.POSIXct('1000-08-24'), y = 0, xend = as.POSIXct('1000-08-24'), yend = 1000), colour = "darkgrey", linetype='dashed') +
#   geom_segment(aes(x = as.POSIXct('1000-10-24'), y = 0, xend = as.POSIXct('1000-10-24'), yend = 1000), colour = "darkgrey", linetype='dashed') 
# 
# 
# ## save PrivateFarms dat ##
# save(datPrivateFarms_FireCountDailyImmedRegion, file='~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/Dats_FireCount_forAnalysis/datPrivateFarms_FireCountDailyImmedRegion.RData')
# 
# ##
# #### Indigenous Lands  ####
# glimpse(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb)
# unique(VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb$LT_name)
# ##
# datIndigenousLands <-VIIRS_LCC_LT_PA_StMun_ImmedReg_CHIRPS_allYears_tb %>% 
#   dplyr::filter(LT_name%in%c('Indigenous Lands'))
# glimpse(datIndigenousLands)
# 
# ## group by Immediate Region #
# datIndigenousLands %>% group_by(year, NM_ImmedRegion, DayMonthYearTIME) %>% 
#   ggplot(.) + geom_histogram(aes(DayMontYearChPosixNumMilli, fill=year)) + facet_wrap(~NM_ESTA, scales = "free_y") # NM_ImmedRegion
# 
# ## within each year, get daily count of fire in each ImmedRegion
# datIndigenousLands_FireCountDailyImmedRegion <- datIndigenousLands %>% group_by(year, NM_ImmedRegion, DayMonthYearTIME) %>% 
#   summarise(DailyFireCountImmedRegion = n(),
#             DroughtYear = unique(DroughtYear),
#             PrecipImmedRegion = unique(PrecipImmedRegion),
#             Month = unique(Month),
#             NM_ESTA = unique(NM_ESTA)) 
# 
# glimpse(datIndigenousLands_FireCountDailyImmedRegion)
# 
# ggplot(datIndigenousLands_FireCountDailyImmedRegion) +
#   geom_point(aes(y= DailyFireCountImmedRegion, x= DayMonthYearTIME, color= year), alpha=0.6) +
#   geom_smooth(aes(y= DailyFireCountImmedRegion, x= DayMonthYearTIME, color=year), span=0.6) +
#   #facet_wrap(~NM_ImmedRegion, scale='free_y') + # some problems here
#   labs(title='Fire count in Indigenous Lands') + theme_bw() +
#   scale_color_manual(values=ColGraphClassesYears) +
#   geom_segment(aes(x = as.POSIXct('1000-08-24'), y = 0, xend = as.POSIXct('1000-08-24'), yend = 1000), colour = "darkgrey", linetype='dashed') +
#   geom_segment(aes(x = as.POSIXct('1000-10-24'), y = 0, xend = as.POSIXct('1000-10-24'), yend = 1000), colour = "darkgrey", linetype='dashed') 
# 
# 
# ## save Indigenous Lands dat ##
# save(datIndigenousLands_FireCountDailyImmedRegion, file='~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/Dats_FireCount_forAnalysis/datIndigenousLands_FireCountDailyImmedRegion.RData')
# 
# ##