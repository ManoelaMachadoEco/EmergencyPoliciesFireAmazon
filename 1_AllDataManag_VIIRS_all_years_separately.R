## Manoela 16 November 2020

## Script to arrange VIIRS data + land cover classes + Protected Areas + Land tenure + States and Municipalities + Precipitation + Immediate regions

## Data:
## VIIRS_LCC and Defo
## VIIRS LT
## VIIRS PA
## VIIRS States and Municipalities
## VIIRS immediate regions (24Sep)

## updating on 07 Jan with 2020 data


#########################################
rm(list=ls())
require("tidyverse")
require("dplyr")
require("sf")
require("ggplot2")
require("lubridate")
require("plotly")
require("formattable")


MBClassCode <- read_csv('~/Oxford/FireAmazon2019_GCRF/DataFromGEE/Phase6_MBcoll5/MapBiomasClassesCODE_forPhase6.csv')
MBClassCode <- MBClassCode %>% select(-X5) %>% select(LCC_Eng, LCC_value) %>% 
  separate(LCC_Eng, sep= '\\. ', into=c('del', 'LCC_name'), fill='left') %>% 
  select(-del) %>% rename(LandCoverClassNumber = LCC_value)
AtlasAgroClassCode <- read_csv('~/Oxford/FireAmazon2019_GCRF/DataFromGEE/Phase6_MBcoll5/LandTenureATLASClassesCODE.csv')
AtlasAgroClassCode <- AtlasAgroClassCode %>% select(LandTenureClassNo, ClassEng) %>% 
  rename(LandTenureClassNumber = LandTenureClassNo) %>% 
  rename(LT_name = ClassEng)


## loop for all , done that. today only 2020
i <- 13
ld[13] # start in 5

ld <- list.dirs('~/Oxford/FireAmazon2019_GCRF/DataFromGEE/Phase6_MBcoll5')
for(i in 5:(length(ld))){
  y <- unlist(strsplit(ld[i], split = "Year"))[2]
  cat("Year: ", y, "...")
#### VIIRS_LCC and Defo ####
flLCC <- list.files(path = ld[i], pattern = "LCC", full.names = T)
flLCC <- grep(pattern = ".shp", flLCC, value = T)
if(length(flLCC)==0) next()
VIIRS_LCC <- read_sf(flLCC)
head(VIIRS_LCC) 
## create var LATLONG and unique ID
VIIRS_LCC <- VIIRS_LCC %>% dplyr::mutate(LATLONG = paste0(LATITUDE,'_', LONGITUDE)) %>% 
                           dplyr::mutate(DATE = paste0(year,'-',monthNo,'-',day)) %>% 
                           dplyr::mutate(UniqueID = paste0(LATLONG, '_', DATE, '_', ACQ_TIME))
range(VIIRS_LCC$DATE)
## filter duplicates out (created by GEE)
VIIRS_LCC_NoDups <- VIIRS_LCC %>% dplyr::filter(!duplicated(UniqueID))
## rename var 'first'
VIIRS_LCC_NoDups <- VIIRS_LCC_NoDups %>% rename(LandCoverClassNumber = first)
## join land cover classes number with names from MapBiomas
VIIRS_LCC_NoDups_MBcode <- VIIRS_LCC_NoDups %>% dplyr::left_join(MBClassCode, by='LandCoverClassNumber')
VIIRS_LCC_NoDups_MBcode$LCC_name <- as.factor(VIIRS_LCC_NoDups_MBcode$LCC_name)


#### VIIRS LT ####
## LT code from Atlas
flLT <- list.files(path = ld[i], pattern = "LT", full.names = T)
flLT <- grep(pattern = ".shp", flLT, value = T)
VIIRS_LT <- read_sf(flLT)
## create var LATLONG and unique ID, and select some vars out (already covered in the LCC dataset)
VIIRS_LT <- VIIRS_LT %>% dplyr::mutate(LATLONG = paste0(LATITUDE,'_', LONGITUDE)) %>% 
                         dplyr::mutate(DATE = paste0(year,'-',monthNo,'-',day)) %>% 
                         dplyr::mutate(UniqueID = paste0(LATLONG, '_', DATE, '_', ACQ_TIME)) %>% select(UniqueID, first)
VIIRS_LT_NoDups <- VIIRS_LT %>% dplyr::filter(!duplicated(UniqueID))
## rename var 'first'
VIIRS_LT_NoDups <- VIIRS_LT_NoDups %>% rename(LandTenureClassNumber = first)
## join land cover classes number with names from MapBiomas
VIIRS_LT_NoDups_Atlascode <- VIIRS_LT_NoDups %>% dplyr::left_join(AtlasAgroClassCode, by='LandTenureClassNumber')
VIIRS_LT_NoDups_Atlascode$LT_name <- as.factor(VIIRS_LT_NoDups_Atlascode$LT_name)

#### VIIRS PA ####
flPA <- list.files(path = ld[i], pattern = "PA", full.names = T)
flPA <- grep(pattern = ".shp", flPA, value = T)
VIIRS_PA <- read_sf(flPA)
## create var LATLONG and unique ID, and select some vars out (already covered in the LCC dataset)
VIIRS_PA <- VIIRS_PA %>% dplyr::mutate(LATLONG = paste0(LATITUDE,'_', LONGITUDE)) %>% 
                         dplyr::mutate(DATE = paste0(year,'-',monthNo,'-',day)) %>% 
                         dplyr::mutate(UniqueID = paste0(LATLONG, '_', DATE, '_', ACQ_TIME)) %>% 
                         select(UniqueID, DESIG_ENG, NAME, IUCN_CAT, REP_AREA)
## filter duplicates out (created by GEE)
VIIRS_PA_NoDups <- VIIRS_PA %>% dplyr::filter(!duplicated(UniqueID))
## Name areas with NA with outside PA
## 'name'
VIIRS_PA_NoDups$NAME[is.na(VIIRS_PA_NoDups$NAME)] <- 'OutsidePA'
VIIRS_PA_NoDups$NAME <- as.factor(VIIRS_PA_NoDups$NAME)
## 'desig'
VIIRS_PA_NoDups$DESIG_ENG[is.na(VIIRS_PA_NoDups$DESIG_ENG)] <- 'OutsidePA'
VIIRS_PA_NoDups$DESIG_ENG <- as.factor(VIIRS_PA_NoDups$DESIG_ENG)
## Fix names acentos
s <- as.character(VIIRS_PA_NoDups$NAME) ##
Encoding(s) <- "latin1"
VIIRS_PA_NoDups$NAME <- iconv(s,from="latin1",to="ASCII//TRANSLIT") ## ok
VIIRS_PA_NoDups$NAME <- as.factor(VIIRS_PA_NoDups$NAME)
VIIRS_PA_NoDups$DESIG_ENG <- as.factor(VIIRS_PA_NoDups$DESIG_ENG)

#### VIIRS States and Municipalities ####
flStMun <- list.files(path = ld[i], pattern = "States_Municipalities", full.names = T)
flStMun <- grep(pattern = ".shp", flStMun, value = T)
VIIRS_StMun <- read_sf(flStMun)
## create var LATLONG and unique ID, and select some vars out (already covered in the LCC dataset)
VIIRS_StMun <- VIIRS_StMun %>% dplyr::mutate(LATLONG = paste0(LATITUDE,'_', LONGITUDE)) %>% 
                               dplyr::mutate(DATE = paste0(year,'-',monthNo,'-',day)) %>% 
                               dplyr::mutate(UniqueID = paste0(LATLONG, '_', DATE, '_', ACQ_TIME)) %>% 
                               select(UniqueID, NM_ESTADO, nm_municip)
## filter duplicates out (created by GEE, but just in case)
VIIRS_StMun_NoDups <- VIIRS_StMun %>% dplyr::filter(!duplicated(UniqueID))
## Fix names acentos
# state
u <- as.character(VIIRS_StMun$NM_ESTADO) 
Encoding(u) <- "latin1"
VIIRS_StMun$NM_ESTADO <- iconv(u,from="latin1",to="ASCII//TRANSLIT") 
VIIRS_StMun$NM_ESTADO <- as.factor(VIIRS_StMun$NM_ESTADO)
# municip
t <- as.character(VIIRS_StMun$nm_municip) 
Encoding(t) <- "latin1"
VIIRS_StMun$nm_municip <- iconv(t,from="latin1",to="ASCII//TRANSLIT") 
VIIRS_StMun$nm_municip <- as.factor(VIIRS_StMun$nm_municip)


#### VIIRS immediate regions ####
flImmedReg <- list.files(path = ld[i], pattern = "Regions", full.names = T)
flImmedReg <- grep(pattern = ".shp", flImmedReg, value = T)
VIIRS_ImmedReg <- read_sf(flImmedReg)
## create var LATLONG and unique ID, and select some vars out (already covered in the LCC dataset)
VIIRS_ImmedReg <- VIIRS_ImmedReg %>% dplyr::mutate(LATLONG = paste0(LATITUDE,'_', LONGITUDE)) %>% 
                                   dplyr::mutate(DATE = paste0(year,'-',monthNo,'-',day)) %>% 
                                   dplyr::mutate(UniqueID = paste0(LATLONG, '_', DATE, '_', ACQ_TIME)) %>% 
                                   select(UniqueID, nome_rgint, nome_rgi)
## Fix names acentos
# intermediate regions (mesoregions)
v <- as.character(VIIRS_ImmedReg$nome_rgint) 
VIIRS_ImmedReg$nome_rgint <- iconv(v,from="latin1",to="ASCII//TRANSLIT") 
VIIRS_ImmedReg$nome_rgint <- as.factor(VIIRS_ImmedReg$nome_rgint) # 
unique(VIIRS_ImmedReg$nome_rgint) # 29 mesoregions

## immediate regions
w <- as.character(VIIRS_ImmedReg$nome_rgi) 
VIIRS_ImmedReg$nome_rgi <- iconv(w,from="latin1",to="ASCII//TRANSLIT") 
VIIRS_ImmedReg$nome_rgi <- as.factor(VIIRS_ImmedReg$nome_rgi) # 
unique(VIIRS_ImmedReg$nome_rgi) # 83 immediate regions



#### save as sf ####
VIIRS_LCC_NoDups_MBcode_tb <- as_tibble(VIIRS_LCC_NoDups_MBcode)
VIIRS_LT_NoDups_Atlascode_tb <- as_tibble(VIIRS_LT_NoDups_Atlascode) %>% select(-geometry)
VIIRS_PA_NoDups_tb <- as_tibble(VIIRS_PA_NoDups) %>% select(-geometry)
VIIRS_StMun_tb <- as_tibble(VIIRS_StMun) %>% select(-geometry)
VIIRS_ImmedReg_tb <- as_tibble(VIIRS_ImmedReg) %>% select(-geometry)
## remove: VIIRS_Chirps_tb <- as_tibble(VIIRS_Chirps) %>% select(-geometry)

## join all ####
VIIRS_LCC_LT_PA_StMun_ImmedReg <- VIIRS_LCC_NoDups_MBcode_tb %>% 
                                     left_join(VIIRS_LT_NoDups_Atlascode_tb, by='UniqueID') %>% 
                                     left_join(VIIRS_PA_NoDups_tb, by='UniqueID') %>% 
                                     left_join(VIIRS_StMun_tb, by='UniqueID') %>% 
                                     left_join(VIIRS_ImmedReg_tb, by='UniqueID')

## SF
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf <- st_as_sf(VIIRS_LCC_LT_PA_StMun_ImmedReg)

## arranging the whole dataset
## Create Months names ####
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$Month <- NA
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$Month[VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$monthNo%in%c("01")] <- 'Jan'
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$Month[VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$monthNo%in%c("02")] <- 'Feb'
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$Month[VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$monthNo%in%c("03")] <- 'Mar'
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$Month[VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$monthNo%in%c("04")] <- 'Apr'
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$Month[VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$monthNo%in%c("05")] <- 'May'
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$Month[VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$monthNo%in%c("06")] <- 'Jun'
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$Month[VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$monthNo%in%c("07")] <- 'Jul'
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$Month[VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$monthNo%in%c("08")] <- 'Aug'
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$Month[VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$monthNo%in%c("09")] <- 'Sep'
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$Month[VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$monthNo%in%c("10")] <- 'Oct'
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$Month[VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$monthNo%in%c("11")] <- 'Nov'
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$Month[VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$monthNo%in%c("12")] <- 'Dec'
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$Month <- as.factor(VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$Month)
##
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$PA_Categ <- NA
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$PA_Categ[
  VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$DESIG_ENG%in%c('Ecological Station','Biological Reserve','Park','Wildlife Refuge',
                                                      'World Heritage Site (natural or mixed)')] <- 'Strictly Protected'
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$PA_Categ[
  VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$DESIG_ENG%in%c('Environmental Protection Area','Area of Relevant Ecological Interest',
                                                      'Forest','Extractive Reserve','Sustainable Development Reserve',
                                                      'Ramsar Site, Wetland of International Importance')] <- 'Sustainable Use'
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$PA_Categ[
  VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$DESIG_ENG%in%c('Indigenous Area','Indigenous Reserve')] <- 'Indigenous Territory'
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$PA_Categ[VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$DESIG_ENG%in%c('OutsidePA')] <- 'Not Protected'
#
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$PA_Categ <- as.factor(VIIRS_LCC_LT_PA_StMun_ImmedReg_sf$PA_Categ)
## save 
VIIRS_LCC_LT_PA_StMun_ImmedReg_sf <- VIIRS_LCC_LT_PA_StMun_ImmedReg_sf %>% select(-ACQ_DATE)
write_sf(VIIRS_LCC_LT_PA_StMun_ImmedReg_sf, 
         dsn = paste0("~/Oxford/FireAmazon2019_GCRF/DataFromR/Phase6_Climatic/VIIRS_LCC_LT_PA_StMun_ImmedReg_AllYearsDats/VIIRS_LCC_LT_PA_StMun_ImmedReg_",y,"_sf.shp"), 
         layer = paste0("VIIRS_LCC_LT_PA_StMun_ImmedReg_",y,"_sf"), driver = "ESRI Shapefile")
cat("\n")
}


