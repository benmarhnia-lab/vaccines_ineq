################################################################# 
#################################################################
#######                      PROJECT:                     ####### 
#######     VACCINATION AND HEALTH OUTCOMES IN LMICs      ####### 
#######                                                   ####### 
#################################################################
#################################################################
# For preparing the input data see project folder: Precipitaiton, vaccination and child health
# DHS Guide to vaccination: https://dhsprogram.com/data/Guide-to-DHS-Statistics/Vaccination.htm

rm(list =ls())
#install.packages("rdhs")
library(rdhs)         #packages is needed for retrieving the country boundaries
library(tidyverse)

rm(list =ls())
library(tidyverse)
library(rgdal)


################################################################################################
### Aggregate the individual-level data to lowest representative admin level for every country
################################################################################################
setwd("C:/Users/annak/Dropbox/Projects/2021_San Diego/Vaccination - spatial analysis/1_Data/")

# NOTES: 
# sample already restricted to children who are alive at the time of the interview (b5 == "yes")
# samples includes children under 3 years of age at the time of the interview
# "wt" is mother's weight   

df0 <- read.csv("./DHS_data_for_analysis.csv") 
df1 <- df0 %>% 
  dplyr::select(#Child characteristics
    psu, hhid, month, year, day,  bord, sex, agemonths, ageyears, birthsize, position,
    bcgvac, dtp1vac, dtp2vac, dtp3vac, dtpvac, opv0vac, opv1vac, opv2vac, opv3vac, opvvac, mcvvac,
    rota1vac, rota2vac, rotavacfull, rotavac, pcv1vac, pcv2vac, pcv3vac, pcvvacfull, pcvvac,
    fic, pic, vactotal, healthcard, 
    #Infectious disease symptoms
    diarrhea, fever, cough, ari,
    #Household & maternal characteristics
    wt, hhwt, region, residence, edulevel, literate, massmedia, resident, partner, wealth, wealthscorenew, wealth_new, whz, bmi,
    distancehealth, toiletshared, toiletimproved, waterpremise, watersafe, floorfinished, fuelsolid, minacceptablediet, hygiene,
    #Other
    latdhs, longdhs, climzone, cntrycode) %>% 
  mutate(dtpvacfull = ifelse(dtpvac == "yes", 1, 0)) %>% 
  mutate(opvvacfull = ifelse(opvvac == "yes", 1, 0)) 

str(df1)

# Agregate the data to the admin level
shares_age_12_23 <- df1 %>%        
  filter(agemonths>=12 & agemonths<=23) %>% 
  group_by(cntrycode, region) %>% 
  summarise(FIC = weighted.mean(fic, wt, na.rm=T),
            PIC = weighted.mean(pic, wt, na.rm=T),
            Diarrhea = weighted.mean(diarrhea, wt, na.rm=T),
            Fever = weighted.mean(fever, wt, na.rm=T),
            Cough = weighted.mean(cough, wt, na.rm=T),
            ARI = weighted.mean(ari, wt, na.rm=T), 
            BCG = weighted.mean(bcgvac, wt, na.rm=T),
            MCV = weighted.mean(mcvvac, wt, na.rm=T),
            DTP = weighted.mean(dtpvacfull, wt, na.rm=T),
            OPV = weighted.mean(opvvacfull, wt, na.rm=T),
            Rota = weighted.mean(rotavacfull, wt, na.rm=T),
            PCV = weighted.mean(pcvvacfull, wt, na.rm=T)
  ) %>% 
  mutate(region = str_to_title(region)) #capitalise region names

## Add World Bank country names
library(countrycode)
shares_age_12_23$cntryname <- countrycode(shares_age_12_23$cntrycode, 'iso2c', 'country.name')  
#?codelist
shares_age_12_23$cntryname[shares_age_12_23$cntrycode=="BU"] <- "Burundi"
shares_age_12_23$cntryname[shares_age_12_23$cntrycode=="MM"] <- "Myanmar"
shares_age_12_23$cntryname[shares_age_12_23$cntrycode=="GU"] <- "Guatemala"
shares_age_12_23$cntryname[shares_age_12_23$cntrycode=="IA"] <- "India"

write.csv(shares_age_12_23, "./dhs_regional_data/shares_age_12_23.csv")


################################################################################################
### Retrieve subnational boundaries for every country and link with the DHS data
################################################################################################
# Regional boundaries can be downloaded for individual DHS countries here: https://spatialdata.dhsprogram.com/boundaries/#view=table&countryId=AF
# rdhs package: https://cran.r-project.org/web/packages/rdhs/rdhs.pdf


## Create a list all DHS countries included in the analysis (DHS wave 7) 
a <- c("AO", "BJ", "BU", "ET", "GH", "GN", "GU", "HT", "IA", "KE", "KH", "LS", "ML", "MM", "MW", "NG", "NP", "PK", "RW", "TD", "TJ", "TL", "TZ", "UG", "ZA", "ZM", "ZW")


## Retrieve a list all DHS surveys and filter the surveys included in the analysis
data_dhs <- dhs_datasets()
data_dhs_IDs <- data_dhs %>% 
  filter(DHS_CountryCode %in% a) %>% 
  filter(SurveyType == "DHS") %>% 
  filter(SurveyYear >= 2014 & SurveyYear < 2019) %>% 
  dplyr::select("SurveyId", "DHS_CountryCode", "CountryName") %>% 
  unique()


## Retrieve the regional boundaries for 1 country 
list_sf <- download_boundaries(surveyId = data_dhs_IDs$SurveyId[11], method = "sf")
data_sf <- sf::st_as_sf(data.table::rbindlist(list_sf)) 
data_sf <- data_sf %>% 
  dplyr::select(DHSCC, CNTRYNAMEE, DHSREGEN)


str(data_sf)
plot(data_sf[3])



## Loop through  the rest of the countries and bind the spatial data 
# Some problem with the code - may beed to download each country file individuallly by rotating j

j = 2

for (j in 2:27){ 
  list_sf_tmp <- download_boundaries(surveyId = df_dhs_IDs$SurveyId[j], method = "sf")
  data_sf_tmp <- sf::st_as_sf(data.table::rbindlist(list_sf_tmp)) 
  data_sf_tmp <- data_sf_tmp %>% 
    dplyr::select(DHSCC, CNTRYNAMEE, DHSREGEN)
  
  sf_data <- rbind(sf_data, sf_data_tmp)
  
}


