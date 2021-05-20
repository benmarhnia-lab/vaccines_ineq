################################################################# 
#################################################################
#######                      PROJECT:                     ####### 
#######     VACCINATION AND HEALTH OUTCOMES IN LMICs      ####### 
#######                                                   ####### 
#######                    CODE: DHS DATA                 #######
#################################################################
#################################################################


# For preparing the input data see project folder: Precipitaiton, vaccination and child health
# DHS Guide to vaccination: https://dhsprogram.com/data/Guide-to-DHS-Statistics/Vaccination.htm

rm(list =ls())
library(foreign)
library(tidyverse)
library(data.table)
library(zoo)
library(psych)
library(lubridate)
#install.packages("raster")
library(raster)
library(sf)
library(ggplot2)
options(scipen=999)
options(digits=5)

setwd("C:/Users/annak/Dropbox/Projects/2020_San Diego/")


################################################################################################
### Aggregate individual data to lowest representative admin level for every country
################################################################################################

# NOTES: 
# sample already restricted to children who are alive at the time of the interview (b5 == "yes")
# samples includes children under 3 years of age at the time of the interview
# "wt" is mother's weight   


# Create an empty data frame where all country data will be stored
df_combined <- read.csv("./Precipitation, vaccination and child health/1_Data/data_for_analysis/AO.csv", header=T)[0,-1]


# Create a list of countries to be included in the analysis; [Potentially a few more could be included - check latest DHS updates]
a <- c("AO", "BJ", "BU", "ET", "GH", "GN", "GU", "HT", "IA", "KE", "KH", "LS", "ML", "MM", "MW", "NG", "NP", "PK", "RW", "TD", "TJ", "TL", "TZ", "UG", "ZA", "ZM", "ZW")

#i <- "PK"

for (i in a){
  df_tmp <- fread(paste("./Precipitation, vaccination and child health/1_Data/data_for_analysis/",i,".csv",sep=""), header=T)[,-1] %>% 
    mutate(wealth = as.integer(wealth)) %>% 
    mutate(cntry.code = i) %>%
    mutate(m15 = as.character(m15)) %>% 
    mutate(m4 = as.character(m4)) %>% 
    mutate(stratum = as.character(stratum)) %>% 
    mutate(DHSCC = i)
  
  #bind together the country data
  df_combined <- bind_rows(df_combined, df_tmp) 
  
}


## Generate variables for vacctiona status
df_combined <-  df_combined %>% 
  dplyr::rename(rcvd.PCV1    = h54,
                rcvd.PCV2    = h55,
                rcvd.PCV3    = h56,
                rcvd.Rota1     = h57,
                rcvd.Rota2     = h58) %>% 
  mutate(PCV1.vac  = ifelse(rcvd.PCV1  == "vaccination date on card" | rcvd.PCV1  == "reported by mother" | rcvd.PCV1   == "vaccination marked on card", 1, 0)) %>% 
  mutate(PCV2.vac  = ifelse(rcvd.PCV1  == "vaccination date on card" | rcvd.PCV2  == "reported by mother" | rcvd.PCV2   == "vaccination marked on card", 1, 0)) %>% 
  mutate(PCV3.vac  = ifelse(rcvd.PCV1  == "vaccination date on card" | rcvd.PCV3  == "reported by mother" | rcvd.PCV3   == "vaccination marked on card", 1, 0)) %>% 
  mutate(Rota1.vac   = ifelse(rcvd.Rota1   == "vaccination date on card" | rcvd.Rota1   == "reported by mother" | rcvd.Rota1    == "vaccination marked on card", 1, 0)) %>% 
  mutate(Rota2.vac   = ifelse(rcvd.Rota2   == "vaccination date on card" | rcvd.Rota2   == "reported by mother" | rcvd.Rota2    == "vaccination marked on card", 1, 0)) %>% 
  mutate(PCV.vac.full  = ifelse(PCV1.vac == 1 & PCV2.vac == 1 & PCV3.vac == 1, 1, 0)) %>% 
  mutate(Rota.vac.full  = ifelse(Rota1.vac == 1 & Rota2.vac == 1, 1, 0)) %>% 
  mutate(DTP.vac = ifelse((DTP1.vac + DTP2.vac + DTP3.vac) == 3, "yes",
                          ifelse((DTP1.vac + DTP2.vac + DTP3.vac) == 0, "no", "partly"))) %>% 
  mutate(OPV.vac = ifelse((OPV1.vac + OPV2.vac + OPV3.vac) == 3, "yes",
                          ifelse((OPV1.vac + OPV2.vac + OPV3.vac) == 0, "no", "partly"))) %>% 
  mutate(PCV.vac = ifelse((PCV1.vac + PCV2.vac + PCV3.vac) == 3, "yes",
                          ifelse((PCV1.vac + PCV2.vac + PCV3.vac) == 0, "no", "partly"))) %>% 
  mutate(Rota.vac = ifelse((Rota1.vac + Rota2.vac) == 2, "yes",
                           ifelse((Rota1.vac + Rota2.vac) == 0, "no", "partly"))) %>% 
  mutate(DTP.vac.full = ifelse(DTP.vac == "yes", 1, 0)) %>% 
  mutate(OPV.vac.full = ifelse(OPV.vac == "yes", 1, 0)) 


# Calculate shares
shares_age_12_23 <- df_combined %>%        
  filter(age.months>=12 & age.months<=23) %>% 
  group_by(DHSCC, region) %>% 
  summarise(FIC = weighted.mean(FIC, wt, na.rm=T),
            PIC = weighted.mean(PIC, wt, na.rm=T),
            Diarrhea = weighted.mean(diarrhea, wt, na.rm=T),
            Fever = weighted.mean(fever, wt, na.rm=T),
            Cough = weighted.mean(cough, wt, na.rm=T),
            ARI = weighted.mean(ARI, wt, na.rm=T), 
            BCG = weighted.mean(BCG.vac, wt, na.rm=T),
            MCV = weighted.mean(MCV.vac, wt, na.rm=T),
            DTP = weighted.mean(DTP.vac.full, wt, na.rm=T),
            OPV = weighted.mean(OPV.vac.full, wt, na.rm=T),
            Rota = weighted.mean(Rota.vac.full, wt, na.rm=T)) %>% 
  mutate(region = str_to_title(region)) %>% #capitalise region names
  rename(REGNAME = region)


