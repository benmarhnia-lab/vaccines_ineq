################################################################# 
#################################################################
#######                                                   ####### 
#######                      PROJECT:                     ####### 
#######     VACCINATION AND HEALTH OUTCOMES IN LMICs      ####### 
#######                                                   ####### 
#################################################################
#################################################################

rm(list =ls())
library(tidyverse)
library(rgdal)
#install.packages("rdhs")
#library(rdhs) #packages is needed for retrieving the country boundaries
library(tidyverse)

setwd("C:/Users/annak/Dropbox/Projects/2021_San Diego/Vaccination - spatial analysis/")


#### Import the individual level data for India
## For preparing the input data see project folder: Precipitaiton, vaccination and child health
## DHS Guide to vaccination: https://dhsprogram.com/data/Guide-to-DHS-Statistics/Vaccination.htm
df0 <- read.csv("./1_Data/DHS_data_for_analysis.csv") %>% 
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
    #Other information
    latdhs, longdhs, climzone, cntrycode) %>% 
  mutate(dtpvacfull = ifelse(dtpvac == "yes", 1, 0)) %>% 
  mutate(opvvacfull = ifelse(opvvac == "yes", 1, 0)) %>% 
  filter(cntrycode == "IA")

#### Import the geographical covariates for India (extract district information)
spdf <-readOGR("C:/Users/annak/Dropbox/Data/DHS/GE/IAGE71FL/IAGE71FL.shp")
df1 <- as.data.frame(spdf) %>% 
  dplyr::select(DHSCC, DHSCLUST, DHSREGNA, DHSREGCO) %>% 
  rename(psu = DHSCLUST,
         dist.name = DHSREGNA,
         dist.code = DHSREGCO) %>% 
  mutate(cntrycode = "IA")

df0 <- df0 %>% 
  left_join(df1)

#### Agregate the data to the district level
df2 <- df0 %>%        
  filter(agemonths>=12 & agemonths<=23) %>% 
  group_by(cntrycode, dist.name, dist.code) %>% 
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
            PCV = weighted.mean(pcvvacfull, wt, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(dist.name = str_to_title(dist.name)) %>% #capitalise district names
  mutate(dist.name = ifelse(dist.name == "Lahul And Spiti", "Lahul and Spiti", dist.name)) %>% 
  mutate(dist.name = ifelse(dist.name == "North District", "North  District", dist.name)) %>% 
  mutate(dist.name = ifelse(dist.name == "Y.s.r.", "Y.S.R.", dist.name))
  

#### Import the subnational boundaries for India (ADMIN2 level)
## download the boundaries from the DHS spatial reposity at the desired level: https://spatialdata.dhsprogram.com/boundaries/#view=table&countryId=IA
bound <- sf::st_read("./1_Data/dhs_spatial_data/IA/shps/sdr_subnational_boundaries2.shp") %>% 
  dplyr::select(DHSCC, REGCODE, REGNAME, SVYNOTES) %>% 
  rename(cntrycode = DHSCC,
         dist.code = REGCODE,
         dist.name = REGNAME)

#plot(bound[,1])
#class(bound)

df3 <-  bound %>% 
  left_join(df2) 

#df4 <- dplyr::select(as.data.frame(df3), -geometry)
#df5 <- df4[duplicated(df4$dist.code),] #find duplicates

rm(list=setdiff(ls(), c("df3")))

save(df3, file="./2_Codes/Data_India.RData")

#### Plot the data
theme_set(theme_bw())
theme_update(panel.grid.minor = element_blank())

ggplot(data = df3) +
  geom_sf(aes(fill = OPV), col="grey50",  size = 0.05) +
  scale_fill_viridis_c(option = "plasma", labels = scales::percent_format(accuracy = 1)) + #limits=c(0, 1), breaks=seq(0, 1, by=0.25)
  labs(fill = "% vaccinated") +
  #coord_sf(crs = "+proj=eqearth") + 
  theme_bw() 
#+ggsave("./4_Figures/Map_India_OPV.png", width = 8, height=6, dpi=300)
  










