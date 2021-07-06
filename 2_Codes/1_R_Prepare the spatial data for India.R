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

setwd("C:/Users/annak/Dropbox/Projects/2021_San Diego/Vaccination - spatial analysis/")


#### Import the individual level data for India
## For preparing the input data see project folder: Precipitaiton, vaccination and child health
## DHS Guide to vaccination: https://dhsprogram.com/data/Guide-to-DHS-Statistics/Vaccination.htm
df0 <- read.csv("./1_Data/DHS_data_for_analysis.csv") %>% 
  dplyr::select(
    #Child characteristics
    psu, bord, agemonths, ageyears, position,
    bcgvac, dtp1vac, dtp2vac, dtp3vac, dtpvac, opv0vac, opv1vac, opv2vac, opv3vac, opvvac, mcvvac,
    rota1vac, rota2vac, rotavacfull, rotavac, pcv1vac, pcv2vac, pcv3vac, pcvvacfull, pcvvac,
    fic, pic, vactotal, healthcard, 
    #Infectious disease symptoms
    diarrhea, fever, cough, ari,
    #Household & maternal characteristics
    wt, hhwt, region, residence, edulevel, literate, resident, wealth, wealthscorenew, cntrycode) %>% 
  mutate(dtpvacfull = ifelse(dtpvac == "yes", 1, 0)) %>% 
  mutate(opvvacfull = ifelse(opvvac == "yes", 1, 0)) %>% 
  filter(cntrycode == "IA") %>% 
  #Generate new variable for fully immunized child (FIC) - child received all 8 vaccines
  mutate(fic = ifelse(bcgvac==1 & dtp1vac==1 & dtp2vac==1 & dtp3vac==1 & opv1vac==1 & opv2vac==1 & opv3vac==1 & mcvvac==1, 1, 0))

#### Import the geographical covariates for India (add ADMIN-1 and ADMIN-2 names and codes to the main data)
spdf <-readOGR("C:/Users/annak/Dropbox/Data/DHS/DHS 7/GE/IAGE71FL/IAGE71FL.shp")
df1 <- as.data.frame(spdf) %>% 
  dplyr::select(DHSCLUST, ADM1NAME, ADM1DHS, DHSREGNA, DHSREGCO) %>% 
  rename(psu       = DHSCLUST,
         reg.name  = ADM1NAME,
         reg.code  = ADM1DHS,
         dist.name = DHSREGNA,
         dist.code = DHSREGCO) %>% 
  mutate(cntrycode = "IA") %>% 
  mutate(dist.name = str_to_title(dist.name)) %>% #capitalise district names
  mutate(dist.name = ifelse(dist.name == "Lahul And Spiti", "Lahul and Spiti", dist.name)) %>% 
  mutate(dist.name = ifelse(dist.name == "North District", "North  District", dist.name)) %>% 
  mutate(dist.name = ifelse(dist.name == "Y.s.r.", "Y.S.R.", dist.name)) %>% 
  mutate(reg.name = as.character(reg.name)) %>% 
  mutate(reg.name = ifelse(reg.name == "Andaman & Nicobar Islands", "Andaman and Nicobar Islands", reg.name)) %>% 
  mutate(reg.name = ifelse(reg.name == "Dadra & Nagar Haveli", "Dadra and Nagar Haveli", reg.name)) %>% 
  mutate(reg.name = ifelse(reg.name == "Daman & Diu", "Daman and Diu", reg.name))

df0 <- left_join(df0, df1)


#### Agregate the data to the district level (ADMIN-2)
df2 <- df0 %>%        
  #filter(agemonths>=12 & agemonths<=23) %>% 
  filter(agemonths>=12) %>% 
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
  ungroup()

#### Import the subnational boundaries for India (ADMIN-2 level)
## download the boundaries from the DHS spatial reposity at the desired level: https://spatialdata.dhsprogram.com/boundaries/#view=table&countryId=IA
bound_adm2 <- sf::st_read("./1_Data/dhs_spatial_data/IA/shps/sdr_subnational_boundaries2.shp") %>% 
  dplyr::select(DHSCC, REGCODE, REGNAME, SVYNOTES) %>% 
  rename(cntrycode = DHSCC,
         dist.code = REGCODE,
         dist.name = REGNAME)

#plot(bound_adm2[,1])
#class(bound_adm2)

#### Join the DHS district level data with the geographical boundaries
df3 <- left_join(bound_adm2, df2) 

#df4 <- dplyr::select(as.data.frame(df3), -geometry)
#df5 <- df4[duplicated(df4$dist.code),] #find duplicates
#rm(list=setdiff(ls(), c("df3")))

save(df3, file="./2_Codes/Data_India_district_level.RData")
save(df0, file="./2_Codes/Data_India_individual_level.RData")

#### Plot the data
ggplot(data = df3) +
  geom_sf(aes(fill = FIC), col="grey50",  size = 0.05) +
  scale_fill_viridis_c(option = "plasma", labels = scales::percent_format(accuracy = 1)) + #limits=c(0, 1), breaks=seq(0, 1, by=0.25)
  labs(fill = "% vaccinated") +
  #coord_sf(crs = "+proj=eqearth") + 
  theme_minimal() +
  ggsave("./4_Figures/Map_India_FIC.png", width = 8, height=6, dpi=300)
  










