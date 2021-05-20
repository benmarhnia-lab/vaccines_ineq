################################################################# 
#################################################################
#######                      PROJECT:                     ####### 
#######     VACCINATION AND HEALTH OUTCOMES IN LMICs      ####### 
#######                                                   ####### 
#######       CODE: PREPARE THE DATA FOR ANALYSIS         #######
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
options(scipen=999)
options(digits=5)


################################################################################################
### 1. Aggregate individual data to lowest admin level for every country
################################################################################################

# NOTES:
# - sample already restricted to children who are alive at the time of the interview (b5 == "yes")
# - samples includes children under 3 years of age at the time of the interview
# - "wt" is mother's weight   

setwd("C:/Users/annak/Dropbox/Projects/2020_San Diego/")

# Create an empty data frame where country level data will be stored
shares_age_12_23 <- data.frame(matrix(ncol=8,nrow=0, dimnames=list(NULL, c("region", "FIC", "PIC", "Diarrhea", "Fever", "Cough", "ARI", "cntry.code"))))

# Create a list of countries to be included in the analysis
# [Potentially a few more could be included - check latest DHS updates]
a <- c("AO", "BJ", "BU", "ET", "GH", "GN", "GU", "HT", "IA", "KE", "KH", "LS", "ML", "MM", "MW", "NG", "NP", "PK", "RW", "TD", "TJ", "TL", "TZ", "UG", "ZA", "ZM", "ZW")

#i <- "TD"

for (i in a){
  tmp_dhs <- fread(paste("./Precipitation, vaccination and child health/1_Data/data_for_analysis/",i,".csv",sep=""), header=T)[,-1] %>% 
    mutate(stratum = as.character(stratum)) 
  
  tmp_shares_age_12_23 <- tmp_dhs %>% 
    filter(age.months>=12 & age.months<=23) %>% 
    group_by(region) %>% 
    summarise(FIC = weighted.mean(FIC, wt, na.rm=T),
              PIC = weighted.mean(PIC, wt, na.rm=T),
              Diarrhea = weighted.mean(diarrhea, wt, na.rm=T),
              Fever = weighted.mean(fever, wt, na.rm=T),
              Cough = weighted.mean(cough, wt, na.rm=T),
              ARI = weighted.mean(ARI, wt, na.rm=T)
              ) %>% 
    mutate(region = str_to_title(region)) %>% #Capitalise region names
    mutate(DHSCC = i) %>% 
    rename(REGNAME = region)
  
  shares_age_12_23 <- rbind(shares_age_12_23, tmp_shares_age_12_23)
  
}



################################################################################################
### 2. Retrieve subnational boundaries 
################################################################################################

# Upload the country-specific spatial boundaries 
sd_Angola <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Angola", level = 1) 
sd_Benin <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Benin", level = 1)
sd_Burundi <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Burundi", level = 1)
sd_Ethiopia <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Ethiopia", level = 1)
sd_Ghana <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Ghana", level = 1)
sd_Guinea <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Guinea", level = 1)
sd_Guatemala <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Guatemala", level = 1)
sd_Haiti <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Haiti", level = 1)
sd_India <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "India", level = 1)
sd_Kenya <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Kenya", level = 1)
sd_Cambodia <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Cambodia", level = 1)
sd_Lesotho <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Lesotho", level = 1) 
sd_Mali <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Mali", level = 1)
sd_Myanmar <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Myanmar", level = 1)
sd_Malawi <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Malawi", level = 1)
sd_Nigeria <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Nigeria", level = 1)
sd_Nepal <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Nepal", level = 1)
sd_Pakistan <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Pakistan", level = 1)
sd_Rwanda <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Rwanda", level = 1)
sd_Chad <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Chad", level = 1)
sd_Tajikistan <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Tajikistan", level = 1)
sd_Timor_Leste <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Timor-Leste", level = 1)
sd_Tanzania <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Tanzania", level = 1)
sd_Uganda <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Uganda", level = 1)
sd_South_Africa <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "South Africa", level = 1)
sd_Zambia <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Zambia", level = 1)
sd_Zimbabwe <- getData(name = "GADM", path = "./Vaccination - spatial analysis/1_Data/gadm/", country = "Zimbabwe", level = 1)


sd_all <- rbind(sd_Angola, sd_Benin, sd_Burundi, sd_Ethiopia, sd_Ghana, sd_Guinea, sd_Guatemala, sd_Haiti, sd_India, sd_Kenya, sd_Cambodia,
                sd_Lesotho, sd_Mali, sd_Myanmar, sd_Malawi, sd_Nigeria, sd_Nepal, sd_Pakistan, sd_Rwanda,  sd_Chad, sd_Tajikistan, sd_Timor_Leste,
                sd_Tanzania, sd_Uganda, sd_South_Africa, sd_Zambia, sd_Zimbabwe)
#plot(sd_all)


################################################################################################
### 3. Match the DHS data with the GADM boundaries
################################################################################################

# Harmonize region's names
df_all <- sd_all@data %>% 
  mutate(cntry.code = substr(HASC_1, 1, 2)) %>%         #add country codes corresponding to the DHS
  mutate(cntry.code = ifelse(cntry.code == "BI", "BU",
                      ifelse(cntry.code == "GT", "GU",
                      ifelse(cntry.code == "IN", "IA",
                      ifelse(cntry.code == "TP", "TL", cntry.code))))) %>% 
  mutate(cntry.code = ifelse(GID_0 == "GIN", "GN", cntry.code)) %>% 
  #select("NAME_0", "NAME_1", "cntry.code") %>% 
  rename(region = NAME_1)


# Cambodia (KH)
df_all <- df_all %>% 
  mutate(region = ifelse(cntry.code == "KH" & region == "Bântéay Méanchey", "Banteay Mean Chey",
                  ifelse(cntry.code == "KH" & region == "Batdâmbâng", "Battambang & Pailin",
                  ifelse(cntry.code == "KH" & region == "Kâmpóng Cham", "Kampong Cham",
                  ifelse(cntry.code == "KH" & region == "Kâmpóng Chhnang", "Kampong Chhnang",
                  ifelse(cntry.code == "KH" & region == "Kâmpóng Spo", "Kampong Speu",
                  ifelse(cntry.code == "KH" & region == "Kâmpóng Thum", "Kampong Thom",
                  ifelse(cntry.code == "KH" & region == "Kâmpôt", "Kampot & Kep",
                  ifelse(cntry.code == "KH" & region == "Kândal", "Kandal",
                  ifelse(cntry.code == "KH" & region == "Kep", "Kampot & Kep",
                  ifelse(cntry.code == "KH" & region == "Kaôh Kong", "Preah Sihanouk & Kaoh Kong",
                  ifelse(cntry.code == "KH" & region == "Krâchéh", "Kratie",
                  ifelse(cntry.code == "KH" & region == "Krong Pailin", "Battambang & Pailin",
                  ifelse(cntry.code == "KH" & region == "Krong Preah Sihanouk", "Preah Sihanouk & Kaoh Kong",
                  ifelse(cntry.code == "KH" & region == "Môndól Kiri", "Mondol Kiri & Rattanak Kiri",
                  #ifelse(cntry.code == "KH" & region == "Otdar Mean Chey", "Otdar Mean Chey",
                  #ifelse(cntry.code == "KH" & region == "Phnom Penh", "Phnom Penh",
                  ifelse(cntry.code == "KH" & region == "Pouthisat", "Pursat",
                  ifelse(cntry.code == "KH" & region == "Preah Vihéar", "Preah Vihear & Steung Treng",
                  ifelse(cntry.code == "KH" & region == "Prey Vêng", "Prey Veng",
                  ifelse(cntry.code == "KH" & region == "Rôtânôkiri", "Mondol Kiri & Rattanak Kiri",
                  ifelse(cntry.code == "KH" & region == "Siemréab", "Siem Reap",
                  ifelse(cntry.code == "KH" & region == "Stong Trêng", "Preah Vihear & Steung Treng",
                  ifelse(cntry.code == "KH" & region == "Svay Rieng", "Svay Rieng",
                  ifelse(cntry.code == "KH" & region == "Takêv", "Takeo",
                  ifelse(cntry.code == "KH" & region == "Tbong Khmum", "Kampong Cham",
                         region))))))))))))))))))))))))
  

# Malawi (MW)
df_all <- df_all %>% 
  mutate(region = ifelse(cntry.code == "MW" & region == "Balaka", "",
                  ifelse(cntry.code == "MW" & region == "Blantyre", "",
                  ifelse(cntry.code == "MW" & region == "Chikwawa", "",
                  ifelse(cntry.code == "MW" & region == "Chiradzulu", "",
                  ifelse(cntry.code == "MW" & region == "Chitipa", "",
                  ifelse(cntry.code == "MW" & region == "Dedza", "",
                  ifelse(cntry.code == "MW" & region == "Dowa", "",
                  ifelse(cntry.code == "MW" & region == "Karonga", "",
                  ifelse(cntry.code == "MW" & region == "Kasungu", "",
                  ifelse(cntry.code == "MW" & region == "Likoma", "",
                  ifelse(cntry.code == "MW" & region == "Lilongwe", "",
                  ifelse(cntry.code == "MW" & region == "Machinga", "",
                  ifelse(cntry.code == "MW" & region == "Mangochi", "",
                  ifelse(cntry.code == "MW" & region == "Mchinji", "",
                  ifelse(cntry.code == "MW" & region == "Mulanje", "",
                  ifelse(cntry.code == "MW" & region == "Mwanza", "",
                  ifelse(cntry.code == "MW" & region == "Mzimba", "",
                  ifelse(cntry.code == "MW" & region == "Neno", "",
                  ifelse(cntry.code == "MW" & region == "Nkhata Bay", "",
                  ifelse(cntry.code == "MW" & region == "Nkhotakota", "",
                  ifelse(cntry.code == "MW" & region == "Nsanje", "",
                  ifelse(cntry.code == "MW" & region == "Ntcheu", "",
                  ifelse(cntry.code == "MW" & region == "Ntchisi", "",
                  ifelse(cntry.code == "MW" & region == "Phalombe", "",
                  ifelse(cntry.code == "MW" & region == "Rumphi", "",
                  ifelse(cntry.code == "MW" & region == "Salima", "",
                  ifelse(cntry.code == "MW" & region == "Thyolo", "",
                  ifelse(cntry.code == "MW" & region == "Zomba", "",
                         region)))))))))))))))))))))))))))))
                  



  
df_all_combined <- df_all %>% 
  full_join(shares_age_12_23)

df_sub <- df_all %>% 
  filter(cntry.code=="MW")
shares_sub <- shares_age_12_23 %>% 
  filter(cntry.code=="MW")




# Convert the SpatialPolyginDataFrame to an object of class sf 
sf_all <- st_as_sf(sd_all) %>% 
  mutate(cntry.code = substr(HASC_1, 1, 2)) %>%         #add country codes corresponding to the DHS
  mutate(cntry.code = ifelse(cntry.code == "BI", "BU",
                             ifelse(cntry.code == "GT", "GU",
                                    ifelse(cntry.code == "IN", "IA",
                                           ifelse(cntry.code == "TP", "TL", cntry.code))))) %>% 
  mutate(cntry.code = ifelse(GID_0 == "GIN", "GN", cntry.code)) %>% 
  rename(region = NAME_1) %>% 
  mutate(region = ifelse(cntry.code == "KH" & region == "Bântéay Méanchey", "Banteay Mean Chey",
                         ifelse(cntry.code == "KH" & region == "Batdâmbâng", "Battambang & Pailin",
                                ifelse(cntry.code == "KH" & region == "Kâmpóng Cham", "Kampong Cham",
                                       ifelse(cntry.code == "KH" & region == "Kâmpóng Chhnang", "Kampong Chhnang",
                                              ifelse(cntry.code == "KH" & region == "Kâmpóng Spo", "Kampong Speu",
                                                     ifelse(cntry.code == "KH" & region == "Kâmpóng Thum", "Kampong Thom",
                                                            ifelse(cntry.code == "KH" & region == "Kâmpôt", "Kampot & Kep",
                                                                   ifelse(cntry.code == "KH" & region == "Kândal", "Kandal",
                                                                          ifelse(cntry.code == "KH" & region == "Kep", "Kampot & Kep",
                                                                                 ifelse(cntry.code == "KH" & region == "Kaôh Kong", "Preah Sihanouk & Kaoh Kong",
                                                                                        ifelse(cntry.code == "KH" & region == "Krâchéh", "Kratie",
                                                                                               ifelse(cntry.code == "KH" & region == "Krong Pailin", "Battambang & Pailin",
                                                                                                      ifelse(cntry.code == "KH" & region == "Krong Preah Sihanouk", "Preah Sihanouk & Kaoh Kong",
                                                                                                             ifelse(cntry.code == "KH" & region == "Môndól Kiri", "Mondol Kiri & Rattanak Kiri",
                                                                                                                    #ifelse(cntry.code == "KH" & region == "Otdar Mean Chey", "Otdar Mean Chey",
                                                                                                                    #ifelse(cntry.code == "KH" & region == "Phnom Penh", "Phnom Penh",
                                                                                                                    ifelse(cntry.code == "KH" & region == "Pouthisat", "Pursat",
                                                                                                                           ifelse(cntry.code == "KH" & region == "Preah Vihéar", "Preah Vihear & Steung Treng",
                                                                                                                                  ifelse(cntry.code == "KH" & region == "Prey Vêng", "Prey Veng",
                                                                                                                                         ifelse(cntry.code == "KH" & region == "Rôtânôkiri", "Mondol Kiri & Rattanak Kiri",
                                                                                                                                                ifelse(cntry.code == "KH" & region == "Siemréab", "Siem Reap",
                                                                                                                                                       ifelse(cntry.code == "KH" & region == "Stong Trêng", "Preah Vihear & Steung Treng",
                                                                                                                                                              ifelse(cntry.code == "KH" & region == "Svay Rieng", "Svay Rieng",
                                                                                                                                                                     ifelse(cntry.code == "KH" & region == "Takêv", "Takeo",
                                                                                                                                                                            ifelse(cntry.code == "KH" & region == "Tbong Khmum", "Kampong Cham",
                                                                                                                                                                                   region))))))))))))))))))))))))


unique(sf_all$cntry.code)


sf_all_sub <- sf_all %>% 
  filter(cntry.code == "KH") %>% 
  left_join(shares_age_12_23)

################################################################################################
### 4. Plot the results
################################################################################################

# set ggplot theme
theme_set(theme_bw())
theme_update(panel.grid.minor = element_blank())

ggplot(data = sf_all_sub) +
  geom_sf(col="grey50",  size = 0.25) +
  geom_sf(aes(fill = PIC), size = 0.25) +
  #scale_fill_brewer(palette="YlOrRd", na.translate = F) + 
  #labs(fill = "PIC") +
  coord_sf(crs = "+proj=eqearth") + 
  theme_minimal() 
  #+theme(axis.text = element_blank()) 
  #+ggsave("Map_studies.png", width = 8, height=4.5, dpi=600)


########################################################################################


# Retrieve country boundaries for GH
sd_GH <- sf::st_read("./Vaccination - spatial analysis/1_Data/dhs_spatial_data/GH/shps/sdr_subnational_boundaries.shp")

# Create a list of countries to be included in the analysis
# [Potentially a few more could be included - check latest DHS updates]

sd_sub <- sd_GH %>% 
  filter(DHSCC %in% a) %>% 
  mutate(REGNAME = str_to_title(REGNAME)) %>% #Capitalise region names
  mutate(REGNAME = as.character(REGNAME),
         DHSREGSP = as.character(DHSREGSP),
         DHSCC = as.character(DHSCC)) %>% 
  mutate(REGNAME = ifelse(DHSCC == "GU", DHSREGSP, REGNAME)) %>% 
  mutate(REGNAME = ifelse(DHSCC == "GU" & REGNAME == "Nor Oriente", "Nororiente",
                          ifelse(DHSCC == "GU" & REGNAME == "Sur Oriente", "Suroriente",
                                 ifelse(DHSCC == "GU" & REGNAME == "Sur Occidente", "Suroccidente",
                                        ifelse(DHSCC == "GU" & REGNAME == "Nor Occidente", "Noroccidente",
                                               REGNAME))))) %>%
  mutate(REGNAME = ifelse(DHSCC == "TJ", as.character(DHSREGEN), REGNAME)) %>% 
  mutate(REGNAME = ifelse(DHSCC == "TJ" & REGNAME == "GBAO", "Gbao", 
                          ifelse(DHSCC == "TJ" & REGNAME == "DRS", "Drs", REGNAME))) %>% 
  mutate(REGNAME = ifelse(DHSCC == "TL" & REGNAME == "Liquica", "Liquiçá", REGNAME)) %>% 
  left_join(shares_age_12_23) 

df_sub <- as.data.frame(sd_sub) %>% 
  filter(DHSCC == "ZW") %>% 
  select(DHSCC, REGNAME, FIC) 
