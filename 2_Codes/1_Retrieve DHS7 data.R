################################################################################
################################################################################
#######                                                                  ####### 
#######                             PROJECT:                             ####### 
#######          VACCINATION INEQUALITY - SPATIAL ANALYSIS               ####### 
#######                                                                  ####### 
#######                    CODE: PREPARE THE DATA                        #######
################################################################################
################################################################################

# CONTENTS:
#
# 1. Retrieve the survey data
# 2. Retrieve the geographical boundaries (at sub-national level)
# 3. Match the survey data with the geographical boundaries
# 4. Generate sub-national aggregates for full immunization coverage


#install.packages("tidyverse")
library(tidyverse)
#install.packages("devtools")
#devtools::install_github("ropensci/rdhs", ref = "issue33_path", force = TRUE)
library(rdhs)         
library(data.table)
library(ggplot2)
library(foreign)
options(scipen=999)
options(digits=5)

## Set up your DHS credentials
set_rdhs_config(email = XXX,
                project = XXX)


set_rdhs_config(email = XXX,
                project = XXX,
                config_path = "rdhs.json",
                cache_path = "dhs 7",
                global = FALSE)

rm(list =ls())

setwd("C:/Users/Vaccination - spatial analysis/1_Data/")

################################################################################
## 1. Retrieve the survey data
################################################################################
## Make a list of eligible surveys and download them
surveys <- dhs_datasets()
str(surveys)

surveys_IDs <- surveys %>% 
  dplyr::filter(SurveyType == "DHS") %>% 
  dplyr::filter(SurveyYear >= 2014) %>% 
  dplyr::filter(FileFormat == "Stata dataset (.dta)") %>% 
  dplyr::filter(FileType == "Children's Recode")

## Keep only latest survey for each country
surveys_IDs <- surveys_IDs %>% 
  dplyr::filter(FileName != "BDKR72DT.ZIP" & FileName != "PGKR71DT.ZIP" & FileName != "ETKR71DT.ZIP" & FileName != "SNKR70DT.zip" & FileName != "SNKR7HDT.ZIP" & FileName != "SNKR7IDT.ZIP" & FileName != "SNKR7ZDT.ZIP" & FileName != "SNKR81DT.ZIP" & FileName != "SNKR8BDT.ZIP")	


downloads <- get_datasets(surveys_IDs$FileName, reformat=TRUE, clear_cache = TRUE)
print(downloads)
#vec <- unlist(downloads)
#vec

## Select relevant variables
vars = c("midx", "v001", "v002", "v003", "v005", "v006", "v007", "v008", "v016", "v135", "v190", "v191", "b3", "b5", "b4","b8", "v106", "v133", "v023", "v024", "v025", "h1", "h0", "h2", "h3", "h4", "h5", "h6", "h7", "h8", "h9",
         ##DTP vaccination variables for Albania are different
         "h51", "h52", "h53",
         ##variable for recalculating the wealth index if necessary to harmonize across surveys
         "v127", "v128", "v129", "v161", "v119", "v120", "v121", "v122", "v123", "v124", "v125"
         )

questions <- search_variables(surveys_IDs$FileName, variables = vars,  reformat=TRUE)


## Extract the data (adding geographical covariates: add_geo = TRUE)
extract <- extract_dhs(questions, add_geo = T)

## quick check 
head(extract[1])

df0 <- rbindlist(extract, use.names=TRUE, fill=TRUE)
str(df0)

## Clean the data and generate immunization variables
df1 <- df0 %>% 
  filter(b5 == "yes") %>%            #keep observation only if child is alive
  dplyr::rename(psu          = v001, 
                hh.id        = v002,
                year         = v007, #year of interview
                month        = v006, #month of interview
                day          = v016, #day of interview
                resident     = v135,
                wealth       = v190,
                wealth.score = v191,
                health.card  = h1,
                age.years    = b8,
                sex          = b4,
                edu.level    = v106,
                edu.years    = v133,
                region       = v024,
                residence    = v025,
                stratum      = v023,
                rcvd.BCG     = h2,
                rcvd.OPV0    = h0,
                rcvd.OPV1    = h4,
                rcvd.OPV2    = h6,
                rcvd.OPV3    = h8,
                rcvd.DTP1    = h3,
                rcvd.DTP2    = h5,
                rcvd.DTP3    = h7,
                rcvd.MCV     = h9
  ) %>%
  mutate(wt=v005/1000000) %>%       #generate weights for the mother
  mutate(age.months = v008-b3) %>%  #age of the child in months
  filter(age.months >=12 & age.months <= 35) %>%  #restrict the sample to children over 12 and under 35 months of age
  mutate(DHSCC = substr(SurveyId, 1, 2)) %>%      #add country code identifier 
  ## generate vaccinaion variables
  mutate(BCG.vac  = ifelse(rcvd.BCG  == "vaccination date on card" | rcvd.BCG  == "reported by mother" | rcvd.BCG   == "vaccination marked on card", 1, 0)) %>% 
  mutate(DTP1.vac = ifelse(rcvd.DTP1 == "vaccination date on card" | rcvd.DTP1 == "reported by mother" | rcvd.DTP1  == "vaccination marked on card", 1, 0)) %>% 
  mutate(DTP2.vac = ifelse(rcvd.DTP2 == "vaccination date on card" | rcvd.DTP2 == "reported by mother" | rcvd.DTP2  == "vaccination marked on card", 1, 0)) %>% 
  mutate(DTP3.vac = ifelse(rcvd.DTP3 == "vaccination date on card" | rcvd.DTP3 == "reported by mother" | rcvd.DTP3  == "vaccination marked on card", 1, 0)) %>% 
  mutate(OPV0.vac = ifelse(rcvd.OPV0 == "vaccination date on card" | rcvd.OPV0 == "reported by mother" | rcvd.OPV0  == "vaccination marked on card", 1, 0)) %>% 
  mutate(OPV1.vac = ifelse(rcvd.OPV1 == "vaccination date on card" | rcvd.OPV1 == "reported by mother" | rcvd.OPV1  == "vaccination marked on card", 1, 0)) %>% 
  mutate(OPV2.vac = ifelse(rcvd.OPV2 == "vaccination date on card" | rcvd.OPV2 == "reported by mother" | rcvd.OPV2  == "vaccination marked on card", 1, 0)) %>% 
  mutate(OPV3.vac = ifelse(rcvd.OPV3 == "vaccination date on card" | rcvd.OPV3 == "reported by mother" | rcvd.OPV3  == "vaccination marked on card", 1, 0)) %>% 
  mutate(MCV.vac  = ifelse(rcvd.MCV  == "vaccination date on card" | rcvd.MCV  == "reported by mother" | rcvd.MCV   == "vaccination marked on card", 1, 0)) %>% 
  ## correct the immunization data for Albania (different DTP variables)
  mutate(BCG.vac  = ifelse(SurveyId == "AL2017DHS" & (rcvd.BCG  == "vaccination date on card" | rcvd.BCG  == "reported by mother" | rcvd.BCG   == "vaccination marked on card"), 1, BCG.vac)) %>% 
  mutate(DTP1.vac = ifelse(SurveyId == "AL2017DHS" & (h51 == "vaccination date on card" | h51 == "reported by mother" | h51  == "vaccination marked on card"), 1, DTP1.vac)) %>% 
  mutate(DTP2.vac = ifelse(SurveyId == "AL2017DHS" & (h52 == "vaccination date on card" | h52 == "reported by mother" | h52  == "vaccination marked on card"), 1, DTP2.vac)) %>% 
  mutate(DTP3.vac = ifelse(SurveyId == "AL2017DHS" & (h53 == "vaccination date on card" | h53 == "reported by mother" | h53  == "vaccination marked on card"), 1, DTP3.vac)) %>% 
  mutate(OPV1.vac = ifelse(SurveyId == "AL2017DHS" & (rcvd.OPV1 == "vaccination date on card" | rcvd.OPV1 == "reported by mother" | rcvd.OPV1  == "vaccination marked on card"), 1, OPV1.vac)) %>% 
  mutate(OPV2.vac = ifelse(SurveyId == "AL2017DHS" & (rcvd.OPV2 == "vaccination date on card" | rcvd.OPV2 == "reported by mother" | rcvd.OPV2  == "vaccination marked on card"), 1, OPV2.vac)) %>% 
  mutate(OPV3.vac = ifelse(SurveyId == "AL2017DHS" & (rcvd.OPV3 == "vaccination date on card" | rcvd.OPV3 == "reported by mother" | rcvd.OPV3  == "vaccination marked on card"), 1, OPV3.vac)) %>% 
  mutate(MCV.vac  = ifelse(SurveyId == "AL2017DHS" & (rcvd.MCV  == "vaccination date on card" | rcvd.MCV  == "reported by mother" | rcvd.MCV   == "vaccination marked on card"), 1, MCV.vac)) %>% 
  ## note that children without health cards are considered as not vaccinated in Albania
  mutate(BCG.vac = ifelse(SurveyId == "AL2017DHS" & health.card == "no card", 0, BCG.vac)) %>% 
  mutate(DTP1.vac = ifelse(SurveyId == "AL2017DHS" & health.card == "no card", 0, DTP1.vac)) %>% 
  mutate(DTP2.vac = ifelse(SurveyId == "AL2017DHS" & health.card == "no card", 0, DTP2.vac)) %>% 
  mutate(DTP3.vac = ifelse(SurveyId == "AL2017DHS" & health.card == "no card", 0, DTP3.vac)) %>% 
  mutate(OPV1.vac = ifelse(SurveyId == "AL2017DHS" & health.card == "no card", 0, OPV1.vac)) %>% 
  mutate(OPV2.vac = ifelse(SurveyId == "AL2017DHS" & health.card == "no card", 0, OPV2.vac)) %>% 
  mutate(OPV3.vac = ifelse(SurveyId == "AL2017DHS" & health.card == "no card", 0, OPV3.vac)) %>% 
  mutate(MCV.vac = ifelse(SurveyId == "AL2017DHS" & health.card == "no card", 0, MCV.vac)) %>%
  ## generate variables for full course DTP and OPV vaccines
  mutate(DTP.vac = ifelse(DTP1.vac == 1 & DTP2.vac == 1 & DTP3.vac == 1, 1, 0)) %>% 
  mutate(OPV.vac = ifelse(OPV1.vac == 1 & OPV2.vac == 1 & OPV3.vac == 1, 1, 0)) %>% 
  ## generate a variable for fully immunized child (FIC) and partially immunized child (PIC)
  mutate(FIC = ifelse((BCG.vac + DTP1.vac + DTP2.vac + DTP3.vac + OPV1.vac + OPV2.vac + OPV3.vac + MCV.vac) == 8 , 1, 0)) %>% 
  ## PIC of 0 denotes children with zero vaccine doses 
  mutate(PIC = ifelse(BCG.vac==1 | (DTP1.vac==1 & DTP2.vac==1 & DTP3.vac==1) | (OPV1.vac==1 & OPV2.vac==1 & OPV3.vac==1) | MCV.vac==1, 1, 0))

save.image(file='data_extract.RData')
rm(list =ls())

################################################################################
## 2. Retrieve the geographical boundaries (at sub-national level)
################################################################################

load("data_extract.RData")

## Download the subnational boundaries for all eligible surveys using surveyId
IDs <- surveys_IDs$SurveyId
IDs <- IDs[-1]
IDs

bord_list <- download_boundaries(surveyId = surveys_IDs$SurveyId[1], method = "sf")
bord_sf <- do.call(rbind.data.frame, bord_list) #convert to special feature dataframe

for (i in IDs) {
  bord_list_tmp <- download_boundaries(surveyId = i, method = "sf") #create a temporary file and loop through each survey
  bord_sf_tmp <- do.call(rbind.data.frame, bord_list_tmp)           #unlist (converts to special feature object) 
  bord_sf <- rbind(bord_sf, bord_sf_tmp)                            #bind the spatial data together
}

rownames(bord_sf) <- NULL
bord_df <- dplyr::select(as.data.frame(bord_sf), -geometry) 
unique(bord_df$DHSCC)

ggplot(bord_sf) + 
  geom_sf(aes()) 

#save.image(file='data_extract_with_bound.RData')

## Multiple boundaries available for some countries (e.g. region, district)
## Use the boundaries for the lowest administrative level 
admin_lvls <- unique(bord_df[c("DHSCC", "LEVELCO")])

bord_sf <- bord_sf %>% 
  mutate(drop = ifelse(LEVELCO == "Groups of Admin1 and the Capital City" | LEVELCO == "Admin 1 and the Capital city" | LEVELCO == "Groups of Governorates" | LEVELCO == "Ecological Regions" |  LEVELCO == "Development Regions", 1, 0)) %>% 
  mutate(drop = ifelse(LEVELCO == "Groups of Admin1" & (DHSCC == "EG"| DHSCC == "GU" | DHSCC == "KE" | DHSCC == "LB" | DHSCC == "MW" | DHSCC == "NG" | DHSCC == "TZ" | DHSCC == "UG" | DHSCC == "JO"), 1, drop)) %>% 
  mutate(drop = ifelse(LEVELCO == "Admin1" & (DHSCC == "RW" | DHSCC == "SL"  | DHSCC == "NP"), 1, drop)) %>% 
  subset(drop == 0)

## Combine district and region names for India (because district names are not unique across regions)
bord_sf <- bord_sf %>% 
  unite(adm.reg, OTHREGNA, REGNAME, sep = "-", remove = FALSE) %>% 
  mutate(DHSREGEN = ifelse(DHSCC == "IA", adm.reg, DHSREGEN)) %>% 
  mutate(region = DHSREGEN)

bord_df <- dplyr::select(as.data.frame(bord_sf), -geometry) 
admin_lvls <- unique(bord_df[c("DHSCC", "LEVELCO")])

save.image(file='data_extract_with_bound.RData')
rm(list =ls())

################################################################################
## 3. Match the survey data with the geographical boundaries
################################################################################
load("data_extract_with_bound.RData")

## Harmonize the region names in the survey and border data

#class(df1) <- class(as.data.frame(df1))
#class(df1)

## Add departments for Colombia (33 subnational areas)
admin_CO <- read.dta("./dhs_survey_data/COKR72DT/COKR72FL.DTA") %>%
  select_if(names(.)  %in% c("v001", "sdepto")) %>% 
  unique() %>% 
  mutate(DHSCC = "CO") %>% 
  rename(psu = v001) %>% 
  mutate(sdepto = as.character(sdepto))

## Add districts for Malawi
admin_MW <- read.dta("./dhs_survey_data/MWKR7ADT/MWKR7AFL.DTA") %>%
  select_if(names(.)  %in% c("v001", "sdist")) %>% 
  unique() %>% 
  mutate(DHSCC = "MW") %>% 
  rename(psu = v001) %>% 
  mutate(sdist = as.character(sdist))

df1 <- df1 %>% 
  left_join(admin_CO) %>% 
  left_join(admin_MW) %>% 
  ## combine district and region names for India (because district names are not unique across regions)
  unite(adm.reg, ADM1NAME, DHSREGNA, sep = "-", remove = FALSE) %>% 
  ## use the correct region names 
  mutate(region = ifelse(DHSCC == "CO", sdepto, region)) %>% 
  mutate(region = ifelse(DHSCC == "EG", ADM1NAME, region)) %>% 
  mutate(region = ifelse(DHSCC == "GU", ADM1NAME, region)) %>% 
  mutate(region = ifelse(DHSCC == "KE", ADM1NAME, region)) %>% 
  mutate(region = ifelse(DHSCC == "LB", ADM1NAME, region)) %>% 
  mutate(region = ifelse(DHSCC == "NG", ADM1NAME, region)) %>% 
  mutate(region = ifelse(DHSCC == "RW", DHSREGNA, region)) %>% 
  mutate(region = ifelse(DHSCC == "SL", DHSREGNA, region)) %>% 
  mutate(region = ifelse(DHSCC == "MW", sdist, region)) %>% 
  mutate(region = ifelse(DHSCC == "IA", adm.reg, region))

## Check if region names are harmonized across the survey and boundary data
match1 <- bord_df %>% 
  select(DHSCC, region) %>% 
  unique() %>% 
  mutate(region = str_to_title(region)) %>%                 #capitalize the region names
  unite(reg1, DHSCC, region, sep = "-", remove = FALSE) %>% #generate country-region identifier
  select(reg1)

match2 <- df1 %>% 
  select(DHSCC, region) %>% 
  unique() %>% 
  mutate(region = str_to_title(region)) %>%                 #capitalize the region names
  unite(reg2, DHSCC, region, sep = "-", remove = FALSE) %>% #generate country-region identifier
  select(reg2)

## Match regions names, taking the closest match
#install.packages("fuzzyjoin")
library(fuzzyjoin)

## match using max distance 1
match3 <- fuzzyjoin::stringdist_join(
  x = match1, 
  y = match2, 
  by = c("reg1" = "reg2"),
  method = "osa",
  ignore_case = TRUE,
  max_dist = 1,
  mode = "inner"
)

## find duplicates and remove them
n_occur <- data.frame(table(match3$reg1))
dupl <- match3[match3$reg1 %in% n_occur$Var1[n_occur$Freq > 1],]

match3 <- match3 %>% 
  filter(!(reg1 == "AF-Zabul" & reg2 != "AF-Zabul")) %>% 
  filter(!(reg1 == "AF-Kabul" & reg2 != "AF-Kabul")) %>% 
  filter(!(reg1 == "NP-Province 1" & reg2 != "NP-Province 1")) %>% 
  filter(!(reg1 == "NP-Province 2" & reg2 != "NP-Province 2")) %>% 
  filter(!(reg1 == "NP-Province 3" & reg2 != "NP-Province 3")) %>% 
  filter(!(reg1 == "NP-Province 4" & reg2 != "NP-Province 4")) %>% 
  filter(!(reg1 == "NP-Province 5" & reg2 != "NP-Province 5")) %>% 
  filter(!(reg1 == "NP-Province 6" & reg2 != "NP-Province 6")) %>% 
  filter(!(reg1 == "NP-Province 7" & reg2 != "NP-Province 7")) %>% 
  filter(!(reg1 == "NG-Osun" & reg2 != "NG-Osun")) %>% 
  filter(!(reg1 == "NG-Ogun" & reg2 != "NG-Ogun"))

matched <- match3

## find missing matches
match1 <- match1 %>% 
  left_join(match3) %>% 
  filter(is.na(reg2)) %>% 
  select(reg1)

## match using max distance 2
match3 <- fuzzyjoin::stringdist_join(
  x = match1, 
  y = match2, 
  by = c("reg1" = "reg2"),
  method = "osa",
  ignore_case = TRUE,
  max_dist = 2,
  mode = "inner"
)

## find duplicates and remove them
n_occur <- data.frame(table(match3$reg1))
dupl <- match3[match3$reg1 %in% n_occur$Var1[n_occur$Freq > 1],]

match3 <- match3 %>% 
  filter(!(reg1 == "HT-North" & reg2 != "HT-Nord")) %>% 
  filter(!(reg1 == "HT-Central" & reg2 != "HT-Centre")) %>% 
  filter(!(reg1 == "MV-Central"))

matched <- rbind(matched, match3)

## find missing matches
match1 <- match1 %>% 
  left_join(match3) %>% 
  filter(is.na(reg2)) %>% 
  select(reg1)


## match using max distance 3
match3 <- fuzzyjoin::stringdist_join(
  x = match1, 
  y = match2, 
  by = c("reg1" = "reg2"),
  method = "osa",
  ignore_case = TRUE,
  max_dist = 3,
  mode = "inner"
)

## find duplicates and remove them
n_occur <- data.frame(table(match3$reg1))
dupl <- match3[match3$reg1 %in% n_occur$Var1[n_occur$Freq > 1],]

match3 <- match3 %>% 
  filter(!(reg1 == "HT-North" & reg2 != "HT-Nord")) %>% 
  filter(!(reg1 == "HT-Northwest")) %>% 
  filter(!(reg1 == "HT-Central" & reg2 != "HT-Centre")) %>% 
  filter(!(reg1 == "MV-Central"))

matched <- rbind(matched, match3)

## find missing matches
match1 <- match1 %>% 
  left_join(match3) %>% 
  filter(is.na(reg2)) %>% 
  select(reg1)

## match using max distance 4
match3 <- fuzzyjoin::stringdist_join(
  x = match1, 
  y = match2, 
  by = c("reg1" = "reg2"),
  method = "osa",
  ignore_case = TRUE,
  max_dist = 4,
  mode = "inner"
)

## find duplicates and remove them
n_occur <- data.frame(table(match3$reg1))
dupl <- match3[match3$reg1 %in% n_occur$Var1[n_occur$Freq > 1],]

match3 <- match3 %>% 
  filter(!(reg1 == "HT-Northwest" & reg2 != "HT-Nord-Ouest")) %>% 
  filter(!(reg1 == "MV-Central")) %>% 
  filter(!(reg1 == "PH-Armm")) %>% 
  filter(!(reg1 == "UG-Central 1")) %>% 
  filter(!(reg1 == "MW-Lilongwe")) %>% 
  filter(!(reg1 == "UG-Central 2"))   

matched <- rbind(matched, match3)

## find missing matches
match1 <- match1 %>% 
  left_join(match3) %>% 
  filter(is.na(reg2)) %>% 
  select(reg1)

## match the remaining regions manually
match2 <- match2 %>% 
  left_join(matched) %>% 
  filter(is.na(reg1))

match1$reg2[match1$reg1 == "KH-Mondul Kiri/Ratanak Kiri"] <- "KH-Mondol Kiri & Rattanak Kiri"
match1$reg2[match1$reg1 == "TD-Ennedi Est/Ennedi Ouest"] <- "TD-Ennedi"
match1$reg2[match1$reg1 == "ET-Benishangul-Gumuz"] <- "ET-Benishangul"
match1$reg2[match1$reg1 == "HT-Metropolitan Area"] <- "HT-Aire Metropolitaine"
match1$reg2[match1$reg1 == "IA-Delhi-Central"] <- "IA-Nct Of Delhi-Central"
match1$reg2[match1$reg1 == "IA-Delhi-East"] <- "IA-Nct Of Delhi-East"
match1$reg2[match1$reg1 == "IA-Delhi-New Delhi"] <- "IA-Nct Of Delhi-New Delhi"
match1$reg2[match1$reg1 == "IA-Delhi-North East"] <- "IA-Nct Of Delhi-North East"
match1$reg2[match1$reg1 == "IA-Delhi-North West"] <- "IA-Nct Of Delhi-North West"
match1$reg2[match1$reg1 == "IA-Delhi-North"] <- "IA-Nct Of Delhi-North"
match1$reg2[match1$reg1 == "IA-Delhi-South West"] <- "IA-Nct Of Delhi-South West"
match1$reg2[match1$reg1 == "IA-Delhi-South"] <- "IA-Nct Of Delhi-South"
match1$reg2[match1$reg1 == "IA-Delhi-West"] <- "IA-Nct Of Delhi-West"
match1$reg2[match1$reg1 == "LB-Montserrado Incl. Monrovia"] <- "LB-Montserrado"
match1$reg2[match1$reg1 == "MW-Lilongwe"] <- "MW-Lilongwe Rural"
match1$reg2[match1$reg1 == "MW-Blantyre"] <- "MW-Blantyre Rural"
match1$reg2[match1$reg1 == "MV-Central"] <- "MV-Central Region"
match1$reg2[match1$reg1 == "PK-Azad, Jammu And Kashmir"] <- "PK-Ajk"
match1$reg2[match1$reg1 == "PK-Federally Administered Tribal Areas"] <- "PK-Fata"
match1$reg2[match1$reg1 == "PK-Islamabad (Ict)"] <- "PK-Ict"
match1$reg2[match1$reg1 == "PK-Gilgit Baltistan"] <- "PK-Gb"
match1$reg2[match1$reg1 == "PK-Khyber Pakhtunkhwa"] <- "PK-Kpk"
match1$reg2[match1$reg1 == "PK-Punjab Excluding Islamabad (Ict)"] <- "PK-Punjab"
match1$reg2[match1$reg1 == "PH-Davao Peninsula"] <- "PH-Davao"
match1$reg2[match1$reg1 == "PH-National Capital Region"] <- "PH-National Capital"
match1$reg2[match1$reg1 == "PH-Cordillera Admin Region"] <- "PH-Cordillera"
match1$reg2[match1$reg1 == "PH-Armm"] <- "PH-Autonomous Region In Muslim Mindanao"
match1$reg2[match1$reg1 == "UG-Central 1"] <- "UG-South Buganda"
match1$reg2[match1$reg1 == "UG-Central 2"] <- "UG-North Buganda"
match1$reg2[match1$reg1 == "ZW-Harare Chitungwiza"] <- "ZW-Harare"
match1$reg2[match1$reg1 == "PK-Sindh\r\nSindh\r\nSindh"] <- "PK-Sindh"

#not matched:
#EG-North Sinai (Excluded)
#EG-South Sinai (Excluded)

matched <- rbind(matched, match1)

## correct mismatched names and add missing matches
matched$reg2[matched$reg1 == "CM-Centre"] <- "CM-Centre (Without Yaounde)"
matched$reg2[matched$reg1 == "CM-Littoral"] <- "CM-Littoral (Without Douala)"
matched$reg2[matched$reg1 == "CO-San Andres"] <- "CO-San Andr?S Y Providencia"
matched$reg2[matched$reg1 == "HT-South"] <- "HT-Sud"
matched$reg2[matched$reg1 == "HT-West"] <- "HT-Rest-Ouest"
matched$reg2[matched$reg1 == "MW-Zomba"] <- "MW-Zomba City"
matched$reg2[matched$reg1 == "MV-North"] <- "MV-North Region"
matched$reg2[matched$reg1 == "MV-South"] <- "MV-South Region"
matched <- matched %>% add_row(reg1 = "MW-Zomba", reg2 = "MW-Zomba Rural")
matched <- matched %>% add_row(reg1 = "MW-Mzimba", reg2 = "MW-Mzuzu City")
matched <- matched %>% add_row(reg1 = "MW-Blantyre", reg2 = "MW-Blantyre City")
matched <- matched %>% add_row(reg1 = "MW-Lilongwe", reg2 = "MW-Lilongwe City")

df1 <- df1 %>% 
  mutate(region = str_to_title(region)) %>%                 #capitalize the region names
  unite(reg2, DHSCC, region, sep = "-", remove = FALSE) %>% #generate country-region identifier
  left_join(matched) %>% #add a column with the matched region names from the boundary data
  ## clean and organize the final data  
  select(!c(CLUSTER, ALT_DEM, LATNUM, LONGNUM, adm.reg, ADM1NAME, DHSREGNA, sdepto, sdist))


## remove Colombia (no vaccination data)
df1 <- df1 %>% 
  filter(DHSCC != "CO")

bord_sf <- bord_sf %>% 
  mutate(region = str_to_title(region)) %>%                 #capitalize the region names
  unite(reg1, DHSCC, region, sep = "-", remove = FALSE) %>% #generate country-region identifier
  select(DHSCC, CNTRYNAMEE, reg1, region, geometry) %>% 
  rename(country = CNTRYNAMEE)
  
bord_sf <- bord_sf %>% 
  filter(DHSCC != "CO")
  
cntry_names <- dplyr::select(as.data.frame(bord_sf), -geometry) %>% 
  select(DHSCC, country) %>% 
  unique()

df1 <- df1 %>% 
  left_join(cntry_names)

unique(df1$country)

## from now on use reg1 to match the survey and boundary data

save.image(file='data_extract_with_bound_matched.RData')
rm(list =ls())


################################################################################
## 4. Generate sub-national aggregates for full immunization coverage
################################################################################

load('data_extract_with_bound_matched.RData')
rm(list=setdiff(ls(), c("df1", "bord_sf")))

class(df1) <- class(as.data.frame(df1))
class(df1)

## Adjusting the weights due to non-response???

## Age group 12-35
df2 <- df1 %>% 
  ##missing weights for two regions in Pakistan - replace with 1 (equal weights)
  mutate(wt = ifelse(reg1 == "PK-Azad, Jammu And Kashmir", 1, wt)) %>% 
  mutate(wt = ifelse(reg1 == "PK-Gilgit Baltistan", 1, wt)) %>% 
  ##generate regional aggregates
  group_by(country, DHSCC, reg1) %>% 
  summarise(FIC = weighted.mean(FIC, wt, na.rm=T),
            PIC = weighted.mean(PIC, wt, na.rm=T),
            BCG = weighted.mean(BCG.vac, wt, na.rm=T),
            MCV = weighted.mean(MCV.vac, wt, na.rm=T),
            DTP1 = weighted.mean(DTP1.vac, wt, na.rm=T),
            DTP2 = weighted.mean(DTP2.vac, wt, na.rm=T),
            DTP3 = weighted.mean(DTP3.vac, wt, na.rm=T),
            OPV1 = weighted.mean(OPV1.vac, wt, na.rm=T),
            OPV2 = weighted.mean(OPV2.vac, wt, na.rm=T),
            OPV3 = weighted.mean(OPV3.vac, wt, na.rm=T),
            DTP = weighted.mean(DTP.vac, wt, na.rm=T),
            OPV = weighted.mean(OPV.vac, wt, na.rm=T)) %>% 
  ungroup()

class(df2) <- class(as.data.frame(df2))
class(df2)
## Join the aggregated survey with the geographical boundaries
share.vac.12.35 <- left_join(bord_sf, df2)


## Age group 12-23
df3 <- df1 %>% 
  filter(age.months <= 23) %>% 
  ##missing weights for two regions in Pakistan - replace with 1 (equal weights)
  mutate(wt = ifelse(reg1 == "PK-Azad, Jammu And Kashmir", 1, wt)) %>% 
  mutate(wt = ifelse(reg1 == "PK-Gilgit Baltistan", 1, wt)) %>% 
  ##generate regional aggregates
  group_by(country, DHSCC, reg1) %>% 
  summarise(FIC = weighted.mean(FIC, wt, na.rm=T),
            PIC = weighted.mean(PIC, wt, na.rm=T),
            BCG = weighted.mean(BCG.vac, wt, na.rm=T),
            MCV = weighted.mean(MCV.vac, wt, na.rm=T),
            DTP1 = weighted.mean(DTP1.vac, wt, na.rm=T),
            DTP2 = weighted.mean(DTP2.vac, wt, na.rm=T),
            DTP3 = weighted.mean(DTP3.vac, wt, na.rm=T),
            OPV1 = weighted.mean(OPV1.vac, wt, na.rm=T),
            OPV2 = weighted.mean(OPV2.vac, wt, na.rm=T),
            OPV3 = weighted.mean(OPV3.vac, wt, na.rm=T),
            DTP = weighted.mean(DTP.vac, wt, na.rm=T),
            OPV = weighted.mean(OPV.vac, wt, na.rm=T)) %>% 
  ungroup()

class(df3) <- class(as.data.frame(df3))
class(df3)
share.vac.12.23 <- left_join(bord_sf, df3)

## Age group 24-35
df4 <- df1 %>% 
  filter(age.months >= 24 & age.months <= 35) %>% 
  ##missing weights for two regions in Pakistan - replace with 1 (equal weights)
  mutate(wt = ifelse(reg1 == "PK-Azad, Jammu And Kashmir", 1, wt)) %>% 
  mutate(wt = ifelse(reg1 == "PK-Gilgit Baltistan", 1, wt)) %>% 
  ##generate regional aggregates
  group_by(country, DHSCC, reg1) %>% 
  summarise(FIC = weighted.mean(FIC, wt, na.rm=T),
            PIC = weighted.mean(PIC, wt, na.rm=T),
            BCG = weighted.mean(BCG.vac, wt, na.rm=T),
            MCV = weighted.mean(MCV.vac, wt, na.rm=T),
            DTP1 = weighted.mean(DTP1.vac, wt, na.rm=T),
            DTP2 = weighted.mean(DTP2.vac, wt, na.rm=T),
            DTP3 = weighted.mean(DTP3.vac, wt, na.rm=T),
            OPV1 = weighted.mean(OPV1.vac, wt, na.rm=T),
            OPV2 = weighted.mean(OPV2.vac, wt, na.rm=T),
            OPV3 = weighted.mean(OPV3.vac, wt, na.rm=T),
            DTP = weighted.mean(DTP.vac, wt, na.rm=T),
            OPV = weighted.mean(OPV.vac, wt, na.rm=T)) %>% 
  ungroup()

class(df4) <- class(as.data.frame(df4))
class(df4)
share.vac.24.35 <- left_join(bord_sf, df4)

save.image(file='data_for_analysis.RData')

## Cross check country level shares with DHS statcompiler
## generate country-level aggregates (age group 12-23 months)
df5 <- df1 %>% 
  filter(age.months <= 23) %>% 
  group_by(country, DHSCC) %>% 
  summarise(FIC = weighted.mean(FIC, wt, na.rm=T),
            PIC = weighted.mean(PIC, wt, na.rm=T),
            BCG = weighted.mean(BCG.vac, wt, na.rm=T),
            MCV = weighted.mean(MCV.vac, wt, na.rm=T),
            DTP1 = weighted.mean(DTP1.vac, wt, na.rm=T),
            DTP2 = weighted.mean(DTP2.vac, wt, na.rm=T),
            DTP3 = weighted.mean(DTP3.vac, wt, na.rm=T),
            OPV1 = weighted.mean(OPV1.vac, wt, na.rm=T),
            OPV2 = weighted.mean(OPV2.vac, wt, na.rm=T),
            OPV3 = weighted.mean(OPV3.vac, wt, na.rm=T),
            DTP = weighted.mean(DTP.vac, wt, na.rm=T),
            OPV = weighted.mean(OPV.vac, wt, na.rm=T)) %>% 
  ungroup()


df6 <- df1 %>% 
  group_by(country, DHSCC, FIC) %>% 
  summarise(obs = n())
  


rm(list =ls())


################################################################################
#### Summary statistics
################################################################################

sum1 <- df1 %>% 
  select(c(hh.id, DHSCC, FIC, wealth, health.card)) %>% 
  filter(!is.na(FIC)) %>% 
  mutate(health.card = ifelse(is.na(health.card), "no card", health.card)) %>% 
  mutate(health.card = ifelse(health.card == "yes, not seen", "no card", health.card)) %>% 
  mutate(health.card = ifelse(health.card == "no longer has card", "no card",  health.card)) %>% 
  mutate(health.card = ifelse(health.card == "yes, card seen from health facility", "yes, seen",  health.card))

sum2 <- df1 %>% 
  group_by(health.card) %>% 
  dplyr::summarise(obs = n())
