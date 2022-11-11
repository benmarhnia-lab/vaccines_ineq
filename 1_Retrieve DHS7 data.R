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
# 4. Generate estimates for full immunization coverage


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
set_rdhs_config(email = "XXX",
                project = "XXX")


set_rdhs_config(email = "XXXX",
                project = "XXXX",
                config_path = "rdhs.json",
                cache_path = "dhs 7",
                global = FALSE)

rm(list =ls())


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



n = surveys_IDs %>% group_by(CountryName) %>% summarise(obs = n())

## Keep only latest survey for each country
surveys_IDs <- surveys_IDs %>% 
  dplyr::filter(FileName != "BDKR72DT.ZIP" & FileName != "ETKR71DT.ZIP" & FileName != 'IAKR74DT.ZIP' & FileName != "RWKR70DT.ZIP" & FileName != "SNKR70DT.zip" & FileName != "SNKR7HDT.ZIP" & FileName != "SNKR7IDT.ZIP" & FileName != "SNKR7ZDT.ZIP" & FileName != "SNKR81DT.ZIP" )	
                  
  # FileName != "PGKR71DT.ZIP" &  FileName != "SNKR8BDT.ZIP"
                  
                 


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


## Multiple boundaries available for some countries (e.g. region, district)
## Use the boundaries for the lowest administrative level 


admin_lvls <- unique(bord_df[c("DHSCC", "LEVELCO")])

bord_sf <- bord_sf %>% 
  mutate(drop = ifelse(LEVELCO == "Groups of Admin1 and the Capital City" | LEVELCO == "Admin 1 and the Capital city" | LEVELCO == "Groups of Governorates" | LEVELCO == "Ecological Regions" |  LEVELCO == "Development Regions", 1, 0)) %>% 
  mutate(drop = ifelse(LEVELCO == "Groups of Admin1" & (DHSCC == "EG"| DHSCC == "GU" | DHSCC == "JO"| DHSCC == "KE" | DHSCC == "LB" | DHSCC == "MW" | DHSCC == "NG" | DHSCC == "PG" | DHSCC == "TZ" | DHSCC == "UG" ), 1, drop)) %>% 
  mutate(drop = ifelse(LEVELCO == "Admin1" & ( DHSCC == "SL"  | DHSCC == "NP"| DHSCC == "IA"), 1, drop)) %>% 
  subset(drop == 0)

## Combine district and region names for India (because district names are not unique across regions)
bord_sf <- bord_sf %>% 
  unite(adm.reg, OTHREGNA, REGNAME, sep = "-", remove = FALSE) %>% 
  mutate(DHSREGEN = ifelse(DHSCC == "IA", adm.reg, DHSREGEN)) %>% 
  mutate(region = DHSREGEN)

bord_df <- dplyr::select(as.data.frame(bord_sf), -geometry) 
admin_lvls <- unique(bord_df[c("DHSCC", "LEVELCO")])



################################################################################
## 3. Match the survey data with the geographical boundaries
################################################################################

## Harmonize the region names in the survey and border data

#class(df1) <- class(as.data.frame(df1))
#class(df1)

## Add districts for Malawi
admin_MW <- read.dta("MWKR7AFL.DTA") %>%
  select_if(names(.)  %in% c("v001", "sdist")) %>% 
  unique() %>% 
  mutate(DHSCC = "MW") %>% 
  rename(psu = v001) %>% 
  mutate(sdist = as.character(sdist))
sort(unique(admin_MW$sdist))


df1 <- df1 %>% 
  # remove Colombia - no data on vaccination
  filter(DHSCC != "CO") %>% 
  left_join(admin_MW) %>% 
  ## combine district and region names for India (because district names are not unique across regions)
  unite(adm.reg, ADM1NAME, DHSREGNA, sep = "-", remove = FALSE) %>% 
  ## use the correct region names 
  mutate(region = ifelse(DHSCC == "EG", ADM1NAME, region)) %>%
  mutate(region = ifelse(DHSCC == "GU", ADM1NAME, region)) %>% 
  mutate(region = ifelse(DHSCC == "IA", adm.reg, region)) %>% 
  mutate(region = ifelse(DHSCC == "KE", ADM1NAME, region)) %>%
  mutate(region = ifelse(DHSCC == "LS", ADM1NAME, region)) %>%
  mutate(region = ifelse(DHSCC == "LB", ADM1NAME, region)) %>% 
  mutate(region = ifelse(DHSCC == "MW", sdist, region)) %>% 
  mutate(region = ifelse(DHSCC == "NG", ADM1NAME, region)) %>% 
  mutate(region = ifelse(DHSCC == "SN", DHSREGNA, region)) %>% 
  mutate(region = ifelse(DHSCC == "SL", DHSREGNA, region)) 

## convert region names to lowercase
df1$region <- tolower(df1$region)
unique(df1$region)


bord_df <- bord_df %>% 
  # remove Colombia - no data on vaccination
  filter(DHSCC != "CO") 
  
## convert region names to lowercase
bord_df$region <- tolower(bord_df$region)
unique(bord_df$region)


## Check if region names are harmonized across the survey and boundary data
match1 <- bord_df %>% 
  select(DHSCC, region) %>% 
  unique() %>% 
  #mutate(region = str_to_title(region)) %>%                 #capitalize the region names
  unite(reg1, DHSCC, region, sep = "-", remove = FALSE) %>% #generate country-region identifier
  select(reg1)

match2 <- df1 %>% 
  select(DHSCC, region) %>% 
  unique() %>% 
  #mutate(region = str_to_title(region)) %>%                 #capitalize the region names
  unite(reg2, DHSCC, region, sep = "-", remove = FALSE) %>% #generate country-region identifier
  select(reg2)

## Match regions names, taking the closest match
#install.packages("fuzzyjoin")
library(fuzzyjoin)

## match using max distance 0
match3 <- fuzzyjoin::stringdist_join(
  x = match1, 
  y = match2, 
  by = c("reg1" = "reg2"),
  method = "osa",
  ignore_case = TRUE,
  max_dist = 0,
  mode = "inner"
)

matched <- match3

## find missing matches
match2 <- match2 %>% 
  left_join(match3) %>% 
  filter(is.na(reg1)) %>% 
  select(reg2)


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

## check if there are duplicates
n_occur <- data.frame(table(match3$reg2))

matched <- rbind(matched, match3)


## find missing matches
match2 <- match2 %>% 
  left_join(match3) %>% 
  filter(is.na(reg1)) %>% 
  select(reg2)

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
n_occur <- data.frame(table(match3$reg2))
dupl <- match3[match3$reg2 %in% n_occur$Var1[n_occur$Freq > 1],]

match3 <- match3 %>% 
  filter(!(reg1 == "CM-centre" & reg2 == "HT-centre"))

matched <- rbind(matched, match3)


## find missing matches
match2 <- match2 %>% 
  left_join(match3) %>% 
  filter(is.na(reg1)) %>% 
  select(reg2)


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
n_occur <- data.frame(table(match3$reg2))
dupl <- match3[match3$reg2 %in% n_occur$Var1[n_occur$Freq > 1],]

match3 <- match3 %>% 
  filter(!(reg1 == "HT-northwest" & reg2 == "HT-nord-est"))

matched <- rbind(matched, match3)

## find missing matches
match2 <- match2 %>% 
  left_join(match3) %>% 
  filter(is.na(reg1)) %>% 
  select(reg2)


## find missing matches
match1 <- match1 %>% 
  left_join(matched) %>% 
  filter(is.na(reg2)) %>% 
  select(reg1)

## match the remaining regions manually
match2$reg1[match2$reg2 == "CM-centre (without yaounde)"] <- "CM-centre"
match2$reg1[match2$reg2 == "CM-littoral (without douala)"] <- "CM-littoral"
match2$reg1[match2$reg2 == "ET-benishangul"] <- "ET-benishangul-gumuz"
match2$reg1[match2$reg2 == "GU-el peten"] <- "GU-petén"
match2$reg1[match2$reg2 == "GU-el quiche"] <- "GU-quiché"
match2$reg1[match2$reg2 == "HT-aire metropolitaine"] <- "HT-metropolitan area"
match2$reg1[match2$reg2 == "HT-nord-ouest"] <- "HT-northwest"
match2$reg1[match2$reg2 == "HT-rest-ouest"] <- "HT-west"
match2$reg1[match2$reg2 == "HT-sud-est"] <- "HT-southeast"
match2$reg1[match2$reg2 == "IA-uttar pradesh-sant ravidas nagar (bhadohi)"] <- "IA-uttar pradesh-sant ravidas nagar"
match2$reg1[match2$reg2 == "ID-jakarta"] <- "ID-dki jakarta"
match2$reg1[match2$reg2 == "KH-mondol kiri & rattanak kiri"] <- "KH-mondul kiri/ratanak kiri"
match2$reg1[match2$reg2 == "KH-preah sihanouk & kaoh kong"] <- "KH-preah sihanouk/koh kong"
match2$reg1[match2$reg2 == "KH-preah vihear & steung treng"] <- "KH-preah vihear/stung treng"
match2$reg1[match2$reg2 == "LB-montserrado"] <- "LB-montserrado incl. monrovia"
match2$reg1[match2$reg2 == "MD-analamanga"] <- "MD-analamanga excluding capital"
match2$reg1[match2$reg2 == "MD-antananarivo"] <- "MD-antananarivo capital"
match2$reg1[match2$reg2 == "MR-hodh echargui"] <- "MR-eastern basin region"
match2$reg1[match2$reg2 == "MR-hodh gharbi"] <- "MR-south nouakchott"
match2$reg1[match2$reg2 == "MR-nouakchott nord"] <- "MR-north nouakchott"
match2$reg1[match2$reg2 == "MR-nouakchott ouest"] <- "MR-west nouakchott"
match2$reg1[match2$reg2 == "MR-nouakchott sud"] <- "MR-south nouakchott"
match2$reg1[match2$reg2 == "MR-hodh gharbi"] <- "MR-western basin region"
match2$reg1[match2$reg2 == "MV-central region"] <- "MV-central"
match2$reg1[match2$reg2 == "MV-north region"] <- "MV-north"
match2$reg1[match2$reg2 == "MV-south region"] <- "MV-south"
match2$reg1[match2$reg2 == "MW-blantyre city"] <- "MW-blantyre"
match2$reg1[match2$reg2 == "MW-blantyre rural"] <- "MW-blantyre"
match2$reg1[match2$reg2 == "MW-lilongwe city"] <- "MW-lilongwe"
match2$reg1[match2$reg2 == "MW-lilongwe rural"] <- "MW-lilongwe"
match2$reg1[match2$reg2 == "MW-mzuzu city"] <- "MW-mzimba"
match2$reg1[match2$reg2 == "MW-zomba city"] <- "MW-zomba"
match2$reg1[match2$reg2 == "MW-zomba rural"] <- "MW-zomba"
match2$reg1[match2$reg2 == "PH-autonomous region in muslim mindanao"] <- "PH-armm"
match2$reg1[match2$reg2 == "PH-cordillera"] <- "PH-cordillera admin region"
match2$reg1[match2$reg2 == "PH-davao"] <- "PH-davao peninsula"
match2$reg1[match2$reg2 == "PH-national capital"] <- "PH-national capital region"
match2$reg1[match2$reg2 == "PK-ajk"] <- "PK-azad, jammu and kashmir"
match2$reg1[match2$reg2 == "PK-fata"] <- "PK-federally administered tribal areas"
match2$reg1[match2$reg2 == "PK-gb"] <- "PK-gilgit baltistan"
match2$reg1[match2$reg2 == "PK-ict"] <- "PK-islamabad (ict)"
match2$reg1[match2$reg2 == "PK-kpk"] <- "PK-khyber pakhtunkhwa"
match2$reg1[match2$reg2 == "PK-punjab"] <- "PK-punjab excluding islamabad (ict)"
match2$reg1[match2$reg2 == "PK-sindh"] <- "PK-sindh\r\nsindh\r\nsindh"
match2$reg1[match2$reg2 == "TD-ennedi"] <- "TD-ennedi est/ennedi ouest"
match2$reg1[match2$reg2 == "UG-north buganda"] <- "UG-central 2"
match2$reg1[match2$reg2 == "UG-south buganda"] <- "UG-central 1"
match2$reg1[match2$reg2 == "ZW-harare chitungwiza"] <- "ZW-harare"


matched <- rbind(matched, match2)

matched$reg1[matched$reg2 == "ZW-harare"] <- "ZW-harare chitungwiza"



df1 <- df1 %>% 
  #mutate(region = str_to_title(region)) %>%                 #capitalize the region names
  unite(reg2, DHSCC, region, sep = "-", remove = FALSE) %>% #generate country-region identifier
  left_join(matched) %>% #add a column with the matched region names from the boundary data
  ## clean and organize the final data  
  select(!c(CLUSTER, ALT_DEM, LATNUM, LONGNUM, adm.reg, ADM1NAME, DHSREGNA, sdist))


bord_sf$region <- tolower(bord_sf$region)

bord_sf <- bord_sf %>% 
  #mutate(region = str_to_title(region)) %>%                 #capitalize the region names
  unite(reg1, DHSCC, region, sep = "-", remove = FALSE) %>% #generate country-region identifier
  select(DHSCC, CNTRYNAMEE, reg1, region, geometry) %>% 
  rename(country = CNTRYNAMEE)
  
bord_sf <- bord_sf %>% 
  filter(DHSCC != "CO") %>% 
  filter(DHSCC != "PG")  


cntry_names <- dplyr::select(as.data.frame(bord_sf), -geometry) %>% 
  select(DHSCC, country) %>% 
  unique()

df1 <- df1 %>% 
  left_join(cntry_names)

unique(df1$country)

# make sure again that all regions are matched
help <- df1 %>% dplyr::select(reg1, reg2) %>% unique()
bord_df <- dplyr::select(as.data.frame(bord_sf), -geometry) 
bord_df <- bord_df %>% left_join(help)
help2 <- bord_df %>% filter(is.na(reg2))


rm(list=setdiff(ls(), c("df1", "bord_sf")))

## from now on use reg1 to match the survey and boundary data


################################################################################
## 4. Generate estimates for full immunization coverage
################################################################################


class(df1) <- class(as.data.frame(df1))
class(df1)


df1 <- df1 %>% 
  filter(age.months >= 15) %>%
  mutate(wealth_n = ifelse(wealth == "poorest" | wealth == "lowest", 1,
                           ifelse(wealth == "poorer" | wealth == "second", 2,
                                  ifelse(wealth == "middle", 3,
                                         ifelse(wealth == "richer" | wealth == "fourth", 4,
                                                ifelse(wealth == "richest" | wealth == "highest", 5, wealth)))))) %>% 
  #missing weights for two regions in Pakistan - replace with 1 (equal weights)
  mutate(wt = ifelse(reg1 == "PK-azad, jammu and kashmir", 1, wt)) %>% 
  mutate(wt = ifelse(reg1 == "PK-gilgit baltistan", 1, wt)) %>% 
  # remove special characters - cannot be read in stata
  mutate(reg3 = str_replace_all(reg1, "[^a-zA-Z0-9]", ""))


# prepare the data for STATA for calculating the concentration index
df.stata <- df1 %>% 
  select(DHSCC, country, reg3, psu, wealth.score, wealth, wealth_n, FIC, wt, age.years, age.months)

write.csv(df.stata, "data_for_stata.csv")

rm(df.stata)

### National estimates

## Age group 15-35
nat.fic.15.35 <- df1 %>% 
  group_by(country, DHSCC) %>% 
  summarise(fic = weighted.mean(FIC, wt, na.rm=T),
            bcg = weighted.mean(BCG.vac, wt, na.rm=T),
            mcv = weighted.mean(MCV.vac, wt, na.rm=T),
            dtp = weighted.mean(DTP.vac, wt, na.rm=T),
            opv = weighted.mean(OPV.vac, wt, na.rm=T),
            obs = n()) %>% 
  ungroup() 


## Age group 24-35
nat.fic.24.35 <- df1 %>%
  filter(age.months >= 24) %>% 
  group_by(country, DHSCC) %>% 
  summarise(fic = weighted.mean(FIC, wt, na.rm=T),
            bcg = weighted.mean(BCG.vac, wt, na.rm=T),
            mcv = weighted.mean(MCV.vac, wt, na.rm=T),
            dtp = weighted.mean(DTP.vac, wt, na.rm=T),
            opv = weighted.mean(OPV.vac, wt, na.rm=T),
            obs = n()) %>% 
  ungroup() 


### Sub-national estimates

## Age group 15-35
reg.fic.15.35 <- df1 %>% 
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
            OPV = weighted.mean(OPV.vac, wt, na.rm=T),
            obs = n()) %>% 
  ungroup()

class(reg.fic.15.35) <- class(as.data.frame(reg.fic.15.35))
class(reg.fic.15.35)



## Age group 24-35
reg.fic.24.35 <- df1 %>% 
  filter(age.months >= 24) %>% 
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
            OPV = weighted.mean(OPV.vac, wt, na.rm=T),
            obs = n()) %>% 
  ungroup()

class(reg.fic.24.35) <- class(as.data.frame(reg.fic.24.35))
class(reg.fic.24.35)

save.image(file='data_for_analysis.RData')






