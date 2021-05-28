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
library(dplyr)
library(purrr)
library(tidyr)
library(broom)

setwd("C:/Users/annak/Dropbox/Projects/2021_San Diego/Vaccination - spatial analysis/")

###############################################
#### Prepare the data
###############################################
### Import the individual level data for India
load("./2_Codes/Data_India_individual_level.RData")

#### Generate district level (ADMIN-2) vaccination rates by socio-economic group (wealth & education)
## By wealth group: 1 - most deprived to 5 - least deprived
df1 <- df0 %>%        
  filter(agemonths>=12 & agemonths<=23) %>% 
  group_by(cntrycode, dist.name, dist.code, wealth) %>% 
  summarise(MCV = weighted.mean(mcvvac, wt, na.rm=T),
            pop = n()) %>% 
  ungroup() %>% 
  group_by(cntrycode, dist.name, dist.code) %>% 
  mutate(total_pop = sum(pop),    #add the total population for each area (without wealth)    
         proportion_pop = pop/total_pop) %>% 
  ungroup()


#### Generate region level (ADMIN-1) vaccination rates by socio-economic group (wealth & education)
## By wealth group: 1 - most deprived to 5 - least deprived
#df1 <- df0 %>%        
#  filter(agemonths>=12 & agemonths<=23) %>% 
#  #filter(agemonths>=12) %>% 
#  group_by(cntrycode, reg.name, reg.code, wealth) %>% 
#  summarise(MCV = weighted.mean(mcvvac, wt, na.rm=T),
#            pop = n()) %>%
#  ungroup() %>% 
#  group_by(cntrycode, dist.name, dist.code) %>% 
#  mutate(total_pop = sum(pop),    #add the total population for each area (without wealth)  
#         proportion_pop = pop/total_pop) %>% 
#  ungroup()


## Add overall values for the whole region/district (without wealth quintile)
overall_values <- df0 %>% 
  group_by(cntrycode, dist.code) %>% 
  summarise(overall = weighted.mean(mcvvac, wt, na.rm=T)) %>% 
  ungroup()
  
df1 <- left_join(df1, overall_values)

### By education group
df2 <- df0 %>%        
  filter(agemonths>=12 & agemonths<=23) %>% 
  group_by(cntrycode, dist.name, dist.code, edulevel) %>% 
  summarise(MCV = weighted.mean(mcvvac, wt, na.rm=T),
            pop = n()) %>% 
  ungroup() %>% 
  group_by(cntrycode, dist.name, dist.code) %>% 
  mutate(total_pop = sum(pop),           #calculate the total population for each area (without wealth quintile)
         proportion_pop = pop/total_pop) %>% 
  ungroup() %>% 
  mutate(edulevel = as.character(edulevel))

## Reorder education levels - 1 is the lowest and 4 is the highest level
df2$edulevel <- factor(df2$edulevel, levels = c("no education", "primary", "secondary", "higher"))
df2 <- left_join(df2, overall_values) %>% 
  arrange(dist.name, edulevel)

###############################################
#### Inequality analysis by wealth quintile
###############################################
## Slope Index of Inequality (SII)
sii_model_1 <- df1 %>%  group_by(cntrycode, dist.code, dist.name) %>% 
  mutate(cumulative_pro = cumsum(proportion_pop),              #cumulative proportion population for each area
         relative_rank = case_when(
           wealth == 1 ~ 0.5*proportion_pop,
           wealth != 1 ~ lag(cumulative_pro) + 0.5*proportion_pop),
         sqr_proportion_pop = sqrt(proportion_pop),            #square root of the proportion of the population in each SIMD
         relrank_sqr_proppop = relative_rank * sqr_proportion_pop,
         value_sqr_proppop = sqr_proportion_pop * MCV) %>%     #value based on population weights
  nest() %>% #creating one column called data with all the variables not in the grouping
  # Calculating linear regression for all the groups, then formatting the results
  # and calculating the confidence intervals
  mutate(model = map(data, ~ lm(value_sqr_proppop ~ sqr_proportion_pop + relrank_sqr_proppop + 0, data = .)),
         #extracting sii from model, a bit fiddly but it works
         sii = -1 * as.numeric(map(map(model, "coefficients"), "relrank_sqr_proppop")),
         cis = map(model, confint_tidy)) %>% #calculating confidence intervals
  ungroup() %>% unnest(cis) %>% #Unnesting the CIs 
  #selecting only even row numbers which are the ones that have the sii cis
  filter(row_number() %% 2 == 0) %>% 
  mutate(lowci_sii = -1 * conf.high, #fixing interpretation
         upci_sii = -1 * conf.low) %>% 
  select(-conf.low, -conf.high)      #non-needed variables

## Merge sii results with main data set
#results <- left_join(df1, sii_model)

#### Relative index of inequality (RII) 
#This is the calculation of the linear RII which is based on the SII values,
#so that section needs to be run before this one.
results_1 <- sii_model_1 %>% 
  left_join(overall_values) %>% 
  mutate(rii = sii / overall,
         lowci_rii = lowci_sii / overall,
         upci_rii = upci_sii / overall,
         #Transforming RII into %. This way it is interpreted as "most deprived areas are
         # xx% above the average" For example: Cancer mortality rate is around 55% higher
         # in deprived areas relative to the mean rate in the population
         rii_int = rii * 0.5 *100,
         lowci_rii_int = lowci_rii * 0.5 *100,
         upci_rii_int = upci_rii * 0.5 *100)

#### Visualise the results
## Import the subnational boundaries for India (ADMIN2 level)
## download the boundaries from the DHS spatial reposity at the desired level: https://spatialdata.dhsprogram.com/boundaries/#view=table&countryId=IA
bound_adm2 <- sf::st_read("./1_Data/dhs_spatial_data/IA/shps/sdr_subnational_boundaries2.shp") %>% 
  dplyr::select(DHSCC, REGCODE, REGNAME, SVYNOTES) %>% 
  rename(cntrycode = DHSCC,
         dist.code = REGCODE,
         dist.name = REGNAME)

## Join the results by district with the geographical boundaries
results_1 <- left_join(bound_adm2, results_1) 

## Plot the results
ggplot(data = results_1) +
  geom_sf(aes(fill = sii), col="grey50",  size = 0.05) +
  scale_fill_viridis_c(option = "plasma") + #limits=c(0, 1), breaks=seq(0, 1, by=0.25)
  labs(fill = "Slope Index of Inequality") +
  #coord_sf(crs = "+proj=eqearth") + 
  theme_minimal() +
  ggsave("./4_Figures/Map_India_MCV_SII_by_wealth_quintile.png", width = 8, height=6, dpi=300)


ggplot(data = results_1) +
  geom_sf(aes(fill = rii), col="grey50",  size = 0.05) +
  scale_fill_viridis_c(option = "plasma") + #limits=c(0, 1), breaks=seq(0, 1, by=0.25)
  labs(fill = "Relative Index of Inequality") +
  #coord_sf(crs = "+proj=eqearth") + 
  theme_minimal() +
  ggsave("./4_Figures/Map_India_MCV_RII_by_wealth_quintile.png", width = 8, height=6, dpi=300)


###############################################
#### Inequality analysis by education group
###############################################
## Slope Index of Inequality (SII)
sii_model_2 <- df2 %>%  group_by(cntrycode, dist.code, dist.name) %>% 
  mutate(cumulative_pro = cumsum(proportion_pop),              #cumulative proportion population for each area
         relative_rank = case_when(
           edulevel == 1 ~ 0.5*proportion_pop,
           edulevel != 1 ~ lag(cumulative_pro) + 0.5*proportion_pop),
         sqr_proportion_pop = sqrt(proportion_pop),            #square root of the proportion of the population in each SIMD
         relrank_sqr_proppop = relative_rank * sqr_proportion_pop,
         value_sqr_proppop = sqr_proportion_pop * MCV) %>%     #value based on population weights
  nest() %>% #creating one column called data with all the variables not in the grouping
  # Calculating linear regression for all the groups, then formatting the results
  # and calculating the confidence intervals
  mutate(model = map(data, ~ lm(value_sqr_proppop ~ sqr_proportion_pop + relrank_sqr_proppop + 0, data = .)),
         #extracting sii from model, a bit fiddly but it works
         sii = -1 * as.numeric(map(map(model, "coefficients"), "relrank_sqr_proppop")),
         cis = map(model, confint_tidy)) %>% #calculating confidence intervals
  ungroup() %>% unnest(cis) %>% #Unnesting the CIs 
  #selecting only even row numbers which are the ones that have the sii cis
  filter(row_number() %% 2 == 0) %>% 
  mutate(lowci_sii = -1 * conf.high, #fixing interpretation
         upci_sii = -1 * conf.low) %>% 
  select(-conf.low, -conf.high)      #non-needed variables

#### Relative index of inequality (RII) 
#This is the calculation of the linear RII which is based on the SII values,
#so that section needs to be run before this one.
results_2 <- sii_model_2 %>% 
  left_join(overall_values) %>% 
  mutate(rii = sii / overall,
         lowci_rii = lowci_sii / overall,
         upci_rii = upci_sii / overall,
         #Transforming RII into %. This way it is interpreted as "most deprived areas are
         # xx% above the average" For example: Cancer mortality rate is around 55% higher
         # in deprived areas relative to the mean rate in the population
         rii_int = rii * 0.5 *100,
         lowci_rii_int = lowci_rii * 0.5 *100,
         upci_rii_int = upci_rii * 0.5 *100)

## Join the results by district with the geographical boundaries
results_2 <- left_join(bound_adm2, results_2) 

## Plot the results
ggplot(data = results_2) +
  geom_sf(aes(fill = sii), col="grey50",  size = 0.05) +
  scale_fill_viridis_c(option = "plasma") + #limits=c(0, 1), breaks=seq(0, 1, by=0.25)
  labs(fill = "Slope Index of Inequality") +
  #coord_sf(crs = "+proj=eqearth") + 
  theme_minimal() +
  ggsave("./4_Figures/Map_India_MCV_SII_by_edu_level.png", width = 8, height=6, dpi=300)


ggplot(data = results_2) +
  geom_sf(aes(fill = rii), col="grey50",  size = 0.05) +
  scale_fill_viridis_c(option = "plasma") + #limits=c(0, 1), breaks=seq(0, 1, by=0.25)
  labs(fill = "Relative Index of Inequality") +
  #coord_sf(crs = "+proj=eqearth") + 
  theme_minimal() +
  ggsave("./4_Figures/Map_India_MCV_RII_by_edu_level.png", width = 8, height=6, dpi=300)
