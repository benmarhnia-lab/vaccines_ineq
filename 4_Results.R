################################################################################
################################################################################
#######                                                                  ####### 
#######                             PROJECT:                             ####### 
#######          VACCINATION INEQUALITY - SPATIAL ANALYSIS               ####### 
#######                                                                  ####### 
#######                    CODE: Tables and maps                         #######
################################################################################
################################################################################

#remove.packages('rlang')
#install.packages("rlang")

rm(list =ls())
library(readxl)
#install.packages("rworldmap")
#install.packages("rnaturalearthdata")
library(rworldmap)
library(rnaturalearth)
#install.packages("rgeos")
library(rgeos)
library(tidyverse)
library(magrittr)
library(lubridate)
library(ggpubr)
#install.packages("svglite")
library(svglite)
#library(zoo)
#library(psych)
options(scipen=999)
options(digits=5)


## Contents:
##
## 1.   National level results
## 2.   Subnational level results
## 2.1. Map: Inequality index
## 2.2. Map: Full immunization (FIC)
## 2.3. Bivariate map
## 4.   Theil index
## 5.1. Upset plot 
## 5.2. Upset plot for individual countries
##



## load the data
load('data_for_analysis.RData')


################################################################################
#### 1. National level results
################################################################################


### Import results for the concentration index of inequality (W and E)
## List all files
a <- list.files(pattern ="\\.xlsx")   
a

nat.cii.15.35 = NULL
for (i in a){

      cii.help <- read_excel(i)
      nat.cii.15.35 <- bind_rows(nat.cii.15.35, cii.help)
}


nat.cii.15.35 <- nat.cii.15.35 %>% 
  ## calculate upper and lower bounds (95% confidence)
  mutate(lb = value - 1.96 * SE,
         ub = value + 1.96 * SE) %>% 
  pivot_wider(names_from = index, values_from = c("value", "SE", "lb", "ub")) %>% 
  ## generate rank (from worst to best performing; absolute values of inequality)
  mutate(rank_wagstaff = dense_rank(desc(abs(value_wagstaff)))) %>% 
  mutate(rank_erreygers = dense_rank(desc(abs(value_erreygers)))) %>% 
  rename(W = value_wagstaff,
         E = value_erreygers) %>% 
  dplyr::select(country, obs, W, lb_wagstaff, ub_wagstaff, rank_wagstaff, E, lb_erreygers, ub_erreygers, rank_erreygers)
  


### Compile all results at the national level
nat.results <- nat.fic.15.35 %>% 
  dplyr::select(-obs) %>% 
  mutate(rank_fic = dense_rank((abs(fic)))) %>% 
  left_join(nat.cii.15.35) 

write.csv(nat.results, "Results_by_country.csv")


## Plot the Inequality Index by country
ggplot(data=nat.cii.15.35,  aes(x = reorder(country, -W), y=W, fill=W)) +
  geom_bar(stat="identity",  colour="#333333") +
  scale_y_continuous(limits = c(-0.35, 0.5), breaks=c(-0.3, -0.2,-0.1, 0, 0.1, 0.2, 0.3, 0.4)) +
  scale_fill_gradient2(low="#ace3e4", high="#aa599a", midpoint=0, na.value = "grey20", limits=c(-0.45, 0.90)) +
  geom_point(aes(x = reorder(country, -W), y=ub_wagstaff), shape = 25, fill="grey20", color="grey20", size = 1) +
  geom_point(aes(x = reorder(country, -W), y=lb_wagstaff), shape = 24, fill="grey20", color="grey20", size = 1) +
  geom_segment(aes(xend = country, y = ub_wagstaff, yend = lb_wagstaff), col = 'grey20') +
  ##scale_fill_manual(values = c("high"="#a66e85",  "medium"="#d9c1cb", "low" = "#f2eaee", "reverse inequality" = "#c1d8cf")) +
  theme_bw() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust=0.2)) +
  theme(axis.line.x = element_line(colour = "#333333"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  xlab(element_blank()) + ylab("Wagstaff index of inequality (W)") 

ggplot(data=nat.cii.15.35,  aes(x = reorder(country, -E), y=E, fill=E)) +
  geom_bar(stat="identity",  colour="#333333") +
  scale_y_continuous(limits = c(-0.3, 0.5), breaks=c(-0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4)) +
  scale_fill_gradient2(low="#ace3e4", high="#aa599a", midpoint=0, na.value = "grey20", limits=c(-0.45, 0.90)) +
  geom_point(aes(x = reorder(country, -W), y=ub_erreygers), shape = 25, fill="grey20", color="grey20", size = 1) +
  geom_point(aes(x = reorder(country, -W), y=lb_erreygers), shape = 24, fill="grey20", color="grey20", size = 1) +
  geom_segment(aes(xend = country, y = ub_erreygers, yend = lb_erreygers), col = 'grey20') +
  ##scale_fill_manual(values = c("high"="#a66e85",  "medium"="#d9c1cb", "low" = "#f2eaee", "reverse inequality" = "#c1d8cf")) +
  theme_bw() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust=0.2)) +
  theme(axis.line.x = element_line(colour = "#333333"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  xlab(element_blank()) + ylab("Erreygers' index of inequality (E)") 


################################################################################
#### 2. Subnational level results
################################################################################

## Get the world map
#remotes::install_github("ropensci/rnaturalearthhires")
library(rnaturalearth)

world <- ne_countries(scale = "large", returnclass = "sf") %>%
  dplyr::select(name, economy, income_grp, continent, region_wb, region_un, subregion) %>% 
  filter(name!="Antarctica") 

names <- unique(reg.fic.15.35$country)
names <- c(names, "Gambia")
names
world_sub <- world %>% 
  filter(name %in% names) 
setdiff(names, world_sub$name)

world_df <- dplyr::select(as.data.frame(world_sub), -geometry) 
colnames(world_df)[1] <- c('country')
world_df$country[world_df$country == "Gambia"] = "The Gambia"

## Import the results
reg_names <- df1 %>% dplyr::select(country, reg1, reg3) %>% unique()

## List all files
a <- list.files(pattern ="\\.xlsx")   
a

reg.cii.15.35 = NULL
for (i in a){
  
  reg.cii.help <- read_excel(i)
  reg.cii.15.35 <- bind_rows(reg.cii.15.35, reg.cii.help)
}

str(reg.cii.15.35)
unique(reg.cii.15.35$index)

## two regions have 0 inequality (either everyone is vaccinated (AL) or nobody is vaccinated (ML))
reg.cii.15.35 <- reg.cii.15.35 %>% 
  add_row(region = "MLkidal",   
          index = "wagstaff",   
          obs = 158,    
          value = 0,
          SE = 0) %>% 
  add_row(region = "MLkidal",   
          index = "erreygers",   
          obs = 158,    
          value = 0,
          SE = 0) %>% 
  add_row(region = "ALkor",   
          index = "wagstaff",   
          obs = 95,    
          value = 0,
          SE = 0) %>% 
  add_row(region = "ALkor",   
          index = "erreygers",   
          obs = 95,    
          value = 0,
          SE = 0) 



reg.cii.15.35 <-  reg.cii.15.35 %>% 
  # add 95% confidence intervals (lower and upper bounds)
  mutate(lb = value - 1.96 * SE,
         ub = value + 1.96 * SE) %>% 
  # determine if significant
  mutate(sign = ifelse((lb<0 & ub<0) | (lb>0 & ub>0), 1, 0)) %>% 
  #pivot_wider(names_from = index, values_from = c("CI", "SE", "lb", "ub", "sign")) %>% 
  rename(reg3 = region) %>% 
  left_join(reg_names) 

reg.cii.erg <- reg.cii.15.35 %>% filter(index == "erreygers")
reg.cii.wag <- reg.cii.15.35 %>% filter(index == "wagstaff")



disparities_erg <- reg.cii.erg %>% group_by(country) %>% summarize(max = max(abs(value)), min = min(abs(value))) %>% 
  mutate(dif = max - min) 


disparities_wag <- reg.cii.wag %>% group_by(country) %>% summarize(max = max(abs(value)), min = min(abs(value))) %>% 
  mutate(diff = max - min) 


################################################################################
#### 2.1. Map: Inequality index
################################################################################

## Plot CI at the sub-national level

reg.cii.erg_sf <- left_join(bord_sf, reg.cii.erg) %>% 
  mutate(value = ifelse(sign == 0, NA, value)) %>% 
  left_join(world_df) 

reg.cii.wag_sf <- left_join(bord_sf, reg.cii.wag) %>% 
  mutate(value = ifelse(sign == 0, NA, value)) %>% 
  left_join(world_df)


## subset to regions with statistically significant ci
#reg.cii.erg_sf <- reg.cii.erg_sf %>% filter(sign == 1)

## Main plot
ggplot() +
  geom_sf(data = world, fill = "darkgray", size = 0.1, colour = "white") +
  geom_sf(data = reg.cii.wag_sf, aes(fill = value),  col="lightgray",  size = 0.1) +
  geom_sf(data = world_sub, fill = NA, size = 0.125, colour = "#333333") +
  #coord_sf(xlim = c(-25, 142), ylim = c(-35, 43), expand = FALSE) +
  theme_void() +
  scale_fill_gradient2(low="#ace3e4", high="#aa599a", midpoint=0, na.value = "darkgray",
                       limits=c(-1,1)) +
  theme(legend.direction='horizontal', legend.position="bottom", legend.key.width = unit(1, 'cm')) +
  theme(legend.key = element_rect(color="black")) + 
  labs(fill = "Inequality index") +
  theme(legend.key = element_rect(color="black")) + 
  theme(legend.direction='horizontal', legend.position="bottom") 


################################################################################
#### 2.2. Map: Full immunization (FIC)
################################################################################

reg.fic.15.35_sf <- left_join(bord_sf, reg.fic.15.35) 

## Plot FIC at the sub-national level

## Main plot
ggplot() +
  geom_sf(data = world, fill = "darkgray", size = 0.1, colour = "white") +
  geom_sf(data = reg.fic.15.35_sf, aes(fill = FIC),  col="lightgray",  size = 0.1) +
  geom_sf(data = world_sub, fill = NA, size = 0.125, colour = "#333333") +
  #coord_sf(xlim = c(-25, 142), ylim = c(-35, 43), expand = FALSE) +
  theme_void() +
  scale_fill_distiller(palette="Spectral", direction=1, 
                       labels = scales::percent_format(accuracy = 1),  
                       na.value = "darkgray",
                       limits=c(0,1)) +
  #theme(legend.justification = c(1, 0), legend.position = c(1, 0))+
  labs(fill = "% fully vaccinated") +
  theme(legend.key = element_rect(color="black"), legend.key.width = unit(1, 'cm')) + 
  theme(legend.direction='horizontal', legend.position="bottom") 


################################################################################
#### 2.3. Bivariate map
################################################################################
# create classes
#install.packages("sf")
#install.packages("biscale")
#install.packages("remotes")
#remotes::install_github("slu-openGIS/biscale")
#install.packages("biscale", dependencies = TRUE)
library(sf)
library(biscale)



df10 <- reg.fic.15.35 %>% 
  dplyr::select(country, reg1, FIC, BCG, MCV, DTP, OPV) %>% 
  left_join(reg.cii.erg)  %>% 
  rename(E = value) %>% 
  dplyr::select(country, reg1, FIC, BCG, MCV, DTP, OPV, E) %>%    
  left_join(reg.cii.wag) %>% 
  rename(W = value) %>% 
  dplyr::select(country, reg1, FIC, BCG, MCV, DTP, OPV, E, W) %>%
  mutate(W=abs(W)) %>% 
  mutate(E=abs(E))


df10 <- df10 %>% 
  mutate(nonFIC = 1-FIC) %>% 
  mutate(cat.nonFIC=cut(nonFIC, breaks=c(-Inf, 0.33, 0.66, Inf), labels=c(1,2,3), ordered_result = TRUE)) %>% 
  mutate(cat.nonFIC = as.numeric(cat.nonFIC)) %>% 
  #mutate(cat.ineq=cut(E, breaks=c(-Inf, 0.1, 0.3, Inf), labels=c(1,2,3), ordered_result = TRUE)) %>% 
  mutate(cat.ineq=cut(W, breaks=c(-Inf, 0.1, 0.3, Inf), labels=c(1,2,3), ordered_result = TRUE)) %>% 
  mutate(cat.ineq = as.numeric(cat.ineq))

df11 <- bi_class(df10, x = cat.nonFIC, y = cat.ineq,
                 style = "quantile", 
                 dim = 3) 

df12 <- bord_sf %>% 
  left_join(df11)


table(df12$bi_class)

# select colors:
custom_pal <- c("1-1" = "#FCF3CF", "2-1" = "#F7DC6F", "3-1" = "#F1C40F",
                "1-2" = "#f0b27a", "2-2" = "#eb984e", "3-2" = "#CA6F1E",
                "1-3" = "#E59866", "2-3" = "#DC7633", "3-3" = "#A04000")

# draw legend
svg("Map_bivariate/legend.svg", width = 2.5, height=2.5)
legend <- bi_legend(pal = custom_pal,
                    xlab = "Lower immunization ",
                    ylab = "Higher inequality ",
                    size = 10)
legend
dev.off() 


## Main plot
ggplot() +
  geom_sf(data = world, fill = "darkgray", size = 0.1, colour = "white") +
  geom_sf(data = df12, mapping = aes(fill = bi_class), col="lightgray",  size = 0.1) +
  geom_sf(data = world_sub, fill = NA, size = 0.125, colour = "#333333") +
  #coord_sf(xlim = c(-25, 142), ylim = c(-35, 43), expand = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3) + #pal = "DkBlue"
  theme_void() +
  theme(legend.position = "none") 


################################################################################
#### 4. Theil index
################################################################################

#install.packages("dineq")
library(dineq)
theil.wtd(df2$FIC)
theil.wtd(cii.wag$CI)
theil.wtd(cii.erg$CI)

## upload the results for the Theil index
theil <- read.csv("Theil_index.csv") 
str(theil)
colnames(theil) <- c('country', 't_bcg', 't_mcv', 't_dtp', 't_opv', 't_fic', 't_erg', 't_wag')

## Combine the data 
df5 <- results.by.cnt %>% 
  #dplyr::select(country, DHSCC, FIC, obs, CI_wagstaff, lb_wagstaff, ub_wagstaff, rank_wagstaff, CI_erreygers, lb_erreygers, ub_erreygers, rank_erreygers) %>% 
  left_join(theil)


ggplot(df5) + 
  geom_point(aes(x=bcg, y=t_bcg), col="red") +
  geom_point(aes(x=mcv, y=t_mcv), col="blue") +
  geom_point(aes(x=dtp, y=t_dtp), col="green") +
  geom_point(aes(x=opv, y=t_opv), col="yellow") +
  geom_point(aes(x=fic, y=t_fic))


ggplot(df5) + 
  geom_point(aes(x=CI_erreygers, y=t_erg), col="red") +
  geom_point(aes(x=CI_wagstaff, y=t_wag), col="blue") 
  

################################################################################
#### 5.1. Upset plot
################################################################################
# code from: https://www.r-graph-gallery.com/upset-plot.html
#install.packages("UpSetR")
library(UpSetR)
library(grid)


age_dist <- read.csv("age_distribution_15to35.csv")[c(2,5)]

df13 <- df1 %>% 
  dplyr::select(country, BCG.vac, MCV.vac, DTP.vac, OPV.vac, wt) %>% 
  dplyr::rename(bcg = BCG.vac,
                mcv = MCV.vac,
                dtp = DTP.vac,
                opv = OPV.vac) %>% 
  mutate(bcg_mcv = ifelse(bcg == 1 & mcv == 1, 1, 0),
         bcg_dtp = ifelse(bcg == 1 & dtp == 1, 1, 0),
         bcg_opv = ifelse(bcg == 1 & opv == 1, 1, 0),
         mcv_dtp = ifelse(mcv == 1 & dtp == 1, 1, 0),
         mcv_opv = ifelse(mcv == 1 & opv == 1, 1, 0),
         dtp_opv = ifelse(dtp == 1 & opv == 1, 1, 0),
         bcg_mcv_dtp = ifelse(bcg == 1 & mcv == 1 & dtp == 1, 1, 0),
         bcg_mcv_opv = ifelse(bcg == 1 & mcv == 1 & opv == 1, 1, 0),
         bcg_dtp_opv = ifelse(bcg == 1 & dtp == 1 & opv == 1, 1, 0),
         mcv_dtp_opv = ifelse(mcv == 1 & dtp == 1 & opv == 1, 1, 0),
         bcg_mcv_dtp_opv = ifelse(bcg == 1 & mcv == 1 & dtp == 1 & opv == 1, 1, 0)) %>% 
  group_by(country) %>% 
  summarise(BCG = weighted.mean(bcg, wt, na.rm=T),
            MCV = weighted.mean(mcv, wt, na.rm=T),
            DTP = weighted.mean(dtp, wt, na.rm=T),
            OPV = weighted.mean(opv, wt, na.rm=T),
            "BCG&MCV" = weighted.mean(bcg_mcv, wt, na.rm=T),
            "BCG&DTP" = weighted.mean(bcg_dtp, wt, na.rm=T),
            "BCG&OPV" = weighted.mean(bcg_opv, wt, na.rm=T),
            "MCV&DTP" = weighted.mean(mcv_dtp, wt, na.rm=T),
            "MCV&OPV" = weighted.mean(mcv_opv, wt, na.rm=T),
            "DTP&OPV" = weighted.mean(dtp_opv, wt, na.rm=T),
            "BCG&MCV&DTP" = weighted.mean(bcg_mcv_dtp, wt, na.rm=T),
            "BCG&MCV&OPV" = weighted.mean(bcg_mcv_opv, wt, na.rm=T),
            "BCG&DTP&OPV" = weighted.mean(bcg_dtp_opv, wt, na.rm=T),
            "MCV&DTP&OPV" = weighted.mean(mcv_dtp_opv, wt, na.rm=T),
            "BCG&MCV&DTP&OPV" = weighted.mean(bcg_mcv_dtp_opv, wt, na.rm=T)) %>% 
  ungroup() %>% 
  gather(key = "name", value = "share", -country) %>% 
  mutate(share = share * 100)


## Generate variables for missing vaccines
df14 <- df1 %>% 
  #filter(age.months>=15) %>% 
  select(country, BCG.vac, MCV.vac, DTP.vac, OPV.vac, wt) %>% 
  rename(bcg = BCG.vac,
         mcv = MCV.vac,
         dtp = DTP.vac,
         opv = OPV.vac) %>% 
  mutate(vac_missing = ifelse(bcg == 0 & mcv == 0 & dtp == 0 & opv == 0, "BCG&MCV&DTP&OPV", 
                              ifelse(bcg == 1 & mcv == 0 & dtp == 0 & opv == 0, "MCV&DTP&OPV", 
                                     ifelse(bcg == 0 & mcv == 1 & dtp == 0 & opv == 0, "BCG&DTP&OPV", 
                                            ifelse(bcg == 0 & mcv == 0 & dtp == 1 & opv == 0, "BCG&MCV&OPV", 
                                                   ifelse(bcg == 0 & mcv == 0 & dtp == 0 & opv == 1, "BCG&MCV&DTP",
                                                          ifelse(bcg == 1 & mcv == 1 & dtp == 0 & opv == 0, "DTP&OPV", 
                                                                 ifelse(bcg == 1 & mcv == 0 & dtp == 1 & opv == 0, "MCV&OPV", 
                                                                        ifelse(bcg == 1 & mcv == 0 & dtp == 0 & opv == 1, "MCV&DTP",
                                                                               ifelse(bcg == 0 & mcv == 1 & dtp == 1 & opv == 0, "BCG&OPV",
                                                                                      ifelse(bcg == 0 & mcv == 1 & dtp == 0 & opv == 1, "BCG&DTP",
                                                                                             ifelse(bcg == 0 & mcv == 0 & dtp == 1 & opv == 1, "BCG&MCV", 
                                                                                                    ifelse(bcg == 0 & mcv == 1 & dtp == 1 & opv == 1, "BCG",
                                                                                                           ifelse(bcg == 1 & mcv == 0 & dtp == 1 & opv == 1, "MCV",
                                                                                                                  ifelse(bcg == 1 & mcv == 1 & dtp == 0 & opv == 1, "DTP",
                                                                                                                         ifelse(bcg == 1 & mcv == 1 & dtp == 1 & opv == 0, "OPV",
                                                                                                                                "none"))))))))))))))))


## Import the WPP population data
pop <-  read.csv("popagesex_2020_single_years.csv") %>% 
  dplyr::select("country", "age1", "age2") %>% 
  mutate(country = ifelse(country == "United Republic of Tanzania", "Tanzania", country)) %>%
  mutate(country = ifelse(country == "Gambia", "The Gambia", country)) %>% 
  mutate(pop = (age1+age2)*1000) %>% 
  left_join(age_dist) %>% 
  mutate(pop_15to35 = pop*share_15to35) %>% 
  filter(!is.na(pop_15to35))


df15 <- df14 %>% 
  group_by(country) %>% 
  dplyr::summarise(tot = sum(wt))

class(df15)
class(df15) <- class(as.data.frame(df15))


df16 <- df14 %>% 
  group_by(country, vac_missing) %>% 
  dplyr::summarise(n = sum(wt)) %>% 
  left_join(df15) %>% 
  mutate(share = n / tot) %>% 
  left_join(pop) %>% 
  mutate(abs = share * pop_15to35) %>% 
  filter(vac_missing != "none" & !is.na(vac_missing)) %>% 
  ungroup()


unique(df16$country)

df17 <- df16 %>% 
  group_by(vac_missing) %>% 
  dplyr::summarise(abs_tot = sum(abs)) %>% 
  mutate(abs_tot = abs_tot / 1000) %>%  #show results in thousands
  ungroup()

input <- tibble::deframe(df17)

## UpSet plot - data for all countries combined
#png(file = "Upsetplot_all_countries.png", units="in", width=14, height=6, res=300)
svg("Upsetplot_all_countries.svg", width=14, height=6)
upset(fromExpression(input), 
      order.by = "freq", 
      decreasing = T, 
      mb.ratio = c(0.6, 0.4),
      number.angles = 0, 
      text.scale = 1.5, 
      point.size = 5, 
      line.size = 1,
      empty.intersections = "on",
      mainbar.y.label = "Intersection size ('000s)",
      sets.x.label = "Number of missed vaccines ('000s)")
dev.off()

## Map vaccines most often missing per country
df18 <- df16 %>% 
  group_by(country) %>% 
  summarise(abs = max(abs)) %>% 
  left_join(df16) %>% 
  select(country, vac_missing) %>% 
  mutate(vac = vac_missing) %>% 
  mutate(vac = ifelse(vac == "BCG&MCV&DTP&OPV", "all 4",
                      ifelse(vac == "MCV&DTP&OPV" | vac == "BCG&MCV&DTP" | vac == "DTP&OPV", "2-3",
                             vac))) %>% 
  mutate(country = ifelse(country == "The Gambia", "Gambia", country)) %>% 
  rename(name = country)

unique(df18$vac)
df18$vac <- ordered(df18$vac, levels = c("MCV", "DTP",  "OPV", "2-3", "all 4"))

miss.vac <- world_sub %>% 
  left_join(df18)


## Map
ggplot() +
  geom_sf(data = world, fill = "lightgray", size = 0.1, colour = "white") +
  geom_sf(data = miss.vac, aes(fill = vac), colour="#333333",  size = 0.1) +
  #coord_sf(xlim = c(-25, 142), ylim = c(-35, 43), expand = FALSE) +
  scale_fill_manual(values=c("DTP" = "#EFBC68", "OPV"="#C2D7D0", "MCV"="#5F9595", "2-3"="#6A798A", "all 4"="#363E48")) +
  labs(fill = "Vaccines most often missed") +
  theme_void() +
  theme(legend.direction='horizontal', legend.position="bottom") 

################################################################################
#### 5.2. Upset plot for individual countries
################################################################################

## UpSet plot by country
## loop doesn't work, do it manually

unique(df16$country)

i = "Zimbabwe"
upset.tmp <- df16 %>% 
  filter(country == i) %>% 
  select(vac_missing, abs)
input.tmp <- tibble::deframe(upset.tmp)
png(file = paste("./4_Figures/upset/Upsetplot_", i, ".png", sep=""), units="in", width=14, height=6, res=300)
upset(fromExpression(input.tmp), 
      order.by = "freq", 
      decreasing = T, 
      mb.ratio = c(0.6, 0.4),
      number.angles = 0, 
      text.scale = 1.5, 
      point.size = 5, 
      line.size = 1,
      empty.intersections = "on",
      mainbar.y.label = NULL,
      sets.x.label = "Number of missed vaccinations")
grid.text(i, x = 0.65, y=0.95, gp=gpar(fontsize=40))
dev.off()


