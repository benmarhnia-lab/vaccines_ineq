################################################################################
################################################################################
#######                                                                  ####### 
#######                             PROJECT:                             ####### 
#######          VACCINATION INEQUALITY - SPATIAL ANALYSIS               ####### 
#######                                                                  ####### 
#######                   CODE: INEQUALITY ANALYSIS                      #######
################################################################################
################################################################################

# CONTENTS:
#
# 1.  Map immunization coverage at the sub-national level
# 2.  Calculate the concentration index of inequality (CI) by wealth score
# 3.  Bi-variate map for incomplete immunization and inequality 
# 4.  Upset plot

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

rm(list =ls())
setwd("C:/Users/annak/Dropbox/Projects/2021_San Diego/Vaccination - spatial analysis/")

load('./1_Data/data_for_analysis.RData')

################################################################################
#### 1. Map immunization coverage at the sub-national level
################################################################################
## Get the world map
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::select(name, economy, income_grp, continent, region_wb) %>% 
  filter(name!="Antarctica") 

names <- unique(share.vac.12.35$country)
names <- c(names, "Gambia")
names
world_sub <- world %>% 
  filter(name %in% names) 
setdiff(names, world_sub$name)

# Plot subnational FIC 
ggplot() +
  geom_sf(data = world, fill = "darkgray", size = 0.001, colour = "white") +
  geom_sf(data = share.vac.12.35, aes(fill = FIC), col="lightgray",  size = 0.001) +
  geom_sf(data = world_sub, fill = NA, size = 0.1, colour = "#333333") +
  #coord_sf(xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE) +
  coord_sf(xlim = c(-95, 145), ylim = c(-45, 50), expand = FALSE) +
  #scale_fill_gradient(low="#9e0926", high="#f5e6e9", labels = scales::percent_format(accuracy = 1), na.value = "darkgray") + #, midpoint=0
  #scale_fill_viridis_c(option = "plasma", labels = scales::percent_format(accuracy = 1),  na.value = "darkgray") + #limits=c(0, 1), breaks=seq(0, 1, by=0.25)
  scale_fill_distiller(palette="Spectral", direction=1, labels = scales::percent_format(accuracy = 1),  na.value = "darkgray") +
  labs(fill = "% fully vaccinated") +
  theme_minimal() +
  theme(legend.direction='horizontal', legend.position="bottom") +
  #coord_sf(crs = "+proj=eqearth") + 
  #geom_text_repel(data= world_lbl, aes(x=X, y=Y, label=name), size = 15, force = 10)+
  ggsave("./4_Figures/Map_fic_age12-35.png", width = 8, height=5, dpi=600, unit="in")


## Plot FIC for Haiti and Guatemala
ggplot() +
  geom_sf(data = world, fill = "darkgray", size = 0.001, colour = "white") +
  geom_sf(data = share.vac.12.35, aes(fill = FIC), col="lightgray",  size = 0.001) +
  geom_sf(data = world_sub, fill = NA, size = 0.1, colour = "#333333", show.legend = FALSE, na.rm = TRUE) +
  coord_sf(xlim = c(-95, -68), ylim = c(10, 23) , expand = FALSE) +
  theme_void() +
  scale_fill_distiller(palette="Spectral", direction=1, labels = scales::percent_format(accuracy = 1),  na.value = "darkgray") +
  theme(legend.position = "none") +
  ggtitle("Central America & Carribean") +
  theme(plot.title = element_text(size = 22)) +
  theme(panel.border = element_rect(color = "#333333", fill = NA, size = 1)) +
  theme(plot.background= element_rect(fill = "white", colour = "white")) +
  ggsave("./4_Figures/Map_fic_age12-35_Americas.png", width = 4.75, height=2.7, dpi=600)


################################################################################
## Estimate and plot the country-level aggregates (age group 12-35 months)
################################################################################
df5 <- df1 %>% 
  group_by(country, DHSCC) %>% 
  summarise(FIC = weighted.mean(FIC, wt, na.rm=T)) %>% 
  ungroup()

df5 <- df5[order(df5$FIC),]

help1 <- df2 %>% 
  group_by(country) %>% 
  summarize(max = max(FIC),
            min = min(FIC))

df5 <- df5 %>% 
  left_join(help1)

## Plot the country-level data for full immunization coverage
ggplot(data=df5,  aes(x = reorder(country, -FIC), y=FIC, fill=FIC)) +
  geom_bar(stat="identity",  colour="#333333") +
  scale_fill_distiller(palette="Spectral", direction=1,  na.value = "grey20",  limits=c(0, 1)) +
  #scale_fill_manual(values = c("high"="#a66e85",  "medium"="#d9c1cb", "low" = "#f2eaee", "reverse inequality" = "#c1d8cf")) +
  geom_point(aes(x = reorder(country, -FIC), y=max), shape = 25, fill="grey20", color="grey20", size = 1) +
  geom_point(aes(x = reorder(country, -FIC), y=min), shape = 24, fill="grey20", color="grey20", size = 1) +
  geom_segment(aes(xend = country, y = max, yend = min), col = 'grey20') +
  theme_bw() +
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust=0.2)) +
  theme(axis.line.x = element_line(colour = "#333333"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  xlab(element_blank()) + ylab("% fully vaccinated") +
  ggsave("./4_Figures/FIC_by_country.svg", width = 8, height = 3, units = "in")


            
################################################################################
#### 2. Calculate the concentration index of inequality (CI) by wealth score
################################################################################
# code from: https://bookdown.org/danieljcarter/socepi/concentration-curve-index.html
library(magrittr)

df6 <- df1 %>% 
  select(reg1, country, wealth.score, FIC, wt, age.years) %>% 
  ##missing weights for two regions in Pakistan - replace with 1 (equal weights)
  mutate(wt = ifelse(reg1 == "PK-Azad, Jammu And Kashmir", 1, wt)) %>% 
  mutate(wt = ifelse(reg1 == "PK-Gilgit Baltistan", 1, wt)) %>% 
  filter(country != "Tajikistan") #missing wealth data

## Create an empty data frame where results will be stored
cii = data.frame(matrix(vector(), 0, 2,
                        dimnames=list(c(), c("reg1", "ACI"))),
                 stringsAsFactors=F) %>% 
  mutate(reg1 = as.character(reg1))

regions <- unique(df6$reg1)

#i =  "TZ-Lindi"
## loop through each region and calculate CI
for (i in regions){
  
  cii.tmp <- df6 %>% 
    #mutate(tot.vac = BCG.vac + DTP.vac + OPV.vac + MCV.vac) %>% 
    dplyr::select(wealth.score, FIC, reg1, wt) %>% 
    filter(wt != 0) %>% 
    filter(reg1 == i) %>%
    drop_na()
  
  cii.tmp <- cii.tmp[order(cii.tmp$wealth.score),]
  cii.tmp$c.vac <- cumsum(cii.tmp$FIC)/sum(cii.tmp$FIC)
  cii.tmp$c.wealth <- cii.tmp %$% cume_dist(wealth.score)
  
  ggplot(cii.tmp, aes(x = c.wealth, y = c.vac)) +
    geom_point(color = "red") +
    geom_line() +
    geom_abline(color = "forestgreen") +
    labs(title = i,
         x = "Cumulative wealth rank",
         y = "Cumulative immunization")
  
  #calculate the absolute concentration index (ACI)
  
  #un-weighted data
  #ACI = 2/mean(cii.tmp$FIC) * cov(cii.tmp$FIC, cii.tmp$c.wealth)
  
  #weighted data
  a = weighted.mean(cii.tmp$FIC, cii.tmp$wt)
  df.tmp = cbind(cii.tmp$FIC, cii.tmp$c.wealth)
  wt = cii.tmp$wt
  b <- cov.wt(df.tmp, wt = wt)$cov[2] 
  
  ACI = 2/a * b
  
  reg1 = i

  cii.tmp <- data.frame(reg1, ACI)
  cii <- rbind(cii, cii.tmp)
}

## CII is missing for few regions: no wealth score data for Tajikistan (remove) 
## & CII for ML-Kidal is NA because FIC is 0 for all children (replace CII with 0 - no inequality)
#cii.miss <- cii %>% 
#  filter(is.na(c.index))
#cii.sub <- df1 %>% 
#  filter(reg1=="ML-Kidal") %>% 
#  select(reg1, DHSCC, FIC, wealth, wealth.score)

obs <- df1 %>% 
  group_by(country, reg1) %>% 
  summarise(obs = n())

cii <- cii %>% 
  mutate(ACI = ifelse(reg1 == "ML-Kidal", 0, ACI)) %>% 
  left_join(df2) %>% 
  select(reg1, ACI, FIC) %>% 
  mutate(RCI = ACI/(1-FIC)) %>% 
  mutate(RCI = ifelse(reg1 == "AL-Korçë", 0, RCI)) %>% #ACI value of 0
  mutate(RCI = ifelse(reg1 == "IA-Punjab-Kapurthala", 0, RCI)) %>%  #ACI value of 0
  filter(reg1 != "AM-Aragatsotn") %>% #remove: too few observations for estimates to be reliable
  filter(reg1 != "AL-Shkodër")

##demonstrating the bounded nature of the CI (Wagstaff 2005, 2011) - the higher the mean FIC the narrower the CI bounds become
library(cowplot)

p1 <- ggplot(cii, aes(x = ACI, y = FIC)) +
  geom_point(color = "red") + theme_minimal() +
  xlab("unadjusted CI") + ylab("full immunization rate")
  

p2 <- ggplot(cii, aes(x = RCI, y = FIC)) +
  geom_point(color = "red") + theme_minimal() +
  xlab("adjusted CI") + ylab("full immunization rate")
  

plot_grid(p1, p2, labels = c("a", "b"))+
  ggsave("./4_Figures/CI_dispursion.png", width = 8, height=5, dpi=600)


#add the spatial boundaries
cii_sf <- left_join(bord_sf, cii) 

## Plot CI at the sub-national level
ggplot() +
  geom_sf(data = world, fill = "darkgray", size = 0.001, colour = "white") +
  geom_sf(data = cii_sf, aes(fill = RCI),  col="lightgray",  size = 0.001) +
  geom_sf(data = world_sub, fill = NA, size = 0.1, colour = "#333333") +
  coord_sf(xlim = c(-95, 145), ylim = c(-45, 50), expand = FALSE) +
  theme_minimal() +
  scale_fill_gradient2(low="#ace3e4", high="#aa599a", midpoint=0, na.value = "darkgray",
                       breaks = c(Inf, 0.5, 0.3, 0.1, 0, -0.5),
                       labels = c("-", "high (over 0.5]", "medium (0.5; 0.3]", "low (0.3; 0.1]", "very low or none  (0.1; -0.1]","reverse (below -0.1)"),
                       guide = guide_legend()) +
  labs(fill = "Inequality") +
  theme(legend.key = element_rect(color="black")) + 
  theme(legend.direction='horizontal', legend.position="bottom") +
  ggsave("./4_Figures/Map_CI_fic_and_wealth_age12-35.png", width = 8, height=5, dpi=600)


#low="#83b29f", high="#803152"
#alternative color scheme (red & blue): low="#b1d0e3", high="#c00e29"


## Plot CI for Haiti and Guatemala
ggplot() +
  geom_sf(data = world, fill = "darkgray", size = 0.001, colour = "white") +
  geom_sf(data = cii_sf, aes(fill = RCI), col="lightgray",  size = 0.001) +
  geom_sf(data = world_sub, fill = NA, size = 0.1, colour = "#333333", show.legend = FALSE, na.rm = TRUE) +
  coord_sf(xlim = c(-95, -68), ylim = c(10, 23) , expand = FALSE) +
  theme_void() +
  scale_fill_gradient2(low="#ace3e4", high="#aa599a", midpoint=0, na.value = "darkgray",
                       breaks = c(Inf, 0.5, 0.3, 0.1, 0, -0.5),
                       labels = c("-", "high", "medium", "low", "very low or none", "reverse"),
                       guide = guide_legend()) +
  theme(legend.position = "none") +
  ggtitle("Central America & Carribean") +
  theme(plot.title = element_text(size = 22)) +
  theme(panel.border = element_rect(color = "#333333", fill = NA, size = 1)) +
  theme(plot.background= element_rect(fill = "white", colour = "white")) +
  ggsave("./4_Figures/Map_CI_fic_and_wealth_age12-35_Americas.png", width = 4.75, height=2.7, dpi=600)




################################################################################
### Calculate and plot country-level CI
################################################################################
df7 <- df1 %>% 
  select(country, wealth.score, FIC, wt) %>% 
  drop_na()

cii.cnt = data.frame(matrix(vector(), 0, 2,
                        dimnames=list(c(), c("country", "ACI"))),
                 stringsAsFactors=F)

countries <- unique(df7$country)

#i = "Albania"
for (i in countries){
  
  cii.cnt.tmp <- df7 %>% 
    dplyr::select(wealth.score, FIC, country, wt) %>% 
    filter(country == i) %>%
    drop_na()
  
  cii.cnt.tmp <- cii.cnt.tmp[order(cii.cnt.tmp$wealth.score),]
  cii.cnt.tmp$c.vac <- cumsum(cii.cnt.tmp$FIC)/sum(cii.cnt.tmp$FIC)
  cii.cnt.tmp$c.wealth <- cii.cnt.tmp %$% cume_dist(wealth.score)
  
  ggplot(cii.cnt.tmp, aes(x = c.wealth, y = c.vac)) +
    geom_point(color = "red") +
    geom_line() +
    geom_abline(color = "forestgreen") +
    labs(title = i,
         x = "Cumulative wealth rank",
         y = "Cumulative immunization")
  
  #un-weighted data
  #c.index = 2/mean(cii.cnt.tmp$FIC) * cov(cii.cnt.tmp$FIC, cii.cnt.tmp$c.wealth)
  
  #weighted data
  a = weighted.mean(cii.cnt.tmp$FIC, cii.cnt.tmp$wt)
  df.tmp <- cbind(cii.cnt.tmp$FIC, cii.cnt.tmp$c.wealth)
  wt = cii.cnt.tmp$wt
  b <- cov.wt(df.tmp, wt = wt)$cov[2] 

  ACI = 2/a * b
  
  country = i
  
  cii.cnt.tmp <- data.frame(country, ACI)
  cii.cnt <- rbind(cii.cnt, cii.cnt.tmp)
}



cii.cnt <- cii.cnt %>% 
  left_join(df5) %>% 
  select(country, ACI, FIC) %>% 
  mutate(RCI = ACI/(1-FIC)) 

cii.cnt <- cii.cnt[order(cii.cnt$RCI),]

cii.cnt <- cii.cnt %>% ## generate categories for CII
  mutate(c.cat=cut(RCI, breaks=c(-Inf, 0, 0.1, 0.3, Inf), labels=c("reverse inequality","low","medium", "high")))

help2 <- cii %>% 
  left_join(df2) %>% 
  group_by(country) %>% 
  summarize(max = max(abs(RCI)),
            min = min(abs(RCI)))

cii.cnt <- cii.cnt %>% 
  left_join(help2)

#correct CI for some countries (negative values are highest)
cii.cnt <- cii.cnt %>% 
  mutate(max = ifelse(country=="Albania", -0.43, max)) %>% 
  mutate(max = ifelse(country=="Armenia", -0.39, max)) %>% 
  mutate(max = ifelse(country=="The Gambia", -0.25, max)) %>% 
  mutate(max = ifelse(country=="Sierra Leone", -0.09, max)) %>% 
  mutate(max = ifelse(country=="South Africa", -0.20, max)) %>% 
  mutate(max = ifelse(country=="Ghana", -0.23, max))
  

## Plot the Inequality Index by country
ggplot(data=cii.cnt,  aes(x = reorder(country, -RCI), y=RCI, fill=RCI)) +
  geom_bar(stat="identity",  colour="#333333") +
  scale_y_continuous(limits = c(-0.45, 0.90), breaks=c(-0.25, 0, 0.25, 0.5, 0.75)) +
  scale_fill_gradient2(low="#ace3e4", high="#aa599a", midpoint=0, na.value = "grey20", limits=c(-0.45, 0.90)) +
  geom_point(aes(x = reorder(country, -RCI), y=max), shape = 25, fill="grey20", color="grey20", size = 1) +
  geom_point(aes(x = reorder(country, -RCI), y=min), shape = 24, fill="grey20", color="grey20", size = 1) +
  geom_segment(aes(xend = country, y = max, yend = min), col = 'grey20') +
  #scale_fill_manual(values = c("high"="#a66e85",  "medium"="#d9c1cb", "low" = "#f2eaee", "reverse inequality" = "#c1d8cf")) +
  theme_bw() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust=0.2)) +
  theme(axis.line.x = element_line(colour = "#333333"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  xlab(element_blank()) + ylab("Inequality index") +
  ggsave("./4_Figures/CI_by_country.svg", width = 8, height = 4)



#hist(cii_sf$RCI)
#cii_sf <- cii_sf %>% mutate(c.index = abs(RCI))
#cii.wealth <- cii_sf %>%
#  filter(!is.na(c.index))
#rm(list=setdiff(ls(), c("cii.wealth", "share.vac.12.35", "world", "world_sub")))
#save.image(file='./2_Codes/data_for_spatial_analysis.RData')

################################################################################
#### 3. Bi-variate map for incomplete immunization and inequality
################################################################################

df8 <- dplyr::select(as.data.frame(cii_sf), -geometry) %>% 
  na.omit()

df9 <- dplyr::select(as.data.frame(share.vac.12.35), -geometry) %>% 
  na.omit() %>% 
  mutate(nonFIC = 1-FIC) 
  
df10 <- df8 %>% 
  left_join(df9) %>% 
  select(nonFIC, RCI, reg1) %>% 
  mutate(RCI=abs(RCI)) %>% 
  mutate(cat.nonFIC=cut(nonFIC, breaks=c(-Inf, 0.33, 0.66, Inf), labels=c(1,2,3), ordered_result = TRUE)) %>% 
  mutate(cat.nonFIC = as.numeric(cat.nonFIC)) %>% 
  mutate(cat.ineq=cut(RCI, breaks=c(-Inf, 0.1, 0.3, Inf), labels=c(1,2,3), ordered_result = TRUE)) %>% 
  mutate(cat.ineq = as.numeric(cat.ineq))

str(df10)

# create classes
#install.packages("sf")
#install.packages("biscale")
#install.packages("remotes")
#remotes::install_github("slu-openGIS/biscale")
library(sf)
library(biscale)


df11 <- bi_class(df10, x = cat.nonFIC, y = cat.ineq,
                 style = "quantile", 
                 dim = 3) 

df12 <- bord_sf %>% 
  left_join(df11)

table(df12$bi_class)
class(df12)

# select colors:
custom_pal <- bi_pal_manual(val_1_1 = "#FEF9E7", val_1_2 = "#F7DC6F", val_1_3 = "#F1C40F",
                            val_2_1 = "#FAD7A0", val_2_2 = "#F5B041", val_2_3 = "#D68910",
                            val_3_1 = "#EDBB99", val_3_2 = "#DC7633", val_3_3 = "#BA4A00")

# create a legend
svg("./4_Figures/Map_bivariate_legend.svg", width = 2.5, height=2.5)
bi_legend(pal = custom_pal,
          dim = 3,
          xlab = "Lower immunization",
          ylab = "Higher inequality",   # expression(paste("Higher ", PM["2.5"],"")),
          size = 10) 
dev.off() 


# create a map
ggplot() +
  geom_sf(data = world, fill = "darkgray", size = 0.001, colour = "white") +
  geom_sf(data = df12, mapping = aes(fill = bi_class), color="lightgray",
          size = 0.001, show.legend = FALSE, na.rm = TRUE) +
  geom_sf(data = world_sub, fill = NA, size = 0.1, colour = "#333333") +
  coord_sf(xlim = c(-95, 145), ylim = c(-45, 50), expand = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3) + #pal = "DkBlue"
  #labs(title = "",
  #subtitle = "") +
  #bi_theme() +
  theme_minimal() +
  ggsave("./4_Figures/Map_bivariate.png", width = 8, height=5, dpi=600)


## Plot the CI for Haiti and Guatemala
ggplot() +
  geom_sf(data = world, fill = "darkgray", size = 0.001, colour = "white") +
  geom_sf(data = df12, aes(fill = bi_class), col="lightgray",  size = 0.001) +
  geom_sf(data = world_sub, fill = NA, size = 0.1, colour = "#333333", show.legend = FALSE, na.rm = TRUE) +
  coord_sf(xlim = c(-95, -68), ylim = c(10, 23) , expand = FALSE) +
  theme_void() +
  bi_scale_fill(pal = custom_pal, dim = 3) + #pal = "DkBlue"
  theme(legend.position = "none") +
  ggtitle("Central America & Carribean") +
  theme(plot.title = element_text(size = 22)) +
  theme(panel.border = element_rect(color = "#333333", fill = NA, size = 1)) +
  theme(plot.background= element_rect(fill = "white", colour = "white")) +
  ggsave("./4_Figures/Map_bivariate_Americas.png", width = 4.75, height=2.7, dpi=600)




## combine map with legend
#library("gridExtra")
#library(cowplot)

#fig <- ggdraw() +
#  draw_plot(map, 0, 0, 1, 1) +
#  draw_plot(legend, .6, .1, .25, .25)





################################################################################
#### 4. Upset plot
################################################################################
# code from: https://www.r-graph-gallery.com/upset-plot.html
#install.packages("UpSetR")
library(UpSetR)
library(grid)

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
#install.packages("wppExplorer")
library(wppExplorer)
wpp.explore() #retrieve the appropriate data from the interface

#pop <- read.csv("./1_Data/popagesex_2020.csv") %>% 
#  select(name, value) %>% 
#  dplyr::rename(country = name,
#         pop = value) %>% 
#  mutate(country = ifelse(country == "Tanzania, United Republic of", "Tanzania", country)) %>%
#  mutate(country = ifelse(country == "Gambia", "The Gambia", country)) %>% 
#  #adjust the population estimates, which are shown in thousands and for age group 0 to 5
#  mutate(pop = pop * 2/5 * 1000) 
  

pop <-  read.csv("./1_Data/popagesex_2020_single_years.csv") %>% 
  dplyr::select("country", "age1", "age2") %>% 
  mutate(country = ifelse(country == "United Republic of Tanzania", "Tanzania", country)) %>%
  mutate(country = ifelse(country == "Gambia", "The Gambia", country)) %>% 
  mutate(pop = (age1+age2)*1000)
  

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
  mutate(abs = share * pop) %>% 
  filter(vac_missing != "none" & !is.na(vac_missing)) %>% 
  ungroup()


df17 <- df16 %>% 
  group_by(vac_missing) %>% 
  dplyr::summarise(abs_tot = sum(abs)) %>% 
  mutate(abs_tot = abs_tot / 1000) %>%  #show results in thousands
  ungroup()

input <- tibble::deframe(df17)

## UpSet plot - data for all countries combined
#png(file = "./4_Figures/Upsetplot_all_countries.png", units="in", width=14, height=6, res=300)
svg("./4_Figures/Upsetplot_all_countries.svg", width=14, height=6)
upset(fromExpression(input), 
      #nintersects = 40, 
      #nsets = 4, 
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

ggplot() +
  geom_sf(data = world, fill = "lightgray", size = 0.001, colour = "white") +
  geom_sf(data = miss.vac, aes(fill = vac), colour="#333333",  size = 0.1) +
  coord_sf(xlim = c(-95, 145), ylim = c(-45, 50), expand = FALSE) +
  scale_fill_manual(values=c("#EFBC68", "#C2D7D0", "#5F9595", "#6A798A", "#363E48")) +
  labs(fill = "Vaccines most often missed") +
  theme_minimal() +
  theme(legend.direction='horizontal', legend.position="bottom") +
  ggsave("./4_Figures/Map_missing_vaccines.svg", width = 8, height=5, dpi=600)


#FFB7A1

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
        sets.x.label = "Number of missed vaccines")
  grid.text(i, x = 0.65, y=0.95, gp=gpar(fontsize=40))
dev.off()


################################################################################
## Export results tables
################################################################################


## Country-level results

tbl.cnt <- df5 %>% 
  select(!c(max, min)) %>% 
  left_join(cii.cnt) %>% 
  select(!c(max, min, c.cat)) %>% 
  arrange(country)

write.csv(tbl.cnt, "./3_Results/table_country_estimates.csv")

## Region-level results
library(sf)
st_geometry(share.vac.12.35) <- NULL
st_geometry(cii) <- NULL


tbl.reg <- share.vac.12.35 %>% 
  left_join(cii)

write.csv(tbl.reg, "./3_Results/table_region_estimates.csv")
