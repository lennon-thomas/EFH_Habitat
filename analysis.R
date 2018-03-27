rm(list = ls())

library(tidyverse)
library(rgdal)
library(raster)
library(tmap)


boxdir<- '/Users/lennonthomas/Box Sync/Council Stuff/data/'

hawaii <- readOGR(dsn=paste0(boxdir,'fishchart2008.shp'),layer="Fishchart2008",stringsAsFactors=FALSE)

ext<-c(-172,-154,18.5,26)

hawaii<-crop(hawaii,ext)


fish_data<-read_csv(paste0(boxdir,"Hawaii -DAR.csv"))

fish_sum<-fish_data %>%
  group_by(Area_FK, Scie_Name) %>%
  summarise(total_landings = log(sum(Lbs_Kept,na.rm = TRUE))) %>%
  filter(!is.na(Area_FK)) %>%
  filter(!Area_FK %in% c(0,10,99999)) 
#spread(Scie_Name, total_landings) %>%

kona_crab<-fish_sum%>%
  filter(Scie_Name=="Ranina ranina")

hawaii_fish<-merge(hawaii,kona_crab, by.x = "AREA_ID", by.y = "Area_FK") 

hawaii_df<-as_data_frame(hawaii) %>%
  mutate(type_two = ifelse(TYPE =="Island","Land","Water")) %>%
  dplyr::select(AREA_ID, type_two)

land<-hawaii[hawaii@data$TYPE=="Island",]

#hawaii_fish<-hawaii_fish[!is.na(hawaii_fish@data$total_landings),]

tm_shape(hawaii_fish) +
  tm_fill(col = "total_landings",colorNA="lightblue") +
  tm_borders(lwd = 1.2) +
  tm_shape(land)+
  tm_fill(col = "white") +
  tm_legend(main.title.size = 2, text.size = 1, position = c("right","top"),main.title = "Kona crab landings- All")+
  tm_grid()

kona_crab_public<-read.csv(paste0(boxdir,"/kcrab.csv")) 
colnames(kona_crab_public)<-c("area","Year","Lic","landings")

kona_crab_public<-kona_crab_public %>%  
                    group_by(area) %>%
                    summarise(total_landings=sum(landings))

kona_crab_sp<-merge(hawaii,kona_crab_public,by.x="AREA_ID",by.y="area")

substrate<-brick(paste0(boxdir,"MHI_backscatterSynthesis/mhi_backscat_60m.nc"))

substrate_reproj<-projectRaster(substrate,crs(hawaii),res=c(60,60))
