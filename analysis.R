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
  tm_grid() +
  tm_text("AREA_ID")

# kona_crab_public<-read.csv(paste0(boxdir,"/kcrab.csv")) 
# colnames(kona_crab_public)<-c("area","Year","Lic","landings")
# 
# kona_crab_public<-kona_crab_public %>%  
#                     group_by(area) %>%
#                     summarise(total_landings=sum(landings))
# 
# kona_crab_sp<-merge(hawaii,kona_crab_public,by.x="AREA_ID",by.y="area")
hawaii_raster<-rasterize(hawaii,depth,field="AREA_ID")
hawaii_fish_raster<-rasterize(hawaii_fish,depth,field = "total_landings",filename=paste0(boxdir,"hawaii_fish_raster.tif",overwrite = TRUE))

depth<-raster(paste0(boxdir,"himbsyn.bathytopo.1km.v19.grd"))

depth[depth>0]<-NA

depth[depth>-2]<-NA
depth[-200>depth]<-NA

area_depth<-area(depth,na.rm = TRUE)

depth_per_zone<-as.data.frame(zonal(area_depth,hawaii_raster,fun = 'sum',na.rm = TRUE)) %>%
  dplyr::mutate(depth_suitabiilty= ifelse(sum==0,0,1))

colnames(depth_per_zone)<-c("AREA_ID","depth_area","depth_suitability")


substrate<-raster(paste0(boxdir,"MHI_backscatterSynthesis/mhi_backscat_60m.nc"))
substrate_reproj<-projectRaster(substrate,hawaii_raster)

substrate_reproj[substrate_reproj>140]<-NA
area_substrate<-area(substrate_reproj,na.rm = TRUE)
substrate_per_zone<-as.data.frame(zonal(area_substrate,hawaii_raster,fun='sum',na.rm=TRUE))
colnames(substrate_per_zone)<-c("AREA_ID","substrate_area")

area_substrate[is.na(area_substrate)]<-0
area_depth[is.na(area_depth)]<-0
area_depth[area_depth>1]<-1

suit<-stack(area_substrate,area_depth)
suit_final<-stackApply(suit,indices=c(1,1),fun=prod,na.rm = TRUE)

suit_zonal<-zonal(suit_final,hawaii_raster,fun='sum',na.rm = TRUE)
colnames(suit_zonal)<-c("AREA_ID","Suitable_area")

all_data<-merge(suit_zonal,depth_per_zone,by="AREA_ID")
all_data<-merge(all_data,substrate_per_zone,by = "AREA_ID")
landings<-hawaii_fish@data[,c("AREA_ID","total_landings")]
all_data<-merge(all_data,landings,by = "AREA_ID")


tm_shape(hawaii) +
  tm_fill(col = "lightblue",alpha=0.3) +
 # tm_text("AREA_ID",size=0.5)+
  tm_borders(lwd = 1.2) +
  tm_shape(land)+
  tm_fill(col = "white") +
  
  tm_shape(depth) +
  tm_raster()
