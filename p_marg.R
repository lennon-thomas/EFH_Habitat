#juvenile habitat 1-30 m ; benthic habitat with intermediate (5-30 cm) vertical relief (Parrish and Polovina 1994)
#adult habitat 20-90 m; Slopes of banks with rocky substrate or  found in cracks and crevices in coral reef habitat  (Polovina 1989; Pitcher 1993)

rm(list = ls())
library(tidyverse)
library(rgdal)
library(raster)
library(tmap)



boxdir<- '/Users/lennonthomas/Box Sync/Council Stuff/data/'
#read in Data

mhi_raster<-raster(paste0(boxdir,"tmp/mhi_raster.tif"))
mhi_depth<-raster(paste0(boxdir,"tmp/mhi_depth.tif"))
mhi_substrate<-raster(paste0(boxdir,"tmp/mhi_substrate.tif"))
mhi_area<-raster(paste0(boxdir,"tmp/mhi_area.tif"))

nwhi_raster<-raster(paste0(boxdir,"tmp/nwhi_raster.tif"))
nwhi_depth<-raster(paste0(boxdir,"tmp/nwhi_depth.tif"))
nwhi_substrate<-raster(paste0(boxdir,"tmp/nwhi_substrate.tif"))
nwhi_area<-raster(paste0(boxdir,"tmp/nwhi_area.tif"))

##########################################################################
##########################################################################
########  Panulirus marginatus Data###############
##########################################################################
##########################################################################

boxdir<- '/Users/lennonthomas/Box Sync/Council Stuff/data/'

fish_data_all<-read_csv(paste0(boxdir,"Hawaii -DAR.csv"))

p_marg<-fish_data_all %>%
  group_by(Area_FK, Scie_Name) %>%
  summarise(total_landings = sum(Lbs_Kept,na.rm = TRUE)) %>%
  filter(!is.na(Area_FK)) %>%
  filter(!Area_FK %in% c(0,10,99999)) %>%
  filter(Scie_Name == "Panulirus marginatus")

hawaii <- readOGR(dsn=paste0(boxdir,'fishchart2008.shp'),layer="Fishchart2008",stringsAsFactors=FALSE)

hawaii_fish<-merge(hawaii,p_marg, by.x = "AREA_ID", by.y = "Area_FK",all=TRUE) 

landings<-hawaii_fish@data[,c("AREA_ID","total_landings")]
landings$AREA_ID<-as.numeric(landings$AREA_ID)

mhi_zonal<-as.data.frame(zonal(mhi_area,mhi_raster,fun = "sum"))
colnames(mhi_zonal)<-c("AREA_ID","Total_area")

nwhi_zonal<-as.data.frame(zonal(nwhi_area,nwhi_raster,fun = "sum"))
colnames(nwhi_zonal)<-c("AREA_ID","Total_area")

results_df<-rbind(mhi_zonal,nwhi_zonal)
results_df<-merge(results_df,p_marg,by.x="AREA_ID",by.y="Area_FK",all=TRUE)


##########################################################################
##########################################################################
######## Adult Panulirus marginatus suitability analysis###############
##########################################################################
##########################################################################

#Adult suitable depth
mhi_adult_depth<-mhi_depth
mhi_adult_depth[mhi_adult_depth>150 | mhi_adult_depth<20]<-NA
mhi_adult_depth_area<-mask(mhi_area,mhi_adult_depth)
mhi_adult_depth_zone<-as.data.frame(zonal(mhi_adult_depth_area,mhi_raster,fun="sum"))
colnames(mhi_adult_depth_zone)<-c("AREA_ID","adult_depth")

nwhi_adult_depth<-nwhi_depth
nwhi_adult_depth[nwhi_adult_depth>150| nwhi_adult_depth<20]<-NA
nwhi_adult_depth_area<-mask(nwhi_area,nwhi_adult_depth)
nwhi_adult_depth_zone<-as.data.frame(zonal(nwhi_adult_depth_area,nwhi_raster,fun="sum"))
colnames(nwhi_adult_depth_zone)<-c("AREA_ID","adult_depth")

suitable_adult_depth<-rbind(mhi_adult_depth_zone,nwhi_adult_depth_zone)

results_df<-merge(results_df,suitable_adult_depth,by="AREA_ID",all = TRUE)

mhi_adult_depth_area[is.na(mhi_adult_depth_area)]<-0
nwhi_adult_depth_area[is.na(nwhi_adult_depth_area)]<-0

#Adults suitable substrate
mhi_subtrate_adult<-mhi_substrate
mhi_subtrate_adult[mhi_subtrate_adult < 140 ]<-NA
mhi_substrate_adult_area<-mask(mhi_area,mhi_subtrate_adult)
mhi_substrate_adult_zone<-as.data.frame(zonal(mhi_substrate_adult_area,mhi_raster,fun="sum")) 
colnames(mhi_substrate_adult_zone)<-c("AREA_ID","adult_substrate")

nwhi_subtrate_adult<-nwhi_substrate
nwhi_subtrate_adult[nwhi_subtrate_adult < 140 ]<-NA
nwhi_substrate_adult_area<-mask(nwhi_area,nwhi_subtrate_adult)
nwhi_substrate_adult_zone<-as.data.frame(zonal(nwhi_substrate_adult_area,nwhi_raster,fun="sum")) 
colnames(nwhi_substrate_adult_zone)<-c("AREA_ID","adult_substrate")

suitable_adult_substrate<-rbind(mhi_substrate_adult_zone,nwhi_substrate_adult_zone)

results_df<-merge(results_df,suitable_adult_substrate)

mhi_substrate_adult_area[is.na(mhi_substrate_adult_area)]<-0
nwhi_substrate_adult_area[is.na(nwhi_substrate_adult_area)]<-0

mhi_adult_s_cells<-overlay(mhi_adult_depth_area,mhi_substrate_adult_area,fun = function (x,y){x*y}) 
writeRaster(mhi_adult_s_cells,filename = paste0(boxdir,"red_lobster_results/mhi_adult_suitable.tif"),overwrite = TRUE)

nwhi_adult_s_cells<-overlay(nwhi_adult_depth_area,nwhi_substrate_adult_area,fun = function (x,y){x*y}) 
writeRaster(mhi_adult_s_cells,filename = paste0(boxdir,"red_lobster_results/nwhi_adult_suitable.tif"),overwrite = TRUE)

mhi_adult_zonal<-as.data.frame(zonal(mhi_adult_s_cells,mhi_raster,fun="sum"))
nwhi_adult_zonal<-as.data.frame(zonal(nwhi_adult_s_cells,nwhi_raster,fun="sum"))

all_adult<-rbind(mhi_adult_zonal,nwhi_adult_zonal)
colnames(all_adult)<-c("AREA_ID","adult_suitable")
adult_results<-merge(results_df,all_adult,by="AREA_ID")

write.csv(results_df,paste0(boxdir,"red_lobster_results/adult_results.csv"))


##########################################################################
##########################################################################
######## Juvenile Panulirus marginatus suitability analysis###############
##########################################################################
##########################################################################

mhi_juvenile_depth<-mhi_depth
mhi_juvenile_depth[mhi_juvenile_depth>30 | mhi_juvenile_depth<1]<-NA
mhi_juvenile_depth_area<-mask(mhi_area,mhi_juvenile_depth)
mhi_juvenile_depth_zone<-as.data.frame(zonal(mhi_juvenile_depth_area,mhi_raster,fun="sum"))
colnames(mhi_juvenile_depth_zone)<-c("AREA_ID","juvenile_depth")

nwhi_juvenile_depth<-nwhi_depth
nwhi_juvenile_depth[nwhi_juvenile_depth>30 | nwhi_juvenile_depth<1]<-NA
nwhi_juvenile_depth_area<-mask(nwhi_area,nwhi_juvenile_depth)
nwhi_juvenile_depth_zone<-as.data.frame(zonal(nwhi_juvenile_depth_area,nwhi_raster,fun="sum"))
colnames(nwhi_juvenile_depth_zone)<-c("AREA_ID","juvenile_depth")

suitable_juvenile_depth<-rbind(mhi_juvenile_depth_zone,nwhi_juvenile_depth_zone)

mhi_juvenile_depth_area[is.na(mhi_juvenile_depth_area)]<-0
nwhi_juvenile_depth_area[is.na(nwhi_juvenile_depth_area)]<-0


mhi_subtrate_juvenile<-mhi_substrate
mhi_subtrate_juvenile[mhi_subtrate_juvenile < 140 ]<-NA
mhi_substrate_juvenile_area<-mask(mhi_area,mhi_subtrate_juvenile)
mhi_substrate_juvnile_zone<-as.data.frame(zonal(mhi_substrate_juvenile_area,mhi_raster,fun="sum")) 
colnames(mhi_substrate_juvnile_zone)<-c("AREA_ID","juvenile_substrate")

nwhi_subtrate_juvenile<-nwhi_substrate
nwhi_subtrate_juvenile[nwhi_subtrate_juvenile < 140 ]<-NA
nwhi_substrate_juvenile_area<-mask(nwhi_area,nwhi_subtrate_juvenile)
nwhi_substrate_juvenile_zone<-as.data.frame(zonal(nwhi_substrate_juvenile_area,nwhi_raster,fun="sum")) 
colnames(nwhi_substrate_juvenile_zone)<-c("AREA_ID","juvenile_substrate")

suitable_juvenile_substrate<-rbind(mhi_substrate_juvnile_zone,nwhi_substrate_juvenile_zone)

mhi_substrate_juvenile_area[is.na(mhi_substrate_juvenile_area)]<-0
nwhi_substrate_juvenile_area[is.na(nwhi_substrate_juvenile_area)]<-0

mhi_juve_s_cells<-overlay(mhi_juvenile_depth_area,mhi_substrate_juvenile_area,fun = function (x,y){x*y}) 
writeRaster(mhi_juve_s_cells,filename = paste0(boxdir,"red_lobster_results/mhi_juvenile_suitable.tif"),overwrite = TRUE)

nwhi_juve_s_cells<-overlay(nwhi_juvenile_depth_area,nwhi_substrate_juvenile_area,fun = function (x,y){x*y}) 
writeRaster(mhi_juve_s_cells,filename = paste0(boxdir,"red_lobster_results/nwhi_juvenile_suitable.tif"),overwrite = TRUE)

mhi_juvenile_zonal<-as.data.frame(zonal(mhi_juve_s_cells,mhi_raster,fun="sum"))
nwhi_juvenile_zonal<-as.data.frame(zonal(nwhi_juve_s_cells,nwhi_raster,fun="sum"))

all_juvenile<-rbind(mhi_juvenile_zonal,nwhi_juvenile_zonal)
colnames(all_juvenile)<-c("AREA_ID","juvenile_suitable)")
juvenile_results<-merge(suitable_juvenile_depth,suitable_juvenile_substrate,by="AREA_ID")
juvenile_results<-merge(juvenile_results,all_juvenile)
write.csv(juvenile_results,paste0(boxdir,"red_lobster_results/juvenile_results.csv"))

##########################################################################
##########################################################################
## Merge juvenile and  Panulirus marginatus suitability analysis results##
##########################################################################
##########################################################################
pangulris_results<-merge(adult_results,juvenile_results,by="AREA_ID")
write.csv(pangulris_results,paste0(boxdir,"red_lobster_results/pangulris_results_df.csv"))
