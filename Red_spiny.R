
#juvenile habitat 1-30 m ; benthic habitat with intermediate (5-30 cm) vertical relief (Parrish and Polovina 1994)
#adult habitat 20-90 m; Slopes of banks with rocky substrate or  found in cracks and crevices in coral reef habitat  (Polovina 1989; Pitcher 1993)

rm(list = ls())
library(tidyverse)
library(rgdal)
library(raster)
library(tmap)



boxdir<- '/Users/lennonthomas/Box Sync/Council Stuff/data/'



fish_data_all<-read_csv(paste0(boxdir,"Hawaii -DAR.csv"))


ggsave(paste0(boxdir,"red_lobster_results/lobster_landings.png"))


p_marg<-fish_data_all %>%
  group_by(Area_FK, Scie_Name) %>%
  summarise(total_landings = sum(Lbs_Kept,na.rm = TRUE)) %>%
  filter(!is.na(Area_FK)) %>%
  filter(!Area_FK %in% c(0,10,99999)) %>%
  filter(Scie_Name == "Panulirus marginatus")

hawaii <- readOGR(dsn=paste0(boxdir,'fishchart2008.shp'),layer="Fishchart2008",stringsAsFactors=FALSE)

hawaii_fish<-merge(hawaii,p_marg, by.x = "AREA_ID", by.y = "Area_FK",all=TRUE) 


mhi_marginus_depth<-raster(paste0(boxdir,"himbsyn.bathytopo.1km.v19.grd"))
mhi_raster<-rasterize(hawaii_fish,mhi_marginus_depth,field = 'AREA_ID')
writeRaster(mhi_raster,paste0(boxdir,"red_lobster_results/mhi_raster.tif"),overwrite = TRUE)

nwhi_marginus_depth<-raster(paste0(boxdir,"Falkor_NWHIfiles/fk140307-0502_bty240.nc"))
crs(nwhi_marginus_depth)<-"+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

nwhi_lobster<-crop(hawaii_fish,nwhi_marginus_depth)
nwhi_lobster_raster<-rasterize(nwhi_lobster,nwhi_marginus_depth,field= 'AREA_ID')
writeRaster(nwhi_lobster_raster,paste0(boxdir,"red_lobster_results/nwhi_marg_landings.tif"))




 
crs(nwhi_lobster_raster)<-"+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

zonal_nwhi_marginus_adult_depth<-as.data.frame(zonal(nwhi_marginus_adult_depth,nwhi_lobster_raster,fun = "sum",na.rm = TRUE))

marinus_adult_suitable<-rbind(zonal_mhi_marginus_adult_depth,zonal_nwhi_marginus_adult_depth)
colnames(marinus_adult_suitable)<-c("AREA_ID","depth_area")

marinus_adult_suitable<-merge(marinus_adult_suitable, p_marg, by.x="AREA_ID",all=TRUE, by.y = "Area_FK")

# 60 m substrate data
substrate<-raster(paste0(boxdir,"MHI_backscatterSynthesis/mhi_backscat_60m.nc"))
substrate[substrate>=140]<-1
substrate[substrate<140]<-NA
substrate<-projectRaster(substrate,mhi_lobster_raster)

substrate_area<-area(substrate, na.rm = TRUE)
zonal_substrate_area<-as.data.frame(zonal(substrate_area,mhi_lobster_raster,fun = "sum", na.rm = TRUE))

nwhi_substrate<-raster(paste0(boxdir,"Falkor_NWHIfiles/fk140307-0502_ss60.nc"))
nwhi_substrate[nwhi_substrate>=140]<-1
nwhi_substrate[nwhi_substrate<140]<-NA
nwhi_substrate<-projectRaster(nwhi_substrate,nwhi_lobster_raster)
nwhi_substrate_area<-area(nwhi_substrate,na.rm = TRUE)
nwhi_zonal_substrate_area<-as.data.frame(zonal(nwhi_substrate_area,nwhi_lobster_raster, fun = "sum", na.rm = TRUE))


all_zonal_substrate_area<-rbind(zonal_substrate_area, nwhi_zonal_substrate_area)
colnames(all_zonal_substrate_area)<-c("AREA_ID","substrate_area")



mhi_marginus_adult_depth[is.na(mhi_marginus_adult_depth)]<-0
nwhi_marginus_adult_depth[is.na(nwhi_marginus_adult_depth)]<-0


substrate[is.na(substrate)]<-0
nwhi_substrate[is.na(nwhi_substrate)]<-0

mhi_s_cells<-overlay(mhi_marginus_adult_depth,substrate,fun = function (x,y){x*y}) 
writeRaster(mhi_s_cells,filename = paste0(boxdir,"red_lobster_results/mhi_marginus_suitable.tif"),overwrite = TRUE)


nwhi_s_cells<-overlay(nwhi_marginus_adult_depth,nwhi_substrate,fun = function (x,y){x*y}) 
writeRaster(nwhi_s_cells,filename = paste0(boxdir,"red_lobster_results/nwhi_marginus_suitable.tif"),overwrite = TRUE)



mhi_s_cells[mhi_s_cells==0]<-NA
nwhi_s_cells[nwhi_s_cells==0]<-NA

mhi_s_area<-area(mhi_s_cells,na.rm = TRUE)
nwhi_s_area<-area(nwhi_s_cells,na.rm = TRUE)

mhi_s_zonal<-as.data.frame(zonal(mhi_s_area,mhi_lobster_raster,fun= "sum",na.rm = TRUE))
nwhi_s_zonal<-as.data.frame(zonal(nwhi_s_area,nwhi_lobster_raster,fun= "sum",na.rm = TRUE))

all_suitable<-rbind(mhi_s_zonal,nwhi_s_zonal)

mhi_s_df<-as.data.frame(zonal(mhi_s_cells,mhi_lobster_raster,fun="sum",na.rm=TRUE))

colnames(mhi_s_df)<-c("AREA_ID","Suitable_area")

marinus_adult_suitable<-merge(marinus_adult_suitable, mhi_s_df, by="AREA_ID",all=TRUE)

write.csv(marinus_adult_suitable, paste0(boxdir,"red_lobster_results/marginus_suitable_df.csv"))

#writeRaster(substrate_reproj,filename = paste0(boxdir,"tmp/mhi_substrate.tif"))
#writeRaster(nwhi_substrate_reproj,filename = paste0(boxdir,"tmp/nwhi_substrate.tif"))
# Read HDAR private and. public fishery data ------------------------------


mhi_juvenile_depth<-mhi_depth
mhi_juvenile_depth[mhi_juvenile_depth>30 | mhi_juvenile_depth<1]<-NA
mhi_juvenile_depth_area<-mask(mhi_area,mhi_juvenile_depth)
mhi_juvenile_depth_zone<-as.data.frame(zonal(mhi_juvenile_depth_area,mhi_raster,fun="sum"))
col_names(mhi_juvenile_depth_zone)<-c("AREA_ID","juvenile_depth")

nwhi_juvenile_depth<-nwhi_depth
nwhi_juvenile_depth[nwhi_juvenile_depth>30 | nwhi_juvenile_depth<1]<-NA
nwhi_juvenile_depth_area<-mask(nwhi_area,nwhi_juvenile_depth)
nwhi_juvenile_depth_zone<-as.data.frame(zonal(nwhi_juvenile_depth_area,nwhi_raster,fun="sum"))
col_names(nwhi_juvenile_depth_zone)<-c("AREA_ID","juvenile_depth")

suitable_juvenile_depth<-rbind(mhi_juvenile_depth_zone,nwhi_depth_zone)

mhi_juvenile_depth_area[is.na(mhi_juvenile_depth_area)]<-0
nwhi_juvenile_depth_area[is.an(mhi_juvenile_depth_area)]<-0


mhi_subtrate_juvenile<-mhi_substrate
mhi_subtrate_juvenile[mhi_subtrate_juvenile < 140 ]<-NA
mhi_substrate_juvenile_area<-mask(mhi_area,mhi_substrate_juvenile)
mhi_substrate_juvnile_zone<-as.data.frame(zonal(mhi_substrate_juvenile_area,mhi_raster,fun="sum")) 
colnames(mhi_substrate_juvnile_zone)<-c("AREA_ID","juvenile_substrate")

nwhi_subtrate_juvenile<-nwhi_substrate
nwi_subtrate_juvenile[nwi_subtrate_juvenile < 140 ]<-NA
nwhi_substrate_juvenile_area<-mask(nwhi_area,mhi_substrate_juvenile)
nwhi_substrate_juvenile_zone<-as.data.frame(zonal(nwhi_substrate_juvenile_area,nwhi_raster,fun="sum")) 
colnames(nwhi_substrate_juvenile_zone)<-c("AREA_ID","juvenile_substrate")

suitable_juvenile_substrate<-rbind(mhi_substrate_juvenile_zone,nwhi_substrate_juvenile_zone)

mhi_substrate_juvenile_area[is.na(mhi_subtrate_juvenile_area)]<-0
nwhi_substrate_juvenile_area[is.na(nwhi_substrate_juvenile_area)]<-0

mhi_juve_s_cells<-overlay(mhi_juvenile_depth_area,mhi_substrate_juvenile_area,fun = function (x,y){x*y}) 
writeRaster(mhi_juve_s_cells,filename = paste0(boxdir,"red_lobster_results/mhi_juvenile_suitable.tif"),overwrite = TRUE)

nwhi_juve_s_cells<-overlay(nwhi_juvenile_depth_area,nwhi_substrate_juvenile_area,fun = function (x,y){x*y}) 
writeRaster(mhi_juve_s_cells,filename = paste0(boxdir,"red_lobster_results/nwhi_juvenile_suitable.tif"),overwrite = TRUE)

mhi_juvenile_zonal<-as.data.frame(zonal(mhi_juve_s_cells,mhi_raster,fun="sum"))
nwhi_juvenile_zonal<-as.data.frame(zonal(nwhi_juve_s_cells,nwhi_raster,fun="sum"))

all_juvenile<-rbind(mhi_juvenile_zonal,nwhi_juvenile_zonal)
colnames(all_juvenile)<-c("AREA_ID","juvenile_suitable)")
juvenile_results<-merge(suitable_juvenile_depth,suitable_juvenile_substrate,all_juvenile)

pangulris_results<-merge(adult_results,juvenile_results)
write.csv(pangulris_results,paste0(boxdir,"red_lobster_results/pangulris_results_df.csv"))