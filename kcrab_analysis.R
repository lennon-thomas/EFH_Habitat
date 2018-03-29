## Crustacean EFH Habitat Analysis
## Authour: Lennon Thomas
## Date: March 27, 2018
## Purpose: Idenify EFH habitat for Kona crab in Hawaii



rm(list = ls())

library(tidyverse)
library(rgdal)
library(raster)
library(tmap)



boxdir<- '/Users/lennonthomas/Box Sync/Council Stuff/data/'


# HDAR fishing area , depth and substrate spatial files ---------------------------------------------


hawaii <- readOGR(dsn=paste0(boxdir,'fishchart2008.shp'),layer="Fishchart2008",stringsAsFactors=FALSE)

#nwhi_extent<-c(-162.005, -152.995, 16.995, 25.005)

#nw_hawaii<-crop(hawaii,nwhi_extent)


# crop to areas with positive crutacean catch
#ext<-c(-172,-154,18.5,26)

#hawaii<-crop(hawaii,ext)

# Land Shapefile
land<-hawaii[hawaii@data$TYPE=="Island",]

# 1km depth raster layer

depth<-raster(paste0(boxdir,"himbsyn.bathytopo.1km.v19.grd"))

nwhi_depth<-raster(paste0(boxdir,"Falkor_NWHIfiles/fk140307-0502_bty240.nc"))


# rasterize catch area shapefile
hawaii_raster<-rasterize(hawaii,depth,field="AREA_ID")
hawaii_raster[hawaii_raster==9998]<-NA
writeRaster(hawaii_raster,filename = paste0(boxdir,"tmp/hawaii_raster.tif"))

nwhi_raster<-rasterize(hawaii,nwhi_depth,field = "AREA_ID", filename=paste0(boxdir,"tmp/nwhi_raster.tif"),overwrite = TRUE)


# 60 m substrate data
substrate<-raster(paste0(boxdir,"MHI_backscatterSynthesis/mhi_backscat_60m.nc"))
substrate[substrate<140]<-1
substrate[substrate>1]<-2


nwhi_substrate<-raster(paste0(boxdir,"Falkor_NWHIfiles/fk140307-0502_ss60.nc"))
nwhi_substrate[nwhi_substrate<140]<-1
nwhi_substrate[nwhi_substrate>1]<-2


# reproject substrate raster
substrate_reproj<-projectRaster(substrate,hawaii_raster)
nwhi_substrate_reproj<-projectRaster(nwhi_substrate,nwhi_raster)
writeRaster(substrate_reproj,filename = paste0(boxdir,"tmp/mhi_substrate.tif"))
writeRaster(nwhi_substrate_reproj,filename = paste0(boxdir,"tmp/nwhi_substrate.tif"))
# Read HDAR private and. public fishery data ------------------------------

fish_data_all<-read_csv(paste0(boxdir,"Hawaii -DAR.csv"))

# Sum all by area for each species
fish_sum<-fish_data_all %>%
  group_by(Area_FK, Scie_Name) %>%
  summarise(total_landings = sum(Lbs_Kept,na.rm = TRUE)) %>%
  filter(!is.na(Area_FK)) %>%
  filter(!Area_FK %in% c(0,10,99999)) 

# Filter out Kona crab data
kona_crab_all<-fish_sum%>%
  filter(Scie_Name=="Ranina ranina")





# read in public Kona crab data
kona_crab_public<-read.csv(paste0(boxdir,"/kcrab.csv")) 
colnames(kona_crab_public)<-c("area","Year","Lic","landings")

kona_crab_public<-kona_crab_public %>%
  group_by(area) %>%
  summarise(total_landings=sum(landings))



# Join landings data to spatial data --------------------------------------

hawaii_fish<-merge(hawaii,kona_crab_all, by.x = "AREA_ID", by.y = "Area_FK") 

hawaii_landings_raster<-rasterize(hawaii_fish,depth,field = "total_landings",filename=paste0(boxdir,"khawaii_fish_raster.tif"),overwrite = TRUE)

hawaii_area<-raster::area(hawaii_raster,na.rm = TRUE)
nwhi_area<-raster::area(nwhi_raster,na.rm = TRUE)

area_per_zone<-as.data.frame(zonal(hawaii_area,hawaii_raster, fun = "sum", na.rm = TRUE))
nwhi_area_zone<-as.data.frame(zonal(nwhi_area,nwhi_raster, fun = "sum", na.rm = TRUE))

area_per_zone<-rbind(area_per_zone,nwhi_area_zone)

colnames(area_per_zone)<-c("AREA_ID","Total_area")

# Calculate suitable depth and substrate type by area ---------------------

depth[depth>0]<-NA

depth[depth>-2]<-NA

depth[-200>depth]<-NA

#nwhi_depth[nwhi_depth>0]<-NA

nwhi_depth[nwhi_depth>-2]<-NA

nwhi_depth[-200>nwhi_depth]<-NA

#area if each cell within 2 to 200 m
area_depth<-raster::area(depth,na.rm = TRUE)

crs(nwhi_area)
area_nwhi_depth<-raster::area(nwhi_depth,na.rm = TRUE)

depth_per_zone<-as.data.frame(zonal(area_depth,hawaii_raster,fun = 'sum',na.rm = TRUE))

area_nwhi_depth<-projectRaster(area_nwhi_depth,nwhi_raster)

nwhi_depth_per_zone<-as.data.frame(zonal(area_nwhi_depth,nwhi_raster,fun = 'sum',na.rm = TRUE))

# min_depth_zone<-as.data.frame(zonal(depth,hawaii_raster,fun = "max", na.rm = TRUE))
depth_per_zone<-rbind(depth_per_zone,nwhi_depth_per_zone)

colnames(depth_per_zone)<-(c("AREA_ID","Suitable_depth_area"))

suitability<-merge(area_per_zone,depth_per_zone, by="AREA_ID")

# 140 is the cutoff for soft substrate
substrate_reproj[substrate_reproj==2]<-NA
nwhi_substrate_reproj[nwhi_substrate_reproj==2]<-NA

area_substrate<-raster::area(substrate_reproj,na.rm = TRUE)
nwhi_area_substrate<-raster::area(nwhi_substrate_reproj,na.rm = TRUE)

substrate_per_zone<-as.data.frame(zonal(area_substrate,hawaii_raster,fun='sum',na.rm=TRUE))
nwhi_substrate_per_zone<-as.data.frame(zonal(nwhi_area_substrate,nwhi_raster,fun='sum',na.rm=TRUE))

substrate_per_zone<-rbind(substrate_per_zone,nwhi_substrate_per_zone)

colnames(substrate_per_zone)<-c("AREA_ID","Suitable_substrate_area")

suitability<-merge(suitability,substrate_per_zone, by="AREA_ID")

suitability<-merge(suitability, kona_crab_all, by.x="AREA_ID", by.y = "Area_FK")

write.csv(suitability,paste0(boxdir,"k_crab_results/kcrab_suitability_df.csv"))
# Find all suitable data --------------------------------------------------
substrate_reproj[is.na(substrate_reproj)]<-0

substrate_reproj[substrate_reproj>0]<-1

nwhi_substrate_reproj[is.na(nwhi_substrate_reproj)]<-0

nwhi_substrate_reproj[substrate_reproj>0]<-1

depth[is.na(depth)]<-0
depth[depth>0] <-1

nwhi_depth[is.na(nwhi_depth)]<-0
nwhi_depth[nwhi_depth>0] <-1

s_cells<-overlay(depth,substrate_reproj,fun = function (x,y){x*y}) 
writeRaster(s_cells,filename = paste0(boxdir,"k_crab_results/mhi_suitable.tif"))

nwhi_substrate_reproj<-projectRaster(nwhi_substrate_reproj,nwhi_depth)

nwhi_s_cells<-overlay(nwhi_depth,nwhi_substrate_reproj,fun = function (x,y){x*y}) 
writeRaster(nwhi_s_cells,filename = paste0(boxdir,"k_crab_results/nwhi_suitable.tif"))

s_cells[s_cells==0]<-NA
nwhi_s_cells[nwhi_s_cells==0]<-NA

s_cells_area<-area(s_cells, na.rm = TRUE) 
nwhi_s_cells_area<-area(nwhi_s_cells, na.rm = TRUE)

nwhi_s_cells_area<-projectRaster(nwhi_s_cells_area,nwhi_raster)
s_cells_zone<-as.data.frame(zonal(s_cells_area,hawaii_raster, fun = "sum", na.rm = TRUE))
nwhi_s_cells_zone<-as.data.frame(zonal(nwhi_s_cells_area,nwhi_raster, fun = "sum", na.rm = TRUE))

s_cells_zone<-rbind(s_cells_zone,nwhi_s_cells_zone)
colnames(s_cells_zone)<-c("AREA_ID","Total_suitable_area")

suitability<-merge(suitability, s_cells_zone, by="AREA_ID")

write.csv(suitability,paste0(boxdir,"k_crab_results/kcrab_suitability_df.csv"))




tm_shape(hawaii_fish) +
  tm_fill(col = "total_landings",colorNA="lightblue") +
  tm_borders(lwd = 1.2) +
  tm_shape(land)+
  tm_fill(col = "white") +
  tm_legend(main.title.size = 2, text.size = 1, position = c("right","top"),main.title = "Kona crab landings- All")+
  tm_grid() +
  tm_text("AREA_ID")

# 
# 
# kona_crab_sp<-merge(hawaii,kona_crab_public,by.x="AREA_ID",by.y="area")






%>%
  dplyr::mutate(depth_suitabiilty= ifelse(sum==0,0,1))

colnames(depth_per_zone)<-c("AREA_ID","depth_area","depth_suitability")
hawaii_df<-as_data_frame(hawaii) %>%
  mutate(type_two = ifelse(TYPE =="Island","Land","Water")) %>%
  dplyr::select(AREA_ID, type_two)

substrate<-raster(paste0(boxdir,"MHI_backscatterSynthesis/mhi_backscat_60m.nc"))
substrate_reproj<-projectRaster(substrate,hawaii_raster)



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