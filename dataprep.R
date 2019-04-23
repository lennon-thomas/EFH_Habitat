## Updated April 22, 2019

rm(list = ls())

library(tidyverse)
library(raster)
library(rgdal)
library(tmap)
library(ncdf4)
library(sp)
boxdir<- '/Users/lennonthomas/Box Sync/Council Stuff/data/'


# MHIs --------------------------------------------------------------------

hawaii <- readOGR(dsn=paste0(boxdir,'fishchart2008.shp'),layer="Fishchart2008",stringsAsFactors=FALSE)
mhi_ext<-c(-161,-154.3,18.5,22.75)
mhi<-crop(hawaii,mhi_ext)
 
mhi_depth<-raster(paste0(boxdir,"himbsyn.bathytopo.v19.grd"))
#mhi_depth<-raster(paste0(boxdir,"hawaii_bty_5m.tif"))
#mhi_depth2<-projectRaster(mhi_depth,crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
mhi_depth<-crop(mhi_depth,mhi_ext)
#crs(mhi_depth)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
mhi_raster<-rasterize(mhi,mhi_depth,field = 'AREA_ID')
crs(mhi_raster)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
mhi_depth[mhi_depth>0]<-NA
mhi_depth<-calc(mhi_depth, fun = function(x){x*-1})
mhi_area<-area(mhi_raster,na.rm = TRUE)
mhi_area<-mask(mhi_area,mhi_depth)

#mhi_substrate<-raster(paste0(boxdir,"hawaii_bs_msc.tif"))
#substrate<-projectRaster(mhi_substrate,mhi_area,filename=paste0(boxdir,"tmp/mhi_substrate.tif"),overwrite = TRUE)

substrate<-raster(paste0(boxdir,"MHI_backscatterSynthesis/mhi_backscat_60m.nc"))
substrate_reproj<-projectRaster(substrate,mhi_raster)
writeRaster(substrate_reproj,filename = paste0(boxdir,"tmp/mhi_substrate.tif"),overwrite=TRUE)

writeRaster(mhi_raster,paste0(boxdir,"tmp/mhi_raster.tif"),overwrite = TRUE)

writeRaster(mhi_depth,paste0(boxdir,"tmp/mhi_depth.tif"),overwrite = TRUE)

writeRaster(mhi_area,paste0(boxdir,"tmp/mhi_area.tif"),overwrite = TRUE)


# #NWHI -------------------------------------------------------------------

nwhi_ext<-c(-179.5012,-161.000, 22.751, 29.3344)
nwhi<-crop(hawaii,nwhi_ext)
#nwhi_depth<-raster(paste0(boxdir,"Falkor_NWHIfiles/fk140307-0502_bty240.nc")) # 816.3474
nwhi_depth<-nc_open(paste0(boxdir,"hurl_bathy_60m_nwhi.nc"))
lon <- ncvar_get(nwhi_depth,"lon")
lat <- ncvar_get(nwhi_depth,"lat")

dname <- "elev"
tmp_array <- ncvar_get(nwhi_depth,dname)
dlname <- ncatt_get(nwhi_depth,dname,"long_name")
dunits <- ncatt_get(nwhi_depth,dname,"units")
fillvalue <- ncatt_get(nwhi_depth,dname,"_FillValue")
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_vec <- as.vector(tmp_array)
#bathy <- tmp_array[]
pts <- data.frame(cbind(lonlat,tmp_vec))
names(pts) <- c("lon","lat",paste(dname,as.character(1), sep="_"))

pts1<-pts

test<-rasterFromXYZ(pts[1:774,],crs="+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")



library(sp)
library(rgdal)
coordinates(pts)=~lon+lat
proj4string(pts)=CRS("+init=epsg:4326") # set it to lat-long
pts = spTransform(pts,CRS("+init=epsg:4326"))

nwhi_depth<-crop(nwhi_depth,nwhi_ext)
crs(nwhi_depth)<-"+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
nwhi_depth<-calc(nwhi_depth, fun = function(x){x*-1})
#nwhi_shape@data[nwhi_shape@data$AREA_ID=='9999'|nwhi_shape@data$AREA_ID=='9998']<-NA
nwhi_raster<-rasterize(nwhi,nwhi_depth,field= 'AREA_ID')#filename=paste0(boxdir,"tmp/nwhi_raster.tif"),overwrite = TRUE)
nwhi_raster[nwhi_raster==9999]<-NA
nwhi_raster[nwhi_raster==9998]<-NA

#crs(nwhi_raster)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
nwhi_area<-area(nwhi_raster,na.rm = TRUE)
#crs(nwhi_area)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
nwhi_substrate<-raster(paste0(boxdir,"Falkor_NWHIfiles/fk140307-0502_ss60.nc"))
nwhi_substrate<-projectRaster(nwhi_substrate,nwhi_depth,filename=paste0(boxdir,"tmp/nwhi_substrate.tif"),overwrite = TRUE)

writeRaster(nwhi_raster,paste0(boxdir,"tmp/nwhi_raster.tif"),overwrite = TRUE)
writeRaster(nwhi_area,paste0(boxdir,"tmp/nwhi_area.tif"),overwrite=TRUE)
writeRaster(nwhi_depth,paste0(boxdir,"tmp/nwhi_depth.tif"),overwrite=TRUE)


eez<-readOGR(dsn=paste0(boxdir,"pac_ocs_usa_eez"),layer = "pac_ocs_usa_eez")
hawaii_eez<-eez[eez@data$name=="Hawaii",]
