hawaii <- readOGR(dsn=paste0(boxdir,'fishchart2008.shp'),layer="Fishchart2008",stringsAsFactors=FALSE)
 
mhi_depth<-raster(paste0(boxdir,"himbsyn.bathytopo.1km.v19.grd"))
mhi_ext<-c(-161.000, -152.995, 16.995, 24.000)
mhi_depth<-crop(mhi_depth,mhi_ext)
crs(mhi_depth)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
mhi_raster<-rasterize(hawaii_fish,mhi_depth,field = 'AREA_ID')
crs(mhi_raster)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
mhi_depth[mhi_depth>0]<-NA
mhi_depth<-calc(mhi_depth, fun = function(x){x*-1})
mhi_area<-area(mhi_raster,na.rm = TRUE)
mhi_area<-mask(mhi_area,mhi_depth)
mhi_substrate<-raster(paste0(boxdir,"MHI_backscatterSynthesis/mhi_backscat_60m.nc"))
substrate<-projectRaster(mhi_substrate,mhi_area,filename=paste0(boxdir,"tmp/mhi_substrate.tif"),overwrite = TRUE)


writeRaster(mhi_raster,paste0(boxdir,"tmp/mhi_raster.tif"),overwrite = TRUE)

writeRaster(mhi_depth,paste0(boxdir,"tmp/mhi_depth.tif"),overwrite = TRUE)

writeRaster(mhi_area,paste0(boxdir,"tmp/mhi_area.tif"),overwrite = TRUE)

nwhi_depth<-raster(paste0(boxdir,"Falkor_NWHIfiles/fk140307-0502_bty240.nc"))
nwhi_ext<-c(-179.5012,-161.0001, 24.0001, 29.3344)
nwhi_depth<-crop(nwhi,nwhi_ext)
#crs(nwhi_depth)<-"+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
nwhi_depth<-calc(nwhi_depth, fun = function(x){x*-1})
nwhi_shape<-crop(hawaii,nwhi_depth)
#nwhi_shape@data[nwhi_shape@data$AREA_ID=='9999'|nwhi_shape@data$AREA_ID=='9998']<-NA
nwhi_raster<-rasterize(nwhi_shape,nwhi_depth,field= 'AREA_ID')#filename=paste0(boxdir,"tmp/nwhi_raster.tif"),overwrite = TRUE)
nwhi_raster[nwhi_raster==9999]<-NA
nwhi_raster[nwhi_raster==9998]<-NA
crs(nwhi_raster)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
nwhi_area<-area(nwhi_raster,na.rm = TRUE)
crs(nwhi_area)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
nwhi_substrate<-raster(paste0(boxdir,"Falkor_NWHIfiles/fk140307-0502_ss60.nc"))
nwhi_substrate<-projectRaster(nwhi_substrate,nwhi_area,filename=paste0(boxdir,"tmp/nwhi_substrate.tif"),overwrite = TRUE)

writeRaster(nwhi_raster,paste0(boxdir,"tmp/nwhi_raster.tif"),overwrite = TRUE)
writeRaster(nwhi_area,paste0(boxdir,"tmp/nwhi_area.tif"),overwrite=TRUE)
writeRaster(nwhi_depth,paste0(boxdir,"tmp/nwhi_depth.tif"),overwrite=TRUE)
