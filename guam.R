guam_reef<-readOGR(dsn=paste0(boxdir,"Guam_shapefiles/",layer ="Guam_NewZones_Polygons_AlongReef.dbf"))

guam_offshore<-readOGR(dsn=paste0(boxdir,"Guam_shapefiles/",layer ="Guam_NewZones_Polygons_300mOffReef.dbf"))

zones<-unique(Guam_catch$Area_FK)
reefzones<-unique(guam_reef$NewZONE)
offzones<-unique(guam_offshore$NewZONE)

all_guam<-union(guam_reef,guam_offshore)
guam<-Guam_catch %>%
  group_by(Scie_Name,Area_FK)%>%
  summarise(total_catch=sum(Est_Lbs),
            total_number=sum(Num_Kept)) %>%
  ungroup() %>%
  filter(Scie_Name=="Panulirus penicillatus")

all_guam_catch<-merge(all_guam,guam,by.x="NewZONE.1",by.y="Area_FK",all=TRUE) 

tm_shape(all_guam_catch)+
  tm_polygons("total_catch",showNA=FALSE,colorNA="lightblue",n=5)+
  tm_legend(main.title.size = 2, text.size = 1, position = c("left","top"),title = "P. Penicillatus landings")
  
guam_depth<-raster(paste0(boxdir,"guam_5m-2/guam_5m.asc"))
#crs(guam_depth)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

xy <- matrix(NA, 11155, 8574)
image(xy)

# Turn the matrix into a raster
rast <- raster(xy)
# Give it lat/lon coords for 36-37°E, 3-2°S
extent(rast) <- c(144.608,145.017,13.216,13.733)
# ... and assign a projection
projection(rast) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

guam_depth[guam_depth>0|-5>guam_depth]<-NA
                   
 writeRaster(guam_depth,paste0(boxdir,"guam_results/guam_lobster_depth.tif"))                  
  
 guam_rugosity<-raster(paste0(boxdir,"guam_rugosity/guam_5m_rugosity.asc"))                 
  
 guam_substrate<-raster(paste0(boxdir,"guam_subsrate/gua-re8101-unspv-5m.tif"))
  tm_shape(all_guam_catch) +
  tm_polygons(total_catch)
  geom_polygon(data=all_guam_catch,aes(x=long,y=lat,group=group,fill='total_catch'))+
  theme_bw()
