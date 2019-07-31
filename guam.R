


boxdir<- '/Users/lennonthomas/Box Sync/Council Stuff/data/'

guam_reef<-readOGR(dsn=paste0(boxdir,"Guam_shapefiles/",layer ="Guam_NewZones_Polygons_AlongReef.dbf"))



guam_offshore<-readOGR(dsn=paste0(boxdir,"Guam_shapefiles/",layer ="Guam_NewZones_Polygons_300mOffReef.dbf"))


#guam_depth<-readOGR(dsn = paste0(boxdir,"Guam_mb_lidar",layer = "Guam_mb_lidar"))

#test<-raster(paste0(boxdir,"Guam_mb_lidar/gua_mb_ld.asc"))


zones<-unique(Guam_catch$Area_FK)
reefzones<-unique(guam_reef$NewZONE)
offzones<-unique(guam_offshore$NewZONE)

all_guam<-union(guam_reef,guam_offshore)

Guam_catch<-read.csv(paste0(boxdir,"Guam -DAWR.csv"))

guam<-Guam_catch %>%
  group_by(Scie_Name,Area_FK)%>%
  summarise(total_catch=sum(Est_Lbs),
            total_number=sum(Num_Kept)) %>%
  ungroup() %>%
  filter(Scie_Name=="Scyllaridae (family)")

guam_df<-merge(all_guam,guam,by.x="NewZONE.1",by.y="Area_FK", all = TRUE)


guam_catch_map<-tm_shape(guam_df)+
  tm_polygons("total_catch",showNA=FALSE,colorNA="lightblue",n=6, title="Total Catch\n(Est. lbs)") +
  tm_legend( position = c("left","top")) +
  tm_legend(main.title.size = 2, text.size = .75)
 
save_tmap(guam_catch_map,paste0(boxdir,"Figures/p_pencill_guam_catch.png"))

fun<-function(x) {x*-1}

#guam_depth2<-raster(paste0(boxdir,"guam_5m/guam_5m.grd"))
guam_depth<-raster(paste0(boxdir,"Guam_mb_lidar/gua_mb_ld.asc"))

proj4string(guam_depth)<-crs("+proj=utm +zone=55 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
guam_depth<-projectRaster(guam_depth,guam_substrate,crs = "+proj=utm +zone=55 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
guam_depth<-calc(guam_depth[["gua_mb_ld"]],fun)

#guam_raster<-rasterize(guam_reef,guam_depth,field=)

guam_substrate<-raster(paste0(boxdir,"guam_subsrate/gua-re8101-unspv-5m.tif"))
guam_substrate[guam_substrate==0]<-NA

guam_substrate_map<-
  tm_shape(all_guam)+

  # tm_fill(col = "lightblue") +
  tm_borders(lwd =0.5) +
  tm_shape(guam_offshore) +
  tm_borders(lwd = 0.5) +
  tm_shape(guam_substrate) +
  tm_layout(legend.position = c("LEFT","top"),legend.outside=FALSE) +
  tm_raster(showNA=FALSE,title="Substrate Type",palette="-YlOrRd",legend.show = TRUE,style='cat',
            labels=c("hard","soft")) 

save_tmap(guam_substrate_map,paste0(boxdir,"Figures/guam_substrate.png"))

guam_depth[guam_depth<0]<-NA
guam_depth_map<- 
  tm_shape(guam_depth) +
  tm_layout(legend.position = c("LEFT","top"),legend.outside=FALSE) +
  tm_raster(showNA=FALSE,title="Depth (m)",palette="YlGnBu", n = 10, contrast = c(0.18, 0.9),legend.show = TRUE)+  
  tm_shape(all_guam)+
  #tm_fill(col = "lightblue") +
  tm_borders(lwd =1.2) 
  
 
save_tmap(guam_depth_map,paste0(boxdir,"Figures/guam_depth.png"))

guam_depth<-crop(guam_depth,guam_substrate)

ext<-extent(guam_substrate)
extent(guam_depth)<-ext

penc_guam_depth<-guam_depth
penc_guam_depth[penc_guam_depth>137 | penc_guam_depth<17]<-0
penc_guam_depth[penc_guam_depth>0]<-1

penc_guam_substrate<-guam_substrate
penc_guam_substrate[penc_guam_substrate==2]<-0
penc_guam_substrate[penc_guam_substrate==1]<-2
penc_guam_substrate[is.na(penc_guam_substrate)]<-1

penc_guam_s_cells<-overlay(penc_guam_depth,penc_guam_substrate,fun = function (x,y){x*y}) 


guam_penc_map<- 
  tm_shape(penc_guam_s_cells) +
  tm_layout(legend.position = c("RIGHT","bottom"),legend.outside=FALSE) +
  tm_raster(title="",showNA=FALSE,palette=c("purple","navy"), style="cat",legend.show = TRUE,labels=c("Adult/Juvenile habitat (depth)",
                                                                                        "Adult/Juvenile habitat (depth + substrate)"))+  
  
  tm_shape(all_guam)+
  #tm_fill(col = "lightblue") +
  tm_borders(lwd =1.2) +
  tm_layout(legend.width=1,frame=FALSE)

#penc_guam_s_cells[penc_guam_s_cells>0]<-1
penc_guam_s_cells[penc_guam_s_cells==0]<-NA
writeRaster(penc_guam_s_cells,filename = paste0(boxdir,"final_results/Guam_Scyllaridae_EFH.tif"),overwrite = TRUE)

penc_guam_s_cells_area<-area(penc_guam_s_cells,na.rm = TRUE)

penc_guam_zonal<-as.data.frame(penc_guam_s_cells_area)

write.csv(penc_guam_zonal,paste0(boxdir,"final_results/Guam_Scyllaridae_EFH_df.csv"))

sum(penc_guam_zonal,na.rm = TRUE)

test<-penc_guam_zonal[!is.na(penc_guam_zonal)]

guam_penc_suit_map<-
  tm_shape(guam_reef)+
  tm_fill(col = "lightblue",alpha=0.5) +
  tm_borders(lwd =0.5) +
  tm_shape(penc_guam_s_cells) +
  tm_layout(legend.position = c("LEFT","top"),legend.outside=FALSE) +
  tm_raster(showNA=FALSE,title="P.penicillatus",palette="red",legend.show = TRUE,labels = "EFH") 

save_tmap(guam_penc_suit_map,paste0(boxdir,"final_results/P_pencillatus_Guam_EFH.png"))

calc(penc_guam_s_cells,fun="sum")
sum(penc_guam_s_cells_area,na.rm=TRUE)
