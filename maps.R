hawaii <- readOGR(dsn=paste0(boxdir,'fishchart2008.shp'),layer="Fishchart2008",stringsAsFactors=FALSE)

ext<-c(-161,-154.3,18.5,22.75)

mhi_hawaii<-crop(hawaii,ext)
mhi_s_cells<-crop(mhi_s_cells,ext)

nwhi_ext<-c(-179.5012,-161.000, 22.751, 29.3344)
nwhi<-crop(hawaii,nwhi_ext)
# Land Shapefile
land<-mhi_hawaii[mhi_hawaii@data$TYPE=="Island",]
nwhi_land<-nwhi[nwhi@data$TYPE=="Island",]
# kcrab_mhi_map<-merge(mhi_hawaii,mhi_kcrab_results,by = "AREA_ID",all = TRUE)
penc_s_cells<-raster(paste0(boxdir,"final_results/MHI_P_penicillatus_EFH.tif"))

penc_mhi_map<- 
  tm_shape(mhi_hawaii)+
  tm_fill(col = "lightblue", alpha = 0.3) +
  tm_borders(lwd =0.4) +
  tm_shape(land)+
  tm_fill(col = "white",alpha = 1) +
  tm_borders(lwd = 1) +
  tm_shape(penc_s_cells) +
  tm_raster(showNA=FALSE,title="P. penicillatus",palette = "red",label="EFH") + 
  tm_legend(main.title.size = 1, text.size = 0.8, position = c("right","top"))


save_tmap(penc_mhi_map, paste0(boxdir,"final_results/MHI_p_penc_EFH.png"))




kcrab_mhi<- 
   tm_shape(mhi_hawaii)+
   tm_fill(col = "lightblue", alpha = 0.3) +
   tm_borders(lwd =0.4) +
  tm_shape(land)+
  tm_fill(col = "white",alpha = 1) +
  tm_borders(lwd = 1) +
  tm_shape(larv_depth,) +
  tm_raster(showNA=FALSE,palette  = "purple",legend.show = FALSE) 
 # tm_legend(main.title.size = 1, text.size = 0.8, position = c("right","top"))

 
save_tmap(kcrab_mhi, paste0(boxdir,"final_results/MHI_kcrab_EFH.png"))

nwhi_kcrab<-raster(paste0(boxdir,"final_results/nwhi_kcrab.tif"))

kcrab_nwhi<- 
  tm_shape(nwhi_kcrab) +
  tm_raster(showNA=FALSE,title="Kona crab",palette = c("blue","green","red")) + 
  tm_layout(legend.outside = TRUE) +
  tm_shape(nwhi)+
  tm_fill(col = "lightblue", alpha = 0.5) +
  tm_borders(lwd =0.3) +
  tm_shape(nwhi_land)+
  tm_fill(col = "white") +
  tm_borders(lwd = 1) 
  
 # tm_legend(main.title.size = 1, text.size = 0.8,legend.outside=TRUE, position = c("right","top"))

save_tmap(kcrab_nwhi, paste0(boxdir,"final_results/nwhi_kcrab_EFH.png"))

marg_ad<-raster(paste0(boxdir,"final_results/mhi_marginatus_adult_EFH.tif"))
marg_ad[marg_ad==0]<-NA
marg_juv<-raster(paste0(boxdir,"final_results/mhi_marginatus_juvenile_EFH.tif"))

marginatus_mhi<- 
  tm_shape(mhi_hawaii)+
  tm_fill(col = "lightblue", alpha = 0.3) +
  tm_borders(lwd =0.4) +
   tm_shape(land)+
  tm_fill(col = "white",alpha = 1) +
  tm_borders(lwd = .5) +
  tm_shape(marg_ad) +
  tm_raster(showNA=FALSE,title="Spiny lobster (P. marginatus)",palette = "red",labels="Adult EFH") + 
  tm_shape(marg_juv) +
    tm_raster(showNA=FALSE,title="",palette = "purple",labels="Juvenile EFH")+
  tm_legend(main.title.size = 1, text.size = 0.8, position = c("right","top"))

 
  

save_tmap(marginatus_mhi, paste0(boxdir,"final_results/MHI_marginatus_EFH.png"))

nwhi_marg_ad<-raster(paste0(boxdir,"final_results/nwhi_marginatus_adult_EFH.tif"))
nwhi_marg_juve<-raster(paste0(boxdir,"final_results/nwhi_marginatus_juvenile_EFH.tif"))


marg_add_nwhi<- 
  tm_shape(nwhi_marg_ad) +
  tm_raster(showNA=FALSE,title="Adult spiny lobster (P. marginatus)",palette = c("blue","green","red")) + 
  tm_layout(legend.outside = TRUE) +
  tm_shape(nwhi)+
  tm_fill(col = "lightblue", alpha = 0.5) +
  tm_borders(lwd =0.3) +
  tm_shape(nwhi_land)+
  tm_fill(col = "white") +
  tm_borders(lwd = 1) 
save_tmap(marg_add_nwhi, paste0(boxdir,"final_results/NWHI_marginatus_adult_EFH.png"))


marg_juve_nwhi<- 
 
  tm_shape(nwhi)+
  tm_fill(col = "lightblue", alpha = 0.5) +
  tm_borders(lwd =0.3) +
  tm_shape(nwhi_land)+
  tm_fill(col = "white") +
  tm_borders(lwd = 1) +
  tm_shape(nwhi_marg_juve) +
  tm_raster(showNA=FALSE,title = "Juvenile spiny lobster(P. marginatus)",palette = c("blue","green")) + 
  tm_layout(legend.outside = TRUE) 
save_tmap(marg_juve_nwhi, paste0(boxdir,"final_results/NWHI_marginatus_juvenile_EFH.png"))



mhi_depth_map<- 
  tm_shape(mhi_depth) +
 # tm_layout(legend.position = c("RIGHT","top"),legend.outside=TRUE) +
  tm_raster(showNA=FALSE,title="Depth (m)",palette="YlGnBu", n = 60, contrast = c(0.18, 0.9),legend.show = FALSE) + 
  tm_shape(mhi_hawaii)+
 # tm_fill(col = "lightblue", alpha = 0) +
  tm_borders(lwd =1) +
   tm_shape(land)+
  tm_fill(col = "white") +
  tm_borders(lwd = 1) 

save_tmap(mhi_depth_map,paste0(boxdir,"Figures/mhi_depth.png"))
 
  #tm_legend(main.title.size = 1, text.size = 0.8, position = c("right","top")) 
 
nwhi_depth_map<- 
  tm_shape(nwhi_depth) +
 # tm_layout(legend.position = c("RIGHT","top"),legend.outside=TRUE) +
  tm_raster(showNA=FALSE,title="Depth (m)",palette="YlGnBu", contrast = c(0.18, 0.9),legend.show=FALSE) + 
  tm_shape(nwhi)+
  # tm_fill(col = "lightblue", alpha = 0) +
  tm_borders(lwd =0.4) +
  tm_shape(nwhi_land)+
  tm_fill(col = "white") +
  tm_borders(lwd = 1) 

save_tmap(nwhi_depth_map,paste0(boxdir,"Figures/nwhi_depth.png"))


mhi_substrate_map<- 
  tm_shape(mhi_substrate) +
 # tm_layout(legend.position = c("RIGHT","top"),legend.outside=TRUE) +
  tm_raster(showNA=FALSE,title="",palette="YlOrRd", n = 60, contrast = c(0.18, 0.9),legend.show = FALSE) + 
  tm_shape(mhi_hawaii)+
  # tm_fill(col = "lightblue", alpha = 0) +
  tm_borders(lwd =0.4) +
  tm_shape(land)+
  tm_fill(col = "white") +
  tm_borders(lwd = 1) 

save_tmap(mhi_substrate_map,paste0(boxdir,"Figures/mhi_substrate.png"))


nwhi_substrate_map<- 
  tm_shape(nwhi_substrate) +
#  tm_layout(legend.position = c("RIGHT","top"),legend.outside=TRUE) +
  tm_raster(showNA=FALSE,title="",palette="YlOrRd", n = 60, contrast = c(0.18, 0.9),legend.show = FALSE) + 
  tm_shape(nwhi)+
  # tm_fill(col = "lightblue", alpha = 0) +
  tm_borders(lwd =0.4) +
  tm_shape(nwhi_land)+
  tm_fill(col = "white") +
  tm_borders(lwd = 1) 

save_tmap(nwhi_substrate_map,paste0(boxdir,"Figures/nwhi_substrate.png"))








tm_shape(mhi_s_cells) +
  tm_raster(showNA = FALSE, legend.show = FALSE, palette = c("white","red"),labels = c("","Suitable Areas")) +
  tm_shape(hawaii,is.master = TRUE) +
  tm_fill(col="lightblue",alpha = 0.4,title ="TESt") +
  tm_borders(lwd = 1.2) +
  tm_legend(main.title.size = 2, text.size = 1, position = c("right","top"))

no<-tm_shape(mhi_hawaii)+
  tm_fill("lightblue")+
  tm_text("AREA_ID",size=0.5)
save_tmap(no,paste0(boxdir,"Figures/fishing_no.png"),width=12,height=12)
