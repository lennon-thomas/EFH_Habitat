rm(list = ls())
library(tidyverse)
library(rgdal)
library(raster)
library(tmap)
library(spex)
library(sf)
library(RArcInfo)
library(spex)
boxdir<- '/Users/lennonthomas/Box Sync/Council Stuff/data/'

hi<-readOGR(dsn='/Users/lennonthomas/Desktop/World_EEZ_v10_20180221/',layer='eez_v10')
hi<-hi[hi@data$Territory1=="Hawaii",]
#testing to see if results between 50m and 60 m are simliar.. they are
depth<-raster(paste0(boxdir,"bath_repro.tif"))

larv_depth<-depth

larv<-reclassify(larv_depth,c(-6000,-150,0,-150,0,1,0,100,0),progress='text',filename=paste0(boxdir,"test.tif"),overwrite=TRUE)
larv<-raster(paste0(boxdir,"test_area.tif"))
larv_f<-reclassify(larv,c(0,0,NA),progress='text')
writeRaster(larv,paste0(boxdir,"revisions/mhi_larv_5m.tif"))
larv<-raster(paste0(boxdir,"revisions/mhi_larv_5m.tif"))
larv_area<-area(larv,na.rm=TRUE,progress='text',filename=paste0(boxdir,"test_area.tif"))
results<-zonal(larv_area,larv,"sum",progress="text")
write.csv(results,paste0(boxdir,"revisions/larv_results.csv"))


hawaii <- readOGR(dsn=paste0(boxdir,'fishchart2008.shp'),layer="Fishchart2008",stringsAsFactors=FALSE)

ext<-c(-160.5,-154.3,18.5,22.75)
ext2<-c(-161.2,-150.5,15.5,26.5)
hi2<-crop(hi,ext2)
water<-hi2[hi2@polygons[[1]]@Polygons$hole==FALSE]
mhi_hawaii<-crop(hawaii,ext)
h<-hi2@polygons[[1]]@Polygons[[1]]
p <- lapply(hi2@polygons , slot , "Polygons")
lapply(p[[1]], function(x) slot(x, "hole"))

land<-mhi_hawaii[mhi_hawaii@data$TYPE=="Island",]

#new plots
larv_depth<-raster(paste0(boxdir,"final_results/mhi_kcrab_larve.tif"))
tm_shape(hi2,is.master=TRUE)+
  tm_fill(title="",col = "MRGID", alpha = 1,labels="EFH is top 150 m of water column",palette="lightblue",legend.z=2,legend.show=TRUE) +
  tm_shape(larv_depth) +
  tm_raster(title="Crustacean Eggs and Larvae",legend.show=TRUE,showNA=FALSE,style="cat",palette = "navy",labels=c("EFH is whole water column") )+ 
  
  tm_shape(land)+
  tm_fill(col = "white",alpha = 1) +
  tm_borders(lwd = 1) +
  tm_legend(legend.title.size = 1.7, legend.text.size = 1.3, position = c(0.6,0.91),legend.width=-.5,legend.height=3)+
  tm_layout(frame=FALSE)
kcrab<-raster(paste0(boxdir,"final_results/mhi_kcrab.tif"))
bs_kcrab<-reclassify(bs_new,c(0,140,1,140,300,0))
depth_kcrab<-reclassify(mhi_depth,c(0,2,0,2,200,1,200,6200,0))
kcrab_final<-overlay(bs_kcrab,depth_kcrab,fun = function (x,y){x*y},filename=paste0(boxdir,"revisions/kcrab_final.tif"),overwrite=TRUE,progress="text") 
kcrab_final<-raster(paste0(boxdir,"revisions/kcrab_final.tif"))
kcrab_shape<-polygonize(kcrab_final,na.rm=TRUE)
st_write(kcrab_shape,dsn=paste0(boxdir,"rev_final"),layer="kona_crab_EFH",driver ="ESRI Shapefile")
kcrab_area<-area(kcrab_final,na.rm=TRUE)
kcrab_results<-zonal(kcrab_area,kcrab_final,"sum")
kstudy<-read.csv("kcrab_mort_study.csv") %>%
  mutate(category="one")
kstudy$new_long<-kstudy$new_long*-1

#fish_pt<-SpatialPointsDataFrame(coords=c(kstudy$new_long,kstudy$new_lat),data=kstudy$no_crabs, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#o<-cbind(kstudy$new_long,kstudy$new_lat)

co<-cbind(kstudy$new_long,kstudy$new_lat)
fish_pt<-SpatialPointsDataFrame(coords=co,data=kstudy,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

kcrab_final<-reclassify(kcrab_final,c(-1,0.5,NA))
plot<-
  tm_shape(mhi_hawaii,is.master=TRUE)+
  tm_fill(col = "lightblue", alpha = 0.5) +
  tm_shape(kcrab_final) +
  tm_raster(title="",legend.show=TRUE,showNA=FALSE,style="cat",palette = "red",labels=c("EFH for juvenile and adult Kona crabs"),alpha=0.7 )+ 
 
  tm_shape(land)+
  tm_fill(col = "white",alpha = 1) +
  tm_borders(lwd = 1) +
  tm_legend(legend.title.size = 1.7, legend.text.size = 1.3, position = c(0.6,0.91),legend.width=-.5,legend.height=3)+
  tm_layout(frame=FALSE) +
  tm_shape(fish_pt) +
  tm_dots(col="category",size=0.08,alpha=1,labels="Verified Kona crab fishing grounds",legend.show = TRUE,title="",palette="navy")

marg_j<-raster(paste0(boxdir,"final_results/mhi_marginatus_juvenile_EFH.tif"))
bath<-raster(paste0(boxdir,"bath_repro.tif"))
bs<-raster(paste0(boxdir,"bs_repro.tif"))

mhi_depth<-raster(paste0(boxdir,"tmp/mhi_depth.tif"))
marg_depth<-reclassify(mhi_depth,c(0,1,0,1,30,1,30,6200,0))
bs_crop<-crop(bs,mhi_depth)

mhi_depth<-crop(mhi_depth,bs_crop)

bs_new<-resample(bs_crop,mhi_depth,method="ngb",filename=paste0(boxdir,"revisions/bs_5m_resample.tif"),progress="text",overwrite=TRUE)
bs_marg<-reclassify(bs_new,c(0,140,0,140,300,1,NA,NA,0.5))

marg_j_final<-overlay(bs_marg,marg_depth,fun = function (x,y){x*y},filename=paste0(boxdir,"revisions/j_marg.tif"),overwrite=TRUE,progress="text") 
marg_j_final<-reclassify(marg_j_final,c(-1,0.1,NA,0.1,2,1))
marg_j_final<-raster(paste0(boxdir,"revisions/j_marg.tif"))
marg_j_area<-area(marg_j_final,na.rm=TRUE)
zonal(marg_j_area,marg_j_final,"sum")
marg_j_final<-reclassify(marg_j_final,c(-1,0.5,NA))
marg_j_shape<-polygonize(marg_j_final,na.rm=TRUE)
st_write(marg_j_shape,dsn=paste0(boxdir,"rev_final/P_marg_juvenile"),layer="P_marginatus_juvenile_EFH",driver ="ESRI Shapefile")
tm_shape(mhi_hawaii,is.master=TRUE)+
  tm_fill(col = "lightblue", alpha = 0.5) +
  tm_shape(marg_j_final) +
  tm_raster(title="",legend.show=TRUE,showNA=FALSE,style="cat",palette = "red",labels=c("EFH for juvenile spiny lobster"),alpha=0.7 )+ 
  
  tm_shape(land)+
  tm_fill(col = "white",alpha = 1) +
  tm_borders(lwd = 1) +
  tm_legend(legend.title.size = 1.7, legend.text.size = 1.3, position = c(0.6,0.91),legend.width=-.5,legend.height=3)+
  tm_layout(frame=FALSE) 
  
marg_a_depth<-reclassify(mhi_depth,c(0,20,0,20,150,1,150,6200,0))
marg_a<-raster(paste0(boxdir,"final_results/mhi_marginatus_adult_EFH.tif"))
marg_a_area<-area(marg_a,na.rm=TRUE)
zonal(marg_a_area,marg_a,"sum")
marg_a_final<-overlay(bs_marg,marg_a_depth,fun = function (x,y){x*y},filename=paste0(boxdir,"revisions/marg_adult_final2.tif"),overwrite=TRUE,progress="text") 
marg_a_final<-raster(paste0(boxdir,"revisions/marg_adult_final2.tif"))
marg_a_area<-area(marg_a_final,na.rm=TRUE)
zonal(marg_a_area,marg_a_final,"sum")

marg_a_final<-reclassify(marg_a_final,c(-1,0.1,NA,.1,1.1,1))
marg_a_shape<-polygonize(marg_a_final,na.rm=TRUE)
st_write(marg_a_shape,dsn=paste0(boxdir,"rev_final/P_marg_adult"),layer="P_marginatus_adult_EFH",driver ="ESRI Shapefile")


tm_shape(mhi_hawaii,is.master=TRUE)+
  tm_fill(col = "lightblue", alpha = 0.5) +
  tm_shape(marg_a_final) +
  tm_raster(title="",legend.show=TRUE,showNA=FALSE,style="cat",palette = "red",labels=c("EFH for adult spiny lobster"),alpha=0.7 )+ 
  
  tm_shape(land)+
  tm_fill(col = "white",alpha = 1) +
  tm_borders(lwd = 1) +
  tm_legend(legend.title.size = 1.7, legend.text.size = 1.3, position = c(0.6,0.91),legend.width=-.5,legend.height=3)+
  tm_layout(frame=FALSE) 

peni_depth<-reclassify(mhi_depth,c(0,16,1,16,6200,0))

peni_final<-overlay(peni_depth,bs_marg,fun = function (x,y){x*y},filename=paste0(boxdir,"revisions/peni_adult_final.tif"),overwrite=TRUE,progress="text") 
peni_final<-raster(paste0(boxdir,"revisions/peni_adult_final.tif"))
peni_area<-area(peni_final,na.rm=TRUE,progress="text")
zonal(peni_area,peni_final,"sum")
peni_final<-reclassify(peni_final,c(-0.1,0.1,NA))
peni_final<-reclassify(peni_final,c(0.1,1.1,1))
peni<-raster(paste0(boxdir,"suit_peni_5_final.tif"))
peni_shape<-polygonize(peni_final,na.rm=TRUE)
st_write(peni_shape,dsn=paste0(boxdir,"rev_final/P_penicillatus"),layer="P_penicillatus_EFH",driver ="ESRI Shapefile")
peni_area<-area(peni,na.rm=TRUE)
peni_results<-read.csv(paste0(boxdir,"final_area_peni.csv"))
peni<-reclassify(peni,c(-1,0.5,NA,1,2,1))
tm_shape(mhi_hawaii,is.master=TRUE)+
  tm_fill(col = "lightblue", alpha = 0.5) +
  tm_shape(peni_final) +
  tm_raster(title="",legend.show=TRUE,showNA=FALSE,style="cat",palette = "red",labels=c("EFH for adult pronghorn spiny lobster"),alpha=0.7 )+ 
  
  tm_shape(land)+
  tm_fill(col = "white",alpha = 1) +
  tm_borders(lwd = 1) +
  tm_legend(legend.title.size = 1.7, legend.text.size = 1.3, position = c(0.6,0.91),legend.width=-.5,legend.height=3)+
  tm_layout(frame=FALSE) 

sqa_depth<-reclassify(mhi_depth,c(0,120,1,120,6200,0))
sqa<-overlay(sqa_depth,bs_marg,fun = function (x,y){x*y},filename=paste0(boxdir,"revisions/sqa_adult_final.tif"),overwrite=TRUE,progress="text") 
sqa<-raster(paste0(boxdir,"revisions/sqa_adult_final.tif"))
sqa<-reclassify(sqa,c(-1,0.1,NA,0.1,3,1))
area_sqa<-area(sqa,na.rm=TRUE)
zonal(area_sqa,sqa,'sum')
sqa_shape<-polygonize(sqa,na.rm=TRUE)
st_write(sqa_shape,dsn=paste0(boxdir,"revisions/S_squa"),layer="S_squammosus_EFH",driver="ESRI Shapefile")
tm_shape(mhi_hawaii,is.master=TRUE)+
  tm_fill(col = "lightblue", alpha = 0.5) +
  tm_shape(sqa) +
  tm_raster(title="",legend.show=TRUE,showNA=FALSE,style="cat",palette = "red",labels=c("EFH for juvenile/adult slipper lobster"),alpha=0.7 )+ 
  
  tm_shape(land)+
  tm_fill(col = "white",alpha = 1) +
  tm_borders(lwd = 1) +
  tm_legend(legend.title.size = 1.7, legend.text.size = 1.3, position = c(0.57,0.91),legend.width=-.5,legend.height=3)+
  tm_layout(frame=FALSE) 
