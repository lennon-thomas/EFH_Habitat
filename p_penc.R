

rm(list = ls())
library(tidyverse)
library(rgdal)
library(raster)
library(tmap)
library(spex)
library(sf)
library(RArcInfo)
boxdir<- '/Users/lennonthomas/Box Sync/Council Stuff/data/'
#read in Data


bathy<-raster(paste0(boxdir,"hawaii_bty_5m/"))

#bathy<-projectRaster(bathy,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",method="ngb",filename=paste0(boxdir,"HI_bathy5m_reproject.tif",overwrite = TRUE),progress='text')

#writeRaster(five_bath,paste0(boxdir,"HI_bathy5m_reproject.tif"))
ext<-c(327900.5, 948525.5,2084429,2469945)

bs_old<-raster(paste0(boxdir,"hawaii_bs_msc/"))
bs_old<-crop(bs_old,ext,progress='text')
bathy<-crop(bathy,ext,progress='text')

bathy_peni<-reclassify(bathy,c(-5779,-16,0,-16,0,1,0,53,0),filename=paste0(boxdir,"bath_peni.tif"),overwrite=TRUE,progress='text')
bs_peni<-reclassify(bs_old,c(0,139,0,139,300,1,NA,NA,2),filename=paste0(boxdir,"bs_peni.tif"),overwrite=TRUE,progress='text')
extent(bs_peni)<-c(327900.5, 948525.5, 2084430, 2469945)
#bathy_peni<-crop(bathy_peni,bs_peni_align,text='progress',snap="near")
#bs_peni_align<-extend(bs_peni,c(327900.5, 948525.5, 2084430, 2469945),value=NA,progress='text')
mhi_adult_s_cells<-overlay(bs_peni,bathy_peni,fun = function (x,y){x*y},filename=paste0(boxdir,"suit_peni_5.tif"),overwrite=TRUE,progress="text") 
#project in QGIS bc processing time is faster
#final<-projectRaster(mhi_adult_s_cells,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",method="ngb",filename=paste0(boxdir,"final_peni_suit.tif",overwrite = TRUE),progress='text')
mhi_adult_s_cells<-raster(paste0(boxdir,"peni_final_qgis.tif"))
final<-reclassify(mhi_adult_s_cells,c(0,0,NA),filename=paste0(boxdir,"suit_peni_5_final.tif"),progress="text",overwrite=TRUE)
final_area<-area(final,na.rm=TRUE,filename=paste0(boxdir,"suit_peni_5_final_area.tif"),progress="text")
final_stats<-zonal(final_area,final,fun="sum",progress='text')
write.csv(final_stats,paste0(boxdir,"final_area_peni.csv"))
mhi_adult_s_cells<-raster(paste0(boxdir,"suit_peni_5_final.tif"))
mhi_ext<-c(-161,-154.3,18.5,22.75)
mhi_raster<-raster(paste0(boxdir,"tmp/mhi_raster.tif"))
mhi_depth<-raster(paste0(boxdir,"tmp/mhi_depth.tif"))
mhi_substrate<-raster(paste0(boxdir,"tmp/mhi_substrate.tif"))
mhi_area<-raster(paste0(boxdir,"tmp/mhi_area.tif"))


##########################################################################
##########################################################################
########  Panulirus pencillatus Data###############
##########################################################################
##########################################################################

boxdir<- '/Users/lennonthomas/Box Sync/Council Stuff/data/'

fish_data_all<-read_csv(paste0(boxdir,"Hawaii -DAR.csv"))

p_penc<-fish_data_all %>%
  group_by(Area_FK, Scie_Name) %>%
  summarise(total_landings = sum(Lbs_Kept,na.rm = TRUE)) %>%
  filter(!is.na(Area_FK)) %>%
  filter(!Area_FK %in% c(0,10,99999)) %>%
  filter(Scie_Name == "Panulirus penicillatus")

hawaii <- readOGR(dsn=paste0(boxdir,'fishchart2008.shp'),layer="Fishchart2008",stringsAsFactors=FALSE)
hawaii<-crop(hawaii,mhi_ext)
 hawaii_fish<-merge(hawaii,p_penc, by.x = "AREA_ID", by.y = "Area_FK",all=TRUE) 
 
 land<-hawaii[hawaii@data$TYPE=="Island",]
 
 tm_shape(hawaii_fish) +
   tm_fill(col = "total_landings",colorNA="lightblue") +
   tm_borders(lwd = 1.2) +
   tm_shape(land)+
   tm_fill(col = "white") +
   tm_legend(main.title.size = 2, text.size = 1, position = c("right","top"),main.title = "P. Pencillatus landings- All")
 
# 
# landings<-hawaii_fish@data[,c("AREA_ID","total_landings")]
# landings$AREA_ID<-as.numeric(landings$AREA_ID)
# 
# mhi_zonal<-as.data.frame(zonal(mhi_area,mhi_raster,fun = "sum"))
# colnames(mhi_zonal)<-c("AREA_ID","Total_area")
# 
# nwhi_zonal<-as.data.frame(zonal(nwhi_area,nwhi_raster,fun = "sum"))
# colnames(nwhi_zonal)<-c("AREA_ID","Total_area")
# 
# results_df<-rbind(mhi_zonal,nwhi_zonal)
# results_df<-merge(results_df,p_marg,by.x="AREA_ID",by.y="Area_FK",all=TRUE)
# 

##########################################################################
##########################################################################
######## Adult Panulirus marginatus suitability analysis###############
##########################################################################
##########################################################################

#Adult suitable depth
#mhi_adult_depth<-mhi_depth
#mhi_adult_depth[mhi_adult_depth>5]<-0
#mhi_adult_depth[mhi_adult_depth>0]<-1
#used reclassify function instead to create _peni tif based 0-100 m depth and >140 m bs
bs_peni<-raster(paste0(boxdir,"bs_peni.tif"))
bath_peni<-raster(paste0(boxdir,"bath_peni.tif"))
bs_peni<-crop(bs_peni,bath_peni,filename=paste0(boxdir,"bs_peni_crop.tif"),progress="text")
bath_peni<-crop(bath_peni,bs_peni,filename=paste0(boxdir,"bath_peni_crop.tif"),progress="text")
extent(bs_peni)<-c(-160.671, -154.6492, 18.80287, 22.33534 )
#Adults suitable substrate
#mhi_substrate<-bs
#mhi_subtrate_adult<-mhi_substrate
#mhi_subtrate_adult[mhi_subtrate_adult < 140 ]<-0
#mhi_subtrate_adult[mhi_subtrate_adult >= 140 ]<-2
#mhi_subtrate_adult[is.na(mhi_subtrate_adult)]<-1
#mhi_subtrate_adult<-mask(mhi_subtrate_adult,mhi_depth)


mhi_adult_s_cells<- overlay(bs_peni,bath_peni,fun = function (x,y){x*y},progress="text") 

writeRaster(mhi_adult_s_cells,filename = paste0(boxdir,"final_results/mhi_penicillatus_habitat.tif"),overwrite = TRUE)
mhi_adult_s_cells<-raster(paste0(boxdir,"final_results/mhi_penicillatus_habitat.tif"))
p_marg_adult_mhi_suit_shape<-polygonize(mhi_adult_s_cells,na.rm = TRUE)
st_write(p_marg_adult_mhi_suit_shape,driver="ESRI Shapefile",dsn=paste0(boxdir,"final_results/"),layer="mhi_penicillatus_adult_habitat")




mhi_adult_s_cells[mhi_adult_s_cells==0]<-NA

mhi_adult_s_cells_area<-area(mhi_adult_s_cells,na.rm=TRUE)

mhi_adult_zonal<-as.data.frame(zonal(mhi_adult_s_cells_area,mhi_raster,fun="sum")) %>%
  set_names("AREA_ID","adult_suitable") 

mhi_adult_zonal<- left_join(mhi_adult_zonal,p_marg, by=(c ("AREA_ID"= "Area_FK")))

mhi_adult_zonal$total_landings[is.na(mhi_adult_zonal$total_landings)]<-0

mhi_marg_results<-mhi_adult_zonal %>%
  mutate(Island = ifelse(AREA_ID<=129, "Big Island",ifelse (AREA_ID>=300 & AREA_ID <= 333, "Maui Nui",
                                                            ifelse(AREA_ID >=400 & AREA_ID <= 429, "Oahu", 
                                                                   ifelse(AREA_ID>=500 & AREA_ID <= 529,"Kauai",
                                                                          "Other")))))

write.csv(mhi_marg_results,paste0(boxdir,"final_results/mhi_penicillatus_adult_EFH_df.csv"))

mhi_marg_summary<-
  mhi_marg_results %>%
  group_by(Island) %>%
  summarise(suitable_area = sum(adult_suitable,na.rm=TRUE))

write.csv(mhi_marg_summary,paste0(boxdir,"final_results/mhi_penicillatus_summary.csv"))




nwhi_adult_depth<-nwhi_depth
nwhi_adult_depth[nwhi_adult_depth>150| nwhi_adult_depth<20]<-0
nwhi_adult_depth[nwhi_adult_depth>0]<-1
#nwhi_adult_depth_area<-mask(nwhi_area,nwhi_adult_depth)
#nwhi_adult_depth_zone<-as.data.frame(zonal(nwhi_adult_depth_area,nwhi_raster,fun="sum"))
#colnames(nwhi_adult_depth_zone)<-c("AREA_ID","adult_depth")

nwhi_subtrate_adult<-nwhi_substrate
nwhi_subtrate_adult[nwhi_subtrate_adult < 140 ]<-0
nwhi_subtrate_adult[nwhi_subtrate_adult > 0 ]<-1
#nwhi_substrate_adult_area<-mask(nwhi_area,nwhi_subtrate_adult)
#nwhi_substrate_adult_zone<-as.data.frame(zonal(nwhi_substrate_adult_area,nwhi_raster,fun="sum")) 
#colnames(nwhi_substrate_adult_zone)<-c("AREA_ID","adult_substrate")
nwhi_adult_s_cells<-overlay(nwhi_adult_depth,nwhi_subtrate_adult,fun = function (x,y){x+y}) 

nwhi_adult_s_cells[nwhi_adult_s_cells==0]<-NA

writeRaster(nwhi_adult_s_cells,filename = paste0(boxdir,"final_results/nwhi_marginatus_adult_EFH.tif"),overwrite = TRUE)

nwhi_adult_s_area<-area(nwhi_adult_s_cells,na.rm = TRUE) 


nwhi_adult_zonal<-as.data.frame(zonal(nwhi_adult_s_area,nwhi_raster,fun="sum")) %>%
  set_names("AREA_ID","adult_suitable") 


nwhi_adult_zonal<- left_join(nwhi_adult_zonal,p_marg, by=(c ("AREA_ID"= "Area_FK")))

nwhi_adult_zonal$total_landings[is.na(nwhi_adult_zonal$total_landings)]<-0

nwhi_marg_results<-nwhi_adult_zonal %>%
  mutate(Island = "NWHI")

write.csv(nwhi_marg_results,paste0(boxdir,"final_results/nwhi_marginatus_adult_EFH_df.csv"))

nwhi_adult_marginus_summary<-
  nwhi_marg_results %>%
  group_by(Island) %>%
  summarise(suitable_area = sum(adult_suitable,na.rm=TRUE))

#write.csv(nwhi_adult_marginus_summary,paste0(boxdir,"mhi_kcrab_summary.csv"))



##########################################################################
##########################################################################
######## Juvenile Panulirus marginatus suitability analysis###############
##########################################################################
##########################################################################

mhi_juvenile_depth<-mhi_depth
mhi_juvenile_depth[mhi_juvenile_depth>30 | mhi_juvenile_depth<1]<-0
#mhi_juvenile_depth_area<-mask(mhi_area,mhi_juvenile_depth)
#mhi_juvenile_depth_zone<-as.data.frame(zonal(mhi_juvenile_depth_area,mhi_raster,fun="sum"))
#colnames(mhi_juvenile_depth_zone)<-c("AREA_ID","juvenile_depth")

mhi_subtrate_juvenile<-mhi_substrate
mhi_subtrate_juvenile[mhi_subtrate_juvenile < 140 ]<-0

#mhi_substrate_juvenile_area<-mask(mhi_area,mhi_subtrate_juvenile)
#mhi_substrate_juvnile_zone<-as.data.frame(zonal(mhi_substrate_juvenile_area,mhi_raster,fun="sum")) 
#colnames(mhi_substrate_juvnile_zone)<-c("AREA_ID","juvenile_substrate")
mhi_juve_s_cells<-overlay(mhi_juvenile_depth,mhi_subtrate_juvenile,fun = function (x,y){x+y}) 
mhi_juve_s_cells[mhi_juve_s_cells>0]<-1
mhi_juve_s_cells[mhi_juve_s_cells==0]<-NA
writeRaster(mhi_juve_s_cells,filename = paste0(boxdir,"final_results/mhi_marginatus_juvenile_EFH.tif"),overwrite = TRUE)
pmarg_mhi_juve<-rasterToPolygons(mhi_juve_s_cells,method = 'gdal')
writeOGR(pmarg_mhi_juve,dsn=paste0(boxdir,"final_results/",layer="mhi_marginatus_juvenile", driver="ESRI"))
mhi_juve_s_cells_area<-area(mhi_juve_s_cells)

mhi_juvenile_zonal<-as.data.frame(zonal(mhi_juve_s_cells_area,mhi_raster,fun="sum")) %>%
  set_names("AREA_ID","juve_suit")

mhi_juve_marg_results<-mhi_juvenile_zonal %>%
  mutate(Island = ifelse(AREA_ID<=129, "Big Island",ifelse (AREA_ID>=300 & AREA_ID <= 333, "Maui Nui",
                                                            ifelse(AREA_ID >=400 & AREA_ID <= 429, "Oahu", 
                                                                   ifelse(AREA_ID>=500 & AREA_ID <= 529,"Kauai",
                                                                          "Other")))))


write.csv(mhi_juve_marg_results,paste0(boxdir,"final_results/mhi_marginatus_juvenile_EFH_df.csv"))

mhi_juve_summary<-
  mhi_juve_marg_results %>%
  group_by(Island) %>%
  summarise(suitable_area = sum(juve_suit,na.rm=TRUE))

mhi_marg_summary<-merge(mhi_marg_summary,mhi_juve_summary,by="Island")

write.csv(mhi_marg_summary,paste0(boxdir,"mhi_marg_summary.csv"))

######################################################################

nwhi_juvenile_depth<-nwhi_depth
nwhi_juvenile_depth[nwhi_juvenile_depth>30 | nwhi_juvenile_depth<1]<-0
nwhi_juvenile_depth[nwhi_juvenile_depth>0]<-1

nwhi_subtrate_juvenile<-nwhi_substrate
nwhi_subtrate_juvenile[nwhi_subtrate_juvenile < 140 ]<-0
nwhi_subtrate_juvenile[nwhi_subtrate_juvenile>0]<-1


nwhi_juve_s_cells<-overlay(nwhi_juvenile_depth,nwhi_subtrate_juvenile,fun = function (x,y){x+y}) 
writeRaster(nwhi_juve_s_cells,filename = paste0(boxdir,"final_results/nwhi_marginatus_juvenile_EFH.tif"),overwrite = TRUE)

nwhi_juve_s_cells[nwhi_juve_s_cells==0]<-NA
nwhi_juve_s_cells_area<-area(nwhi_juve_s_cells,na.rm = TRUE)

nwhi_juvenile_zonal<-as.data.frame(zonal(nwhi_juve_s_cells_area,nwhi_raster,fun="sum")) %>%
  set_names("AREA_ID","juvenile_suitable") %>%
  mutate(Island="NWHI")

write.csv(nwhi_juvenile_zonal,paste0(boxdir,"final_results/nwhi_marginatus_juvenile_EFH_df.csv"))

nwhi_juve_summary<-
  nwhi_juvenile_zonal %>%
  group_by(Island) %>%
  summarise(suitable_area = sum(juvenile_suitable,na.rm=TRUE))

nwhi_marg_summary<-merge(nwhi_adult_marginus_summary,nwhi_juve_summary,by="Island")

write.csv(nwhi_marg_summary,paste0(boxdir,"final_results/nwhi_marg_summary.csv"))

=======
  summarise(suitable_area = sum(suitable_habitat,na.rm=TRUE))

write.csv(mhi_marg_summary,paste0(boxdir,"final_results/mhi_penc_summary2.csv"))

# make map
hawaii <- readOGR(dsn=paste0(boxdir,'fishchart2008.shp'),layer="Fishchart2008",stringsAsFactors=FALSE)

ext<-c(-161,-154.3,18.5,22.75)
land<-hawaii[hawaii@data$TYPE=="Island",]
mhi_hawaii<-crop(hawaii,ext)
mhi_raster<-raster(paste0(boxdir,"tmp/mhi_raster.tif"))
mhi_depth<-raster(paste0(boxdir,"tmp/mhi_depth.tif"))
mhi_substrate<-raster(paste0(boxdir,"tmp/mhi_substrate.tif"))
mhi_area<-raster(paste0(boxdir,"tmp/mhi_area.tif"))

hawaii<-crop(hawaii,ext)

tidy_hawaii<-tidy(hawaii)

temp_df<-as.data.frame(hawaii@data)

temp_df$id <- row.names(temp_df)

hawaii_df <- merge(tidy_hawaii, temp_df, by="id")

# Land Shapefile
land<-hawaii_df %>%
  filter(TYPE=="Island"|TYPE=="Islet")

water<-hawaii_df %>%
  filter(TYPE!="Island")

suit_df<-as_data_frame(rasterToPoints(mhi_adult_s_cells))
suit_df$mhi_penicillatus_adult_EFH<-as.factor(suit_df$mhi_penicillatus_adult_EFH)

hawaii_base<-
  ggplot() + 
  geom_polygon(data = water,aes(x = long,y = lat,group = group), fill =  "lightblue", colour = "black", size = 0.007 , alpha = 0.5) +
  geom_polygon(data = land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.007) +
  theme(legend.position="none") +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude")

hawaii_base +
geom_raster(data = suit_df, aes(x=x,y=y, fill=mhi_penicillatus_adult_EFH)) +
  scale_fill_manual("",labels=c("Habitat (depth)","Habitat\n(depth +substrate hardness)"),values = c("darkorange","red")) +
  theme(legend.position = c(0.8,0.9))





# %>% fortify() %>% dplyr::select(long,lat) 
hawaii_df<-as_data_frame(rasterToPoints(mhi_raster))


all_data<-left_join(hawaii_df,p_penc,by=c("mhi_raster"="Area_FK")) 
all_data<-all_data %>%
  filter(!is.na(total_landings))#%>%
#select(long,lat, total_landings)
hawaii_base +
 
     # ggplot(data=all_data)+tm_raster(showNA=FALSE,title="Depth (m)",palette="YlGnBu", n = 10, contrast = c(0.18, 0.9),legend.show = TRUE)
     geom_raster(data=all_data,aes(x=x,y=y,fill=total_landings))+
     theme(legend.position = "topright") +
     scale_fill_viridis("Total landings (lbs)\n 1948-2016")

land<-mhi_hawaii[mhi_hawaii@data$TYPE=="Island",]

catch_data<-merge(mhi_hawaii, p_penc, by.x="AREA_ID",by.y="Area_FK")

tm_shape(mhi_hawaii)+
tm_fill(col = "lightblue", alpha = 0.3) +
tm_borders(lwd =0.4) +
tm_shape(land)+
tm_fill(col = "white",alpha = 1) +
tm_borders(lwd = 0.1) +
tm_shape(mhi_adult_s_cells) +
tm_raster(col="mhi_penicillatus_adult_EFH",showNA=FALSE,colorNA=NULL,style="cat", labels=
            c("Habitat (depth)","Habitat (depth + substrate hardness"), title="",palette =  c("purple","red")) +
#tm_borders(lwd = 0.1) +
tm_layout( legend.outside=TRUE) 
mhi_marg_results$AREA
mhi_marg_results<-mhi_marg_results %>%
  mutate(log_catch = log(total_landings+0.001))
reg<-lm(suitable_habitat~log_catch,data=mhi_marg_results)

plot(mhi_marg_results$suitable_habitat,mhi_marg_results$log_catch)
>>>>>>> 14770a0ded16159b07a32f1a77b2e11fff68638d
