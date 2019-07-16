#juvenile habitat 1-30 m ; benthic habitat with intermediate (5-30 cm) vertical relief (Parrish and Polovina 1994)
#adult habitat 20-90 m; Slopes of banks with rocky substrate or  found in cracks and crevices in coral reef habitat  (Polovina 1989; Pitcher 1993)

rm(list = ls())
library(tidyverse)
library(rgdal)
library(raster)
library(tmap)
library(spex)
library(sf)

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
########  Panulirus pencillatus Data###############
##########################################################################
##########################################################################

boxdir<- '/Users/lennonthomas/Box Sync/Council Stuff/data/'

fish_data_all<-read_csv(paste0(boxdir,"Hawaii -DAR.csv"))

p_penc<-fish_data_all %>%
  group_by(Area_FK, Scie_Name) %>%
  summarise(total_landings = sum(Lbs_Kept,na.rm = TRUE),
            no_lic=length(Lbs_Kept)) %>%
  ungroup() %>%
  filter(!is.na(Area_FK)) %>%
  filter(!Area_FK %in% c(0,10,99999)) %>%
  filter(Scie_Name == "Panulirus penicillatus") %>%
  filter(no_lic > 3)

# hawaii <- readOGR(dsn=paste0(boxdir,'fishchart2008.shp'),layer="Fishchart2008",stringsAsFactors=FALSE)
# 
# hawaii_fish<-merge(hawaii,p_marg, by.x = "AREA_ID", by.y = "Area_FK",all=TRUE) 
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
mhi_adult_depth<-mhi_depth
mhi_adult_depth[mhi_adult_depth>5]<-0
mhi_adult_depth[mhi_adult_depth>0]<-1

#Adults suitable substrate

mhi_subtrate_juvenile<-mhi_substrate
mhi_subtrate_juvenile[mhi_subtrate_juvenile < 140 ]<-0
mhi_subtrate_juvenile[mhi_subtrate_juvenile > 0 ]<-2
mhi_subtrate_juvenile[is.na(mhi_subtrate_juvenile)]<-1

mhi_adult_s_cells<-overlay(mhi_adult_depth,mhi_subtrate_juvenile,fun = function (x,y){x*y}) 
mhi_adult_s_cells[mhi_adult_s_cells==0]<-NA
writeRaster(mhi_adult_s_cells,filename = paste0(boxdir,"final_final/mhi_p_peni_habitat.tif"),overwrite = TRUE)

mhi_adult_s_cells<-raster(paste0(boxdir,"final_results/mhi_penicillatus_adult_EFH.tif"))
#mhi_adult_s_cells_shape<-rasterToPolygons(mhi_adult_s_cells,n=16,na.rm=TRUE,digits=12)
mhi_adult_s_cells_shape<-polygonize(mhi_adult_s_cells,na.rm=TRUE)

st_write(mhi_adult_s_cells_shape,driver="ESRI Shapefile",dsn=paste0(boxdir,"final_final/"),layer="mhi_p_peni_habitat",update=TRUE)
p_marg<-readOGR(dsn=paste0(boxdir,"final_final/"),layer="mhi_p_peni_habitat")
colnames(p_marg@data[1])<-"p_peni"
writeOGR(p_marg,dsn=paste0(boxdir,"final_final/"),layer="mhi_p_peni_habitat",driver="ESRI Shapefile",overwrite=TRUE)
mhi_adult_s_cells[mhi_adult_s_cells==0]<-NA
mhi_adult_s_cells[mhi_adult_s_cells==1]<-NA
mhi_adult_s_cells_area<-area(mhi_adult_s_cells,na.rm=TRUE)

mhi_adult_zonal<-as.data.frame(zonal(mhi_adult_s_cells_area,mhi_raster,fun="sum")) %>%
set_names("AREA_ID","suitable_habitat") 

mhi_adult_zonal<- left_join(mhi_adult_zonal,p_penc, by=(c ("AREA_ID"= "Area_FK")))

mhi_adult_zonal$total_landings[is.na(mhi_adult_zonal$total_landings)]<-0

mhi_marg_results<-mhi_adult_zonal %>%
  mutate(Island = ifelse(AREA_ID<=129, "Big Island",ifelse (AREA_ID>=300 & AREA_ID <= 333, "Maui Nui",
                                                            ifelse(AREA_ID >=400 & AREA_ID <= 429, "Oahu", 
                                                                   ifelse(AREA_ID>=500 & AREA_ID <= 529,"Kauai",
                                                                          "Other")))))

write.csv(mhi_marg_results,paste0(boxdir,"final_results/mhi_penicilatus_adult_EFH_df.csv"))

mhi_marg_summary<-
  mhi_marg_results %>%
  group_by(Island) %>%
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
