
rm(list = ls())

library(tidyverse)
library(raster)
library(rgdal)
library(tmap)
library(ncdf4)
library(sp)
boxdir<- '/Users/lennonthomas/Box Sync/Council Stuff/data/'

## Public data summary

kcrab<-read.csv(paste0(boxdir,"kcrab.csv")) %>%
  mutate(Species ="Kona crab")

p_marg<-read.csv(paste0(boxdir,"spiny_lobster.csv")) %>%
  mutate(Species = "Spiny lobster")

slip<-read.csv(paste0(boxdir,"slipper_lobster.csv")) %>%
  mutate(Species = "Slipper lobster")

all_public<-rbind(kcrab,p_marg,slip) %>%
  group_by(Fiscal, Species) %>%
  summarise(annual_landings = sum(Lbs.))

all_public$Species<-as.factor(all_public$Species)
all_public$Species<-factor(all_public$Species,levels(all_public$Species)[c(1,2,3)])


ggplot(all_public,aes(x = Fiscal,y = annual_landings,fill = Species)) +
geom_bar(stat = "identity",position = "stack") +
  theme_bw() +
  xlab("Year") +
  ylab("Landings (lbs) (> 3 licenses)") +
  scale_y_continuous(expand = c(0,2),lim=c(0,7e+04)) +
  scale_x_continuous(lim=c(1948,2017),expand = c(0,0)) +
  #scale_fill_brewer ("Species",palette = "Set5")
#  scale_fill_discrete("Species",direction = -1) +
  scale_fill_viridis(discrete=TRUE) +
   ggtitle("1948-2017") 

hawaii <- readOGR(dsn=paste0(boxdir,'fishchart2008.shp'),layer="Fishchart2008",stringsAsFactors=FALSE)


fish_data_all<-read_csv(paste0(boxdir,"Hawaii -DAR.csv"))
fish_data_all$Scie_Name<-as.factor(fish_data_all$Scie_Name)

fish_data_all$Scie_Name = factor(fish_data_all$Scie_Name,levels(fish_data_all$Scie_Name)[c(7,6,2,3,5,1,4)])



annual_summary<-fish_data_all %>%
 # filter(!Scie_Name=="Ranina ranina") %>%
  group_by(Report_Year,Scie_Name) %>%
  summarise(sp_landings = sum(Lbs_Kept,na.rm = TRUE)) %>%
  #ungroup() %>%
  group_by(Report_Year)%>%
  mutate(landings=sum(sp_landings,na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop_total = sp_landings/landings*100) 

#annual_summary$Scie_Name<-as.factor(annual_summary$Scie_Name)

totals<-fish_data_all %>%
  group_by(Scie_Name) %>%
  summarise(total_land = sum(Lbs_Kept)) %>%
  arrange(desc(total_land))
totals$Scie_Name<-as.factor(totals$Scie_Name)





recent_summary<-annual_summary %>%
  filter(Report_Year > 1995) 
 # arrange(Scie_Name)

ggplot(annual_summary,aes(x=Report_Year,y=sp_landings,fill=(Scie_Name)))+
  geom_bar(stat = "identity",position = "stack")+
  theme_bw()+
  xlab("Year") +
  ylab("Landings (lbs)") +
  scale_y_continuous(expand = c(0,2),lim=c(0,7e+05)) +
  #scale_fill_brewer ("Species",palette = "Set5")
  scale_fill_discrete("Species",direction = -1) +
  ggtitle("1948-2017") 
#  scale_fill_viridis("Species",discrete=TRUE,direction = -1,option="C")


ggplot(recent_summary,aes(x=Report_Year,y=sp_landings,fill=(Scie_Name)))+
 geom_bar(stat = "identity") +
  #geom_area(position = 'stack') +
  theme_bw()+
  xlab("Year") +
  ylab("Landings (lbs)") +
  scale_fill_discrete("Species",direction = -1) +
 # scale_y_continuous(expand = c(0,0), lim=c(0,10000))+
#  scale_fill_discrete("Species") +
  ggtitle("1996-2017 ") +
 # scale_fill_viridis(discrete=TRUE)

ggplot(recent_summary,aes(x=Report_Year,y=sp_landings,fill=Scie_Name))+
  geom_area(position = 'stack') +
  theme_bw()+
  xlab("Year") +
  ylab("Landings (lbs)") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_discrete("Species") +
  ggtitle("Hawaii Commercial Crustacean Landings")

recent_landings_area<-fish_data_all %>%
  filter(Report_Year>1996) %>%
  group_by(Area_FK,Scie_Name) %>%
  summarise(total_landings = sum(Lbs_Kept,na.rm = TRUE)) %>%
  filter(!is.na(Area_FK)) %>%
  filter(!Area_FK %in% c(0,10,99999))
 # spread(Scie_Name,total_landings)


hawaii@data<-left_join(hawaii@data,sp_public,by=c("AREA_ID"= "Area.code"))

tm_shape(hawaii) +
  tm_fill(col = "lightblue",alpha=0.3) +
  # tm_text("AREA_ID",size=0.5)+
  tm_borders(lwd = 1.2) +
  tm_shape()+
  tm_fill(col = "white") +
  
  tm_shape(depth) +
  tm_raster()



Species<-unique(fish_data_all$Scie_Name)

for (i in 1:length(Species)){

  recent_landings_area<-fish_data_all %>%
    filter(Report_Year>1996) %>%
    group_by(Area_FK,Scie_Name) %>%
    summarise(total_landings = sum(Lbs_Kept,na.rm = TRUE)) %>%
    filter(!is.na(Area_FK)) %>%
    filter(!Area_FK %in% c(0,10,99999)) %>%
    filter(Scie_Name==Species[i])
  # spread(Scie_Name,total_landings)
  
  
  
  long_and_lat <-coordinates(hawaii)
  colnames(long_and_lat)<-c("long","lat")
  
  # %>% fortify() %>% dplyr::select(long,lat) 
  hawaii_df<-as.data.frame(hawaii)
  hawaii_df<-cbind(hawaii_df,long_and_lat) 
  
  
  kcrab<-read.csv(paste0(boxdir,"kcrab.csv")) %>%
    group_by(Area.code) %>%
    summarise(landings=log(sum(Lbs.)))%>%
    select(Area.code,landings) %>%
    setNames(c("Area","Kona_crab"))
  
  p_marg<-read.csv(paste0(boxdir,"spiny_lobster.csv")) %>%
    group_by(Area.code) %>%
    summarise(landings=log(sum(Lbs.)))%>%
    select(Area.code,landings) %>%
   setNames(c("Area","Spiny_lobster"))
  
  slip<-read.csv(paste0(boxdir,"slipper_lobster.csv")) %>%
    group_by(Area.code) %>%
    summarise(landings=log(sum(Lbs.)))%>%
    select(Area.code,landings) %>%
    setNames(c("Area","Slipper_lobster"))
  
sp_public<-full_join(p_marg,kcrab)  

sp_public<-left_join(sp_public,slip)
  

  hawaii_df<-merge(hawaii,sp_public,by.x="AREA_ID",by.y="Area",all=TRUE)
#  hawaii_df$Scie_Name<-as.character(Species[i])
  land<-hawaii[hawaii@data$TYPE=="Island",]  
  #hawaii_df2<-left_join(hawaii_df,recent_landings_area,by=c("AREA_ID"= "Area_FK"))
  mhi_ext<-c(-161,-154.3,18.5,22.75)
  mhi<-crop(hawaii,mhi_ext)
  

    tm_shape(mhi)+
    tm_fill(col="lightblue", alpha = 0.4) +
    tm_shape(hawaii_df) +
    tm_fill(col="Kona_crab",colorNA = "lightblue", showNA = FALSE, title = "Kona crab \n landings (lbs)") +
    tm_borders(lwd = 1.2) +
    tm_layout(legend.position = c("RIGHT","top"),legend.outside=TRUE) +
    tm_shape(land)+
    tm_fill(col = "white") 
   
    tm_shape(mhi)+
    tm_fill(col="lightblue",alpha = 0.4) +
    tm_shape(hawaii_df) +
    tm_fill(col="Spiny_lobster",colorNA = "lightblue", showNA = FALSE, title = "Spiny lobster landings (lbs)") +
    tm_borders(lwd = 1.2) +
    tm_layout(legend.position = c("RIGHT","top"),legend.outside=TRUE) +
    tm_shape(land)+
    tm_fill(col = "white") 
  
  tm_shape(mhi)+
    tm_fill(col="lightblue",alpha=0.4) +
    tm_borders(lwd = 1.2) +
    tm_shape(hawaii_df) +
    tm_fill(col="Slipper_lobster",colorNA = "lightblue", showNA = FALSE, title = "Slipper lobster landings (lbs)") +
    tm_borders(lwd = 1.2) +
    tm_layout(legend.position = c("RIGHT","top"),legend.outside=FALSE) +
    tm_shape(land)+
    tm_fill(col = "white") 
  
  
   
  
ha
ggplot() + 
  geom_polygon(data = hawaii,aes(x = long,y = lat,group = group), colour = "black") +
  geom_fill() +
 geom_raster(data=hawaii_df, aes(x=long,y=lat,fill= sp_landings), colour = "black", size = 0.1 , alpha = 0.8) +
#  facet_wrap(~Scie_Name) +
 # geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "white", colour = "black", size = 0.1) +
  scale_fill_viridis(paste(Species[i],"Catch (1996-2016")) +
  guides(fill = guide_colorbar(title.vjust = 0.75)) +
  # ggtitle("(all suitable cells)") +
  theme_bw() + 
  #theme(legend.position = 'bottom') +
  # theme(legend.title.align =0.5,legend.position=c(0.87,0.87),legend.background = element_rect( fill = "white", color = "white")) +
  xlab("Longitude") +
  ylab("Latitude") 
  #coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30))
ggsave(paste0(boxdir,"Figures/",Species[i] "spatial_catch.jpeg"))

}

guam<-read.csv(paste0(boxdir,"Guam -DAWR.csv"))
names(Guam_catch)

guam$Scie_Name<-as.factor(guam$Scie_Name)

guam$Scie_Name = factor(guam$Scie_Name,levels(guam$Scie_Name)[c(3,2,1,5,7,6,4)])


guam<-guam %>%
  group_by(Year,Scie_Name)%>%
  summarise(total_catch=sum(Est_Lbs),
            total_number=sum(Num_Kept)) %>%
  ungroup()

ggplot(guam,aes(x=Year,y=total_catch,fill=(Scie_Name)))+
  geom_bar(stat = "identity") +
  #geom_area(position = 'stack') +
  theme_bw()+
  xlab("Year") +
  ylab("Landings (lbs)") +
  scale_y_continuous(expand = c(0,0))+#lim=c(0,10000))+
  #  scale_fill_discrete("Species") +
  ggtitle("Guam Commercial Crustacean Landings") +
  scale_fill_viridis(discrete=TRUE)

totals<-guam %>%
  group_by(Scie_Name) %>%
  summarise(total_land = sum(total_catch)) %>%
  arrange(desc(total_land))

recent_guam<-guam %>%
  filter(Year>1995)

ggplot(guam,aes(x=Year,y=total_catch,fill=(Scie_Name)))+
  geom_bar(stat = "identity") +
  #geom_area(position = 'stack') +
  theme_bw()+
  xlab("Year") +
  ylab("Landings (lbs)") +
  scale_y_continuous(expand = c(0,0),lim=c(0,300))+
  #  scale_fill_discrete("Species") +
  ggtitle("1983-2016") +
  scale_fill_viridis("Species",discrete=TRUE,direction=-1)



k_crab_results<-raster( paste0(boxdir,"k_crab_results/mhi_suitable.tif"))


## Comparing kona crab data

# data from google drive

kcrab_data<-read_csv(paste0(boxdir,"kcrab.csv"))

hdar<-read_csv(paste0(boxdir,"Hawaii -DAR.csv")) %>%
  filter(Scie_Name=="Ranina ranina")

kcrab_year<-kcrab_data %>%
  group_by(Area code, Fiscal) %>%
  summarise(kcrab_landings= sum(Lbs.)) %>%
  ungroup()

hdar_year<-hdar %>%
  group_by(Report_Year,Area_FK) %>%
  summarise(hdar_landings = sum(Lbs_Kept)) %>%
  left_join(kcrab_year, by= c("Report_Year"= "Fiscal")) %>%
  gather(key=datatype,value=kcrab_landings,hdar_landings)



