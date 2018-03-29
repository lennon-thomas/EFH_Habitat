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

annual_summary$Scie_Name<-as.factor(annual_summary$Scie_Name)

totals<-fish_data_all %>%
  group_by(Scie_Name) %>%
  summarise(total_land = sum(Lbs_Kept)) %>%
  arrange(desc(total_land))
totals$Scie_Name<-as.factor(totals$Scie_Name)

hawaii <- readOGR(dsn=paste0(boxdir,'fishchart2008.shp'),layer="Fishchart2008",stringsAsFactors=FALSE)





recent_summary<-annual_summary %>%
  filter(Report_Year > 1995) 
 # arrange(Scie_Name)

ggplot(annual_summary,aes(x=Report_Year,y=sp_landings,fill=Scie_Name))+
  geom_bar(stat = "identity",position = "stack")+
  theme_bw()+
  xlab("Year") +
  ylab("Landings (lbs)") +
  scale_y_continuous(expand = c(0,2),lim=c(0,7e+05)) +
#  scale_fill_discrete("Species") +
  ggtitle("Hawaii Commercial Crustacean Landings")+
  scale_fill_viridis(discrete=TRUE)


ggplot(recent_summary,aes(x=Report_Year,y=sp_landings,fill=(Scie_Name)))+
 geom_bar(stat = "identity") +
  #geom_area(position = 'stack') +
  theme_bw()+
  xlab("Year") +
  ylab("Landings (lbs)") +
  scale_y_continuous(expand = c(0,0), lim=c(0,10000))+
#  scale_fill_discrete("Species") +
  ggtitle("Hawaii Commercial Crustacean Landings") +
  scale_fill_viridis(discrete=TRUE)

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


hawaii@data<-left_join(hawaii@data,recent_landings_area,by=c("AREA_ID"= "Area_FK"))




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
  hawaii_df<-merge(hawaii_df,recent_landings_area,by.x="AREA_ID",by.y="Area_FK",all=TRUE)
  hawaii_df$Scie_Name<-as.character(Species[i])
  
  #hawaii_df2<-left_join(hawaii_df,recent_landings_area,by=c("AREA_ID"= "Area_FK"))
  
  
ha
ggplot() + 
  geom_polygon(data = hawaii,aes(x = long,y = lat,group = group), colour = "black") +
 geom_raster(data=hawaii_df, aes(x=long,y=lat,fill= total_landings), colour = "black", size = 0.1 , alpha = 0.8) +
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

Guam_catch<-read.csv(paste0(boxdir,"Guam -DAWR.csv"))
names(Guam_catch)

guam<-Guam_catch %>%
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

recent_guam<-guam %>%
  filter(Year>1995)

ggplot(recent_guam,aes(x=Year,y=total_catch,fill=(Scie_Name)))+
  geom_bar(stat = "identity") +
  #geom_area(position = 'stack') +
  theme_bw()+
  xlab("Year") +
  ylab("Landings (lbs)") +
  scale_y_continuous(expand = c(0,0),lim=c(0,500))+
  #  scale_fill_discrete("Species") +
  ggtitle("Guam Commercial Crustacean Landings") +
  scale_fill_viridis(discrete=TRUE)