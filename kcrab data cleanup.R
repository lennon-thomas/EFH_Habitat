library(tidyverse)
library(measurements)

bottom_data<-read.csv("kcrab_bottom_data.csv",stringsAsFactors = FALSE)

bottom_data$Latitude = substr(bottom_data$Latitude,1,nchar(bottom_data$Latitude)-1)

bottom_data$Latitude = gsub('°', ' ', bottom_data$Latitude)

bottom_data$new_lat<-conv_unit(bottom_data$Latitude,from="deg_dec_min",to="dec_deg")

bottom_data$new_lat<-as.numeric(as.character(bottom_data$new),length=6)

#Longitude

bottom_data$Longitude = substr(bottom_data$Longitude,1,nchar(bottom_data$Longitude)-1)

bottom_data$Longitude = gsub('°', ' ', bottom_data$Longitude)

bottom_data$new_long<-conv_unit(bottom_data$Longitude,from="deg_dec_min",to="dec_deg")

bottom_data$new_long<-as.numeric(as.character(bottom_data$new_long),length=6)

## Bottomtype
bottom_data$Bottom.type<-tolower(bottom_data$Bottom.type)
bottom_data$Bottom.type<-as.factor(bottom_data$Bottom.type)

bottom_summary<-bottom_data%>%
  group_by(Date,Set,Island,new_lat,new_long)%>%
  summarise(bottom=unique(Bottom.type))

# depth -------------------------------------------------------------------


depth_data<-read.csv("kcrab_depth.csv",stringsAsFactors = FALSE)
depth_data$Latitude = substr(depth_data$Latitude,1,nchar(depth_data$Latitude)-1)

depth_data$Latitude = gsub('°', ' ', depth_data$Latitude)

depth_data$new_lat<-conv_unit(depth_data$Latitude,from="deg_dec_min",to="dec_deg")

depth_data$new_lat<-as.numeric(as.character(depth_data$new),length=6)

#Longitude

depth_data$Longitude = substr(depth_data$Longitude,1,nchar(depth_data$Longitude)-1)

depth_data$Longitude = gsub('°', ' ', depth_data$Longitude)

depth_data$new_long<-conv_unit(depth_data$Longitude,from="deg_dec_min",to="dec_deg")

depth_data$new_long<-as.numeric(as.character(depth_data$new_long),length=6)

depth_data$Carapace.Length..in.<-as.numeric(as.character(depth_data$Carapace.Length..in.))

bottom_data<-bottom_data %>%
  dplyr::select("Date","Island","Set","Bottom.type","new_lat","new_long")

depth_summary<- depth_data %>%
#  select("Date","Island","Set","Depth..fa.","Carapace.Length..in.","Sex","new_lat","new_long") %>%
  group_by(Date,Island,Set,new_lat,new_long) %>%
  summarise( no_crabs=length(Carapace.Length..in.),
            mean_length=mean(Carapace.Length..in.),
         # 
            mean_depth=mean(Depth..fa.,na.rm=TRUE)) %>%
  ungroup() %>%
  left_join(bottom_summary)

write.csv(depth_summary,"kcrab_mort_study.csv")  
   # left_join(bottom_data)
#test<-merge(depth_data,bottom_data,by=c("Date","Island","Set","new.lat","new.long"),all.x=TRUE,all.y=FALSE)
