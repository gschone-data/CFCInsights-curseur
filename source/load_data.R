library(data.table)
library(lubridate)
GPS<-fread("source/CFC GPS Data.csv")
IPA<-fread("source/CFC Individual Priority Areas.csv")
PCD<-fread("source/CFC Physical Capability Data_.csv")
RSD<-fread("source/CFC Recovery status Data.csv")

convert_to_sec<-function(x){
  prov<-strsplit(x,":")[[1]]
  h=as.integer(prov[1])
  m=as.integer(prov[2])
  s=as.integer(prov[3])
  return=(h*60+m)*60+s
  }
#ajout pourcentage de voitesser et de zone
GPS[,zone1:=convert_to_sec(hr_zone_1_hms)/(day_duration*60)]
GPS[,zone2:=convert_to_sec(hr_zone_2_hms)/(day_duration*60)]
GPS[,zone3:=convert_to_sec(hr_zone_3_hms)/(day_duration*60)]
GPS[,zone4:=convert_to_sec(hr_zone_4_hms)/(day_duration*60)]
GPS[,zone5:=convert_to_sec(hr_zone_5_hms)/(day_duration*60)]
