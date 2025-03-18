library(data.table)
library(lubridate)
library(dplyr)

GPS<-fread("source/CFC GPS Data.csv")
IPA<-fread("source/CFC Individual Priority Areas.csv")
PCD<-fread("source/CFC Physical Capability Data_.csv")
RSD<-fread("source/CFC Recovery status Data.csv")


# travaux GPS -------------------------------------------------------------

#hh:mm:ss to number of seconds (integer)
convert_to_sec<-function(x){
  prov<-strsplit(x,":")[[1]]
  h=as.integer(prov[1])
  m=as.integer(prov[2])
  s=as.integer(prov[3])
  return=(h*60+m)*60+s
}
#vectorized size
convert_to_sec_vec<-Vectorize(convert_to_sec)

#conversion
GPS[,hr1:=convert_to_sec_vec(hr_zone_1_hms)]
GPS[,hr2:=convert_to_sec_vec(hr_zone_2_hms)]
GPS[,hr3:=convert_to_sec_vec(hr_zone_3_hms)]
GPS[,hr4:=convert_to_sec_vec(hr_zone_4_hms)]
GPS[,hr5:=convert_to_sec_vec(hr_zone_5_hms)]

#time with zone specified
GPS[,time:=hr1+hr2+hr3+hr4+hr5]

#date conversion
GPS[,date:=dmy(date)]
#add month facility
GPS[,mois:=paste0(year(date),sprintf("%02d",month(date)))]

#conversion second of day duration (natively in minute)
GPS[,day_duration_s:=floor(day_duration)*60+floor((day_duration%%1)*100)]

#add variable with difference between day_duration_s et time
GPS[,hr0:=day_duration_s-time]

#replace NA by 0 

GPS[,ecart:=time-day_duration_s]
GPS[,zone1:=coalesce(hr1/day_duration_s,0)]
GPS[,zone2:=coalesce(hr2/day_duration_s,0)]
GPS[,zone3:=coalesce(hr3/day_duration_s,0)]
GPS[,zone4:=coalesce(hr4/day_duration_s,0)]
GPS[,zone5:=coalesce(hr5/day_duration_s,0)]

GPS[,scoring:=case_when(
  (zone5+zone4)>0.4~50,
  (zone5+zone4)>0.2~40,
  (zone3+zone4)>0.2~30,
  (zone2+zone3)>0.1~10,
  T~ 0)]
GPS[,cat:=case_when(
  (zone5+zone4)>0.4~"S",
  (zone5+zone4+zone3)>0.5~"A",
  (zone3+zone4)>0.3~"B",
  (zone2+zone3)>0.1~"C",
  T~ "E")]

GPS[,match:=trimws(opposition_code)!=""]
