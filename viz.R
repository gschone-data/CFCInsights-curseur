library(ggplot2)

library(waffle)
#source("source/load_data.R")

GPS_l<-GPS |> select(date,scoring,cat,mois) |> filter(scoring!=0)
setorder(GPS_l,date)
ggplot(GPS_l,aes(x=date,y=scoring))+
  geom_line()+ylab("")->p
p+scale_x_date(date_labels = "%W")
setDT(GPS_l)
GPS_l[,group:=cat]
GPS_l[,subgroup:=mois]
GPS_l[,value:=scoring]

ggplot(data = GPS_l, aes(fill=subgroup, values=value)) +
  geom_waffle(color = "white", size = 1.125, n_rows = 6) +
  facet_wrap(~group, ncol=1) +
  theme_void()
