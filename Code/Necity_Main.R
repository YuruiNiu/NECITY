#Dataset Description
#Date is from 2000--2021 6.01--8.31
#Each observation in a has a valid unique CODE. 
#The CODE for each observation is constant in different files.
#Data extracted from 
#1. https://developers.google.com/earth-engine/datasets/catalog/NASA_ORNL_DAYMET_V4#bands
#2. https://developers.google.com/earth-engine/datasets/catalog/NASA_ORNL_DAYMET_V4

### 1. Packages ####
#install.packages("patchwork")
library(dplyr)
library(data.table)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(lubridate)
library(purrr)

### 2. Import the datasets ####
NEcity_FID0impervious <- read.csv("~/Desktop/Work/city0/NEcity_FID0impervious.csv")
NEcity_FID0_band_srad <- read.csv("~/Desktop/Work/city0/NEcity_FID0_band_srad.csv")
NEcity_FID0_band_vp <- read.csv("~/Desktop/Work/city0/NEcity_FID0_band_vp.csv")
NEcity_FID0_band_tmax <- read.csv("~/Desktop/Work/city0/NEcity_FID0_band_tmax.csv")
NEcity_FID0_band_tmin <- read.csv("~/Desktop/Work/city0/NEcity_FID0_band_tmin.csv")
NEcity_FID0_band_vs <- read.csv("~/Desktop/Work/city0/NEcity_FID0_GRIDMET_vs_ALL_daily.csv")
NEcity_FID0_band_tdmean <- read.csv("~/Desktop/Work/city0/NEcity_FID0_PRISM_tdmean_ALL_daily.csv")

# Creat a new dataframe LatLon
NEcity_FID0_band_Latlon<-NEcity_FID0_band_vs[ , colnames(NEcity_FID0_band_vs) %in% c("lat","lon")]


### 3. Data Cleaning  ####

#Tdmean cleaning
#Fix May and September data included due to extraction issues
cols <- grep("^X20\\d{2}05", names(NEcity_FID0_band_tdmean), value = TRUE)
NEcity_FID0_band_tdmean <- NEcity_FID0_band_tdmean[, -which(names(NEcity_FID0_band_tdmean) %in% cols)]
cols <- grep("^X20\\d{2}09", names(NEcity_FID0_band_tdmean), value = TRUE)
NEcity_FID0_band_tdmean <- NEcity_FID0_band_tdmean[, -which(names(NEcity_FID0_band_tdmean) %in% cols)]

#remove useless columns
Impervious_data_clean<- as.data.frame(NEcity_FID0impervious[ , colnames(NEcity_FID0impervious) %in% c("mean")])
Tmax_data_clean <- NEcity_FID0_band_tmax[ , !colnames(NEcity_FID0_band_tmax) %in% c("system.index","Id",".geo","CODE")]
Tmin_data_clean <- NEcity_FID0_band_tmin[ , !colnames(NEcity_FID0_band_tmin) %in% c("system.index","Id",".geo","CODE")]
Vp_data_clean <-NEcity_FID0_band_vp[ , !colnames(NEcity_FID0_band_vp) %in% c("system.index","Id",".geo","CODE")]
Srad_data_clean <-NEcity_FID0_band_srad[ , !colnames(NEcity_FID0_band_srad) %in% c("system.index","Id",".geo","CODE")]
VS_data_clean<- NEcity_FID0_band_vs[ , !colnames(NEcity_FID0_band_vs) %in% c("system.index","Id",".geo","CODE","lat","lon")]
LatLon_data_clean<-NEcity_FID0_band_Latlon[ , !colnames(NEcity_FID0_band_Latlon) %in% c("Id",".geo","CODE")]
Tdmean_data_clean<-NEcity_FID0_band_tdmean[ , !colnames(NEcity_FID0_band_tdmean) %in% c("system.index","Id",".geo","CODE","lat","lon")]

#Delete impervious data when mean = 0 and weather data with missing values.
zero_code_Imperviouness<-select(NEcity_FID0impervious,c(CODE,mean))%>%filter(mean == 0)
weather_zero_list <-which(rowSums(is.na(Tmax_data_clean)) > 0)
zero_list<-c(zero_code_Imperviouness[,1],weather_zero_list)

#Delete zero list code for each files
Impervious_data_clean <- as.data.frame(Impervious_data_clean[-zero_list, ])
colnames(Impervious_data_clean)<-c("Impervious_mean")
Tmin_data_clean <- Tmin_data_clean[-zero_list, ]
Tmax_data_clean <- Tmax_data_clean[-zero_list, ]
Srad_data_clean <- Srad_data_clean[-zero_list, ]
Vp_data_clean <- Vp_data_clean[-zero_list, ]
VS_data_clean<- VS_data_clean[-zero_list, ]
Tdmean_data_clean <-Tdmean_data_clean[-zero_list, ]
LatLon_data_clean<-LatLon_data_clean[-zero_list, ]

#Create Nnew variables
Tmean<-(Tmax_data_clean+Tmin_data_clean)/2 #need change  title
Saturated_vapor_pressure <- 0.6108*exp(17.27*Tmean/(Tmean+237.3)) #unit is kPa
Saturated_vapor_pressure<-Saturated_vapor_pressure*1000 #unit in Pa
Relative_humidity<- 100*(Vp_data_clean/Saturated_vapor_pressure)

#Convert wind speed to 2m from ground
VS_data_clean<- (VS_data_clean*4.87)/(log(67.8*10-5.42))

#Divide Impervious to 10s group
Impervious_group<-cut(Impervious_data_clean$Impervious_mean,breaks = 10,labels = c(10,20,30,40,50,60,70,80,90,100))
Impervious_data_group<-cbind(Impervious_data_clean,Impervious_group)
colnames(Impervious_data_group)<-c("Impervious_mean","Impervious_level")

#Month Indicator
#Filter out monthly data for each variable
June_cols<-grep("^X20\\d{2}06", names(Tmax_data_clean), value = TRUE)
July_cols<-grep("^X20\\d{2}07", names(Tmax_data_clean), value = TRUE)
Aug_cols<-grep("^X20\\d{2}08", names(Tmax_data_clean), value = TRUE)

#Tmax
Tmax_June <- Tmax_data_clean[, which(names(Tmax_data_clean) %in% June_cols)]
Tmax_July<-Tmax_data_clean[, which(names(Tmax_data_clean) %in% July_cols)]
Tmax_Aug<-Tmax_data_clean[, which(names(Tmax_data_clean) %in% Aug_cols)]

June_mean=as.data.frame((rowMeans(Tmax_June, na.rm=TRUE)))  #each row is a unique pixel, the value is the average in June 
July_mean=as.data.frame((rowMeans(Tmax_July, na.rm=TRUE)))
Aug_mean=as.data.frame((rowMeans(Tmax_Aug, na.rm=TRUE)))

Tmax_mean = cbind(June_mean,July_mean,Aug_mean)
colnames(Tmax_mean) <- c("June","July","Aug")

#Tmin
June_cols<-grep("^X20\\d{2}06", names(Tmin_data_clean), value = TRUE)
July_cols<-grep("^X20\\d{2}07", names(Tmin_data_clean), value = TRUE)
Aug_cols<-grep("^X20\\d{2}08", names(Tmin_data_clean), value = TRUE)

Tmin_June <- Tmin_data_clean[, which(names(Tmin_data_clean) %in% June_cols)]
Tmin_July<-Tmin_data_clean[, which(names(Tmin_data_clean) %in% July_cols)]
Tmin_Aug<-Tmin_data_clean[, which(names(Tmin_data_clean) %in% Aug_cols)]
June_mean=as.data.frame((rowMeans(Tmin_June, na.rm=TRUE)))  #each row is a unique pixel, the value is the average in June 
July_mean=as.data.frame((rowMeans(Tmin_July, na.rm=TRUE)))
Aug_mean=as.data.frame((rowMeans(Tmin_Aug, na.rm=TRUE)))

Tmin_mean = cbind(June_mean,July_mean,Aug_mean)
colnames(Tmin_mean) <- c("June","July","Aug")

#Relative Humidity
June_cols<-grep("^X20\\d{2}06", names(Relative_humidity), value = TRUE)
July_cols<-grep("^X20\\d{2}07", names(Relative_humidity), value = TRUE)
Aug_cols<-grep("^X20\\d{2}08", names(Relative_humidity), value = TRUE)

RH_June <- Relative_humidity[, which(names(Relative_humidity) %in% June_cols)]
RH_July<-Relative_humidity[, which(names(Relative_humidity) %in% July_cols)]
RH_Aug<-Relative_humidity[, which(names(Relative_humidity) %in% Aug_cols)]
June_mean=as.data.frame((rowMeans(RH_June, na.rm=TRUE)))  #each row is a unique pixel, the value is the average in June 每一个格子一个月内的平均值
July_mean=as.data.frame((rowMeans(RH_July, na.rm=TRUE)))
Aug_mean=as.data.frame((rowMeans(RH_Aug, na.rm=TRUE)))

RH_mean = cbind(June_mean,July_mean,Aug_mean)
colnames(RH_mean) <- c("June","July","Aug")

#EA
June_cols<-grep("^X20\\d{2}06", names(Vp_data_clean), value = TRUE)
July_cols<-grep("^X20\\d{2}07", names(Vp_data_clean), value = TRUE)
Aug_cols<-grep("^X20\\d{2}08", names(Vp_data_clean), value = TRUE)

EA_June <- Vp_data_clean[, which(names(Vp_data_clean) %in% June_cols)]
EA_July<-Vp_data_clean[, which(names(Vp_data_clean) %in% July_cols)]
EA_Aug<-Vp_data_clean[, which(names(Vp_data_clean) %in% Aug_cols)]

June_mean=as.data.frame((rowMeans(EA_June, na.rm=TRUE)))  #each row is a unique pixel, the value is the average in June 
July_mean=as.data.frame((rowMeans(EA_July, na.rm=TRUE)))
Aug_mean=as.data.frame((rowMeans(EA_Aug, na.rm=TRUE)))

EA_mean = cbind(June_mean,July_mean,Aug_mean)
colnames(EA_mean) <- c("June","July","Aug")

#Vs
VS_June <- VS_data_clean[, which(names(VS_data_clean) %in% June_cols)]
VS_July<-VS_data_clean[, which(names(VS_data_clean) %in% July_cols)]
VS_Aug<-VS_data_clean[, which(names(VS_data_clean) %in% Aug_cols)]

June_mean=as.data.frame((rowMeans(VS_June, na.rm=TRUE)))  #each row is a unique pixel, the value is the average in June 
July_mean=as.data.frame((rowMeans(VS_July, na.rm=TRUE)))
Aug_mean=as.data.frame((rowMeans(VS_Aug, na.rm=TRUE)))

VS_mean = cbind(June_mean,July_mean,Aug_mean)
colnames(VS_mean) <- c("June","July","Aug")

#Tdmean
June_cols<-grep("^X20\\d{2}06", names(Tdmean_data_clean), value = TRUE)
July_cols<-grep("^X20\\d{2}07", names(Tdmean_data_clean), value = TRUE)
Aug_cols<-grep("^X20\\d{2}08", names(Tdmean_data_clean), value = TRUE)

Tdmean_June<- Tdmean_data_clean[, which(names(Tdmean_data_clean) %in% June_cols)]
Tdmean_July<-Tdmean_data_clean[, which(names(Tdmean_data_clean) %in% July_cols)]
Tdmean_Aug<-Tdmean_data_clean[, which(names(Tdmean_data_clean) %in% Aug_cols)]

June_mean=as.data.frame((rowMeans(Tdmean_June, na.rm=TRUE)))  #each row is a unique pixel, the value is the average in June 
July_mean=as.data.frame((rowMeans(Tdmean_July, na.rm=TRUE)))
Aug_mean=as.data.frame((rowMeans(Tdmean_Aug, na.rm=TRUE)))

Tdmean_mean = cbind(June_mean,July_mean,Aug_mean)
colnames(Tdmean_mean) <- c("June","July","Aug")


### 4. Data visualization  ####

#Tmax plot
Impervious_Tmax <- cbind(Tmax_mean,Impervious_data_group)
June_tmax_violin<-
  ggplot(Impervious_Tmax) +
  aes(x = Impervious_level, y = June) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()+ ggtitle("Tmax Violin plot")

July_tmax_violin<-
  ggplot(Impervious_Tmax) +
  aes(x = Impervious_level, y = July) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()

Aug_tmax_violin<-
  ggplot(Impervious_Tmax) +
  aes(x = Impervious_level, y = Aug) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()

June_tmax_violin/July_tmax_violin/Aug_tmax_violin


#Tmin plot
Impervious_Tmin <- cbind(Tmin_mean,Impervious_data_group)

June_tmin_violin<-
  ggplot(Impervious_Tmin) +
  aes(x = Impervious_level, y = June) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()+ ggtitle("Tmin Violin plot")

July_tmin_violin<-
  ggplot(Impervious_Tmin) +
  aes(x = Impervious_level, y = July) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()

Aug_tmin_violin<-
  ggplot(Impervious_Tmin) +
  aes(x = Impervious_level, y = Aug) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()

June_tmin_violin/July_tmin_violin/Aug_tmin_violin


#Relative Humidity plot
Impervious_RH <- cbind(RH_mean,Impervious_data_group)

June_RH_violin<-
  ggplot(Impervious_RH) +
  aes(x = Impervious_level, y = June) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()+ ggtitle("RH Violin plot")

July_RH_violin<-
  ggplot(Impervious_RH) +
  aes(x = Impervious_level, y = July) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()

Aug_RH_violin<-
  ggplot(Impervious_RH) +
  aes(x = Impervious_level, y = Aug) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()

June_RH_violin/July_RH_violin/Aug_RH_violin


#EA
Impervious_EA <- cbind(EA_mean,Impervious_data_group)

June_EA_violin<-
  ggplot(Impervious_EA) +
  aes(x = Impervious_level, y = June) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +ggtitle("EA Violin Plot")+theme_minimal()

July_EA_violin<-
  ggplot(Impervious_EA) +
  aes(x = Impervious_level, y = July) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()

Aug_EA_violin<-
  ggplot(Impervious_EA) +
  aes(x = Impervious_level, y = Aug) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()

June_EA_violin/July_EA_violin/Aug_EA_violin


#Tas
Tas_mean <- (Tmax_mean+Tmin_mean)/2
Impervious_Tas <- cbind(Tas_mean,Impervious_data_group)

June_Tas_violin<-
  ggplot(Impervious_Tas) +
  aes(x = Impervious_level, y = June) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +ggtitle("Tas Violin Plot")+theme_minimal()

July_Tas_violin<-
  ggplot(Impervious_Tas) +
  aes(x = Impervious_level, y = July) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()

Aug_Tas_violin<-
  ggplot(Impervious_Tas) +
  aes(x = Impervious_level, y = Aug) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()

June_Tas_violin/July_Tas_violin/Aug_Tas_violin


### 5. Data converting  ####
#We need Tas(Tmean), Dewp(Tdmean), wind(VS),radiation(srad),Latlon to calculate wbgt

#transpose Tas(Tmean)
Tmean <- as.data.frame(t(Tmean))
Tmean <- data.frame(Date = row.names(Tmean), Tmean)
Tmean$Date<- gsub("_tmax", "", Tmean$Date)
Tmean$Date<- gsub("X", "", Tmean$Date)
Tmean$Date <-ymd(Tmean[,1])

#transpose Dewp(Tdmean)
Tdmean <- as.data.frame(t(Tdmean_data_clean))
Tdmean <- data.frame(Date = row.names(Tdmean), Tdmean)
Tdmean$Date<- gsub("_tdmean", "", Tdmean$Date)
Tdmean$Date<- gsub("X", "", Tdmean$Date)
Tdmean$Date <-ymd(Tdmean[,1])

#transpose wind(VS)
VS <- as.data.frame(t(VS_data_clean))
VS <- data.frame(Date = row.names(VS), VS)
VS$Date<- gsub("_vs", "", VS$Date)
VS$Date<- gsub("X", "", VS$Date)
VS$Date <-ymd(VS[,1])

#transpose radiation(srad)
Srad <- as.data.frame(t(Srad_data_clean))
Srad <- data.frame(Date = row.names(Srad), Srad)
Srad$Date<- gsub("_srad", "", Srad$Date)
Srad$Date<- gsub("X", "", Srad$Date)
Srad$Date <-ymd(Srad[,1])

#transpose Latlon
LatLon <- as.data.frame(t(LatLon_data_clean))
LatLon <- data.frame(X = row.names(LatLon), LatLon)






