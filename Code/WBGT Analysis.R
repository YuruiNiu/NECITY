#Extract extreme days

### 1. Import the datasets ####
data1<-output
data4=data.frame(data1[,1],data1[,4],data1[,5])
colnames(data4) = c("wbgt","dates","point")
result<- data4%>% pivot_wider(names_from = dates, values_from = wbgt)
res<-result


### 2. Data Cleaning ####

#Extracted data for each year (i.e year_00)
#Find value that is larger than 90 percentage for each grid (year_00_extreme)

#2000
cols <- grep("^2000", names(result), value = TRUE)
year_00 <- result[, which(names(result) %in% cols)]
year_00_extreme<-year_00
year_00_extreme$quantile<- purrr::map_dfr(1:nrow(year_00_extreme),function(x){
  quantile(unlist(as.vector(year_00_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_00_extreme[,1:92] - as.numeric(unlist(year_00_extreme$quantile)))
year_00_extreme[,1:92] <- (indicator > 0)*year_00_extreme[,1:92]
year_00_extreme[year_00_extreme==0] =NA
year_00_extreme<-year_00_extreme[,-93]

#2001
cols <- grep("^2001", names(result), value = TRUE)
year_01 <- result[, which(names(result) %in% cols)]
year_01_extreme<-year_01
year_01_extreme$quantile<- purrr::map_dfr(1:nrow(year_01_extreme),function(x){
  quantile(unlist(as.vector(year_01_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_01_extreme[,1:92] - as.numeric(unlist(year_01_extreme$quantile)))
year_01_extreme[,1:92] <- (indicator > 0)*year_01_extreme[,1:92]
year_01_extreme[year_01_extreme==0] =NA
year_01_extreme<-year_01_extreme[,-93]

#2002
cols <- grep("^2002", names(result), value = TRUE)
year_02 <- result[, which(names(result) %in% cols)]
year_02_extreme<-year_02
year_02_extreme$quantile<- purrr::map_dfr(1:nrow(year_02_extreme),function(x){
  quantile(unlist(as.vector(year_02_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_02_extreme[,1:92] - as.numeric(unlist(year_02_extreme$quantile)))
year_02_extreme[,1:92] <- (indicator > 0)*year_02_extreme[,1:92]
year_02_extreme[year_02_extreme==0] =NA
year_02_extreme<-year_02_extreme[,-93]

#2003
cols <- grep("^2003", names(result), value = TRUE)
year_03 <- result[, which(names(result) %in% cols)]
year_03_extreme<-year_03
year_03_extreme$quantile<- purrr::map_dfr(1:nrow(year_03_extreme),function(x){
  quantile(unlist(as.vector(year_03_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_03_extreme[,1:92] - as.numeric(unlist(year_03_extreme$quantile)))
year_03_extreme[,1:92] <- (indicator > 0)*year_03_extreme[,1:92]
year_03_extreme[year_03_extreme==0] =NA
year_03_extreme<-year_03_extreme[,-93]

#2004
cols <- grep("^2004", names(result), value = TRUE)
year_04 <- result[, which(names(result) %in% cols)]
year_04_extreme<-year_04
year_04_extreme$quantile<- purrr::map_dfr(1:nrow(year_04_extreme),function(x){
  quantile(unlist(as.vector(year_04_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_04_extreme[,1:92] - as.numeric(unlist(year_04_extreme$quantile)))
year_04_extreme[,1:92] <- (indicator > 0)*year_04_extreme[,1:92]
year_04_extreme[year_04_extreme==0] =NA
year_04_extreme<-year_04_extreme[,-93]

#2005
cols <- grep("^2005", names(result), value = TRUE)
year_05 <- result[, which(names(result) %in% cols)]
year_05_extreme<-year_05
year_05_extreme$quantile<- purrr::map_dfr(1:nrow(year_05_extreme),function(x){
  quantile(unlist(as.vector(year_05_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_05_extreme[,1:92] - as.numeric(unlist(year_05_extreme$quantile)))
year_05_extreme[,1:92] <- (indicator > 0)*year_05_extreme[,1:92]
year_05_extreme[year_05_extreme==0] =NA
year_05_extreme<-year_05_extreme[,-93]

#2006
cols <- grep("^2006", names(result), value = TRUE)
year_06 <- result[, which(names(result) %in% cols)]
year_06_extreme<-year_06
year_06_extreme$quantile<- purrr::map_dfr(1:nrow(year_06_extreme),function(x){
  quantile(unlist(as.vector(year_06_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_06_extreme[,1:92] - as.numeric(unlist(year_06_extreme$quantile)))
year_06_extreme[,1:92] <- (indicator > 0)*year_06_extreme[,1:92]
year_06_extreme[year_06_extreme==0] =NA
year_06_extreme<-year_06_extreme[,-93]

#2007
cols <- grep("^2007", names(result), value = TRUE)
year_07 <- result[, which(names(result) %in% cols)]
year_07_extreme<-year_07
year_07_extreme$quantile<- purrr::map_dfr(1:nrow(year_07_extreme),function(x){
  quantile(unlist(as.vector(year_07_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_07_extreme[,1:92] - as.numeric(unlist(year_07_extreme$quantile)))
year_07_extreme[,1:92] <- (indicator > 0)*year_07_extreme[,1:92]
year_07_extreme[year_07_extreme==0] =NA
year_07_extreme<-year_07_extreme[,-93]

#2008
cols <- grep("^2008", names(result), value = TRUE)
year_08 <- result[, which(names(result) %in% cols)]
year_08_extreme<-year_08
year_08_extreme$quantile<- purrr::map_dfr(1:nrow(year_08_extreme),function(x){
  quantile(unlist(as.vector(year_08_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_08_extreme[,1:92] - as.numeric(unlist(year_08_extreme$quantile)))
year_08_extreme[,1:92] <- (indicator > 0)*year_08_extreme[,1:92]
year_08_extreme[year_08_extreme==0] =NA
year_08_extreme<-year_08_extreme[,-93]

#2009
cols <- grep("^2009", names(result), value = TRUE)
year_09 <- result[, which(names(result) %in% cols)]
year_09_extreme<-year_09
year_09_extreme$quantile<- purrr::map_dfr(1:nrow(year_09_extreme),function(x){
  quantile(unlist(as.vector(year_09_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_09_extreme[,1:92] - as.numeric(unlist(year_09_extreme$quantile)))
year_09_extreme[,1:92] <- (indicator > 0)*year_09_extreme[,1:92]
year_09_extreme[year_09_extreme==0] =NA
year_09_extreme<-year_09_extreme[,-93]

#2010
cols <- grep("^2010", names(result), value = TRUE)
year_10 <- result[, which(names(result) %in% cols)]
year_10_extreme<-year_10
year_10_extreme$quantile<- purrr::map_dfr(1:nrow(year_10_extreme),function(x){
  quantile(unlist(as.vector(year_10_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_10_extreme[,1:92] - as.numeric(unlist(year_10_extreme$quantile)))
year_10_extreme[,1:92] <- (indicator > 0)*year_10_extreme[,1:92]
year_10_extreme[year_10_extreme==0] =NA
year_10_extreme<-year_10_extreme[,-93]

#2011
cols <- grep("^2011", names(result), value = TRUE)
year_11 <- result[, which(names(result) %in% cols)]
year_11_extreme<-year_11
year_11_extreme$quantile<- purrr::map_dfr(1:nrow(year_11_extreme),function(x){
  quantile(unlist(as.vector(year_11_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_11_extreme[,1:92] - as.numeric(unlist(year_11_extreme$quantile)))
year_11_extreme[,1:92] <- (indicator > 0)*year_11_extreme[,1:92]
year_11_extreme[year_11_extreme==0] =NA
year_11_extreme<-year_11_extreme[,-93]

#2012
cols <- grep("^2012", names(result), value = TRUE)
year_12 <- result[, which(names(result) %in% cols)]
year_12_extreme<-year_12
year_12_extreme$quantile<- purrr::map_dfr(1:nrow(year_12_extreme),function(x){
  quantile(unlist(as.vector(year_12_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_12_extreme[,1:92] - as.numeric(unlist(year_12_extreme$quantile)))
year_12_extreme[,1:92] <- (indicator > 0)*year_12_extreme[,1:92]
year_12_extreme[year_12_extreme==0] =NA
year_12_extreme<-year_12_extreme[,-93]

#2013
cols <- grep("^2013", names(result), value = TRUE)
year_13<- result[, which(names(result) %in% cols)]
year_13_extreme<-year_13
year_13_extreme$quantile<- purrr::map_dfr(1:nrow(year_13_extreme),function(x){
  quantile(unlist(as.vector(year_13_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_13_extreme[,1:92] - as.numeric(unlist(year_13_extreme$quantile)))
year_13_extreme[,1:92] <- (indicator > 0)*year_13_extreme[,1:92]
year_13_extreme[year_13_extreme==0] =NA
year_13_extreme<-year_13_extreme[,-93]

#2014
cols <- grep("^2014", names(result), value = TRUE)
year_14 <- result[, which(names(result) %in% cols)]
year_14_extreme<-year_14
year_14_extreme$quantile<- purrr::map_dfr(1:nrow(year_14_extreme),function(x){
  quantile(unlist(as.vector(year_14_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_14_extreme[,1:92] - as.numeric(unlist(year_14_extreme$quantile)))
year_14_extreme[,1:92] <- (indicator > 0)*year_14_extreme[,1:92]
year_14_extreme[year_14_extreme==0] =NA
year_14_extreme<-year_14_extreme[,-93]

#2015
cols <- grep("^2015", names(result), value = TRUE)
year_15 <- result[, which(names(result) %in% cols)]
year_15_extreme<-year_15
year_15_extreme$quantile<- purrr::map_dfr(1:nrow(year_15_extreme),function(x){
  quantile(unlist(as.vector(year_15_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_15_extreme[,1:92] - as.numeric(unlist(year_15_extreme$quantile)))
year_15_extreme[,1:92] <- (indicator > 0)*year_15_extreme[,1:92]
year_15_extreme[year_15_extreme==0] =NA
year_15_extreme<-year_15_extreme[,-93]

#2016
cols <- grep("^2016", names(result), value = TRUE)
year_16<- result[, which(names(result) %in% cols)]
year_16_extreme<-year_16
year_16_extreme$quantile<- purrr::map_dfr(1:nrow(year_16_extreme),function(x){
  quantile(unlist(as.vector(year_16_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_16_extreme[,1:92] - as.numeric(unlist(year_16_extreme$quantile)))
year_16_extreme[,1:92] <- (indicator > 0)*year_16_extreme[,1:92]
year_16_extreme[year_16_extreme==0] =NA
year_16_extreme<-year_16_extreme[,-93]

#2017
cols <- grep("^2017", names(result), value = TRUE)
year_17<- result[, which(names(result) %in% cols)]
year_17_extreme<-year_17
year_17_extreme$quantile<- purrr::map_dfr(1:nrow(year_17_extreme),function(x){
  quantile(unlist(as.vector(year_17_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_17_extreme[,1:92] - as.numeric(unlist(year_17_extreme$quantile)))
year_17_extreme[,1:92] <- (indicator > 0)*year_17_extreme[,1:92]
year_17_extreme[year_17_extreme==0] =NA
year_17_extreme<-year_17_extreme[,-93]

#2018
cols <- grep("^2018", names(result), value = TRUE)
year_18 <- result[, which(names(result) %in% cols)]
year_18_extreme<-year_18
year_18_extreme$quantile<- purrr::map_dfr(1:nrow(year_18_extreme),function(x){
  quantile(unlist(as.vector(year_18_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_18_extreme[,1:92] - as.numeric(unlist(year_18_extreme$quantile)))
year_18_extreme[,1:92] <- (indicator > 0)*year_18_extreme[,1:92]
year_18_extreme[year_18_extreme==0] =NA
year_18_extreme<-year_18_extreme[,-93]

#2019
cols <- grep("^2019", names(result), value = TRUE)
year_19<- result[, which(names(result) %in% cols)]
year_19_extreme<-year_19
year_19_extreme$quantile<- purrr::map_dfr(1:nrow(year_19_extreme),function(x){
  quantile(unlist(as.vector(year_19_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_19_extreme[,1:92] - as.numeric(unlist(year_19_extreme$quantile)))
year_19_extreme[,1:92] <- (indicator > 0)*year_19_extreme[,1:92]
year_19_extreme[year_19_extreme==0] =NA
year_19_extreme<-year_19_extreme[,-93]

#2020
cols <- grep("^2020", names(result), value = TRUE)
year_20<- result[, which(names(result) %in% cols)]
year_20_extreme<-year_20
year_20_extreme$quantile<- purrr::map_dfr(1:nrow(year_20_extreme),function(x){
  quantile(unlist(as.vector(year_20_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_20_extreme[,1:92] - as.numeric(unlist(year_20_extreme$quantile)))
year_20_extreme[,1:92] <- (indicator > 0)*year_20_extreme[,1:92]
year_20_extreme[year_20_extreme==0] =NA
year_20_extreme<-year_20_extreme[,-93]

#2021
cols <- grep("^2021", names(result), value = TRUE)
year_21<- result[, which(names(result) %in% cols)]
year_21_extreme<-year_21
year_21_extreme$quantile<- purrr::map_dfr(1:nrow(year_21_extreme),function(x){
  quantile(unlist(as.vector(year_21_extreme[x,1:92])),0.9,na.rm=T)})
indicator <- as.data.frame(year_21_extreme[,1:92] - as.numeric(unlist(year_21_extreme$quantile)))
year_21_extreme[,1:92] <- (indicator > 0)*year_21_extreme[,1:92]
year_21_extreme[year_21_extreme==0] =NA
year_21_extreme<-year_21_extreme[,-93]

#Merge the extreme data for each year
wbgt_extreme<-cbind(year_00_extreme,year_01_extreme,year_02_extreme,year_03_extreme,year_04_extreme,year_05_extreme,
                    year_06_extreme,year_07_extreme,year_08_extreme,year_09_extreme,year_10_extreme,year_11_extreme,
                    year_12_extreme,year_13_extreme,year_14_extreme,year_15_extreme,year_16_extreme,year_17_extreme,
                    year_18_extreme,year_19_extreme,year_20_extreme,year_21_extreme)

### 3. Caluclate Mean and SD ####
year_00_mean<-mean((as.matrix(year_00)),na.rm=TRUE);year_00_sd<-sd((as.matrix(year_00)),na.rm=TRUE);
year_01_mean<-mean((as.matrix(year_01)),na.rm=TRUE);year_01_sd<-sd((as.matrix(year_01)),na.rm=TRUE)
year_02_mean<-mean((as.matrix(year_02)),na.rm=TRUE);year_02_sd<-sd((as.matrix(year_02)),na.rm=TRUE)
year_03_mean<-mean((as.matrix(year_03)),na.rm=TRUE);year_03_sd<-sd((as.matrix(year_03)),na.rm=TRUE)
year_04_mean<-mean((as.matrix(year_04)),na.rm=TRUE);year_04_sd<-sd((as.matrix(year_04)),na.rm=TRUE)
year_05_mean<-mean((as.matrix(year_05)),na.rm=TRUE);year_05_sd<-sd((as.matrix(year_05)),na.rm=TRUE)
year_06_mean<-mean((as.matrix(year_06)),na.rm=TRUE);year_06_sd<-sd((as.matrix(year_06)),na.rm=TRUE)
year_07_mean<-mean((as.matrix(year_07)),na.rm=TRUE);year_07_sd<-sd((as.matrix(year_07)),na.rm=TRUE)
year_08_mean<-mean((as.matrix(year_08)),na.rm=TRUE);year_08_sd<-sd((as.matrix(year_08)),na.rm=TRUE)
year_09_mean<-mean((as.matrix(year_09)),na.rm=TRUE);year_09_sd<-sd((as.matrix(year_09)),na.rm=TRUE)
year_10_mean<-mean((as.matrix(year_10)),na.rm=TRUE);year_10_sd<-sd((as.matrix(year_10)),na.rm=TRUE)
year_11_mean<-mean((as.matrix(year_11)),na.rm=TRUE);year_11_sd<-sd((as.matrix(year_11)),na.rm=TRUE)
year_12_mean<-mean((as.matrix(year_12)),na.rm=TRUE);year_12_sd<-sd((as.matrix(year_12)),na.rm=TRUE)
year_13_mean<-mean((as.matrix(year_13)),na.rm=TRUE);year_13_sd<-sd((as.matrix(year_13)),na.rm=TRUE)
year_14_mean<-mean((as.matrix(year_14)),na.rm=TRUE);year_14_sd<-sd((as.matrix(year_14)),na.rm=TRUE)
year_15_mean<-mean((as.matrix(year_15)),na.rm=TRUE);year_15_sd<-sd((as.matrix(year_15)),na.rm=TRUE)
year_16_mean<-mean((as.matrix(year_16)),na.rm=TRUE);year_16_sd<-sd((as.matrix(year_16)),na.rm=TRUE)
year_17_mean<-mean((as.matrix(year_17)),na.rm=TRUE);year_17_sd<-sd((as.matrix(year_17)),na.rm=TRUE)
year_18_mean<-mean((as.matrix(year_18)),na.rm=TRUE);year_18_sd<-sd((as.matrix(year_18)),na.rm=TRUE)
year_19_mean<-mean((as.matrix(year_19)),na.rm=TRUE);year_19_sd<-sd((as.matrix(year_19)),na.rm=TRUE)
year_20_mean<-mean((as.matrix(year_20)),na.rm=TRUE);year_20_sd<-sd((as.matrix(year_20)),na.rm=TRUE)
year_21_mean<-mean((as.matrix(year_21)),na.rm=TRUE);year_21_sd<-sd((as.matrix(year_21)),na.rm=TRUE)

wbgt_mean<-rbind(year_00_mean,year_01_mean,year_02_mean,year_03_mean,year_04_mean,year_05_mean,
                    year_06_mean,year_07_mean,year_08_mean,year_09_mean,year_10_mean,year_11_mean,
                    year_12_mean,year_13_mean,year_14_mean,year_15_mean,year_16_mean,year_17_mean,
                    year_18_mean,year_19_mean,year_20_mean,year_21_mean)
wbgt_sd<-rbind(year_00_sd,year_01_sd,year_02_sd,year_03_sd,year_04_sd,year_05_sd,
                 year_06_sd,year_07_sd,year_08_sd,year_09_sd,year_10_sd,year_11_sd,
                 year_12_sd,year_13_sd,year_14_sd,year_15_sd,year_16_sd,year_17_sd,
                 year_18_sd,year_19_sd,year_20_sd,year_21_sd)

wbgt_year<-as.data.frame(cbind(2000:2021,wbgt_mean,wbgt_sd))
colnames(wbgt_year)<-c("year","mean","sd")

### 4. WBGT visualization ####
Wbgt_June = res %>%
  select(matches("-06-01|-06-02|-06-03|-06-04|-06-05|-06-06|-06-07|-06-08|-06-09|-06-10|-06-11|-06-12|-06-13|-06-14|-06-15|-06-16|-06-17|-06-18|-06-19|-06-20|-06-21|-06-22|-06-23|-06-24|-06-25|-06-26|-06-27|-06-28|-06-29|-06-30"))
Wbgt_July = res %>%
  select(matches("-07-01|-07-02|-07-03|-07-04|-07-05|-07-06|-07-07|-07-08|-07-09|-07-10|-07-11|-07-12|-07-13|-07-14|-07-15|-07-16|-07-17|-07-18|-07-19|-07-20|-07-21|-07-22|-07-23|-07-24|-07-25|-07-26|-07-27|-07-28|-07-29|-07-30|-07-31"))
Wbgt_Aug = res %>%
  select(matches("-08-01|-08-02|-08-03|-08-04|-08-05|-08-06|-08-07|-08-08|-08-09|-08-10|-08-11|-08-12|-08-13|-08-14|-08-15|-08-16|-08-17|-08-18|-08-19|-08-20|-08-21|-08-22|-08-23|-08-24|-08-25|-08-26|-08-27|-08-28|-08-29|-08-30|-08-31"))


June_mean=as.data.frame((rowMeans(Wbgt_June, na.rm=TRUE)))  #each row is a unique pixel, the value is the average in June 
July_mean=as.data.frame((rowMeans(Wbgt_July, na.rm=TRUE)))
Aug_mean=as.data.frame((rowMeans(Wbgt_Aug, na.rm=TRUE)))

Wbgt_mean = cbind(June_mean,July_mean,Aug_mean)
colnames(Wbgt_mean) <- c("June","July","Aug")
Impervious_wbgt <- cbind(Wbgt_mean,Impervious_data_group[-1,])

June_Wbgt_violin<-
  ggplot(Impervious_wbgt) +
  aes(x = Impervious_level, y = June) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +ggtitle("WBGT Violin Plot")+theme_minimal()

July_Wbgt_violin<-
  ggplot(Impervious_wbgt) +
  aes(x = Impervious_level, y = July) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222")+theme_minimal()

Aug_Wbgt_violin<-
  ggplot(Impervious_wbgt) +
  aes(x = Impervious_level, y = Aug) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222")+theme_minimal()

June_Wbgt_violin/July_Wbgt_violin/Aug_Wbgt_violin


### 5. WBGT extreme visualization ####
#WBGT extreme
Wbgt_extreme_June = wbgt_extreme %>%
  select(matches("-06-01|-06-02|-06-03|-06-04|-06-05|-06-06|-06-07|-06-08|-06-09|-06-10|-06-11|-06-12|-06-13|-06-14|-06-15|-06-16|-06-17|-06-18|-06-19|-06-20|-06-21|-06-22|-06-23|-06-24|-06-25|-06-26|-06-27|-06-28|-06-29|-06-30"))
Wbgt_extreme_July = wbgt_extreme %>%
  select(matches("-07-01|-07-02|-07-03|-07-04|-07-05|-07-06|-07-07|-07-08|-07-09|-07-10|-07-11|-07-12|-07-13|-07-14|-07-15|-07-16|-07-17|-07-18|-07-19|-07-20|-07-21|-07-22|-07-23|-07-24|-07-25|-07-26|-07-27|-07-28|-07-29|-07-30|-07-31"))
Wbgt_extreme_Aug = wbgt_extreme %>%
  select(matches("-08-01|-08-02|-08-03|-08-04|-08-05|-08-06|-08-07|-08-08|-08-09|-08-10|-08-11|-08-12|-08-13|-08-14|-08-15|-08-16|-08-17|-08-18|-08-19|-08-20|-08-21|-08-22|-08-23|-08-24|-08-25|-08-26|-08-27|-08-28|-08-29|-08-30|-08-31"))

June_mean=as.data.frame((rowMeans(Wbgt_extreme_June, na.rm=TRUE)))  #each row is a unique pixel, the value is the average in June 
July_mean=as.data.frame((rowMeans(Wbgt_extreme_July, na.rm=TRUE)))
Aug_mean=as.data.frame((rowMeans(Wbgt_extreme_Aug, na.rm=TRUE)))

Wbgt_extreme_mean = cbind(June_mean,July_mean,Aug_mean)
colnames(Wbgt_extreme_mean) <- c("June","July","Aug")
Impervious_wbgt <- cbind(Wbgt_extreme_mean,Impervious_data_group[-1,])

June_Wbgt_extreme_violin<-
  ggplot(Impervious_wbgt) +
  aes(x = Impervious_level, y = June) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  ggtitle("WBGT extreme Violin Plot")+
  theme_minimal() 

July_Wbgt_extreme_violin<-
  ggplot(Impervious_wbgt) +
  aes(x = Impervious_level, y = July) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()

Aug_Wbgt_extreme_violin<-
  ggplot(Impervious_wbgt) +
  aes(x = Impervious_level, y = Aug) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal() 

June_Wbgt_extreme_violin/July_Wbgt_extreme_violin/Aug_Wbgt_extreme_violin

### 6. Ec visualization ####
data2=data.frame(data1[,2],data1[,4],data1[,5])
colnames(data2) = c("Tnwb","dates","point")
result_tnwb<- data2%>% pivot_wider(names_from = dates, values_from = Tnwb)

#The calculation problem of the wbgt function will cause the first point (X1) to be missed
#We delete  the data of the first point in every file except files related to wbgt

Tnwb<-result_tnwb[,-1]
Tmean_1<-(Tmax_data_clean+Tmin_data_clean)/2
Ec=Tmean_1[-1,]-Tnwb


June_cols<-grep("^X20\\d{2}06", names(Ec), value = TRUE)
July_cols<-grep("^X20\\d{2}07", names(Ec), value = TRUE)
Aug_cols<-grep("^X20\\d{2}08", names(Ec), value = TRUE)

Ec_June<- Ec[, which(names(Ec) %in% June_cols)]
Ec_July<-Ec[, which(names(Ec) %in% July_cols)]
Ec_Aug<-Ec[, which(names(Ec) %in% Aug_cols)]

June_mean=as.data.frame((rowMeans(Ec_June, na.rm=TRUE)))  #each row is a unique pixel, the value is the average in June 
July_mean=as.data.frame((rowMeans(Ec_July, na.rm=TRUE)))
Aug_mean=as.data.frame((rowMeans(Ec_Aug, na.rm=TRUE)))

Ec_mean = cbind(June_mean,July_mean,Aug_mean)
colnames(Ec_mean) <- c("June","July","Aug")
Impervious_Ec <- cbind(Ec_mean,Impervious_data_group[-1,])

June_Ec_violin<-
  ggplot(Impervious_Ec) +
  aes(x = Impervious_level, y = June) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +ggtitle("Ec Violin Plot")+theme_minimal()

July_Ec_violin<-
  ggplot(Impervious_Ec) +
  aes(x = Impervious_level, y = July) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()

Aug_Ec_violin<-
  ggplot(Impervious_Ec) +
  aes(x = Impervious_level, y = Aug) +
  geom_violin(adjust = 1L, scale = "area", fill = "#B22222") +
  theme_minimal()

June_Ec_violin/July_Ec_violin/Aug_Ec_violin


### 7.Export the Data ####
res<-result[,-1]
LatLon_data<-LatLon_data_clean[-1,]

write.csv(res, file = "~/Desktop/Work/city0/city0 result/city0_wbgt_original.csv")
write.csv(wbgt_extreme, file = "~/Desktop/Work/city0/city0 result/city0_wbgt_extreme.csv")
write.csv(LatLon_data, file = "~/Desktop/Work/city0/city0 result/city0_Latlon.csv")
write.csv(wbgt_year, file = "~/Desktop/Work/city0/city0 result/city0_wbgt_mean_SD.csv",row.names = FALSE)
write.csv(Ec,file="~/Desktop/Work/city0/city0 result/city0_Ec.csv",row.names = FALSE)




