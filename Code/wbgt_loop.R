### 1. Packages ####
#devtools::install_github("anacv/HeatStress")
library(HeatStress)
#install.packages("pacman")
pacman::p_load(readr,purrr,foreach,doParallel,stats)

### 2. Import the datasets ####
wind<-VS
tas<-Tmean
dewp<-Tdmean
radiation<-Srad

### 3. Create the function  ####
degToRad <- function(angleDeg){
  
  degToRad <- pi * angleDeg / 180
  return(degToRad)
}

wbgt.Liljegren <- function(tas, dewp, wind, radiation, dates, lon, lat, tolerance=1e-4, 
                           noNAs=TRUE, swap=FALSE, hour=FALSE){
  
  ##################################################
  ##################################################
  # Assumptions
  propDirect <- 0.8  # Assume a proportion of direct radiation = direct/(diffuse + direct)
  Pair <- 1010  # Atmospheric pressure in hPa
  MinWindSpeed <- 0.1   # 0 wind speed upsets log function
  
  ##################################################
  ##################################################
  # Assertion statements
  assertthat::assert_that(is.logical(hour), msg="'hour' should be logical")
  assertthat::assert_that(is.logical(noNAs), msg="'noNAs' should be logical")
  assertthat::assert_that(is.logical(swap), msg="'swap' should be logical")
  assertthat::assert_that(length(tas)==length(dewp) & length(dewp)==length(wind)
                          & length(wind)==length(radiation), 
                          msg="Input vectors do not have the same length")
  assertthat::assert_that(is.numeric(Pair), msg="'Pair' is not an integer")
  assertthat::assert_that(is.numeric(MinWindSpeed), msg="'min.speed' is not an integer")
  assertthat::assert_that(propDirect < 1, msg="'propDirect' should be [0,1]")  
  #assertthat::assert_that(is.numeric(lon), msg="'lon' is not an integer")
  #assertthat::assert_that(is.numeric(lat), msg="'lat' is not an integer")
  assertthat::assert_that(lon <= 180 & lon >=-180, msg="Invalid lon")
  assertthat::assert_that(lat <= 90 & lat >=-90, msg="Invalid lat")
  
  ######################
  ######################
  ndates <- length(tas)
  Tnwb <- rep(NA, ndates)
  Tg <- rep(NA, ndates)
  
  # Do not allow negative wind and radiation
  radiation[radiation<0] <- 0
  wind[wind<0] <- 0
  
  # Filter data to calculate the WBGT with optimization function  
  xmask <- !is.na(tas + dewp + wind + radiation)  
  
  if (noNAs & swap){
    tastmp <- pmax(tas, dewp)
    dewp <- pmin(tas, dewp)
    tas <- tastmp
  } else if(noNAs & !swap){
    noway <- (dewp - tas) > tolerance
    dewp[which(noway)] <- tas[which(noway)]
  } else if(!noNAs){
    xmask <- xmask & tas >= dewp
  }
  
  # Calculate relative humidity from air temperature and dew point temperature
  relh <- dewp2hurs(tas,dewp) # input in degC, output in %
  
  
  # **************************************
  # *** Calculation of the Tg and Tnwb ***
  # **************************************
  for (i in which(xmask)){
    # Calculate zenith angle (radians are needed)
    zenithDeg <- calZenith(dates[i], lon, lat, hour)
    ZenithAngle <- degToRad(zenithDeg)
    
    # Calculate globe temperature
    Tg[i] <- fkg(tas[i], relh[i], Pair, wind[i], MinWindSpeed, radiation[i], 
                 propDirect, ZenithAngle, tolerance=tolerance)
    
    # Calculate natural wet bulb temperature
    Tnwb[i] <- fTnwb(tas[i], dewp[i], relh[i], Pair, wind[i], MinWindSpeed, 
                     radiation[i], propDirect, ZenithAngle, tolerance=tolerance)
    rm(zenithDeg, ZenithAngle)
  }
  
  # *******************************
  # *** Calculation of the WBGT ***
  # *******************************
  wbgt <- list(data = 0.7 * Tnwb + 0.2 * Tg + 0.1 * tas, 
               Tnwb = Tnwb,
               Tg = Tg)
  
  
  return(wbgt)
}

fkg <- function (tas, relh, Pair, wind, min.speed, radiation, propDirect, 
          zenith, SurfAlbedo = 0.4, tolerance = 1e-04) 
{
  stefanb <- 5.6696e-08
  cp <- 1003.5
  m.air <- 28.97
  m.h2o <- 18.015
  r.gas <- 8314.34
  r.air <- r.gas/m.air
  ratio <- cp * m.air/m.h2o
  Pr <- cp/(cp + (1.25 * r.air))
  emis.globe <- 0.95
  alb.globe <- 0.05
  diam.globe <- 0.05
  emis.sfc <- 0.999
  alb.sfc <- SurfAlbedo
  if (zenith <= 0) 
    zenith <- 1e-10
  if (radiation > 0 & zenith > 1.57) 
    zenith <- 1.57
  if (radiation > 15 & zenith > 1.54) 
    zenith <- 1.54
  if (radiation > 900 & zenith > 1.52) 
    zenith <- 1.52
  if (radiation < 10 & zenith == 1.57) 
    radiation <- 0
  Tair <- tas + 273.15
  RH <- relh * 0.01
  cza <- cos(zenith)
  Tsfc <- Tair
  fr <- function(Tglobe_prev, Tair, Pair) {
    Tref <- 0.5 * (Tglobe_prev + Tair)
    h <- HeatStress:::h_sphere_in_air(Tref, Pair, wind, min.speed, diam.globe)
    Tglobe <- (0.5 * (HeatStress:::emis_atm(Tair, RH) * Tair^4 + emis.sfc * 
                        Tsfc^4) - h/(emis.globe * stefanb) * (Tglobe_prev - 
                                                                Tair) + radiation/(2 * emis.globe * stefanb) * (1 - 
                                                                                                                  alb.globe) * (propDirect * (1/(2 * cza) - 1) + 1 + 
                                                                                                                                  alb.sfc))^0.25
    abs(Tglobe - Tglobe_prev)
  }
  opt <- stats::optimize(fr, range(Tair - 2, Tair + 10), Tair, 
                         Pair, tol = tolerance)
  Tg <- opt$minimum - 273.15
  return(Tg)
}

WBGT <- function(n){
  cat(n)
  data = data.frame(
    tas=tas[,n], dewp=dewp[,n], 
    wind=wind[,n], radiation=radiation[,n],Dates=wind[,1]
  )
  colnames(data) <- c("tas","dewp","wind","radiation","Dates")
  
  wbgt.outdoors <- wbgt.Liljegren(tas=data$tas, dewp=data$dewp, 
                                  wind=data$wind, radiation=data$radiation, dates= data$Dates, lon=LatLon[2,n], lat=LatLon[1,n])
  wbgt.outdoors = data.frame(wbgt.outdoors,dates= data$Dates,point = colnames(tas)[n])
  return(wbgt.outdoors) 
}

# 4. Call the function ####

#detectCores(logical=TRUE) # Check the number of cores of the computer
cl = makeCluster(8) #Set how many cores to run
registerDoParallel(cl) #Set the maximum cores to optimize running time       
output <-  foreach(
  k= 3 : ncol(wind),
  .combine=rbind,  
  .packages = c("HeatStress","stats")
) %dopar% WBGT(k)
stopCluster(cl)

# 5. Export the Data ####
write.csv(output,file="~/Desktop/Work/city0/output.csv",row.names = FALSE,fileEncoding = "UTF-8")
