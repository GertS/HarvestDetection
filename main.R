## main.R
## author: Gert Sterenborg 
## email: gertsterenborg@gmail.com
## 22 April 2016



# Dependencies and global parameters --------------------------------------

rm(list = ls())
library(caTools)
library(rgdal)
library(rgeos)
library(raster)


# Load data ---------------------------------------------------------------
#raster
filename = "./Data/ndvi2015.dat"
dates=c("07-Jan-2015","17-Jan-2015","16-Feb-2015","08-Mar-2015","11-Mar-2015","12-Mar-2015","24-Mar-2015","10-Apr-2015","20-Apr-2015","24-Mei-2015","05-Jun-2015","01-Jul-2015","02-Jul-2015","21-Jul-2015","03-Aug-2015","07-Aug-2015","23-Aug-2015","09-Sep-2015","27-Sep-2015","02-Oct-2015","11-Oct-2015","25-Oct-2015","04-Dec-2015")
datesnr=c(20150107,20150117,20150216,20150308,20150311,20150312,20150324,20150410,20150420,20150424,20150605,20150701,20150702,20150721,20150803,20150807,20150823,20150909,20150927,20151002,20151011,20151025,20151204)
NDVIraster <- stack(filename)
#NDVIraster = raster(filename)

#polygons
parcels = readOGR(dsn = "./Data/parcels2015.geojson", layer = "OGRGeoJSON")
#parcels = spTransform(parcels, CRS(projection(NDVIraster)))
parcels = spTransform(parcels, CRS("+init=epsg:28992"))

#plot it
#plot(NDVIraster)
plot(NDVIraster$"X07.Jan.2015")
plot(parcels,add=T)

# Find Centroids ----------------------------------------------------------
centroids <- gCentroid(parcels,byid = T)
points(centroids,pch=3)


# extract centroid NDVI ---------------------------------------------------
NDVIpoints = extract(NDVIraster,centroids)
NDVIpoints[NDVIpoints == 0 ] <- NA
centroidData <- data.frame(coordinates(centroids),parcels@data$NAME,parcels@data$CROP,parcels@data$Oogst,parcels@data$Oogst2)
names(centroidData) <- c("x","y","NAME","CROP","HARVEST","HARVEST2")

#correcting data:
#centroidData[36,3] <- 20150925

#plot it
NDVIplot <- function(NDVIpoints,centroidData,datesnr,fieldnr=1){
  x=as.Date(as.character(datesnr),"%Y%m%d")
  plot(x=x,y=NDVIpoints[fieldnr,],ylim=c(0,10000),ylab="NDVI (/10000)",xlab="date")
  title(main=centroidData[fieldnr,]$NAME,sub=centroidData[fieldnr,]$CROP)
  if (!is.na(centroidData[fieldnr,]$HARVEST)) {
    abline(v=as.Date(as.character(centroidData[fieldnr,]$HARVEST),"%Y%m%d"),col="red")
    text(strtoi(centroidData[fieldnr,]$HARVEST),9500, as.Date(as.character(centroidData[fieldnr,]$HARVEST),"%Y%m%d"), col = "red", adj = c(-.1, -.1))
  } 
  if (!is.na(centroidData[fieldnr,]$HARVEST2)) {
    abline(v=as.Date(as.character(centroidData[fieldnr,]$HARVEST2),"%Y%m%d"),col="blue")
    text(strtoi(centroidData[fieldnr,]$HARVEST2),8500, as.Date(as.character(centroidData[fieldnr,]$HARVEST2),"%Y%m%d"), col = "blue", adj = c(-.1, -.1))
  }
}

for (fieldnr in seq(length(centroidData[,1]))){
  if (!is.na(centroidData[fieldnr,]$HARVEST)){
    NDVIplot(NDVIpoints,centroidData,datesnr,fieldnr)
  }
}


# Maximum threshold calculation -------------------------------------------

minmax_aardappel = 10000
minmax_suikerbiet= 10000
minmax_graan     = 10000
for (fieldnr in seq(length(centroidData[,1]))){
  if (!is.na(centroidData[fieldnr,]$HARVEST)){
    croptype= centroidData[fieldnr,]$CROP
    if (croptype == "Aardappel"){
      curmax = max(NDVIpoints[fieldnr,],na.rm=T)
      minmax_aardappel = min(curmax,minmax_aardappel)
    }
    if (croptype == "Suikerbiet"){
      curmax = max(NDVIpoints[fieldnr,],na.rm=T)
      minmax_suikerbiet = min(curmax,minmax_suikerbiet)
    }
    if (croptype == "Wintertarwe" | croptype == "Zomergerst" | croptype == "Japanse haver" | croptype == "Winterrogge" ){
      curmax = max(NDVIpoints[fieldnr,],na.rm=T)
      minmax_graan = min(curmax,minmax_graan)
    }
  }
}
print(paste("Aardappel :",minmax_aardappel)) #8245
print(paste("Suikerbie :",minmax_suikerbiet))#7872
print(paste("Granen    :",minmax_graan))     #6326


# Thresholds calculation --------------------------------------------------

periodValidation <- function(centroidData,id,predperiodMin,predperiodMax){
  hdate = levels(droplevels(centroidData[id,]$HARVEST))
  periodmin1 = as.integer(gsub("-","",as.character(as.Date(as.character(predperiodMin),"%Y%m%d")-5)))
  periodmax1 = as.integer(gsub("-","",as.character(as.Date(as.character(predperiodMax),"%Y%m%d")+5)))
  return (all(periodmin1 <= hdate,periodmax1 >= hdate))
}
periodValidation(centroidData,3,20150820,20150930)
periodValidation(centroidData,3,20150610,20150710)

returnPeriod <- function(NDVIpoints,datesnr,id,beforeValue,afterValue){
  searchBeforeValue = TRUE
  for (i in seq(from=1,to=length(NDVIpoints[id,]))){
    if (!is.na(NDVIpoints[id,i])){
      if (searchBeforeValue){
        if (NDVIpoints[id,i] >= beforeValue){
          searchBeforeValue = FALSE
          beforeDay = datesnr[i]
        }
      }else{
       if (NDVIpoints[id,i] <= afterValue){
         #print(paste(id,i,afterValue))
         afterDay = datesnr[i]
         return (c(beforeDay,afterDay))
       }else{
         beforeDay = datesnr[i]
       }
      }
    }
  }
  return (c(NA,NA))
}

returnPeriod(NDVIpoints,datesnr,3,8000,5000)
returnPeriod(NDVIpoints,datesnr,3,8000,1000)

before_harvest_threshold_aardappel = 8000
after_harvest_threshold_aardappel = 0

n = length(seq(before_harvest_threshold_aardappel,0,-10))
threshold <- numeric(n)
truePositive <- numeric(n)
falsePositive <- numeric(n)

row_id = 0
for (i in seq(before_harvest_threshold_aardappel,0,-10)){
  row_id = row_id + 1
  threshold[row_id] <- i
  truePositive[row_id] <- 0
  falsePositive[row_id] <- 0
  for (fieldnr in seq(length(centroidData[,1]))){
    if (!is.na(centroidData[fieldnr,]$HARVEST)){
      croptype= centroidData[fieldnr,]$CROP
      if (croptype == "Aardappel"){
        period = returnPeriod(NDVIpoints,datesnr,fieldnr,before_harvest_threshold_aardappel,i)
        validity = periodValidation(centroidData,fieldnr,period[1],period[2])
        if (!is.na(validity) & validity){
          truePositive[row_id] <- truePositive[row_id] + 1
        }else{
          falsePositive[row_id] <- falsePositive[row_id] +1
        }
      }
    }
  }
}
aardappel_df <- data.frame(threshold,truePositive,falsePositive)
plot(x = aardappel_df$threshold,y=aardappel_df$falsePositive)
plot(x = aardappel_df$threshold,y=aardappel_df$truePositive)

findThreshold <- function(crop_df){
  maxCount = max(crop_df$truePositive)
  searching = TRUE
  bestRange = 0
  bestValue = 0
  for (i in seq(length(crop_df[,1]))){
    if (searching){
      if (crop_df[i,]$truePositive == maxCount){
        firstValue = crop_df[i,]$threshold
        searching = FALSE
      }
    }else{
      if (crop_df[i,]$truePositive < maxCount){
        currentRange = firstValue - crop_df[i,]$threshold
        if (currentRange > bestRange){
          bestRange = currentRange
          bestValue = (firstValue - crop_df[i,]$threshold) / 2 + crop_df[i,]$threshold
        }
        searching = TRUE
      }
    }
  }
  return (bestValue)
}

findThreshold(aardappel_df)

# find false positives ----------------------------------------------------

#sugarbeet: 7500 - 5165
#potatoe:   8000 - 3280
#wheats:    6326 - 2956 (3506 @ 5 day speling)

for (fieldnr in seq(length(centroidData[,1]))){
  if (!is.na(centroidData[fieldnr,]$HARVEST)){
    croptype= centroidData[fieldnr,]$CROP
    if (croptype == "Aardappel"){
      period = returnPeriod(NDVIpoints,datesnr,fieldnr,6326,3280)
      validity = periodValidation(centroidData,fieldnr,period[1],period[2])
      if(is.na(validity)){
        validity = FALSE
      }
      if (validity){
        print(paste("Valid: ",period[1],period[2],centroidData[fieldnr,]$NAME,fieldnr))
        NDVIplot(NDVIpoints,centroidData,datesnr,fieldnr)
      }else{
        print(paste("NOT VALID: ",period[1],period[2], centroidData[fieldnr,]$NAME,fieldnr))
        NDVIplot(NDVIpoints,centroidData,datesnr,fieldnr)
      }
    }
  }
}
