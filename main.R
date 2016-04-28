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
parcels = spTransform(parcels, CRS(projection(NDVIraster)))

#plot it
plot(NDVIraster)
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

#plot it
NDVIplot <- function(NDVIpoints,centroidData,datesnr,fieldnr=1){
  plot(x=datesnr,y=NDVIpoints[fieldnr,],ylim=c(0,10000),ylab="NDVI (/10000)",xlab="date")
  title(main=centroidData[fieldnr,]$NAME,sub=centroidData[fieldnr,]$CROP)
  if (!is.na(centroidData[fieldnr,]$HARVEST)) {
    abline(v=strtoi(centroidData[fieldnr,]$HARVEST),col="red")
    text(strtoi(centroidData[fieldnr,]$HARVEST),9500, centroidData[fieldnr,]$HARVEST, col = "red", adj = c(-.1, -.1))
  } 
  if (!is.na(centroidData[fieldnr,]$HARVEST2)) {
    abline(v=strtoi(centroidData[fieldnr,]$HARVEST2),col="blue")
    text(strtoi(centroidData[fieldnr,]$HARVEST2),8500, centroidData[fieldnr,]$HARVEST2, col = "blue", adj = c(-.1, -.1))
  }
}

for (fieldnr in seq(length(centroidData[,1]))){
  if (!is.na(centroidData[fieldnr,]$HARVEST)){
    NDVIplot(NDVIpoints,centroidData,datesnr,fieldnr)
  }
}

