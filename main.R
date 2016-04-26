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
filename = "./Data/ndvi2015.dat"
parcels = readOGR(dsn = "./Data/parcels2015.geojson", layer = "OGRGeoJSON")
#NDVIraster = read.ENVI(filename, headerfile=paste(filename, ".hdr", sep=""))
#NDVIraster = readGDAL(filename)
dates=c("07-Jan-2015","17-Jan-2015","16-Feb-2015","08-Mar-2015","11-Mar-2015","12-Mar-2015","24-Mar-2015","10-Apr-2015","20-Apr-2015","24-Mei-2015","05-Jun-2015","01-Jul-2015","02-Jul-2015","21-Jul-2015","03-Aug-2015","07-Aug-2015","23-Aug-2015","09-Sep-2015","27-Sep-2015","02-Oct-2015","11-Oct-2015","25-Oct-2015","04-Dec-2015")
test = raster(filename)
#names(test) = dates
plot(test)
plot(parcels,add=T)

# Find Centroids ----------------------------------------------------------
centroids <- gCentroid(parcels,byid=TRUE)
points(centroids,pch=3)


# extract centroid NDVI ---------------------------------------------------


