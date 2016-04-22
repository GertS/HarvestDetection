## main.R
## author: Gert Sterenborg 
## email: gertsterenborg@gmail.com
## 22 April 2016



# Dependencies and global parameters --------------------------------------

rm(list = ls())
library(rgdal)
library(rgeos)
library(raster)


# Load data ---------------------------------------------------------------
parcels = readOGR(dsn = "./Data/parcels2015.geojson", layer = "OGRGeoJSON")
test = raster("./Data/ndvi2015.dat")
plot(test)
plot(parcels,add=T)

# Find Centroids ----------------------------------------------------------
centroids <- gCentroid(parcels,byid=TRUE)
points(centroids,pch=3)


