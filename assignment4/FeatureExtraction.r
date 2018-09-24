########################################
# Feature extraction for tree segments #
########################################

rm(list=ls())
setwd("C:/Users/oyeda/Desktop/ADV_REM_SENS/assignment4")
.libPaths("C:/Users/oyeda/Desktop/ADV_REM_SENS/library")


##################################################################################

# Read 3D point data (TEXAS_lidar.txt)

Lidar <- read.table(file="TEXAS_lidar.txt", head=FALSE, sep="\t") 
colnames(Lidar) <- c("Year","Pulse","X","Y","Z","h","range","int","agc")

summary(Lidar)
library(sp)
library(maptools)
library(rgdal)

# Specify column names of the coordinates 
coordinates(Lidar) = c("X", "Y")


##################################################################################

# Read the shapefile (TrainingTrees.shp)
#install.packages("sp")
#install.packages("maptools")


S <- readShapePoly("TrainingTrees.shp") #Tree segments
SP <- as(S, "SpatialPolygons") 
#?readShapePoly

# Plot tree segments

plot(SP)



##################################################################################

# Analyse, which Lidar points are located inside the segments

Lidar$In <- over(Lidar,SP)


# Select the 2010 Lidar data, and remove the points located outside of the segments

Lidar10 <- subset(Lidar, Lidar$Year==10 & Lidar$In!="NA")


tail(Lidar10)
summary (Lidar10)

#######################################################################################


#####################################################################
### TEE ALLA OLEVAAN KOODIIN TARVITTAVAT LISÄYKSET JA MUOKKAUKSET ###
#####################################################################
#5) Create a code which calculates the following Lidar features for individual tree
#segments. Use only first and single echoes while calculating the features:
#  . Tree position
#o X-coordinate of maximum height observation (X)
#o Y-coordinate of maximum height observation (Y)
#. Maximum height of the observations (Hmax)
#. Mean height of the observations (Hmean)
#. Standard deviation of the height observations (Hstd)
#. Coefficient of variation (CV)


ID <- 999; X <- 999; Y <- 999; Hmax <- 999; Hmean <- 999; Hstd <- 999; CV <- 999; VD <- 999;
h10 <- 999; h20 <- 999; h30 <- 999; h40 <- 999; h50 <- 999; h60 <- 999; h70 <- 999; h80 <- 999; h90 <- 999;

Lidar10 <- data.frame(Lidar10)
from <- min(Lidar10$In)
to<-max(Lidar10$In)

Lidar10<- data.frame(Lidar10)

for (i in (from:to)){
  if(match(i,Lidar10$In, nomatch = 999999)<999999){
    x<- subset(Lidar10, Lidar10$In==i)
    ID[i]<- i
    X[i] <- lfisi[which.max(lfisi[,"h"]),"X"]
    Y[i] <- lfisi[which.max(lfisi[,"h"]),"Y"]
  }
  else{ID[i]<-9999999999}
  
}
results <- cbind(ID,X,Y)

#use first or single only which are have height more than 2m but and less than 50m
#these are the defined thresholds for the trees. Because the trees are assumed to be not
#not more than 50m, and anything more than is taken as error. also, below 2m is assumed
#not to be a tree.

lfisi2 <- subset(Lidar10, (Lidar10$Pulse=="Fi" | Lidar10$Pulse=="Si") 
                 & Lidar10$h>2 &Lidar10$h<50)

#o X-coordinate of maximum height observation (X)
#o Y-coordinate of maximum height observation (Y)
#. Maximum height of the observations (Hmax)
#. Mean height of the observations (Hmean)
#. Standard deviation of the height observations (Hstd)
#. Coefficient of variation (CV)
#. Vegetation density, i.e. percentage of observations above 2 meters (VD)
#. Quantiles of the height observations (h10-h90)


for (i in sort(unique(lfisi2$In))) {
  
  lfisi <- subset(Lidar10, (Lidar10$Pulse=="Fi" | Lidar10$Pulse=="Si") 
                  & Lidar10$h>2 &Lidar10$h<50)
  lfisi<-subset(lfisi, lfisi$In == i)
  ID[i]<- i
  # And remember also to subset only first and single echoes!	
  X[i] <- lfisi[which.max(lfisi[,"h"]),"X"] #Returns "X" from the point that has the maximum "h" value
  Y[i] <- lfisi[which.max(lfisi[,"h"]),"Y"]   #Returns "Y" from the point that has the maximum "h" value
  Hmax[i] <- lfisi[which.max(lfisi[,"h"]),"h"]
  Hmean[i]<-mean(lfisi$h)
  Hstd[i]<- sd(lfisi$h)
  #CV[i]<- (Hstd[i]/ Hmean[i])
  CV[i]<-sd(lfisi$h)/mean(lfisi$h)
  
 
  VD[i]<- (nrow(lfisi)*100/nrow(Lidar10))
  
  #VD2[i]<- (length(lfisi)/length(l2))
  #nrow(lfisi)
  output <- cbind(ID,X,Y, Hmax, Hmean, Hstd, CV,VD)
}



resultz <- cbind(ID,X,Y)#,Hmax
 results <- cbind(X,Y) #,Hmax
View(lfisi)

X[i] <- lfisi[which.max(lfisi[,"h"]),"X"]









