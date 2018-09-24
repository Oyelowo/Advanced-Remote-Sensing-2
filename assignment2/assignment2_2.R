#clear list
rm(list = ls())

.libPaths("C:/Users/oyeda/Desktop/ADV_REM_SENS/library")

#set working directory
setwd("C:/Users/oyeda/Desktop/ADV_REM_SENS/assignment2")

#1) Read TEXAS_lidar.txt to R
lidar <- read.table("TEXAS_lidar.txt", head = FALSE,  sep = "\t")
colnames(lidar) <- c("Year","Pulse","X","Y","Z","h","range","int","agc")
#acquisition year: 6,7,or 10)
#echo type: Fi, Si, In, La
#x, add 2510000!
#y, add 6860000!
#z, N60
#h, normalized h based on 2004 DTM
#range, distance to scanner
#intensity
#agc: automatic gain control, only in 2007 and 2010
#Fi = first, Si= single, In= intermediate, La= Last





#2) Select only first and single echoes of 2010 data
FiSi10 <- subset(lidar, lidar$Year==10 & (lidar$Pulse=="Fi" | lidar$Pulse=="Si"))

#load package
library(raster)
library(rasterVis)
?raster
?seq
#3) Create a raster using the cell size of 0.5 meters

#The below are added to x and y in case when the field data(points) are superimposed/overlain on it,
#they have different projections and this is somewhat converting the projection to make them match.
#x, add 2510000!
#y, add 6860000!
x10<-FiSi10$X + 2510000
y10<-FiSi10$Y + 6860000
h10<-FiSi10$h
xy10<-cbind(x10,y10)

r05_10 <- raster(ncols=length(seq(min(x10), max(x10), 0.5)), nrows=length(seq(min(y10),max(y10), 0.5)),
              xmn=min(x10), xmx=max(x10),ymn=min(y10), ymx=max(y10),crs=NA)

?seq
?rasterize
#Create CHM by adding maximum height for each raster cell
#I used h column because it is the normalised height which can also
#stand for chm/nDSM(normalised digital surface model). This can 
#also be done by: CHM = DSM - DTM
#NA can also be 0
chm05_10 <- rasterize(xy10, r05_10, h10,na.rm=T, fun="max", background=NA) 
plot(chm05_10)

#5) Fill NA-cells with focal-filter (only NA cells!)

?focal
#install.packages("rgdal")
#install.packages("raster")
#install.packages("rasterVis")
#install.packages("sp")
#install.packages("maptools")
#install.packages("rgl")
library(sp)
library(maptools)
library(rgl)
library(rgdal)
library(raster)
library(rasterVis)

#6) Test different filtering parameters
plot(chm05_10, main="CHM 2010 without filter")

par(mfrow=c(2,3))
# 3x3 mean filter
chm05_10foc3<-focal(chm05_10,w=matrix(1/9, nc=3, nr=3), fun=mean, 
                 na.rm=T, NAonly=TRUE)
plot(chm05_10foc3, main="CHM 2010 with 3X3 filter")
plot3D(chm05_10foc3)

chm05_10foc3<-focal(chm05_10,w=matrix(1,3,3), fun=mean, 
                 na.rm=T, NAonly=T)
plot(chm05_10foc3, main="CHM 2010 with 3X3 filter")

# 5x5 mean filter
chm05_10foc5<-focal(chm05_10,w=matrix(1/25,nrow=5,ncol=5), 
                 na.rm=TRUE, NAonly=TRUE)
plot(chm05_10foc5, main="CHM 2010 with 5X5 filter")

#Laplacian filter: filter=matrix(c(0,1,0,1,-4,1,0,1,0), nrow=3)
chm05_10focl<-focal(chm05_10,w=matrix(c(0,1,0,1,-4,1,0,1,0), nrow=3), 
                 na.rm=TRUE, NAonly=TRUE)
plot(chm05_10focl, main="CHM 2010 with Laplacian filter")       
          
#Sobel filter: filter=matrix(c(1,2,1,0,0,0,-1,-2,-1) / 4, nrow=3)
chm05_10focs<-focal(chm05_10,w=matrix(c(1,2,1,0,0,0,-1,-2,-1) / 4, nrow=3), 
                 na.rm=TRUE, NAonly=TRUE)
plot(chm05_10focs, main="CHM 2010 with Sobel filter")

# Gaussian filter                 
gf <- focalWeight(chm05_10, 2, "Gauss")
chm05_10focg<-focal(chm05_10,w=gf, 
                 na.rm=TRUE, NAonly=TRUE)
plot(chm05_10focg, main="CHM 2010 with Gaussian filter")




#Workflow (Change detection):
#  7) Generate another CHM based on 2006 data
#2) Select only first and single echoes of 2010 data
FiSi6 <- subset(lidar, lidar$Year==6 & (lidar$Pulse=="Fi" | lidar$Pulse=="Si"))


x6<-FiSi6$X + 2510000
y6<-FiSi6$Y + 6860000
h6<-FiSi6$h
xy6<-cbind(x6,y6)

r05_6 <- raster(ncols=length(seq(min(x6), max(x6), 0.5)), nrows=length(seq(min(y6),max(y6), 0.5)),
                 xmn=min(x6), xmx=max(x6),ymn=min(y6), ymx=max(y6),crs=NA)


#Create CHM by adding maximum height for each raster cell
#NA can also be 0
chm05_6 <- rasterize(xy6, r05_6, h6,na.rm=T, fun="max", background=NA) 
par(mfrow=c(1,2))
plot(chm05_6, main="0.5 resolution CHM 2006 without filter")

# 3x3 mean filter
chm05_6foc3<-focal(chm05_6,w=matrix(1/9, nc=3, nr=3), fun=mean, 
                    na.rm=T, NAonly=TRUE)
plot(chm05_6foc3, main="0.5 resolution CHM 2006 with 3X3 filter")
plot3D(chm05_6foc3)

#8) Calculate the difference of CHM 2006 and CHM 2010, 
#and plot the changes
dif10_6<-chm05_10foc3 -  chm05_6foc3
plot(dif10_6)


#LOAD tree data:
treeData<-read.csv("Field.csv", header = TRUE, sep = ";")

#9) Classify field data as damaged and undamaged trees
#(Damage_class 0 = No damage; Damage_classes 1, 2 and 3 = Damaged)
damaged_trees<- subset(treeData, treeData$Damage_class!=0)
no_damage<-subset(treeData, treeData$Damage_class==0)

#10) Plot the reclassified field data over the CHM changes
#damaged_trees
points(damaged_trees$X, damaged_trees$Y, col="black")

points(no_damage$X, no_damage$Y, col="red")

#?legend




#legend (x=2515870, y= 6861190, c("damaged tree", "no damage"), 
 #       col= c("black", "red"), lty=c(1,1), lwd = c(2.5,2.5))
#legend ("bottomleft", c("damaged tree", "no damage"), 
 #   col= c("black", "red"), pch=c(1,1))
       




#Questions:
#Find the areas where CHM 2006 is higher than CHM 2010. 
#What might have happened?
#The trees might have been damaged or cutdown

dif6_10<-chm05_6foc3-chm05_10foc3

#?plot

#par(mfcol=c(1,2))
plot(dif10_6, main= "chm2010 - chm2006")
#plot(dif6_10, main="chm2006 - chm2010")
points(damaged_trees$X, damaged_trees$Y, col="red")
points(no_damage$X, no_damage$Y, col="blue")

legend ("bottomleft", c("damaged tree", "no damage"), 
        col= c("red", "blue"), pch=c(1,1))


#plot.new()
#legend("bottomleft", legend = c("damaged trees from Chm"), 
 #      bty = c(1,1), fill = c("green"), cex= 1)
?col


l#egend(2515825, 6861200, legend = c("damaged trees_Chm", "damaged"), 
  #    bty = c(0.1,2), fill = c("white","brown"), cex= 1)

#?legend
plot3D(dif6_10)


#Does the observed damages in the field data correspond with 
#those areas?
#yes, they do, to a very large extent.

#???What is the mean difference of CHMs 2006 and 2010
#(estimate for tree height growth)?


#This can be done directly by:
cellStats(dif10_6, stat = 'mean')
?cellStats

#or by following a longer process of converting to data frame and 
#then getting the summary descriptive stat from it

#below is the alternative method:

chm10_6<-as.data.frame(dif10_6, row.names = NULL,
                       optional = T, xy=TRUE, na.rm=T)
mean(chm10_6$layer)

#???Calculate vertical canopy cover (CHM ??? 1.3 meters)
#estimates for years 2006 and 2010?
#vertical canopy cover for 2006
chm6.df <-as.data.frame(chm05_6foc3, xy=T, na.rm=T)
chm6_13<- subset(chm6.df, chm6.df$layer >= 1.3)
mean(chm6_13$layer)
summary(chm6_13$layer)
plot(chm6_layer)
hist(chm6_layer, main="histogram of chm>=1.3 2006") 


#vertical canpy cover for 2010
chm10.df <-as.data.frame(chm05_10foc3, xy=T, na.rm=T)
summary(chm10.df)
chm10_13<- subset(chm10.df, chm10.df$layer >= 1.3)
summary(chm10_13$layer)
hist(chm10_13$layer, main="histogram of chm>=1.3 2010") 


#compare the length chm >=1.3 and the one without. Use length fxn
length(chm10_13$layer)
length(chm10.df$layer)
summary(chm10.df$layer)
summary(chm10_13$layer)

chm10_13<- subset(chm10.df, chm10.df$layer >= 1.3)
summary(chm10_13$layer)
#?writeRaster

writeRaster(chm05_6foc3, filename = "chm2006.tif", format= "GTiff")
writeRaster(chm05_6foc3, filename = "chm2006..asc", format= "ascii")

writeRaster(chm05_10foc3, filename = "chm2010.tif", format= "GTiff")
