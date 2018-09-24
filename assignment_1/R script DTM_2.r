##############################
#Exercise 2, Mikko Vastaranta#
##############################

rm(list=ls())
.libPaths("Z:/GIS202-2013/Library")
setwd("Z:/GIS202-2013")

###########
#Read data#
###########
Lidar <- read.table(file="TEXAS_lidar.txt", head=FALSE, sep="\t") #ok
colnames(Lidar) <- c("Year","Pulse","X","Y","Z","h","range","int","agc")

#acquisition year: 6,7,or 10)
#echo type: Fi, Si, In, La
#x, add 2510000!
#y, add 6860000!
#z, N60
#h, normalized h based on 2004 DTM
#range, distance to scanner
#intensity
#agc: automatic gain control, only in 2007 and 2010


summary(Lidar)

#############################
#Select year#################
#############################

data<-subset(Lidar, Lidar$Year==6)
summary(data)



##################################
#plot different kind of echos#
################################## 
first<-subset(data, data$Pulse=="Fi")
plot(first$X, first$Y)

last<-subset(data, data$Pulse=="La")
points(last$X, last$Y, col="red")

si<-subset(data, data$Pulse=="Si")
plot(si$X, si$Y, col="green")

##########################
#Select some smaller area#
##########################

library(sp)
library(maptools)
library(rgl)

#create a grid
grid = GridTopology(c(max(data$X-20),min(data$Y+20)), c(5,5), c(1,1))
poly<-as.SpatialPolygons.GridTopology(grid, proj4string = CRS(as.character(NA)))

#plot(poly)

coordinates(data) = c("X", "Y")

#overlay 

data$In<-overlay(data,poly) 
grid_points<-subset(data, data$In!="NA") 


summary(grid_points)



plot(grid_points$X, grid_points$Y)

plot3d(grid_points$X, grid_points$Y, grid_points$h, box=T, axes=F)


first<-subset(grid_points, grid_points$Pulse=="Fi")
last<-subset(grid_points, grid_points$Pulse=="La")

plot3d(first$X, first$Y,first$h, col="red", box=F, axes=F)
points3d(last$X, last$Y,last$h, col="green")


plot(first$X, first$Y)

last<-subset(Lidar, Lidar$Pulse=="La")
points(last$X, last$Y, col="red")

last<-subset(Lidar, Lidar$Pulse=="La")
points(last$X, last$Y, col="red")

si<-subset(Lidar, Lidar$Pulse=="Si")
points(si$X, si$Y, col="green")


#Height distributions

height<-quantile(Lidar$h,probs = seq(0.1, 0.95, 0.05),)
plot(height, type="l",ylim=c(0,20))

##################################################################################
#Create DTM based on 2010 data. ##################################################
##################################################################################
#Creation of DTM

library(raster)
library(rasterVis)

l10<-subset(Lidar, Lidar$Year=="10")
l10<-subset(l10, l10$Pulse!="Fi")
l10<-subset(l10, l10$Pulse!="In")

z <- l10$Z
x <- l10$X
y <- l10$Y


xy <- cbind(x, y)

#Start with coarse raster with 8m cells

r8 <- raster(ncols=length(seq(min(x), max(x), 8)), nrows=length(seq(min(y),max(y), 8)),
	xmn=min(x), xmx=max(x),ymn=min(y), ymx=max(y),crs=NA)

dtm8 <- rasterize(xy, r8, z,na.rm=T, fun="min", background=NA)

#plot(dtm8)
#plot3D(dtm8)


#######################################################################################
r4 <- raster(ncols=length(seq(min(x), max(x), 4)), nrows=length(seq(min(y),max(y), 4)),
	xmn=min(x), xmx=max(x),ymn=min(y), ymx=max(y),crs=NA)

dtm4 <- rasterize(xy, r4, z,na.rm=T, fun="min", background=185)

#plot3D(dtm4)

dtm48<-resample(dtm8, dtm4, method="bilinear")
dif<-dtm4-dtm48

#cellStats(dif, stat="max")
#plot3D(dif)


for (i in 1:ncell(dtm4)){
 if (dif[i]>6)
	{dtm4[i]<-dtm48[i]}

}


plot3D(dtm4)

###########################################################################################
r2 <- raster(ncols=length(seq(min(x), max(x), 2)), nrows=length(seq(min(y),max(y), 2)),
	xmn=min(x), xmx=max(x),ymn=min(y), ymx=max(y),crs=NA)

dtm2 <- rasterize(xy, r2, z,na.rm=T, fun="min", background=181)


dtm24<-resample(dtm4, dtm2, method="bilinear")
dif<-dtm2-dtm24


for (i in 1:ncell(dtm2)){
 if (dif[i]>3)
	{dtm2[i]<-dtm24[i]}

}

plot3D(dtm2)
##################################################################################################
r1 <- raster(ncols=length(seq(min(x), max(x), 1)), nrows=length(seq(min(y),max(y), 1)),
	xmn=min(x), xmx=max(x),ymn=min(y), ymx=max(y),crs=NA)

dtm1 <- rasterize(xy, r1, z,na.rm=T, fun="min", background=1000)


dtm12<-resample(dtm2, dtm1, method="bilinear")
dif<-dtm1-dtm12


for (i in 1:ncell(dtm1)){
 if (dif[i]>1.5)
	{dtm1[i]<-dtm12[i]}

}


plot3D(dtm1)
##################################################################################################

r05 <- raster(ncols=length(seq(min(x), max(x), 0.5)), nrows=length(seq(min(y),max(y), 0.5)),
	xmn=min(x), xmx=max(x),ymn=min(y), ymx=max(y),crs=NA)

dtm05 <- rasterize(xy, r05, z,na.rm=T, fun="min", background=1000)


dtm051<-resample(dtm1, dtm05, method="bilinear")
dif<-dtm05-dtm051


for (i in 1:ncell(dtm05)){
 if (dif[i]>0.8)
	{dtm05[i]<-dtm051[i]}

}

plot3D(dtm05)

plot(dtm05)


#######################################
#Calculate the differences#############
#######################################


summary(dtm05)
summary(dtm8)

test<-resample(dtm8, dtm05, method="bilinear")

summary(test)
plot3D(test)


difference<-dtm05-test
summary(difference)


##################################################################################################
writeRaster(dtm1, filename="C:/HY-temp/GIS202Mikko/dtm1.tif",overwrite=T)



################################################################################################
#dtm_filtered <- focal(dtm05, fun=mean, w=c(3,3))
#plot3D(dtm_filtered)











