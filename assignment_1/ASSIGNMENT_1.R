##############################
#Exercise 2, Mikko Vastaranta#
##############################

rm(list=ls())
.libPaths("C:/Users/oyeda/Desktop/ADV_REM_SENS/library")
setwd("C:/Users/oyeda/Desktop/ADV_REM_SENS/assignment_1")

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
#Fi = first, Si= single, In= intermediate, La= Last

#Just testing the below:
#head(Lidar)
#head(Lidar, 10)
#tail(Lidar, 3)

#############################
#Select year#################
#############################

data6<-subset(Lidar, Lidar$Year==6)
summary(data6)


#derive information about idar for 2007 and 2010 too
data7<-subset(Lidar, Lidar$Year==7)
summary(data7)

data10<-subset(Lidar, Lidar$Year==10)
summary(data10)

xrange<- (max(data6$X)-min(data6$X))
yrange<- (max(data6$Y)-min(data6$Y))

area_pd= xrange * yrange  #area of the plot fot calculating pulse density
area_pd

#to get the total pulse from first ad single. I used the 2 steps 
#below. afterwards, i tried to execute the two steps in a line
#and it worked out.
fiSi6<- subset(data6, data6$Pulse=="Fi" | data6$Pulse=="Si")
fiSi6<-length(fiSi6$Pulse)

#The firect method
fiSi6_direct<- length((subset(data6, data6$Pulse=="Fi" | 
                         data6$Pulse=="Si")) $ Pulse)
fiSi6
fiSi6_direct

#you can even do it more directly, and reduce lines of code,
#by subsetting directly from the original lidar,
#without doing different subsets for different years.
fisi6_2<- length(subset(Lidar,Lidar$Year==6 &(Lidar$Pulse=="Fi" 
        | Lidar$Pulse=="Si"))$Pulse)
fisi6_2
#fiAndSiPulse6 <- 18935 + 38995 #this is the manual way.

density6 <- fiSi6/area_pd
density6


fiSi7<-length((subset(data7, data7$Pulse=="Fi" | 
                        data7$Pulse=="Si"))$Pulse)
#fiAndSiPulse7 <- 27030 + 64272

density7 <- fiSi7/area_pd
density7


fiSi10<-length((subset(data10, data10$Pulse=="Fi" | 
                        data10$Pulse=="Si"))$Pulse)
#fiAndSiPulse10 <- 67577 + 164395
density10 <- fiSi10/area_pd
density10

#calculate for average flying altitude
#for 2006, I used range and averge of last returns only
altitude6<-subset(Lidar, Lidar$Year==6 & Lidar$Pulse=="La")
mean(altitude6$range)


#average flying altitude for 2007
altitude7<-subset(Lidar, Lidar$Year==7 & Lidar$Pulse=="La")
mean(altitude7$range)

#average flying altitude for 2010
altitude10<-subset(Lidar, Lidar$Year==10 & Lidar$Pulse=="La")
mean(altitude10$range)



#shortcut for checking help : ?subset


##################################
#plot different kind of echos#
################################## 
first6<-subset(data6, data6$Pulse=="Fi")
summary(first6)
plot(first6$X, first6$Y)

last6<-subset(data6, data6$Pulse=="La")
points(last6$X, last6$Y, col="red")

si6<-subset(data6, data6$Pulse=="Si")
plot(si6$X, si6$Y, col="green")

##########################
#Select some smaller area#
##########################
#install.packages("sp")
#install.packages("maptools")
#install.packages("rgl")
library(sp)
library(maptools)
library(rgl)

#create a grid
?GridTopology
grid = GridTopology(c(max(data6$X-20),min(data6$Y+20)), c(5,5), c(1,1))
poly<-as.SpatialPolygons.GridTopology(grid, proj4string = CRS(as.character(NA)))

#plot(poly)

coordinates(data6) = c("X", "Y")

#overlay 
#I changed the overlay to over as I use the new software which where the function was changed
data6$In<-over(data6,poly) 
grid_points6<-subset(data6, data6$In!="NA") 


summary(grid_points6)



plot(grid_points6$X, grid_points6$Y)

plot3d(grid_points6$X, grid_points6$Y, grid_points6$h, box=T, axes=F)


first6<-subset(grid_points6, grid_points6$Pulse=="Fi")
last6<-subset(grid_points6, grid_points6$Pulse=="La")

plot3d(first6$X, first6$Y,first6$h, col="red", box=F, axes=F)
points3d(last6$X, last6$Y,last6$h, col="green")


plot(first6$X, first6$Y)

last6<-subset(Lidar, Lidar$Pulse=="La")
points(last6$X, last6$Y, col="red")

#last6<-subset(Lidar, Lidar$Pulse=="La")
#points(last6$X, last6$Y, col="red")

si6<-subset(Lidar, Lidar$Pulse=="Si")
points(si6$X, si6$Y, col="green")


#Height distributions
#height<-quantile(Lidar6$h,probs = seq(0.1, 0.95, 0.05),)
heightFirst6<-quantile(first6$h,probs = seq(0.1, 0.95, 0.05))
plot(heightFirst6, type="l",ylim=c(0,20))


heightLast6<-quantile(last6$h,probs = seq(0.1, 0.95, 0.05))
plot(heightLast6, type="l",ylim=c(0,20))



#height<-quantile(last6$h,probs = seq(0.1, 0.95, 0.05))
#plot(height, type="l",ylim=c(0,20))

##################################################################################
#Create DTM based on 2010 data. ##################################################
##################################################################################
#Creation of DTM
#install.packages("raster")
#install.packages("rasterVis")
library(raster)
library(rasterVis)


#lidar 10
#l10<- subset(Lidar, Lidar$Year=="10")
#l10 <- subset(l10, l10$Pulse=="La" | l10$Pulse=="Si")
#plot(l10)

#plot3d(l10)
#head(l10)

l6<-subset(Lidar, Lidar$Year=="6")
l6<-subset(l6, l6$Pulse!="Fi")
l6<-subset(l6, l6$Pulse!="In")

z <- l6$Z
x <- l6$X
y <- l6$Y


xy <- cbind(x, y)

#Start with coarse raster with 8m cells
library(raster)
?raster
r8 <- raster(ncols=length(seq(min(x), max(x), 8)), nrows=length(seq(min(y),max(y), 8)),
             xmn=min(x), xmx=max(x),ymn=min(y), ymx=max(y),crs=NA)

dtm8 <- rasterize(xy, r8, z,na.rm=T, fun="min", background=NA)

plot(dtm8)
plot3D(dtm8)


#######################################################################################
r4 <- raster(ncols=length(seq(min(x), max(x), 4)), nrows=length(seq(min(y),max(y), 4)),
             xmn=min(x), xmx=max(x),ymn=min(y), ymx=max(y),crs=NA)

dtm4 <- rasterize(xy, r4, z,na.rm=T, fun="min", background=185)

plot(dtm4)

dtm48<-resample(dtm8, dtm4, method="bilinear")
dif<-dtm4-dtm48

cellStats(dif, stat="max")
plot(dif)
plot3D(dif)

for (i in 1:ncell(dtm4)){
  if (dif[i]>6)
  {dtm4[i]<-dtm48[i]}
  
}


plot(dtm4)

###########################################################################################
r2 <- raster(ncols=length(seq(min(x), max(x), 2)), nrows=length(seq(min(y),max(y), 2)),
             xmn=min(x), xmx=max(x),ymn=min(y), ymx=max(y),crs=NA)

dtm2 <- rasterize(xy, r2, z,na.rm=T, fun="min", background=181)
plot(dtm2)
?resample
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

plot(dtm1)
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

summary(dtm05)
plot(dtm05)

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
cellStats(dtm05, stat = "max") #this can extract the specific stat from the summary
cellStats(dtm05, stat = "min")
summary(dtm8)
#difference between dtm8 and dtm0.5 
dtm058<-resample(dtm8, dtm05, method="bilinear")

summary(dtm058)
plot(dtm058)


difference058<-dtm05-dtm058
summary(difference058)
cellStats(difference058, stat="max")

#difference between dtm4 and dtm0.5 
dtm054<-resample(dtm4, dtm05, method="bilinear")

summary(dtm054)
plot(dtm054)


difference054<-dtm05-dtm054
summary(difference054)
cellStats(difference054, stat="max")



#difference between dtm2 and dtm0.5 
dtm052<-resample(dtm2, dtm05, method="bilinear")

summary(dtm052)
plot(dtm052)


difference052<-dtm05-dtm052
summary(difference052)
cellStats(difference052, stat="max")


#difference between dtm1 and dtm0.5 
dtm051<-resample(dtm1, dtm05, method="bilinear")

summary(dtm051)
plot(dtm051)


difference051<-dtm05-dtm051
summary(difference051)
cellStats(difference051, stat="max")



##################################################################################################
writeRaster(dtm1, filename="C:/Users/oyeda/Desktop/ADV_REM_SENS/assignment_1/dtm1.tif",overwrite=T)



################################################################################################
#dtm_filtered <- focal(dtm05, fun=mean, w=c(3,3))
#plot3D(dtm_filtered)








