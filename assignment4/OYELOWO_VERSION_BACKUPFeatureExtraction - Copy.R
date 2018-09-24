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
Lidar10<-data.frame(Lidar10)

tail(Lidar10)
summary (Lidar10)

#######################################################################################


#####################################################################
### TEE ALLA OLEVAAN KOODIIN TARVITTAVAT LISÄYKSET JA MUOKKAUKSET ###
#####################################################################
#5) Create a code which calculates the following Lidar features for individual tree
#segments. Use only first and single echoes while calculating the features:
#  . Tree position



#use first or single only which are have height more than 2m but and less than 50m
#these are the defined thresholds for the trees. Because the trees are assumed to be not
#not more than 50m, and anything more than is taken as error. also, below 2m is assumed
#not to be a tree.


#o X-coordinate of maximum height observation (X)
#o Y-coordinate of maximum height observation (Y)
#. Maximum height of the observations (Hmax)
#. Mean height of the observations (Hmean)
#. Standard deviation of the height observations (Hstd)
#. Coefficient of variation (CV)
#. Vegetation density, i.e. percentage of observations above 2 meters (VD)
#. Quantiles of the height observations (h10-h90)



#I tried two ways. 
#create empty objects to be filled later
#instead of using the long method below, i used a more efficient means
#which follows immediately after
#ID <- 0; X <- 0; Y <- 0; Hmax <- 0; Hmean <- 0;
#Hstd <- 0; CV <- 0; VD <- 0; 
#h10 <- 0; h20 <- 0; h30 <- 0; h40 <- 0; h50 <- 0;
#h60 <- 0; h70 <- 0; h80 <- 0; h90 <- 0;


#the second method below:
ID<-ID2<-X<-Y<-Hmax<-Hmean<-Hstd<-CV<-VD<-h10<-h20<-h30<-h40<-h50<-h60<-
  h70<-h80<-h90<-0

# subset only first and single echoes!	
lfisi2 <- subset(Lidar10, (Lidar10$Pulse=="Fi" | Lidar10$Pulse=="Si") )

#subset height greater than 2m and less than 50. I could have executed
#the above and below on a single line but I did them separately
#for a calculation in the loop afterwards, where I needed to calculate
#the percentage of the trees(i.e above 2m), covering the area
lfisi<- subset(lfisi2, lfisi2$h>2 &lfisi2$h<50)



#min(lfisi$In):max(lfisi$In) this can also be used instead of 
#sort(unique(lfisi$In)) in this case
for (i in sort(unique(lfisi$In))) {
  
  #subset only first and single echoes!	
  lfisi2 <- subset(Lidar10, (Lidar10$Pulse=="Fi" | Lidar10$Pulse=="Si") )
  lfisi<- subset(lfisi2, lfisi2$h>2 &lfisi2$h<50)
  lfisi<-subset(lfisi, lfisi$In == i)
  ID[i]<- i
  
  #the below is done for the joining operation in Arcgis for the 
  #field data column with numbers running from 0 to 103
  #also note:  you can also use #can also use: ID2<-seq(0, 103) 
  #or ID2<- seq(0, i-1) but when using the sequence, avoid the vector [i]
  #as it will generate 0 throughout the column instead of 
  #sequencing from 0 to 103. This is because it returns 
  ID2[i]<- i-1 
    
  
  X[i] <- lfisi[which.max(lfisi[,"h"]),"X"] #Returns "X" from the point that has the maximum "h" value
  Y[i] <- lfisi[which.max(lfisi[,"h"]),"Y"]   #Returns "Y" from the point that has the maximum "h" value
  Hmax[i] <- lfisi[which.max(lfisi[,"h"]),"h"]
  Hmean[i]<-mean(lfisi$h)
  Hstd[i]<- sd(lfisi$h)
  #CV[i]<- (Hstd[i]/ Hmean[i])
  #or
  CV[i]<-sd(lfisi$h)/mean(lfisi$h)
  
  h10[i]<-quantile(lfisi$h, probs = c(0.1))
  h20[i]<-quantile(lfisi$h, probs = c(0.2))
  h30[i]<-quantile(lfisi$h, probs = c(0.3))
  h40[i]<-quantile(lfisi$h, probs = c(0.4))
  h50[i]<-quantile(lfisi$h, probs = c(0.5))
  h60[i]<-quantile(lfisi$h, probs = c(0.6))
  h70[i]<-quantile(lfisi$h, probs = c(0.7))
  h80[i]<-quantile(lfisi$h, probs = c(0.8))
  h90[i]<-quantile(lfisi$h, probs = c(0.9))
  
  
  VD[i]<- (nrow(lfisi)*100/nrow(lfisi2))
  #0r
  #VD[i]<- (length(lfisi$h)*100/length(lfisi2$h))
  
 
  output <- cbind.data.frame(ID,ID2,X,Y, Hmax, Hmean, Hstd, CV,VD,h10,h20,
                  h30,h40,h50,h60,h70,h80,h90)
}

#I already converted the cbind directly to dataframe above in
#the loop by using cbind.data.frame() instead of just cbind
#output<-data.frame(output) #convert vector to dataframe

#the vertical point height distribution
hist(output$Hmean, main = "vertical point height distributions",
     xlab = "Height", col = "purple")


#field and lidar data have been joined in Arcgis, here, I will 
#import the shp file and caculate the errors
fieldLidar<- readShapePoly("C:/Users/oyeda/Desktop/ADV_REM_SENS/assignment4/fieldJoinLidar") 

#this is not really necessary now, as I only need to import
#the shapfile and convert to dataframe to calculate the errors
#only did it to try it out to see how it works
#fieldLidar <- as(S, "SpatialPolygons")


plot(fieldLidar, col= "blue")  #plot the shapefile

#convert the shapefile to dataframe to allow manipulation and analysis.
fieldLidarDf <- data.frame(fieldLidar)


#function that returns the RMSE(root mean square error)
rmse <- function(actual, predicted)
{       
  sqrt(mean((actual - predicted)^2))
}

#function that returns the mean absolute error
mae <- function(actual, predicted)
{       
  mean(abs(actual - predicted))
}


#Here, I will explore the various columns and compare them to calculate
#errors
summary(fieldLidarDf)

#compare X and X_1, Y and Y_1
RMSE_X<-rmse(fieldLidarDf$X, fieldLidarDf$X_1)
RMSE_Y<-rmse(fieldLidarDf$Y, fieldLidarDf$Y_1)
RMSE_H<-rmse(fieldLidarDf$h, fieldLidarDf$Hmax)


MAE_X<-mae(fieldLidarDf$X, fieldLidarDf$X_1)
MAE_Y<-mae(fieldLidarDf$Y, fieldLidarDf$Y_1)
MAE_H<-mae(fieldLidarDf$h, fieldLidarDf$Hmax)

errors<-cbind(RMSE_X,RMSE_Y,RMSE_H,MAE_X,MAE_Y,MAE_H)
errors<-data.frame(errors)


X<-fieldLidarDf$X
X1<-fieldLidarDf$X_1
Y<-fieldLidarDf$Y
Y1<-fieldLidarDf$Y_1
H<-fieldLidarDf$h 
H1<-fieldLidarDf$Hmax

errorPlot<- cbind.data.frame(X,X1,Y,Y1,H,H1)
plot(errorPlot, type="s")


?plot
#It can also be broken into steps but I prefer the first
#because it is more straightforward

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

# Calculate error
#where, error <- actual - predicted
















write.table(output, file = "ouput.csv", dec=".", sep=",",row.names=F)
write.table(output, file="ouput.txt", dec=".", sep="\t",row.names=F)








