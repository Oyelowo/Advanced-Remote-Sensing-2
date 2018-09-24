########################################
# Feature extraction for tree segments #
########################################

rm(list=ls())
setwd("C:/HY-data/MIKNIEMI/GIS202/Exercises/Exercise 5 materials")


##################################################################################

# Read 3D point data (TEXAS_lidar.txt)

Lidar <- read.table(file="TEXAS_lidar.txt", head=FALSE, sep="\t") 
colnames(Lidar) <- c("Year","Pulse","X","Y","Z","h","range","int","agc")

summary(Lidar)

# Specify column names of the coordinates 
coordinates(Lidar) = c("X", "Y")


##################################################################################

# Read the shapefile (TrainingTrees.shp)

library(sp)
library(maptools)

S <- readShapePoly("TrainingTrees.shp") #Tree segments
SP <- as(S, "SpatialPolygons") 


# Plot tree segments

plot(SP)


##################################################################################

# Analyse, which Lidar points are located inside the segments

Lidar$In <- over(Lidar,SP)

# Select the 2010 Lidar data, and remove the points located outside of the segments

Lidar10 <- subset(Lidar, Lidar$Year==10)
Lidar10 <- subset(Lidar10, Lidar10$In!="NA")

summary (Lidar10)

#######################################################################################


#####################################################################
### TEE ALLA OLEVAAN KOODIIN TARVITTAVAT LISÄYKSET JA MUOKKAUKSET ###
#####################################################################


ID <- 999; X <- 999; Y <- 999; Hmax <- 999; Hmean <- 999; Hstd <- 999; CV <- 999; VD <- 999;
h10 <- 999; h20 <- 999; h30 <- 999; h40 <- 999; h50 <- 999; h60 <- 999; h70 <- 999; h80 <- 999; h90 <- 999;

Lidar10 <- data.frame(Lidar10)
range(Lidar10$In)
View(Lidar10)

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















temp <- subset(Lidar10, Lidar10$In == 1)
# And remember also to subset only first and single echoes!	

X <- temp[which.max(temp[,"h"]),"X"]   #Returns "X" from the point that has the maximum "h" value
Y <- temp[which.max(temp[,"h"]),"Y"]   #Returns "Y" from the point that has the maximum "h" value
Hmax <- temp[which.max(temp[,"h"]),"h"] 

# Find the maximum height
# Calculate the mean height of point height values
# Calculate the standard deviation of point height values
# Calculate the coefficient of variation in height values
# Calculate the vegetation density
# Calculate quantiles      #help(quantile) 


results <- cbind(ID,X,Y,Hmax,Hmean,Hstd,CV,VD,h10,h20,h30,h40,h50,h60,h70,h80,h90)

results <- as.data.frame(results)

write.table(results, file = "Results.csv", dec=".", sep=",",row.names=F)
