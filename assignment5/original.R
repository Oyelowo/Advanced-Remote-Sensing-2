
##########################################################
### Estimate tree-level attributes to the canopy segments ###
##########################################################


# Clear the cache and set the working directory:
rm(list=ls())
setwd("C:/Users/oyeda/Desktop/ADV_REM_SENS/assignment5")


#Read in data:
data <- read.csv(file="TreeVariableImputation.csv", head=T, sep=";")
head(data)

attach(data)      # check ?attach


# Create dummy variables from tree species and damage classes:
data$Species <- as.factor(data$Species)
data$Damage_cla <- as.factor(data$Damage_cla)


# Required commands for the following work phases:
plot(h,h90)
abline(0,1)
cor(h,h90)


#######################
### k-NN estimation ###
#######################

library(yaImpute)


# Select the explanatory and response variables:
x <- data[,c("Hmax","pene","h80","h90")]    # Explanatory variables
y <- data[,c("d13_2009","h","Species")]     # Response variables


# Search k amount of neighbors for each canopy segment
euc <- yai(x = x, y = y, k=5, method = "euclidean")
euc


# Impute tree-level attributes for each canopy segments 
knn_Imp <- impute(euc, k=5, method="dstWeighted", method.factor="median")
head(knn_Imp)


# Calculate RMSE
rmsd.yai(knn_Imp, scale=F)


# Calculate correlation
cor.yai(knn_Imp)


# Plot correlations
plot(knn_Imp)


# Calculate bias
BIAS <- mean(knn_Imp[,"d13_2009.o"] - knn_Imp[,"d13_2009"])
BIAS


# Write the result table in to .csv file
write.table(knnImp, file = "knn.csv", dec=".", sep=";")

