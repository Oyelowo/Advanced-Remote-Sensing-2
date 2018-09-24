
##########################################################
### Estimate tree-level attributes to the canopy segments ###
##########################################################
.libPaths("C:/Users/oyeda/Desktop/ADV_REM_SENS/library")

# Clear the cache and set the working directory:
rm(list=ls())
setwd("C:/Users/oyeda/Desktop/ADV_REM_SENS/assignment5")

#install.packages("yaImpute")
library("yaImpute")
#Read in data:
data <- read.csv(file="TreeVariableImputation.csv", head=T, sep=";")
head(data)

attach(data)      # check ?attach


# Create dummy variables from tree species and damage classes:
data$Species2 <- as.factor(data$Species)
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
x <- data[,c("Hmax","pene", "Hmean")]    # Explanatory variables
y <- data[,c("d13_2009","h","Species")]     # Response variables
y2 <- data[,c("d13_2009","h","Species2")]

# Search k amount of neighbors for each canopy segment
euc <- yai(x = x, y = y2, k=3, method = "euclidean")

#factors are not allowed for msn, hence, I didnt convert the
#species variable to factor earlier.
msn <- yai(x = x, y = y, k=3, method = "msn")
euck5 <- yai(x = x, y = y2, k=5, method = "euclidean")
msnk5 <- yai(x = x, y = y, k=5, method = "msn")
?yai

#Method randomForest works best when there are few variables and when factors are used
#rather than continuous variables. that was why I converted to the species factor earlier.
rf <- yai(x = x, y = y2, k=3,  method = "randomForest")

#meh <- yai(x = x, y = y, k=3, method = "mahalanobis")
#ica <- yai(x = x, y = y, k=3, method = "ica")
#msn2 <- yai(x = x, y = y, k=3, method = "msn2")
#gnn <- yai(x = x, y = y, k=3, method = "gnn")

euc


# Impute tree-level attributes for each canopy segments 
euc_Imp <- impute(euc, k=3, method="dstWeighted", method.factor="median")
euck5_Imp <- impute(euck5, k=5, method="dstWeighted", method.factor="median")

euc_Imp2 <- impute(euc, k=3, method="mean", method.factor="closest")
euck5_Imp2 <- impute(euck5, k=5, method="mean", method.factor="mean")


msn_Imp <- impute(msn, k=3, method="dstWeighted", method.factor="median")
msnk5_Imp <- impute(msnk5, k=5, method="dstWeighted", method.factor="median")

msn_Imp2 <- impute(msn, k=3, method="mean", method.factor="closest")
msnk5_Imp2 <- impute(msnk5, k=5, method="mean", method.factor="mean")

rf_Imp <- impute(rf, k=3, method="mean", method.factor="closest")


# Calculate RMSE
euc1<-rmsd.yai(euc_Imp, scale=F)
euc2<-rmsd.yai(euck5_Imp, scale=F)
euc3<-rmsd.yai(euc_Imp2, scale=F)
euc4<-rmsd.yai(euck5_Imp2, scale=F)
msn1<-rmsd.yai(msn_Imp, scale=F)
msn2<-rmsd.yai(msnk5_Imp, scale=F)
msn3<-rmsd.yai(msn_Imp2, scale=F)
msn4<-rmsd.yai(msnk5_Imp2, scale=F)
rf<-rmsd.yai(rf_Imp, scale=F)

RMSE=cbind.data.frame(euc1, euc2, euc3, euc4, msn1,msn2,
                     msn3,msn4,rf)
RMSE= head(RMSE, 3)
colnames(RMSE)<-c("euc1", "euc2", "euc3", "euc4", "msn1","msn2",
                "msn3","msn4", "rf")
rownames(RMSE) = c("d13_2009", "h", "Species2")


# Calculate correlation
cor1<-cor.yai(euc_Imp)
cor2<-cor.yai(euck5_Imp)
cor3<-cor.yai(euc_Imp2)
cor4<-cor.yai(euck5_Imp2)
cor5<-cor.yai(msn_Imp)
cor6<-cor.yai(msnk5_Imp)
cor7<-cor.yai(msn_Imp2)
cor8<-cor.yai(msnk5_Imp2)
cor9<-cor.yai(rf_Imp)

COR<-cbind.data.frame(cor1,cor2,cor3,cor4,cor5,cor6,cor7,cor8,cor9)

COR= head(COR, 3)
colnames(COR)<-c("euc1", "euc2", "euc3", "euc4", "msn1","msn2",
                  "msn3","msn4", "rf")
rownames(COR) = c("d13_2009", "h", "Species2")


# Plot correlations
plot(euc_Imp)
plot(euck5_Imp)
plot(euc_Imp2)
plot(euck5_Imp2)
plot(msn_Imp)
plot(msnk5_Imp)
plot(msn_Imp2)
plot(msnk5_Imp2)
plot(rf_Imp)


# Calculate bias
BIAS1 <- mean(euc_Imp[,"d13_2009.o"] - euc_Imp[,"d13_2009"])
BIAS2 <- mean(euck5_Imp[,"d13_2009.o"] - euck5_Imp[,"d13_2009"])
BIAS3 <- mean(euc_Imp2[,"d13_2009.o"] - euc_Imp2[,"d13_2009"])
BIAS4 <- mean(msn_Imp[,"d13_2009.o"] - msn_Imp[,"d13_2009"])
BIAS5 <- mean(msnk5_Imp[,"d13_2009.o"] - msnk5_Imp[,"d13_2009"])
BIAS6 <- mean(msnk5_Imp[,"d13_2009.o"] - msnk5_Imp[,"d13_2009"])
BIAS7 <- mean(msn_Imp2[,"d13_2009.o"] - msn_Imp2[,"d13_2009"])
BIAS8 <- mean(msnk5_Imp2[,"d13_2009.o"] - msnk5_Imp2[,"d13_2009"])
BIAS9 <- mean(rf_Imp[,"d13_2009.o"] - rf_Imp[,"d13_2009"])


biasD<-cbind.data.frame(BIAS1, BIAS2,BIAS3,BIAS4,BIAS5,BIAS6,BIAS7,BIAS8, BIAS9)

colnames(biasD)<-c("euc1", "euc2", "euc3", "euc4", "msn1","msn2",
                   "msn3","msn4", "rf")
#biasD <- head(biasD, 3)
rownames(biasD) = c("diameter bias")



# Calculate bias
BIASh1 <- mean(euc_Imp[,"h.o"] - euc_Imp[,"h"])
BIASh2 <- mean(euck5_Imp[,"h.o"] - euck5_Imp[,"h"])
BIASh3 <- mean(euc_Imp2[,"h.o"] - euc_Imp2[,"h"])
BIASh4 <- mean(msn_Imp[,"h.o"] - msn_Imp[,"h"])
BIASh5 <- mean(msnk5_Imp[,"h.o"] - msnk5_Imp[,"h"])
BIASh6 <- mean(msnk5_Imp[,"h.o"] - msnk5_Imp[,"h"])
BIASh7 <- mean(msn_Imp2[,"h.o"] - msn_Imp2[,"h"])
BIASh8 <- mean(msnk5_Imp2[,"h.o"] - msnk5_Imp2[,"h"])
BIASh9 <- mean(rf_Imp[,"h.o"] - rf_Imp[,"h"])


biasH<-cbind.data.frame(BIASh1, BIASh2,BIASh3,BIASh4,BIASh5,BIASh6,BIASh7,BIASh8, BIASh9)
biasH <- head(biasH, 3)
colnames(biasH)<-c("euc1", "euc2", "euc3", "euc4", "msn1","msn2",
                  "msn3","msn4", "rf")
rownames(biasH) = c("height bias")



# Write the result table in to .csv file
write.csv(RMSE, file = "RMSE.csv", sep=",")
write.csv(COR, file = "CORRELATION.csv", sep=",")
write.csv(biasD, file = "BIAS_DIAMETER.csv", sep=",")
write.csv(biasH, file = "BIAS_HEIGHT.csv", sep=",")

