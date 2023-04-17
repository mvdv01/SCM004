## Root
rd <- read.csv("rootdepth.csv") ## read in root depth data (Mills and Cowling 2006) 
## View(rd)
## Make the table workable
library(tidyr) ## load tidyr library
rd1 <- pivot_longer(rd, cols = grep("C.kg", names(rd)), names_to = "Depth", values_to = "C.kg")
rd1SE <- rd1[grep("SE", rd1$Depth),]
rd1noSE <- rd1[-grep("SE", rd1$Depth),]
## Make a general cleaning function
clean <- function(x) { 
    tt <- x[,c(-3,-4,-5)]
    tt$Depth <- gsub("C.kg.m2.|SE.", "", tt$Depth)
    tt
}
##
rd1SE <- clean(rd1SE)
rd1noSE <- clean(rd1noSE)
rd1noSE$C.kg.SE <- rd1SE$C.kg
rootd <- rd1noSE
## View(rootd) ## A workable data frame
proxyRD <- rootd[which(rootd$Stand == "Transformed"),] ## define proxy / baseline
noproxy <- rootd[-which(rootd$Stand == "Transformed"),] ## define non-proxy
sites <- unique(noproxy$Stand) ## make a unique sites variable
noproxy$yield <- noproxy$C.kg - rep(proxyRD$C.kg, nrow(noproxy)/3) ## 
noproxy$Age <- as.numeric(gsub("K|R","", noproxy$Stand)) ## define ages from stand names
noproxy$yieldtCO2e <- noproxy$yield * 1000/100 * 44/12 # kg.m2 to tC.ha-1 to tCO2e
##
root100 <- noproxy[which(noproxy$Depth==100),c(-4,-5,-6)]  ## remove unwanted columns
root100 <- rbind.data.frame(root100, c(NA, NA, 100, 0, 0)) ## bind a theoretical zero point to the root set
root500 <- noproxy[which(noproxy$Depth==500),c(-4,-5,-6)] ## remove unwanted columns
root500 <- rbind.data.frame(root500, c(NA, NA, 500, 0, 0)) ## bind a theoretical zero point to the root set
root1000 <- noproxy[which(noproxy$Depth==1000),c(-4,-5,-6)] ## remove unwanted columns
root1000 <- rbind.data.frame(root1000, c(NA, NA, 1000, 0, 0))  ## bind a theoretical zero point to the root set
##
root500T <- cbind.data.frame(root100[,-5], "yieldtCO2e" = root100$yieldtCO2e + root500$yieldtCO2e)
root1000T <- cbind.data.frame(root100[,-5], "yieldtCO2e" = root100$yieldtCO2e + root500$yieldtCO2e + root1000$yieldtCO2e)
r100lmNL <- lm(yieldtCO2e ~ Age, data = root100) 
r500lmNL <- lm(yieldtCO2e ~ Age, data = root500T)
r1000lmNL <- lm(yieldtCO2e ~ Age, data = root1000T) 
##
