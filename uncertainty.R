### Uncertainty #################
###  Download libraries ###
library(grid)
library(gridExtra)
library(ggpubr)
library(ggplot2)
library(sf)
############
## load(file = "uptoUncertainty.RData")
############################
## U = 1.96.sigma/mu * 100% 
Usoc30 <- (1.96 * bgtab$SE[1])/mean(BGyields[,2]) * 100
## Usoc30 <- (1.96 * bgtab$sig[1])/mean(BGyields[,2]) * 100 ## don't use standard error
(1.96 * bgtab$SE)/colMeans(BGyields[,c(2:7)]) * 100
(1.96 * bgtab$SE)/colMeans(BGyields[,c(2:7)]) * 100
n <-  5
##
bgtab$U <- (qt(0.95,df=n-1) * bgtab$SE)/colMeans(BGyields[,c(2:7)]) * 100
#######
## bgtab
write.csv(bgtab, file = "bgUncertaintyTab.csv")
## mapply
################################
#### Uncertainty discount table
bgtab$discount = unlist(
lapply(c(1:nrow(bgtab)), function(x) {
if(bgtab$U[x]<=10) { 
bgtab$SE[x] * 0 
} else if (x%in%c(11:15)) { 
bgtab$SE[x] * 0.25 
} else if (x%in%c(16:20)) {
bgtab$SE[x] * 0.5
} else if (x%in%c(21:30)) {
bgtab$SE[x] * 0.75
} else {
bgtab$SE[x] * 1
}
}))
#############################
## Apply to SE
BGyUdisctab <- BGyields[,-1] - matrix(rep(t(bgtab$discount), each = 30), 30, 6)
BGyUdisctab <- round(BGyUdisctab, 2)

x<- BGyUdisctab[,1]

## change values below 0 to 0
apply(BGyUdisctab, 2, function(x) {
for(i in c(1:length(x)))
if(x[i] > 0) {
    x[i] <- x[i]
} else {
    x[i] <- 0
}
}
)
BGyUdisctab[BGyUdisctab<=0] <- 0
BGyUdisctab <- cbind.data.frame("Age" = c(0:29), BGyUdisctab)
################################################################
write.csv(BGyUdisctab, file = "bgUncertaintyDiscountTab.csv")
#### latex table
## library(knitr)
## library(kableExtra)
## kbl(BGyUdisctab, format = "latex", booktabs = TRUE, linesep="") %>%
## kable_styling(latex_options = "striped")
