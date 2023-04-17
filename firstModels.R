## Make preliminary models without subtracting proxy/baseline values
### Root
## Root_no0 <- Root[-which(Root$Age==0),]
Root50R <- na.omit(Root) ## remove unavailable data rows
Root30R <- Root50R[which(Root50R$Age<=30),] ## Choose only samples from stands <= 30 years
root30lmNL <- lm(RootC ~ Age, data = Root30R) ## Simple 50-year linear model
root50lmNL <- lm(RootC ~ Age, data = Root50R) ## Simple 30-year linear model
#### Do the same for Litter Biomass and Soil
## Litter
Litter50R <- na.omit(Litter)
Litter30R <- Litter50R[which(Litter50R$Age<=30),]
litter30lmNL <- lm(LitterC ~ Age, data = Litter30R)
litter50lmNL <- lm(LitterC ~ Age, data = Litter50R)
## Biomass
Biom50R <- na.omit(Biomass)
Biom30R <- Biom50R[which(Biom50R$Age<=30),]
Biom50Rl <- na.omit(Biomass) 
Biom30Rl <-  Biom50Rl[which(Biom50Rl$Age<=30),]
biom30lmNL <- lm(BiomassC ~ Age, data = Biom30R) ## Biom30R) ## NAs
biom50lmNL <- lm(BiomassC ~ Age, data = Biom50R) ## Biom50R)
## Soil
## Soil_no0 <- Soil[-which(Soil$Age==0),]
Soil0 <- rbind(c(0,0), Soil) ## add a zero-point to soil data on both axes (i.e. year = 0, tCO2 = 0)
Soil50R <- na.omit(Soil0) 
Soil30R <- Soil50R[which(Soil50R$Age<=30),]
soil30lmNL <- lm(SoilC ~ Age, data = Soil30R) ## 
soil50lmNL <- lm(SoilC ~ Age, data = Soil50R) ## 
## AGB different treatments
### 
ABtrtmntlm <- lm(BiomassC ~ Age, data = ABtrtmnt) 
Ctrtmntlm <- lm(BiomassC ~ Age, data = Ctrtmnt) ## 2m spacing, wthout extra treatments
LMNtrtmntlm <- lm(BiomassC ~ Age, data = LMNtrtmnt) ##Biom50Rl)
## Model equations as functions - giving the yearly yield
litfun30 <- function(x) {coef(litter30lmNL)[2]*x+coef(litter30lmNL)[1]}
litfun50 <- function(x) {coef(litter50lmNL)[2]*x+coef(litter50lmNL)[1]}
rootfun30 <- function(x) {coef(root30lmNL)[2]*x+coef(root30lmNL)[1]}
rootfun50 <- function(x) {coef(root50lmNL)[2]*x+coef(root50lmNL)[1]}
biomfun30 <- function(x) {coef(biom30lmNL)[2]*x+coef(biom30lmNL)[1]}
biomfun50 <- function(x) {coef(biom50lmNL)[2]*x+coef(biom50lmNL)[1]}
### Litter
## restoration trajectories by year
yrs30 <- c(1:30) 
yrs50 <- c(1:50)
## make yearly increment projections from the models
Litter30yrly <- as.numeric(lapply(yrs30, "litfun30")) ## prediction per year
Litter30yrlyInc <- c(0, lapply(c(2:30), function(x) {Litter30yrly[x] - Litter30yrly[x-1]})) ## yearly increment
Litter50yrly <- as.numeric(lapply(yrs50, "litfun50"))
Litter50yrlyInc <- c(0, lapply(c(2:30), function(x) {Litter50yrly[x] - Litter50yrly[x-1]})) ## yearly increment
## Root
Root30yrly <- as.numeric(lapply(yrs30, "rootfun30")) ## prediction per year
Root30yrlyInc <- c(0, lapply(c(2:30), function(x) {Root30yrly[x] - Root30yrly[x-1]})) ## yearly increment
Root50yrly <- as.numeric(lapply(yrs50, "rootfun50")) ## prediction per year
Root50yrlyInc <- c(0, lapply(c(2:30), function(x) {Root50yrly[x] - Root50yrly[x-1]})) ## yearly increment
## Biomass 
Biom30yrly <- as.numeric(lapply(yrs30, "biomfun30")) ## prediction per year
Biom30yrlyInc <- c(0, lapply(c(2:30), function(x) {Biom30yrly[x] - Biom30yrly[x-1]})) ## yearly increment
Biom50yrly <- as.numeric(lapply(yrs50, "biomfun50")) ## prediction per year
Biom50yrlyInc <- c(0, lapply(c(2:30), function(x) Biom50yrly[x] - Biom50yrly[x-1])) ## yearly increment
###########################################
