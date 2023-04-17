## Models to apply over restoration trajectory (in years) to get yearly sequestration values
litfun30 <- function(x)  coef(litter30lmNL)[2]*x+coef(litter30lmNL)[1]
litfun50 <- function(x)  coef(litter50lmNL)[2]*x+coef(litter50lmNL)[1]
###
rootfun30 <- function(x) coef(root30lmNL)[2]*x+coef(root30lmNL)[1]
rootfun50 <- function(x) coef(root50lmNL)[2]*x+coef(root50lmNL)[1]
###
biomfun30 <- function(x) coef(biom30lmNL)[2]*x+coef(biom30lmNL)[1]
biomfun50 <- function(x) coef(biom50lmNL)[2]*x+coef(biom50lmNL)[1]
###
r100fun30 <- function(x) coef(r100lmNL)[2]*x+coef(r100lmNL)[1]
r500fun30 <- function(x) coef(r500lmNL)[2]*x+coef(r500lmNL)[1]
r1000fun30 <- function(x) coef(r1000lmNL)[2]*x+coef(r1000lmNL)[1]
###
sy30fun30 <- function(x) coef(sy30lmNL0)[2]*x+coef(sy30lmNL0)[1]
sy50fun30 <- function(x) coef(sy50lmNL0)[2]*x+coef(sy50lmNL0)[1]
sy100fun30 <- function(x) coef(sy100lmNL0)[2]*x+coef(sy100lmNL0)[1]
########
ABtrtmntFun <- function(x) coef(ABtrtmntlm)[2]*x+coef(ABtrtmntlm)[1]
CtrtmntFun <- function(x) coef(Ctrtmntlm)[2]*x+coef(Ctrtmntlm)[1]
LMNtrtmntFun <- function(x) coef(LMNtrtmntlm)[2]*x+coef(LMNtrtmntlm)[1]
## Apply the models
### Biomass
Biom30Yrly <- as.numeric(lapply(yrs30, "biomfun30"))
Biom50Yrly <- as.numeric(lapply(yrs30, "biomfun50"))
### Litter
lit30Yrly <- as.numeric(lapply(yrs30, "litfun30"))
lit50Yrly <- as.numeric(lapply(yrs30, "litfun50"))
## Root depth
r100FunYrly <- as.numeric(lapply(yrs30, "r100fun30"))
r500FunYrly <- as.numeric(lapply(yrs30, "r500fun30"))
r1000FunYrly <- as.numeric(lapply(yrs30, "r1000fun30"))
######
## Soil depth
sy30FunYrly <- as.numeric(lapply(yrs30, "sy30fun30"))
sy50FunYrly <- as.numeric(lapply(yrs30, "sy50fun30"))
sy100FunYrly <- as.numeric(lapply(yrs30, "sy100fun30"))
##############################################################################
### Calculate yields by subtracting proxy values - belowground
## soil
syields <- cbind.data.frame("Age" = c(1:30), "Soil 30cm" = round(sy30FunYrly,2), "Soil 50cm" = round(sy50FunYrly,2), "Soil 100cm" = round(sy100FunYrly, 2))

syields <- do.call("rbind", apply(syields, 1, FUN = function(x) x - syields[1,]))

## root
ryields <- cbind.data.frame("Age" = c(1:30), "Root 10cm" = round(r100FunYrly,2), "Root 50cm" = round(r500FunYrly,2), "Root 100cm" =  round(r1000FunYrly,2))

ryields <- do.call("rbind", apply(ryields, 1, FUN = function(x) x - ryields[1,]))

## total belowground
BGyields <- cbind.data.frame(syields, ryields[,-1])

### Calculate yields - aboveground
## Note these yields are only from planted spekboom and does not account for background biomass carbon present at the time of planting
## Baseline biomass data need to be accounted for during baseline development using standard allometric models and should include litter samples.

## Thicket-Wide Plot treatments
ABtrtmntlmYrly <- as.numeric(lapply(yrs30, "ABtrtmntFun"))
CtrtmntlmYrly <- as.numeric(lapply(yrs30, "CtrtmntFun"))
LMNtrtmntlmYrly <- as.numeric(lapply(yrs30, "LMNtrtmntFun"))

AGByields <- cbind.data.frame("Age" = c(1:30), "Biomass30y" = round(Biom30Yrly,2), "Biomass50y" = round(Biom50Yrly,2), "Litter30y" = round(lit30Yrly,2), "Litter50y" = round(lit50Yrly,2), "Planting 1" = round(ABtrtmntlmYrly,2), "Planting 2" = round(CtrtmntlmYrly, 2), "Planting 3" = round(LMNtrtmntlmYrly, 2))

AGByields[c(1:4), 2] <- 0 ## change minus values to zero
AGByields[1, 4] <- 0 ## change minus values to zero

### make a latex table
## library(knitr)
## library(kableExtra)
## kbl(BGyields, format = "latex", booktabs = TRUE, linesep="") %>%
## kable_styling(latex_options = "striped")
## kbl(AGByields, format = "latex", booktabs = TRUE, linesep="") %>%
## kable_styling(latex_options = "striped")
### save 
write.csv(AGByields, "AGByields.csv")
write.csv(BGyields, "BGyields.csv")
##

########
sy30lmNL0 <- lm(tCO2eha ~ Age, data = syield30z) ## Soil30R) ## NAs
sy50lmNL0<- lm(tCO2eha ~ Age, data = syield50z) ## Soil50R)
sy100lmNL0<- lm(tCO2eha ~ Age, data = syield100z) ## Soil50R)
##
r100lmNL <- lm(yieldtCO2e ~ Age, data = root100) ## Soil30R) ## NAs
r500lmNL <- lm(yieldtCO2e ~ Age, data = root500T) ## Soil50R)
r1000lmNL <- lm(yieldtCO2e ~ Age, data = root1000T) ## Soil50R)
##
litter30lmNL <- lm(LitterC ~ Age, data = Litter30R)##_no0R)) ## NAs
litter50lmNL <- lm(LitterC ~ Age, data = Litter50R)
##
biom30lmNL <- lm(BiomassC ~ Age, data = Biom30R) ## Biom30R) ## NAs
biom50lmNL <- lm(BiomassC ~ Age, data = Biom50R) ## Biom50R)
##
ABtrtmntlm <- lm(BiomassC ~ Age, data = ABtrtmnt) ##Biom30Rl) ## NAs
## Atrtmntlm <- lm(BiomassC ~ Age, data = Atrtmnt) ##Biom50Rl)
Ctrtmntlm <- lm(BiomassC ~ Age, data = Ctrtmnt) ##Biom30Rl) ## NAs
LMNtrtmntlm <- lm(BiomassC ~ Age, data = LMNtrtmnt) ##Biom50Rl)
###########
