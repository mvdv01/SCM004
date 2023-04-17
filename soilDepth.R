## Soil Depth Carbon Models
outdir <- '/home/mv/bin/werk/chepri/Projects/Spekboom/SpekboomNetZero/data/githubexportfiles'
### Soil depth 
soil <- read.csv(paste(outdir, "SoilKP2006.csv", sep = "/")) ## read the soil depth data set (Mills and Cowling 2006, table 5)
soil$Cpc <- (soil$C_g.kg / 1000) * 100  ## get soil values as percentage after converting kg/kg
## Get soil bulk density values
## make vector of soil bulk density data
soil$bulkdens_kg.m3 <- c(rep(1210,5), rep(870, 5), rep(1610, 90)) ## from Mills and Cowling (2006), table 4 
soil$bulkdens_g.cm3 <- soil$bulkdens_kg.m3 * 0.001  ## conversion of BD to g.cm3
## 1 kg = 1000 g; 1 m = 100 cm
## 1000 g/(100)^3 cm^3
## = 10^3 / 10^6
## = 10+^3 -^6
## = 10^-3
## Define layer depths
## from Mills and Cowling (2006), table 3, layer depth is in meters
soil$lyrdepth_m <-  c(rep(0.01, 10), rep(0.02, 10), rep(0.03, 10), 
                      rep(0.04, 10), rep(0.05, 20), rep(0.1, 10), 
                      rep(0.2, 10), rep(0.25,20)) 
## Soil Carbon without regard for stone volume
## Soil tCha = % C * bulkdensity_g.cm3 * sampledepth_cm3
soil$tCha <- soil$Cpc * soil$bulkdens_g.cm3 * (soil$lyrdepth_m * 100) 
soil$tCO2ha <- soil$tCha * 44/12
## Mills and Cowling 2006: 
## "The bulk density of soil under P. afra and in the open
## was estimated by excavating samples 100–150 mm into
## the soil pit wall for the following depth intervals: 0–100,
## 100–500, 500–1,000 mm (Krompoort), and 500–750 mm
## (Kudu Reserve). The dimensions of each excavation for
## each sample were measured to determine the soil volume.
## After air-drying, the samples were weighed and sieved to
## remove roots and stones greater than 2 mm. Stones were
## weighed, and the volume of stones was determined by the
## displacement of water. Bulk density was calculated by
## dividing the mass of soil by the volume of soil (both excluding stones >2 mm).
## "Soil carbon storage was calculated
## using the soil carbon content, the bulk density of soil, and
## the volume of stones at each depth interval. The volume
## of stones was subtracted from each depth layer to provide
## an estimate of soil volume in each layer." (last lines of methods).
##
## This is used as justification for the assumption that Poepler et al 2016's recommended
## method for minimising bias (M4), Eq. 3, was adhered to in determining bulk density and thus
## eq.5 is recommended to determine SOC stock
soil$stoneVolPc <- c(rep(15.2, 40),rep(22.8, 40), rep(30.3, 20)) 
## SOCstock_i = SOCcon_finesoil * BD_sample * depth_i * (1-stone fraction) 
## Poepler et al 2016 eq. 5 (M4):
soil$tCha_M4 <- soil$Cpc * soil$bulkdens_g.cm3 * (soil$lyrdepth_m * 100) * (1-soil$stoneVolPc/100) 
soil$tCO2ha_M4 <- soil$tCha_M4 * 44/12 ## C/ha to CO2e/ha
###
## Mills and Cowling 2006
## Soil carbon storage was calculated using the soil carbon content, the bulk density of soil, and
## the volume of stones at each depth interval. The volume of stones was subtracted from each depth layer to provide an estimate of soil volume in each layer.
##    kg/m 3 to g/cm 3, divide by 1,000.
##    g/cm 3 to kg/m 3, multiply by 1,000.
## 1 kg = 1000 g
## 1 m = 100 cm
## 1000 g/(100)^3 cm^3
## = 10^3 / 10^6
## = 10+^3 -^6
## = 10^-3
## Soil Carbon with regard for stone volume
## Poeplau et al. 2017:
## M4 equations:
## Eq 3 : BD_finesoil = (mass_sample - mass_stones) / (volume_sample - (mass_stones/rho_stones))
## Eq 5 : SOCstock_i = SOCcon_finesoil * BD_sample * depth_i * (1-stone fraction)
##  Thus, the equations in M4 could be reformulated as:
## FSS_i = (mass_finesoil/volume_sample) * depth_i
## SOCstock_i = SOCcon_finesoil * FSS_i
######################################################

## Age
soil$Age <- gsub("K|R|Transformed", "", soil$Site)
soil$Age <- as.numeric(soil$Age)
soil$Age[is.na(soil$Age)] <- 0
## depth intervals
## names(soil)
## View(soil) ## see output table
## change name of tCO2 var for easier ref
names(soil)[which(names(soil)=="tCO2ha_M4")] <-  "tCO2eha"
## Define selected variables
vars <- c("C_g.kg", "SE", "n", "lyrdepth_m", "tCO2ha", "tCO2eha")
## Add up values found for each layer for the 30, 50 and 100 cm depth intervals:
## 30 cm depth
soil030 <- soil[c(1:10),vars] + soil[c(11:20),vars] + soil[c(21:30), vars] +  soil[c(31:40), vars] + soil[c(41:50),  vars] +  soil[c(51:60), vars] + soil[c(61:70),vars] 
## 50 cm depth
soil050 <- soil[c(1:10),vars] + soil[c(11:20),vars] + soil[c(21:30), vars] +  soil[c(31:40),vars] + soil[c(41:50), vars] +  soil[c(51:60),vars] + soil[c(61:70), vars] +  soil[c(71:80),vars]
### 100 cm depth
soil100 <- soil[c(1:10),vars] + soil[c(11:20),vars] + soil[c(21:30), vars] +  soil[c(31:40),vars] + soil[c(41:50), vars] +  soil[c(51:60),vars] + soil[c(61:70), vars] +  soil[c(71:80),vars] + soil[c(81:90), vars] +  soil[c(91:100), vars]
## Get values for yields (total - proxy|baseline|degraded values)
soil30 <- cbind.data.frame(soil[c(1:10), c(1:2)], "Depth" = rep("0-300",10), soil030) ## make a working table for 30 cm depth
Age <- c(0, 5, 7, 13, 27, 5, 7, 13, 27, 27)
soil30$Age <- Age
syield30 <- soil30[-which(soil30$Age == 0),] ## Define not proxy
sproxy <- soil30[which(soil30$Age == 0),] ## Define proxy
syield30$d_tCO2eha <- syield30[,"tCO2eha"] - sproxy[,"tCO2eha"] ## subtract baseline|proxy from yields: 30 cm depth intvl
## Repeat procedure for other two intervals:
## 50 cm interval
soil50 <- cbind.data.frame(soil[c(1:10), c(1:2)], "Depth" = rep("0-500",10), soil050) ## subtract baseline|proxy to get yields: 50 cm depth intvl
soil50$Age <- Age
syield50 <- soil50[-which(soil50$Age == 0),] ## define not proxy 
sproxy <- soil50[which(soil50$Age == 0),] ## define proxy
syield50$d_tCO2eha <- syield50[,"tCO2eha"] - sproxy[,"tCO2eha"] ## subtract baseline|proxy to get yields: 50 cm depth intvl
## 100 cm interval
soil100 <- cbind.data.frame(soil[c(1:10), c(1:2)], "Depth" = rep("0-1000",10), soil100)
soil100$Age <- Age
syield100 <- soil100[-which(soil100$Age == 0),] ## define not proxy 
sproxy <- soil100[which(soil100$Age == 0),] ## define proxy
syield100$d_tCO2eha <- syield100[,"tCO2eha"] - sproxy[,"tCO2eha"] ## subtract baseline|proxy to get yields: 100 cm depth intvl
## 

#### Simple Linear Models per depth interval

## Models without baseline|proxy subtracted:
s30lmNL <- lm(tCO2eha ~ Age, data = soil30) ## Soil30R) ## NAs
s50lmNL <- lm(tCO2eha ~ Age, data = soil50) ## Soil50R)
s100lmNL <- lm(tCO2eha ~ Age, data = soil100) ## Soil50R)
## SOC yield models (with baseline|proxy subtracted) without zero point
sy30lmNL <- lm(d_tCO2eha ~ Age, data = syield30) ## Soil30R) ## NAs
sy50lmNL <- lm(d_tCO2eha ~ Age, data = syield50) ## Soil50R)
sy100lmNL <- lm(d_tCO2eha ~ Age, data = syield100) 
##
## make a zero point for each interval depth set where yield = 0 and time = 0 :
syield30z <- rbind.data.frame(syield30,  c("11", "OpenCanopy", "0-300",rep(0,3), 0.3,rep(0, 4)))  
syield30z[,c(4:11)] <- lapply(syield30z[,c(4:11)], function(x) as.numeric(x))
syield50z <- rbind.data.frame(syield50,  c("11", "OpenCanopy", "0-500", rep(0,3), 0.5,rep(0, 4)))    
syield50z[,c(4:11)] <- lapply(syield50z[,c(4:11)], function(x) as.numeric(x))
syield100z <- rbind.data.frame(syield100,  c("11", "OpenCanopy", "0-1000",rep(0,3), 1 ,rep(0, 4)))    
syield100z[,c(4:11)] <- lapply(syield100z[,c(4:11)], function(x) as.numeric(x))
## SOC yield models per depth interval according to yield data with added theoretical 0 point for each interval depth set
sy30lmNL0 <- lm(d_tCO2eha ~ Age, data = syield30z) 
sy50lmNL0<- lm(d_tCO2eha ~ Age, data = syield50z)
sy100lmNL0<- lm(d_tCO2eha ~ Age, data = syield100z)
####
