## load files
## set working directory where source files are kept
wdir <- getwd()  
## githubexport
RH_tC.ha <- read.csv(paste(wdir, "RHvdvyver2013.csv", sep = "/"))[,-1] ## load it without rownames
## extract numeric values and convert to tCO2e and rebind new data.frame
RHtCO2eha <- RH_tC.ha[,c(3:8)] * 44/12
RHtCO2eha <-  cbind.data.frame(RH_tC.ha[,c(1,2)], RHtCO2eha)
## load KP data (Mills and Cowling 2006) and set to t/ha 
KP_kg.m2 <- read.csv("KPmills2006.csv")[,-1] ## load it without rownames
KPtCha <- KP_kg.m2[,c(3:16)] * 10 ## multiply all numeric value columns by 10
## convert them to tCO2e/ha
KPtCO2eha <- KPtCha * 44/12 
## rebind it into a data.frame
KPtCO2eha <- cbind.data.frame(KP_kg.m2[, c(1,2)], KPtCO2eha)
## set the age of each sample from Rhinosterhoek (RH) set as "Age"
RHtCO2eha$Age <- c(rep(50, 7), rep(35, 7), rep(0,7))
## View(RHtCO2eha)View(KPtCO2eha)
## Delete from the working dataset the results from the Kudu Reserve also sampled by Mills and Cowling, 2006### View(KPtCO2eha)
KPtCO2eha <- KPtCO2eha[c(-18:-24),] 
## set age of the KP set
KPtCO2eha$Age <- c(c(0,5,7,13,27),rep(c(0,5,7,13,27,27), 2))
## View(KPtCO2eha)
## Get the baseline/proxy background carbon values for the Krompoort site from the degraded stand not revegetated
nodegRH <- RHtCO2eha[which(RHtCO2eha$Status == "Degraded"),]
## Remove it from the working set
RHtCO2eha <- RHtCO2eha[-which(RHtCO2eha$Status == "Degraded"),]
## Get the means for the baseline/proxy from the degraded set
RHproxies <- colMeans(nodegRH[,c(3:8)], na.rm = TRUE)
## Do the same for the Rhinosterhoek data set "SoilKP2006.csv", sep = "/"))
nodegKP <- KPtCO2eha[which(KPtCO2eha$Site=="KP.Transformed"),]
KPproxies <- colMeans(KPtCO2eha[which(KPtCO2eha$Site=="KP.Transformed"),][,c(3:14)])
KPtCO2eha <- KPtCO2eha[-which(KPtCO2eha$Site=="KP.Transformed"),]
## Get the root C dataset by subtracting the isolated proxys from each of the different aged stands for each of the two data-sets and bind them together
Root <- cbind.data.frame("RootC" = c((KPtCO2eha$Root - KPproxies["Root"]), 
 (RHtCO2eha$RootC - RHproxies["RootC"])), "Age" = c(KPtCO2eha$Age, RHtCO2eha$Age))
## Do the same for Litter:
Litter <- cbind.data.frame("LitterC" = c((KPtCO2eha[which(KPtCO2eha$Cover%in%c("Canopy", "Combined")), "Litter"] - KPproxies["Litter"]), 
 (RHtCO2eha$LitC - RHproxies["LitC"])), 
 "Age" = c(KPtCO2eha[which(KPtCO2eha$Cover %in% c("Canopy", "Combined")), "Age"] , RHtCO2eha$Age))
Litter <- rbind.data.frame(Litter, c(0,0)) ## add in a zero point for a condition where age = 0 and LitterC = 0
## Do the same for SOC
Soil <- cbind.data.frame("SoilC" = c((KPtCO2eha$Soil.Ca - KPproxies["Soil.Ca"]),
 (RHtCO2eha$SoilC - RHproxies["SoilC"])), 
 "Age" = c(KPtCO2eha$Age, RHtCO2eha$Age)
  )
## Do the same process for Biomass:
## Here the background baseline/proxy values and those for revegetated stands are defined as including both Biomass and 'Opslag' - 
## 'Opslag' is defined as ephemeral grass and forb vegetation associated with rainfall. 
## Opslag was also quantified separately from tree and shrub biomass by Mils and Cowling, 2006.
Biomass <-  cbind.data.frame(
"BiomassC" = c((KPtCO2eha[which(KPtCO2eha$Cover%in%c("Canopy", "Combined")),"Biomass"] - (KPproxies["Biomass"] + KPproxies["Opslag"])), 
(RHtCO2eha$BiomC - RHproxies["BiomC"])), 
"Age" = c(KPtCO2eha[which(KPtCO2eha$Cover%in%c("Canopy","Combined")),"Age"], RHtCO2eha$Age))
## View outputs
## View(Litter)
## View(Soil)
## View(Root)
## View(Biomass)
## Function to get standard deviation
std <- function(x) sd(x)/sqrt(length(x))
### upload the Thicket-wide plot data
twpDF <- read.csv(file = paste(wdir, "twpDF.csv", sep = "/"))[,-1] 
## upload the twp data set without rownames
## load libraries
require(magrittr) 
require(dplyr)
## From the TWP dataset select different planting treatment rows planted in Spekboom habitat and its ecotones on sites where browsing disturbance up to the date of sampling was of medium or less intensity.  
ABDEspekbrowselte3 <- twpDF %>%
  filter(trtmnt %in% c("A", "B", "D", "E", "ABDE"), habitat %in% c("Spek", "EtSpek"), browse <= 3)%>%
  group_by(Plot, ntc, ageYrs) %>%
  summarize("AGBC" = mean(VCU_tCO2ha), "AGBCSE" = std(VCU_tCO2ha), "AGBCrate" = mean(VCU_tCO2ha_rate), "AGBCrateSE" = std(VCU_tCO2ha_rate))
  ##
ABspekbrowselte3 <- twpDF %>%
  filter(trtmnt %in% c("A","B"), habitat %in% c("Spek", "EtSpek"), browse <= 3)%>%
  group_by(Plot, ntc, ageYrs) %>%
  summarize("AGBC" = mean(VCU_tCO2ha), "AGBCSE" = std(VCU_tCO2ha), "AGBCrate" = mean(VCU_tCO2ha_rate), "AGBCrateSE" = std(VCU_tCO2ha_rate))
  ##
Cspekbrowselte3 <- twpDF %>%
  filter(trtmnt %in% c("C"), habitat %in% c("Spek", "EtSpek"), browse <= 3) %>%
  group_by(Plot, ntc, ageYrs) %>%
  summarize("AGBC" = mean(VCU_tCO2ha), "AGBCSE" = std(VCU_tCO2ha), "AGBCrate" = mean(VCU_tCO2ha_rate), "AGBCrateSE" = std(VCU_tCO2ha_rate))
  ##
Aspekbrowselte3 <- twpDF %>%
  filter(trtmnt %in% c("A"), habitat %in% c("Spek", "EtSpek"), browse <= 3) %>%
  group_by(Plot, ntc, ageYrs) %>%
  summarize("AGBC" = mean(VCU_tCO2ha), "AGBCSE" = std(VCU_tCO2ha), "AGBCrate" = mean(VCU_tCO2ha_rate), "AGBCrateSE" = std(VCU_tCO2ha_rate))
  ##
LMNspekbrowselte3 <- twpDF %>%
  filter(trtmnt %in% c("L", "M", "N", "LMN"), habitat %in% c("Spek", "EtSpek"), browse <= 3) %>%
  group_by(Plot, ntc, ageYrs) %>%
  summarize("AGBC" = mean(VCU_tCO2ha), "AGBCSE" = std(VCU_tCO2ha), "AGBCrate" = mean(VCU_tCO2ha_rate), "AGBCrateSE" = std(VCU_tCO2ha_rate))
  ## 
twpDFMns <- twpDF %>%
  filter(trtmnt %in% c("A", "B", "D", "E", "ABDE"), habitat %in% c("Spek", "EtSpek"), browse <= 3)%>%
  group_by(Plot, ntc, ageYrs) %>%
  summarize("AGBC" = mean(VCU_tCO2ha), 
            "AGBCSE" = std(VCU_tCO2ha), 
            "AGBCrate" = mean(VCU_tCO2ha_rate), 
            "AGBCrateSE" = std(VCU_tCO2ha_rate))
## names(twpDF)
twpsum <- twpDF %>%
  filter(habitat %in% c("Spek", "EtSpek") & browse <= 3 & ntc %in% c("A", "B", "C", "D", "E", "L", "M", "N", "LMN")) %>%
  group_by(Plot, ntc, ageYrs) %>%
  summarize("AGBC" = mean(VCU_tCO2ha), 
            "AGBCSE" = std(VCU_tCO2ha), 
            "AGBCrate" = mean(VCU_tCO2ha_rate), 
            "AGBCrateSE" = std(VCU_tCO2ha_rate))
## Get different planting pattern scenarios
ABDEtrtmnt <-  cbind.data.frame("BiomassC" = ABDEspekbrowselte3$AGBC, 
                                "Age" = ABDEspekbrowselte3$ageYrs)
ABtrtmnt <-  cbind.data.frame("BiomassC" = ABspekbrowselte3$AGBC, 
                              "Age" = ABspekbrowselte3$ageYrs)
## Atrtmnt <-  cbind.data.frame("BiomassC" = Aspekbrowselte3$AGBC, "Age" = Aspekbrowselte3$ageYrs)
Ctrtmnt <- cbind.data.frame("BiomassC" = Cspekbrowselte3$AGBC, 
                            "Age" =Cspekbrowselte3$ageYrs)
LMNtrtmnt <- cbind.data.frame("BiomassC" = LMNspekbrowselte3$AGBC, 
                              "Age" = LMNspekbrowselte3$ageYrs)
