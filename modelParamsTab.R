### uncertainty tables
bgmods <- c("sy30lmNL0", "sy50lmNL0", "sy100lmNL0", "r100lmNL", "r500lmNL", "r1000lmNL" )
agmods <- c("litter30lmNL", "litter50lmNL", "biom30lmNL", "biom50lmNL", "ABtrtmntlm", "Ctrtmntlm", "LMNtrtmntlm")
#####
bg_b <- lapply(bgmods, function(x) try(coef(get(x))[1]))
bg_a <- lapply(bgmods, function(x) try(coef(get(x))[2]))
bg_sig <- lapply(bgmods, function(x) try(sigma(get(x))))
bg_SE <- lapply(bgmods, function(x) sigma(get(x))/sqrt(nrow(get(x)$model)))
bg_r2 <- lapply(bgmods, function(x) summary(get(x))$adj.r.squared)
bgtab <- cbind.data.frame(bgmods, unlist(bg_a), unlist(bg_b), unlist(bg_sig), unlist(bg_SE), unlist(bg_r2))
names(bgtab) <- c("mods", "a", "b", "sig", "SE", "r2")
####
modtab <- function(y) { 
## function to extract specific parameters from each model. 
## y is a list of linear model (lm objects) names (as character).
b <- lapply(y, function(x) try(coef(get(x))[1]))
a <- lapply(y, function(x) try(coef(get(x))[2]))
sig <- lapply(y, function(x) try(sigma(get(x))))
SE <- lapply(y, function(x) sigma(get(x))/sqrt(nrow(get(x)$model)))
r2 <- lapply(y, function(x) summary(get(x))$adj.r.squared)
modtab <- cbind.data.frame(y, unlist(a), unlist(b), unlist(sig), unlist(SE), unlist(r2))
names(modtab) <- c("mods", "a", "b", "sig", "SE", "adj.r2")
modtab
}

agtab <- modtab(agmods)
Description <- c("Litter 30yr", "Litter 50yr", "AGBC 30yr", "AGBC 50yr", "Planting1 AGBC 2-5yr", "Planting2 AGBC 2-5yr", "Planting3 AGBC 2-5yr")

agtab[,1] <- Description
names(agtab)[1] <- "Description"
bgtab <- modtab(bgmods)
Description <- c("SOC 0-30cm depth", "SOC 0-50cm depth", "SOC 0-100cm depth", "Root 0-10cm depth", "Root 0-50cm depth", "Root 0-100cm depth")
bgtab[,1] <- Description
names(bgtab)[1] <- "Description"
################
write.csv(bgtab, file = "belowground_Co2Table.csv")
write.csv(agtab, file = "aboveground_Co2Table.csv")
