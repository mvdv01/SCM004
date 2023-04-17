

## Flow of scripts: sourceData.R ->  soilDepth.R -> rootDepth.R -> modelFuns -> modelParamsTab.R -> uncertainty.R

source("sourceData.R")
source("firstModels.R") ## exploratory
source("soilDepth.R")
source("rootDepth.R")
source("modelFuns.R")
source("modelParamsTab.R") ## writes parameter table for both belowground and aboveground yield models used as .csv files
source("uncertainty.R") ## writes uncertainty discounted table for belowground carbon yields as .csv file
