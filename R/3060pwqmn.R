#3060 pwqmn data
setwd("~/Desktop/Masters Analysis/analysis")

pw <- read.csv("3060siteinfo.csv")
fishspecies <- read.csv("3060speciesinfo.csv")

pw[ pw$Alosa_pseudoharengus == 1, names(pw) == "Alosa_pseudoharengus"] <- 30

