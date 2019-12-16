#10_allfish analysis
setwd("~/Desktop/Masters Analysis/analysis")
source("EmilyThesis/9_abundances.R")


fish <- read.csv("data/2018fishwbulk.csv")
sitefish <- full_join(sitedata, fish, by = "sitecode.x")

keeppp <- c("creek chub")#underhill removed
#keepsc <- c("AC", "BW", "EP1", "HT", "JW", "LEST", "PF", "PVTF", "SC", "UH", "WA", "WH", "WW")
#removed DE and BW because need to do separately
sitefish <- sitefish[sitefish$species %in% keeppp,]


fitccw <- lm(log(forkmm) ~ agimpact, data=sitefish)
summary(fitccw)
plot(fitccw$resid)
plot(log(sitefish$forkmm)~sitefish$agimpact, ylab="creek chub fork length (mm)",xlab="agricultural gradient")
abline(88.592, -4.043)

fitccw <- lm(forkmm ~ temp, data=sitefish)
summary(fitccw)
plot(fitccw$resid)
plot(sitefish$forkmm~sitefish$temp, ylab="creek chub fork length",xlab="summer water temperature (C)")
abline(111.2272, -1.9561)

fitedible <- lm(stomachTP ~ agimpact, data = sitefish)
summary(fitedible)
plot(sitefish$stomachTP ~ sitefish$agimpact, xlab = "agricultural intensity gradient", ylab = "stomach content trophic position")
abline(1.82803, -0.04496)
boxplot(agimpact ~ stomachTP, data = sitefish, xlab = "agricultural intensity gradient", ylab = "stomach content trophic position")

mod_te<-lm(stomachTP ~ agimpact + forkmm, data = sitefish)
anova(mod_te)
summary(mod_te)

avPlots(mod_te,"forkmm", xlab="Partial Forklength (mm)", ylab="stomach content trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")
avPlots(mod_te,"agimpact", xlab="Partial Agricuture Intensity", ylab="stomach content trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")
