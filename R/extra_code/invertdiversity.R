#7_invertdiversity
# create the full sitedata dataset
setwd("~/Desktop/Masters Analysis/analysis")

library(tidyverse)
library(car)
library(Hmisc)
library(corrgram)
library ( vegan )

pca <- read.csv("data/2018mastersanalysis.csv")
fishmeta <- read.csv("data/2018fishmetadata.csv")
colnames(fishmeta)[1] <- "sitecode.x"
envdata <- read.csv("data/2018PCAaggrad.csv")

sitedata <- left_join(envdata, pca, by = "sitecode.x")
sitedata <- left_join(sitedata, fishmeta, by = "sitecode.x")

sitedata$log_p_ag <- log10(sitedata$p_ag_watershed)
sitedata$log_sal <- log10(sitedata$salinity)
sitedata$log_sin <- log10(sitedata$sinuosity)
sitedata$log_D <- log10(sitedata$discharge)
sitedata$log_grain <- log10(sitedata$grain_size)
sitedata$log_drainarea <- log(sitedata$drainage_area)


pcadata = select(sitedata,c("sitecode.x","log_p_ag","log_D", "turbidity","buf_width","log_sin"))
rownames(pcadata) = pcadata$sitecode.x
pcadata = select(pcadata,-"sitecode.x")

corrgram(pcadata, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt)

pca = prcomp(pcadata,scale = TRUE)
biplot(pca)
pca$x
summary(pca)

pcascores = as.data.frame(pca$x)
pcascores$sitecode.x = rownames(pcascores)
rownames(pcascores) = NULL
pcascores = select(pcascores,c("sitecode.x", "PC1", "PC2"))

sitedata <- right_join(sitedata,pcascores)
plot(sitedata$creek.chub~sitedata$PC1)
sitedata$agimpact = sitedata$PC1*-1
plot(sitedata$creek.chub~sitedata$agimpact)
sitedata$agimpact = sitedata$agimpact+5
plot(sitedata$creek.chub~sitedata$agimpact)
plot(sitedata$creek.chub~log(sitedata$agimpact))


#min(sitedata$agimpact_o)
#sitedata$agimpact = sitedata$agimpact_o+4.871
#sitedata$agimpact_p = sitedata$agimpact+4.870377
#plot(sitedata$creek.chub~sitedata$agimpact_o)
#plot(sitedata$creek.chub~sitedata$agimpact)

terinv <- read.csv("data/ter_invert18.csv")

#Calculates diversity 
div_terinv <-terinv[c(1:28),c(4:28)]
terinv$ter_div<-diversity(div_terinv, index = "shannon")
terinv$ter_div
#evenness
terinv$ter_even <- terinv$ter_div/log(specnumber(div_terinv))
terinv$ter_even


aqinv <- read.csv("data/aq_invert18.csv")

#Calculates diversity 
div_aqinv <-aqinv[c(1:27),c(4:24)]
aqinv$aq_div<-diversity(div_aqinv, index = "shannon")
aqinv$aq_div

#evenness
aqinv$aq_even <- aqinv$aq_div/log(specnumber(div_aqinv))
aqinv$aq_even

sitedata <- full_join(sitedata, terinv, by = "sitecode.x")
sitedata <- full_join(sitedata, aqinv, by = "sitecode.x")

#Calculates diversity 
div_fish <-sitedata[c(1:28),c(39:70)]
sitedata$fish_div<-diversity(div_fish, index = "shannon")
sitedata$fish_div

#evenness
sitedata$fish_even <- sitedata$fish_div/log(specnumber(div_fish))
sitedata$fish_even


fit4 <- lm(fish_div~agimpact, data=sitedata)
summary(fit4)
plot(fit4$resid)
plot(sitedata$fish_div~sitedata$agimpact, ylab="fish community shannon diversity",xlab="agricultural impact gradient")
#abline(1.10708, -0.05942)


fit4 <- lm(aq_div~agimpact, data=sitedata)
summary(fit4)
plot(fit4$resid)
plot(sitedata$aq_div~sitedata$agimpact, ylab="aquatic invertebrate order diversity",xlab="agricultural impact gradient")
#abline(1.270065,0.004178)

fit5 <- lm(ter_div~agimpact, data=sitedata)
summary(fit5)
plot(fit5$resid)
plot(sitedata$ter_div~sitedata$agimpact, ylab="terrestrial invertebrate order diversity",xlab="agricultural impact gradient")
#abline(1.850538, 0.004968)

#fitrich <- lm(species_richness~agimpact, data=sitedata)
#summary(fitrich)
#plot(sitedata$species_richness~sitedata$agimpact, ylab="fish species richness",xlab="agricultural impact gradient")
#f <- ggplot(sitedata, aes(species_richness, agimpact)) + geom_boxplot() + coord_flip()
#f


#fitrich <- lm(species_richness~log_grain, data=sitedata)
#summary(fitrich)
#plot(sitedata$species_richness~sitedata$log_grain, ylab="fish species richness",xlab="log average grain size")
#abline(5.3551,1.2148)
#**

##### 
#fish div by abiotic
fitfishdiv <- lm(fish_div~log_grain, data=sitedata)
summary(fitfishdiv)
plot(sitedata$fish_div~sitedata$log_grain, ylab="fish shannon diversity",xlab="log average grain size")
abline(1.13471,0.20037)
#*

fitfishdiv <- lm(fish_div~log_p_ag, data=sitedata)
summary(fitfishdiv)
plot(sitedata$fish_div~sitedata$log_p_ag, ylab="fish shannon diversity",xlab="log agriculture")


fitfishdiv <- lm(fish_div~log_drainarea, data=sitedata)
summary(fitfishdiv)
plot(sitedata$fish_div~sitedata$log_drainarea, ylab="fish shannon diversity",xlab="log drainage area")
abline(0.5328,0.2294)
#***

fitfishdiv <- lm(fish_div~valley_slope, data=sitedata)
summary(fitfishdiv)
plot(sitedata$fish_div~sitedata$valley_slope, ylab="fish shannon diversity",xlab="valley slope")
abline(0.88705,-0.03278)
#.

fitfishdiv <- lm(fish_div~rip_tree_cover, data=sitedata)
summary(fitfishdiv)
plot(sitedata$fish_div~sitedata$rip_tree_cover, ylab="fish shannon diversity",xlab="percent tree cover")


fitfishdiv <- lm(fish_div~dissolved_O2, data=sitedata)
summary(fitfishdiv)
plot(sitedata$fish_div~sitedata$dissolved_O2, ylab="fish shannon diversity",xlab="dissolved oxygen")
abline(0.922789,0.014801)
#.

fitfishdiv <- lm(fish_div~temp, data=sitedata)
summary(fitfishdiv)
plot(sitedata$fish_div~sitedata$temp, ylab="fish shannon diversity",xlab="summer stream temperature (C)")
abline(-0.54995,0.08169)
#**

fitab <- lm(temp ~ order, data=sitedata)
summary(fitfishdiv)
plot(sitedata$temp~sitedata$order, xlab="stream order",ylab="summer stream temperature (C)")

fitfishdiv <- lm(fish_div~max_depth, data=sitedata)
summary(fitfishdiv)
plot(sitedata$fish_div~sitedata$max_depth, ylab="fish shannon diversity",xlab="stream max channel depth")
#**

fitfishdiv <- lm(fish_div~log(w_width), data=sitedata)
summary(fitfishdiv)
plot(sitedata$fish_div~log(sitedata$w_width), ylab="fish shannon diversity",xlab="log summer stream width")
abline(0.4615,0.5050)
#**

fitfishdiv <- lm(fish_div~buf_width, data=sitedata)
summary(fitfishdiv)
plot(sitedata$fish_div~sitedata$buf_width, ylab="fish shannon diversity",xlab="buffer width")
#abline(0.968762,0.001190)

##### 
#ter invert div vs abiotic


fitterdiv <- lm(ter_div~rip_tree_cover, data=sitedata)
summary(fitterdiv)
plot(sitedata$ter_div~sitedata$rip_tree_cover, ylab="terrestrial invert shannon diversity",xlab="percent tree canopy coverage")
abline(2.012570,-0.003229)
#.

fitterdiv <- lm(ter_div~buf_width, data=sitedata)
summary(fitterdiv)
plot(sitedata$ter_div~sitedata$buf_width, ylab="terrestrial invert shannon diversity",xlab="buffer width")
#abline(1.8363241,0.0001247)

#####
# aq invert div vs abiotic

fitaqdiv <- lm(aq_div~rip_tree_cover, data=sitedata)
summary(fitaqdiv)
plot(sitedata$aq_div~sitedata$rip_tree_cover, ylab="aquatic invert diversity",xlab="percent tree canopy coverage")
abline(1.676482, -0.007812)
#*

fitaqdiv <- lm(aq_div~pH, data=sitedata)
summary(fitaqdiv)
plot(sitedata$aq_div~sitedata$pH, ylab="aquatic invert diversity",xlab="pH")
abline(-2.9264, 0.4815)
#.

fitaqdiv <- lm(aq_div~buf_width, data=sitedata)
summary(fitaqdiv)
plot(sitedata$aq_div~sitedata$buf_width, ylab="aquatic invert diversity",xlab="buffer width")
#abline(1.3576418, -0.0007372)
