#8_RDAs
setwd("~/Desktop/Masters Analysis/analysis")
source("EmilyThesis/7_invertdiversity.R")

library(tidyverse)
library(car)
library(Hmisc)
library(corrgram)
library ( vegan )

#RDA terrestrial invert
Speciester <- sitedata[,168:192]
#Data were square root transformed
Speciester
Speciester.sq <- sqrt(Speciester)
Speciester.sq

ExplVar <- sitedata[, c("log_p_ag","log_D", "turbidity","buf_width","log_sin")]
terinv_RDA<-rda(Speciester.sq~ log_p_ag+log_D+turbidity+buf_width+ 
                  log_sin,
                data=ExplVar, scale=T) #from vegan package

summary(terinv_RDA)

anova(terinv_RDA, by="axis", permutation=999)
anova(terinv_RDA, by="terms", permutation=999)  ###Sequential significance
anova(terinv_RDA, by="margin", permutation=999)  ###marginal significance

plot(terinv_RDA, scaling=1, main="Site Scaling")

plot(terinv_RDA, scaling=2, main="Species Scaling")

#RDA aquatic inverts, currently doesnt work because of NA values
Speciesaq <- sitedata[c(1:3,5:28),197:217]
#Data were square root transformed
Speciesaq
Speciesaq.sq <- sqrt(Speciesaq)
Speciesaq.sq

ExplVar <- sitedata[c(1:3,5:28), c("log_p_ag","log_D", "turbidity","buf_width","log_sin")]
aqinv_RDA<-rda(Speciesaq.sq~ log_p_ag+log_D+turbidity+buf_width+ 
                 log_sin,
               data=ExplVar, scale=T) #from vegan package

summary(aqinv_RDA)

anova(aqinv_RDA, by="axis", permutation=999)
anova(aqinv_RDA, by="terms", permutation=999)  ###Sequential significance
anova(aqinv_RDA, by="margin", permutation=999)  ###marginal significance

plot(aqinv_RDA, scaling=1, main="Site Scaling") 

#RDA fish
Speciesfish <- sitedata[,39:70]
#Data were square root transformed
Speciesfish
Speciesfish.sq <- sqrt(Speciesfish)
Speciesfish.sq

ExplVar <- sitedata[, c("log_p_ag","log_D", "turbidity","buf_width","log_sin")]
fish_RDA<-rda(Speciesfish.sq~ log_p_ag+log_D+turbidity+buf_width+ 
                 log_sin,
               data=ExplVar, scale=T) #from vegan package

summary(fish_RDA)

anova(fish_RDA, by="axis", permutation=999)
anova(fish_RDA, by="terms", permutation=999)  ###Sequential significance
anova(fish_RDA, by="margin", permutation=999)  ###marginal significance

plot(fish_RDA, scaling=1, main="Site Scaling") 
