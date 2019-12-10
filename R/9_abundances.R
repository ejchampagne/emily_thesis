#9_abundances
setwd("~/Desktop/Masters Analysis/analysis")
source("EmilyThesis/7_invertdiversity.R")

library(tidyverse)
library(car)
library(Hmisc)
library(corrgram)
library ( vegan )

plot(sitedata$creek.chub~sitedata$agimpact)
plot(sitedata$creek.chub~sitedata$PC1)

sitedata$edibles <- sitedata$trichoptera + sitedata$amphipoda + sitedata$diptera + sitedata$ephemeroptera + 
  sitedata$hemiptera + sitedata$plecoptera + sitedata$hirudinia + sitedata$lepidoptera + sitedata$coleoptera + 
  sitedata$isopoda + sitedata$hymenoptera + sitedata$megaloptera + sitedata$odonata

sitedata$terpred <- sitedata$p_aranae + sitedata$p_hymenoptera + sitedata$p_opiliones + sitedata$p_odonata + 
  sitedata$p_acari + sitedata$p_neuroptera + sitedata$p_mantodea + sitedata$p_mecoptera

sitedata$tercon <- sitedata$p_hemiptera + sitedata$p_coleoptera + sitedata$p_lepidoptera + 
  sitedata$p_orthoptera + sitedata$p_diplopoda + sitedata$p_psocoptera + sitedata$pphasmatodea + 
  sitedata$p_ephemeroptera + sitedata$p_thysanoptera

sitedata$terom <- sitedata$p_p_diptera + sitedata$p_stylommatophora + sitedata$p_trichoptera + 
  sitedata$p_dermaptera + sitedata$p_collembola + sitedata$p_isopoda + sitedata$p_plecoptera

sitedata$aqpred <- sitedata$oligochaeta + sitedata$hydrachnidia + sitedata$hirudinia + sitedata$megaloptera + 
  sitedata$odonata

sitedata$aqcon <- sitedata$gastropoda + sitedata$ephemeroptera + sitedata$coleoptera + sitedata$unionidae + 
  sitedata$nematoda + sitedata$psocoptera + sitedata$lepidoptera

sitedata$aqom <- sitedata$trichoptera + sitedata$amphipoda + sitedata$diptera + sitedata$hemiptera + 
  sitedata$plecoptera + sitedata$isopoda + sitedata$hymenoptera

sitedata[,168:192] [ sitedata[,168:192] == 0 ] <- NA
sitedata[,197:217] [ sitedata[,197:217] == 0 ] <- NA
#sitedata[c(2,10,13),195:218]  <- NA
sitedata[,39:70] [ sitedata[,39:70] == 0 ] <- NA

#keep <- c("AC", "AT", "BG",  "EP1", "EP2", "EP3", "EP4", "FP", "GF", "HC", "HT", "JC", "JW", "KC", "LEF", "LEST", "MT", "PF", "PVTF", "SC", "UH", "VN", "WA", "WH", "WW")
#removed DE and BW because need to do separately
#sitedata <- sitedata[sitedata$sitecode.x %in% keep,]


fitcc <- lm(log(creek.chub) ~ july.nitrogen, data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(log(sitedata$creek.chub)~sitedata$july.nitrogen, ylab="creek chub abundance",xlab="july nitrogen")
#abline(53.251, 10.791)

fitcc <- lm(log(creek.chub) ~ agimpact, data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(log(sitedata$creek.chub)~sitedata$agimpact, ylab="log creek chub abundance",xlab="agricultural impact gradient")
#abline(3.3976, 0.2442)

fitcc <- lm(log(creek.chub) ~ log(july.phosphorus), data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(log(sitedata$creek.chub)~log(sitedata$july.phosphorus), ylab="log creek chub abundance",xlab="log july phosphorus")
#abline(4.07836, -0.01347)

fitcc <- lm(log(creek.chub) ~ log(buf_width), data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(log(sitedata$creek.chub)~log(sitedata$buf_width), ylab="log creek chub abundance",xlab="log buffer width(m)")
#abline(4.07836, -0.01347)

fitcc <- lm(log(creek.chub) ~ edibles, data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(log(sitedata$creek.chub)~sitedata$edibles, ylab="log creek chub abundance",xlab="edible aquatic insect abundance")
abline(2.638262, 0.007632)
#

fitcc <- lm(log(creek.chub) ~ agimpact, data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(log(sitedata$creek.chub)~sitedata$agimpact, ylab="log creek chub abundance",xlab="prey fish abundance")
#abline(2.638262, 0.007632)
#
sitedata$cc_inv <- sitedata$creek.chub/sitedata$totalcount.y

keep <- c("AC", "AT", "BG",  "EP1", "EP2", "EP3", "EP4", "FP", "GF", "HC", "HT", "JC", "JW", "KC", "LEF", "LEST", "MT", "PF", "PVTF", "SC", "UH", "VN", "WA", "WH", "WW")
#removed DE and BW because need to do separately
sitedata1 <- sitedata[sitedata$sitecode.x %in% keep,]

fitcc <- lm(log10(cc_inv) ~ agimpact, data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(log10(sitedata$cc_inv)~sitedata$agimpact, ylab="creek chub to aq invert ratio",xlab="agricultural impact gradient")
abline(-6.0030, 0.9572)

fitcc <- lm((totalcount.y/algae_cover) ~ agimpact, data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot((sitedata$totalcount.y/sitedata$algae_cover)~sitedata$agimpact, ylab= "aq invert to algae cover ratio",xlab="agricultural impact gradient")
abline(-6.0030, 0.9572)

fitcc <- lm(algae_cover ~ agimpact, data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(sitedata$algae_cover~sitedata$agimpact, ylab= "percent algae cover",xlab="agricultural impact gradient")
abline(-6.0030, 0.9572)

fitcc <- lm(qlogis(creek.chub/totalcount.y) ~ log(july.phosphorus), data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(qlogis(sitedata$creek.chub/sitedata$totalcount.y)~log(sitedata$july.phosphorus), ylab="creek chub to aq invert ratio",xlab="log july total phosphorus")
#abline(3.3976, 0.2442)

fitcc <- lm(qlogis(creek.chub/totalcount.y) ~ july.nitrogen, data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(qlogis(sitedata$creek.chub/sitedata$totalcount.y)~sitedata$july.nitrogen, ylab="creek chub to aq invert ratio",xlab="july total nitrogen")
#abline(3.3976, 0.2442)

fittotalaq <- lm(log(totalcount.y)~agimpact, data=sitedata)
summary(fittotalaq)
plot(fittotalaq$resid)
plot(log(sitedata$totalcount.y)~sitedata$agimpact, ylab="log aquatic invertebrate abundance",xlab="agricultural impact gradient")
abline(2.78181,0.20884)

fittotalaq <- lm((edibles/totalcount.y)~agimpact, data=sitedata)
summary(fittotalaq)
plot(fittotalaq$resid)
plot((sitedata$edibles/sitedata$totalcount.y)~sitedata$agimpact, ylab="log aquatic invertebrate abundance",xlab="agricultural impact gradient", 
     ylim = c(0, 1.2))
abline(1.11509,-0.05093)

fittotalaq <- lm(log(totalcount.y)~rip_tree_cover, data=sitedata)
summary(fittotalaq)
plot(fittotalaq$resid)
plot(log(sitedata$totalcount.y)~sitedata$rip_tree_cover, ylab="log aquatic invertebrate abundance",xlab="percent canopy cover")
abline(4.817509, -0.019984)

fittotalaq <- lm(log(totalcount.y)~buf_width, data=sitedata)
summary(fittotalaq)
plot(fittotalaq$resid)
plot(log(sitedata$totalcount.y)~sitedata$buf_width, ylab="log aquatic invertebrate abundance",xlab="buffer width (m)")
abline(3.879455, -0.001547)

fittotalaq <- lm(log(totalcount.y)~july.nitrogen, data=sitedata)
summary(fittotalaq)
plot(fittotalaq$resid)
plot(log(sitedata$totalcount.y)~sitedata$july.nitrogen, ylab="log aquatic invertebrate abundance",xlab="july total nitrogen")
#abline(4.817509, -0.019984)

fittotalaq <- lm(log(totalcount.y)~log(july.phosphorus), data=sitedata)
summary(fittotalaq)
plot(fittotalaq$resid)
plot(log(sitedata$totalcount.y)~log(sitedata$july.phosphorus), ylab="log aquatic invertebrate abundance",xlab="log july total phosphorus")
abline(4.7951, 0.3365)



fitn <- lm(july.nitrogen~agimpact, data = sitedata)
summary(fitn)
plot(fitn$resid)
plot(sitedata$july.nitrogen~sitedata$agimpact, ylab="July total nitrogen",xlab="agricultural impact gradient", ylim = c(-1,17))
abline(-1.8045,1.4437)

fitn <- lm(july.nitrogen~buf_width, data = sitedata)
summary(fitn)
plot(fitn$resid)
plot(sitedata$july.nitrogen~sitedata$buf_width, ylab="July total nitrogen",xlab="agricultural impact gradient")

fitn <- lm(log(july.phosphorus)~agimpact, data = sitedata)
summary(fitn)
plot(fitn$resid)
plot(log(sitedata$july.phosphorus)~sitedata$agimpact, ylab="log July total Phosphorus",xlab="agricultural impact gradient", ylim = c(-5, 0))
abline(-4.04136,0.20917)

fitn <- lm(log(july.phosphorus)~buf_width, data = sitedata)
summary(fitn)
plot(fitn$resid)
plot(log(sitedata$july.phosphorus)~sitedata$buf_width, ylab="July total Phosphorus",xlab="buffer width")
abline(-4.0198,0.2241)

fitn <- lm(agimpact ~ log(july.phosphorus) + july.nitrogen, data = sitedata)
summary(fitn)
anova(fitn)
plot(fitn$resid)
plot(log(sitedata$july.phosphorus) + sitedata$july.nitrogen ~sitedata$agimpact, ylab="July total Phosphorus",xlab="agricultural impact gradient")
abline(-4.0198,0.2241)

fitcas <- aov(agimpact ~ algae_cover2, data = sitedata)
summary(fitcas)
plot(fitcas$resid)
plot(sitedata$algae_cover2~sitedata$agimpact, ylab="may chlorophyll",xlab="agricultural impact gradient")
boxplot(agimpact ~ algae_cover2, data = sitedata, order = c("low", "med", "high"))
TukeyHSD(fitcas)

sitedata$inv_alg <- sitedata$totalcount.y/sitedata$algae_cover2

fitcas <- lm(agimpact ~ totalcount.y/algae_cover2, data = sitedata)
summary(fitcas)
plot(fitcas$resid)
plot(sitedata$totalcount.y/sitedata$algae_cover2~sitedata$agimpact, ylab="algae coverage",xlab="agricultural impact gradient")
abline(3.9121494,0.07814)

fitedible <- lm(buf_width ~ agimpact, data = sitedata)
summary(fitedible)
plot(sitedata$buf_width ~ sitedata$agimpact, ylab = "buffer width", xlab = "agricultural intensity gradient")
abline(326.939, -42.584)


fitedible <- lm((terpred/100) ~ agimpact, data = sitedata)
summary(fitedible)
plot((sitedata$terpred/100) ~ sitedata$agimpact, xlab = "agricultural intensity gradient", ylab = "proportion of predatory terrestrial invertebrates")

fitedible <- lm((aqpred/totalcount.y) ~ agimpact, data = sitedata)
summary(fitedible)
plot((sitedata$aqpred/sitedata$totalcount.y) ~ sitedata$agimpact, xlab = "agricultural intensity gradient")

fitedible <- lm(debris_cover ~ agimpact, data = sitedata)
summary(fitedible)
plot(sitedata$debris_cover ~ sitedata$agimpact, xlab = "agricultural intensity gradient", ylab = "percent organic debris cover")
boxplot(agimpact ~ debris_cover, data = sitedata, xlab = "agricultural intensity gradient", ylab = "percent organic debris cover")

