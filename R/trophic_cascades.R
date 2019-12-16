source("R/5_coupling_tp_regressions.R")

# read in fish metadata
fishmeta <- read.csv("data/2018fishmetadata.csv")

# calculate proportion of fish with algae in stomachs
fish %>% transform(stomach = tolower(stomach)) %>%
  transform(algae = ifelse(grepl("algae", stomach, TRUE), 1, 0)) %>%
  group_by(sitecode) %>% 
  mutate(num_cc = n()) %>%
  summarise(num_cc = mean(num_cc), num_with_algae = length(sitecode[algae == 1])) %>%
  mutate(prop_with_algae = num_with_algae/num_cc)




p_algae <- read.csv("data/p_algae.csv")
sitefish <- right_join(sitefish, p_algae, by = "sitecode")

si <- right_join(si, p_algae, by = "sitecode")

# model for terrestrial energy
hist(plogis(si$ter_energy_pp))
mod_prop_ter <- lm(plogis(ter_energy_pp) ~ p_algae + forkmm, data = si)
summary(mod_prop_ter)

# model for trophic position
hist(si$tp_2_pp)
mod_tp <- lm(tp_2_pp ~ p_algae + forkmm, data = si)
summary(mod_tp)
mod_tp <- update(mod_tp, .~. -forkmm)
summary(mod_tp)

sitefish <- right_join(fish, pcascores, by = "sitecode")
keeppp <- c("creek chub")
sitefish <- sitefish[sitefish$species %in% keeppp,]
sitefish <- sitefish %>% mutate (stomach = as.character)



# plots
par(mfrow = c(1,1))
# terrestrial energy
avPlots(mod_prop_ter,"forkmm", xlab="Partial Forklength (mm)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-0.2, 0.1))

avPlots(mod_prop_ter,"p_algae", xlab="proportion of creek chub with algae in stomach", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-0.25, 0.15))

# trophic position
avPlots(mod_tp,"forkmm", xlab="Partial Forklength (mm)", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "grey", bty ="l")

avPlots(mod_tp,"p_algae", xlab="proportion of creek chub with algae in stomach", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")

fishdiv <- read.csv("data/2018mastersanalysis.csv")
sitedata <- left_join(envdata, pcascores, by = "sitecode")
sitedata <- left_join(sitedata, fishmeta, by = "sitecode")
sitedata <- right_join(sitedata,fishdiv)

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

sitedata <- full_join(sitedata, terinv, by = "sitecode")
sitedata <- full_join(sitedata, aqinv, by = "sitecode")

#Calculates diversity 
div_fish <-sitedata[c(1:28),c(139:170)]
sitedata$fish_div<-diversity(div_fish, index = "shannon")
sitedata$fish_div

#evenness
sitedata$fish_even <- sitedata$fish_div/log(specnumber(div_fish))
sitedata$fish_even

sitedata[,139:170] [ sitedata[,139:170] == 0 ] <- NA

sitedata <- left_join(sitedata, p_algae, by = "sitecode")

sitedata$edibles <- sitedata$trichoptera + sitedata$amphipoda + sitedata$diptera + sitedata$ephemeroptera + 
  sitedata$hemiptera + sitedata$plecoptera + sitedata$hirudinia + sitedata$lepidoptera + sitedata$coleoptera + 
  sitedata$isopoda + sitedata$hymenoptera + sitedata$megaloptera + sitedata$odonata

sitedata$terpred <- sitedata$p_aranae + sitedata$p_hymenoptera + sitedata$p_opiliones + sitedata$p_odonata + 
  sitedata$p_acari + sitedata$p_neuroptera + sitedata$p_mantodea + sitedata$p_mecoptera

sitedata$aqpred <- sitedata$oligochaeta + sitedata$hydrachnidia + sitedata$hirudinia + sitedata$megaloptera + 
  sitedata$odonata
sitedata <- right_join(sitedata, pcascores, by = "sitecode")

fitcc <- lm(log(creek.chub) ~PC2.y, data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(log(sitedata$creek.chub)~sitedata$PC1.y, ylab="log creek chub abundance",xlab="PC2")
#abline(2.638262, 0.007632)
#

fitalg <- lm(p_algae ~ PC2.y, data = sitedata)
summary(fitalg)
plot(sitedata$p_algae ~ sitedata$PC2.y)

fittotalaq <- lm((creek.chub/(edibles/totalcount.y))~PC1.y, data=sitedata)
summary(fittotalaq)
plot(fittotalaq$resid)
plot((sitedata$creek.chub/(sitedata$edibles/sitedata$totalcount.y))~sitedata$PC1.y, ylab="creek chub: edible aquatic invertebrate proportion",xlab="agricultural impact gradient")
abline(116.07,-83.52)

fitpred <- lm((terpred/100) ~ PC1.y, data = sitedata)
summary(fitpred)
plot((sitedata$terpred/100) ~ sitedata$PC1.y, xlab = "agricultural intensity gradient", ylab = "proportion of predatory terrestrial invertebrates")

si <- right_join(si, sitedata)

#terrestrial predator model
# model for terrestrial energy
hist(plogis(si$ter_energy_pp))
mod_prop_ter <- lm(plogis(ter_energy_pp) ~ terpred + forkmm, data = si)
summary(mod_prop_ter)

# model for trophic position
hist(si$tp_2_pp)
mod_tp <- lm(tp_2_pp ~ terpred + forkmm, data = si)
summary(mod_tp)
mod_tp <- update(mod_tp, .~. -forkmm)
summary(mod_tp)


# plots
par(mfrow = c(1,1))
# terrestrial energy
avPlots(mod_prop_ter,"forkmm", xlab="Partial Forklength (mm)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-0.2, 0.1))

avPlots(mod_prop_ter,"terpred", xlab="proportion terrestrial predatory insects", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-0.25, 0.15))

# trophic position
avPlots(mod_tp,"forkmm", xlab="Partial Forklength (mm)", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "grey", bty ="l")

avPlots(mod_tp,"terpred", xlab="proportion terrestrial predatory insects", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")

#model for cc:edible invert ratio
# model for terrestrial energy
hist(plogis(si$ter_energy_pp))
mod_prop_ter <- lm(plogis(ter_energy_pp) ~ (creek.chub/(edibles/totalcount.y)) + forkmm, data = si)
summary(mod_prop_ter)

# model for trophic position
hist(si$tp_2_pp)
mod_tp <- lm(tp_2_pp ~ (creek.chub/(edibles/totalcount.y)) + forkmm, data = si)
summary(mod_tp)
mod_tp <- update(mod_tp, .~. -forkmm)
summary(mod_tp)


# plots
par(mfrow = c(1,1))
# terrestrial energy
avPlots(mod_prop_ter,"forkmm", xlab="Partial Forklength (mm)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-0.2, 0.1))

avPlots(mod_prop_ter,"(creek.chub/(edibles/totalcount.y))", xlab="cc:edible aq insects", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-0.25, 0.15))

# trophic position
avPlots(mod_tp,"forkmm", xlab="Partial Forklength (mm)", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "grey", bty ="l")

avPlots(mod_tp,"(creek.chub/(edibles/totalcount.y))", xlab="cc:edible aq insects", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")
