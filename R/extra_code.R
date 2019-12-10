




pca <- read.csv("data/2018mastersanalysis.csv")
fish <- read.csv("data/2018fishwbulk.csv")
fishmeta <- read.csv("data/2018fishmetadata.csv")


#si <- left_join(si, pca, by = "sitecode.x")
#si <- left_join(si, fish, by = "sampleid")
#colnames(si)[colnames(si)=="sitecode.x.x"] <- "sitecode.x"
#si <- left_join(si, fishmeta, by = "sitecode.x")
#si <- left_join(si, envdata, by = "sitecode.x")




#join terinv and aqinv data to si data
dat <- full_join(si, terinv, by = "sitecode.x")
dat <- full_join(si, aqinv, by = "sitecode.x")

#si[,228:252] [ si[,228:252] == 0 ] <- NA
#si[,257:278] [ si[,257:278] == 0 ] <- NA
#si[,56:87] [ si[,56:87] == 0 ] <- NA

si <- select(si,-c(X.x,old.sampleid.x, replicate,old.sampleid.y, sitecode.y, species.y,role.y, 
                   pc1, adj_pc1, pc2, adj_pc2, sitename.x.x, date, reachdistance, volts, hertz, pass1start,
                   pass1end, pass2start, pass2end, pass3start, pass3end, totaltime, X.y, mt_setdate, 
                   mt_settime, mt_enddate, mt_endtime, uptrap, midtrap, downtrap, mt_notes, sitename.x.x.x, 
                   sitename.y.y.y, sitecode.x.y, sitename.y))


agpca <- read.csv("data/ag_pca_scores.csv")



















plot(si$ter_energy_pp, si$ter_energy_sc)



plot(si$tp_2_pp ~ si$PC1)

mod_te<-lm(ter_energy_sc ~ PC1 + forkmm, data = si)
anova(mod_te)
summary(mod_te)

par(mfrow=c(1,1))
avPlots(mod_te,"forkmm", xlab="Partial Forklength (mm)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")
avPlots(mod_te,"PC1", xlab="Partial Agricuture Intensity", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")


mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ temp + forkmm, data = si)
anova(mod_te)
summary(mod_te)

avPlots(mod_te,"forkmm", xlab="Partial Forklength (mm)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")
avPlots(mod_te,"temp", xlab="summer water temperature", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ buf_width + forkmm, data = si)
anova(mod_te)
summary(mod_te)

avPlots(mod_te,"forkmm", xlab="Partial Forklength (mm)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")
avPlots(mod_te,"buf_width", xlab="buffer width (m)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "grey", bty ="l")

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ july.nitrogen + forkmm, data = si)
anova(mod_te)
summary(mod_te)

avPlots(mod_te,"forkmm", xlab="Partial Forklength (mm)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-9, 7))
avPlots(mod_te,"july.nitrogen", xlab="Partial July nitrogen", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "grey", bty ="l", ylim = c(-9, 7))

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ log(july.phosphorus) + forkmm, data = si)
anova(mod_te)
summary(mod_te)

avPlots(mod_te,"forkmm", xlab="Partial Forklength (mm)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-9, 7))
avPlots(mod_te,"log(july.phosphorus)", xlab="Partial log July phosphorus", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "grey", bty ="l", ylim = c(-9, 7))

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ totalcount.y + forkmm, data = si)
anova(mod_te)
summary(mod_te)

avPlots(mod_te,"forkmm", xlab="Partial Forklength (mm)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")
avPlots(mod_te,"totalcount.y", xlab="aquatic invertebrate abundance", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "grey", bty ="l")

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ (aqpred/totalcount.y) + forkmm, data = si)
anova(mod_te)
summary(mod_te)

avPlots(mod_te,"forkmm", xlab="Partial Forklength (mm)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")
avPlots(mod_te,"(aqpred/totalcount.y)",  ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "grey", bty ="l")

mod_tp<-lm(tp_2_pp ~ PC1 + forkmm, data = si)
anova(mod_tp)
summary(mod_tp)

avPlots(mod_tp,"forkmm", xlab="Partial Forklength (mm)", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")

avPlots(mod_tp,"PC1", xlab="Partial Agriculture Intensity", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "grey", bty ="l")


mod_tp<-lm(tp_2_pp ~ temp + forkmm, data = si)
anova(mod_tp)
summary(mod_tp)

avPlots(mod_tp,"forkmm", xlab="Partial Forklength (mm)", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "white", bty ="l")

avPlots(mod_tp,"temp", xlab="partial summer temperature (C)", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "white", bty ="l")

mod_tp<-lm(tp_2_pp ~ buf_width + forkmm, data = si)
anova(mod_tp)
summary(mod_tp)

avPlots(mod_tp,"forkmm", xlab="Partial Forklength (mm)", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")

avPlots(mod_tp,"buf_width", xlab="buffer width (m)", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")

mod_tp<-lm(tp_2_pp ~ july.nitrogen + forkmm, data = si)
anova(mod_tp)
summary(mod_tp)

avPlots(mod_tp,"forkmm", xlab="Partial Forklength (mm)", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-2, 3))

avPlots(mod_tp,"july.nitrogen", xlab="Partial July nitrogen", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-2, 3))

mod_tp<-lm(tp_2_pp ~ log(july.phosphorus) + forkmm, data = si)
anova(mod_tp)
summary(mod_tp)

avPlots(mod_tp,"forkmm", xlab="Partial Forklength (mm)", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-2, 2))

avPlots(mod_tp,"log(july.phosphorus)", xlab="Partial log July phosphorus", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-2, 2))

mod_tp<-lm(tp_2_pp ~ aqpred/totalcount.y + forkmm, data = si)
anova(mod_tp)
summary(mod_tp)

avPlots(mod_tp,"forkmm", xlab="Partial Forklength (mm)", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")
avPlots(mod_tp,"aqpred/totalcount.y",  ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "grey", bty ="l")

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ log_p_ag, data = si)
anova(mod_te)
summary(mod_te)
plot(si$log_p_ag, qlogis(si$ter_energy_pp_0_1))
abline(1.8092, -4.7196)

mod_te<-lm(tp_2_pp ~ log_p_ag, data = si)
anova(mod_te)
summary(mod_te)
plot(si$log_p_ag, si$tp_2_pp)
abline(3.0495, -1.6379)

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ PC1, data = si)
anova(mod_te)
summary(mod_te)
plot(si$PC1, qlogis(si$ter_energy_pp_0_1))
abline(5.5925, -0.6108) #*
abline(4.7279, -0.4600) #***

mod_te<-lm(tp_2_pp ~ PC1, data = si)
anova(mod_te)
summary(mod_te)
plot(si$PC1, si$tp_2_pp)
abline(4.06617, -0.18577)#*
abline(2.643557, 0.003580)#.


mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ log(diptera), data = si)
anova(mod_te)
summary(mod_te)
plot(log(si$diptera), qlogis(si$ter_energy_pp_0_1))
#abline(2.5388, -0.6108)


mod_te<-lm(tp_2_pp ~ log(p_p_diptera), data = si)
anova(mod_te)
summary(mod_te)
plot(log(si$p_p_diptera), si$tp_2_pp)
abline(3.55831, -0.13703)
#*

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ (aqpred/totalcount.y), data = si)
anova(mod_te)
summary(mod_te)
plot((si$aqpred/si$totalcount.y), qlogis(si$ter_energy_pp_0_1))
#abline(2.5388, -0.6108)


mod_te<-lm(tp_2_pp ~ aqpred/totalcount.y, data = si)
anova(mod_te)
summary(mod_te)
plot(si$aqpred/si$totalcount.y, si$tp_2_pp)
abline(3.1922233, -0.1637264)
#**

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ terpred, data = si)
anova(mod_te)
summary(mod_te)
plot(si$terpred, qlogis(si$ter_energy_pp_0_1))
abline(1.47931, 0.03639)


mod_te<-lm(tp_2_pp ~ terpred, data = si)
anova(mod_te)
summary(mod_te)
plot(si$terpred, si$tp_2_pp)
abline(2.630314, 0.016592)
#**

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ qlogis(creek.chub/totalcount.y), data = si)
anova(mod_te)
summary(mod_te)
plot(qlogis(si$creek.chub/si$totalcount.y), qlogis(si$ter_energy_pp_0_1))
abline(1.5835, -1.0177) #***

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ edibles, data = si)
anova(mod_te)
summary(mod_te)
plot(si$edibles, qlogis(si$ter_energy_pp_0_1))
#abline(1.8092, -4.7196)


mod_te<-lm(tp_2_pp ~ qlogis(creek.chub/totalcount.y), data = si)
anova(mod_te)
summary(mod_te)
plot(qlogis(si$creek.chub/si$totalcount.y), si$tp_2_pp)
abline(2.94763, -0.25366)
#**
mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ stomachTP, data = si)
anova(mod_te)
summary(mod_te)
plot(si$stomachTP, qlogis(si$ter_energy_pp_0_1))
abline()
#**

mod_tp<-lm(tp_2_pp ~ stomachTP, data = si)
anova(mod_tp)
summary(mod_tp)
boxplot(tp_2_pp ~ stomachTP, data=si, xlab = "stomach content trophic position", ylab = "trophic position")

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ stomachTP, data = si)
anova(mod_te)
summary(mod_te)
boxplot(qlogis(ter_energy_pp_0_1) ~ stomachTP, data=si, xlab = "stomach content trophic position", ylab = "logit percent terrestrial coupling")

mod_tp<-lm(tp_2_pp ~ debris_cover, data = si)
anova(mod_tp)
summary(mod_tp)
boxplot(tp_2_pp ~ debris_cover, data=si, xlab = "percent organic debris cover", ylab = "trophic position")

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ debris_cover, data = si)
anova(mod_te)
summary(mod_te)
boxplot(qlogis(ter_energy_pp_0_1) ~ debris_cover, data=si, xlab = "percent organic debris cover", ylab = "logit percent terrestrial coupling")

mod_te<-lm(tp_2_pp ~ p_coleoptera, data = si)
anova(mod_te)
summary(mod_te)
plot(si$p_coleoptera, si$tp_2_pp)
abline(3.373918, -0.022123)
#***

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ buf_width, data = si)
anova(mod_te)
summary(mod_te)
plot(si$buf_width, qlogis(si$ter_energy_pp_0_1))
abline(1.828911, 0.004894)

mod_te<-lm(tp_2_pp ~ buf_width, data = si)
anova(mod_te)
summary(mod_te)
plot(si$buf_width, si$tp_2_pp, ylab = "trophic position", xlab = "buffer width")
abline(2.319539, 0.007326)

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ log(july.phosphorus), data = si)
anova(mod_te)
summary(mod_te)
plot(log(si$july.phosphorus), qlogis(si$ter_energy_pp_0_1))
abline(1.828911, 0.004894)
abline(1.3026, -0.3873) #.

mod_te<-lm(tp_2_pp ~ log(july.phosphorus), data = si)
anova(mod_te)
summary(mod_te)
plot(log(si$july.phosphorus), si$tp_2_pp, ylab = "trophic position", xlab = "July phosphorus")
abline(3.21133, -0.03005)

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ july.nitrogen, data = si)
anova(mod_te)
summary(mod_te)
plot(si$july.nitrogen, qlogis(si$ter_energy_pp_0_1))
#abline(1.828911, 0.004894)
abline(3.02080, -0.12059) #*

mod_te<-lm(tp_2_pp ~ july.nitrogen, data = si)
anova(mod_te)
summary(mod_te)
plot(si$july.nitrogen, si$tp_2_pp, ylab = "trophic position", xlab = "July nitrogen")
abline(3.21133, -0.03005)
abline(3.17630, -0.02628) #.

lm(tp_2_pp ~ forkmm, data = si)


mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ log_D + turbidity + log_p_ag + log_grain + forkmm + dissolved_O2 + rip_tree_cover + rip_shrub_cover + rip_grass_cover + algae_cover, data = si)
anova(mod_te)
summary(mod_te)
mod_te1<-update(mod_te, .~. -rip_grass_cover)
anova(mod_te1)
mod_te2<-update(mod_te1, .~. -log_p_ag)
anova(mod_te2)
mod_te3<-update(mod_te2, .~. -algae_cover)
anova(mod_te3)
mod_te4<-update(mod_te3, .~. -dissolved_O2)
anova(mod_te4)
mod_te5<-update(mod_te4, .~. -rip_tree_cover)
anova(mod_te5)
summary(mod_te5)

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ log_D, data = si)
anova(mod_te)
summary(mod_te)
plot(si$log_D, qlogis(si$ter_energy_pp_0_1))
abline(1.9477, 0.4283)
#*

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ conductivity, data = si)
anova(mod_te)
summary(mod_te)
plot(si$conductivity, qlogis(si$ter_energy_pp_0_1))
abline(3.7838439, -0.0020145)
#.

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ turbidity, data = si)
anova(mod_te)
summary(mod_te)
plot(si$turbidity, qlogis(si$ter_energy_pp_0_1))
abline(4.895888, -0.005199)
#*


mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ forkmm, data = si)
anova(mod_te)
summary(mod_te)
plot(si$forkmm, qlogis(si$ter_energy_pp_0_1))
abline(-0.139222, 0.028952)
#***

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ dissolved_O2, data = si)
anova(mod_te)
summary(mod_te)
plot(si$dissolved_O2, qlogis(si$ter_energy_pp_0_1))
abline(4.895888, -0.005199)

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ rip_shrub_cover, data = si)
anova(mod_te)
summary(mod_te)
plot(si$rip_shrub_cover, qlogis(si$ter_energy_pp_0_1))
abline(3.56445, -0.02334)


mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ log_grain + log_D + log_p_ag + forkmm, data = si)
anova(mod_te)
mod_te1<-update(mod_te, .~. -log_grain)
anova(mod_te1)
mod_te2<-update(mod_te1, .~. -velocity)
anova(mod_te2)
summary(mod_te2)
avPlots(mod_te2, "log_p_ag")
avPlots(mod, "forkmm")



mod_tp<-lm(tp_2_pp ~ grain_size + log_D + log_p_ag + forkmm, data = si)
anova(mod_tp)
summary(mod_tp)
mod_te1<-update(mod_te, .~. -grain_size)
anova(mod_te1)
mod_te2<-update(mod_te1, .~. -velocity)
anova(mod_te2)
summary(mod_te2)
avPlots(mod, "log_p_ag")
avPlots(mod, "forkmm")

mod_tp<-lm(tp_2_pp ~ turbidity + log_p_ag + log_D + forkmm + w_width + max_depth + temp + log_sal + log_grain + pH + sex + buf_width + log_sin + valley_slope + log_drainarea, data = si)
anova(mod_tp)
summary(mod_tp)
mod_tp1<-update(mod_tp, .~. -log_sin)
anova(mod_tp1)
summary(mod_tp1)
mod_tp2<-update(mod_tp1, .~. -log_grain)
anova(mod_tp2)
summary(mod_tp2)
mod_tp3<-update(mod_tp2, .~. -log_p_ag)
anova(mod_tp3)
summary(mod_tp3)
mod_tp4<-update(mod_tp3, .~. -valley_slope)
anova(mod_tp4)
mod_tp5<-update(mod_tp4, .~. -pH)
anova(mod_tp5)
mod_tp6<-update(mod_tp5, .~. -w_width)
anova(mod_tp6)
mod_tp7<-update(mod_tp6, .~. -sex) 
anova(mod_tp7)
mod_tp8<-update(mod_tp7, .~. -log_drainarea)
anova(mod_tp8)
mod_tp9<-update(mod_tp8, .~. -buf_width)
anova(mod_tp9)
summary(mod_tp9)

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ log_D + turbidity + log_p_ag + forkmm + dissolved_O2 + trout_present + rip_tree_cover + rip_shrub_cover + rip_grass_cover + algae_cover + FDOM, data = si)
anova(mod_te)
summary(mod_te)
mod_te1<-update(mod_te, .~. -log_p_ag)
anova(mod_te1)
mod_te2<-update(mod_te1, .~. -trout_present)
anova(mod_te2)
mod_te3<-update(mod_te2, .~. -algae_cover)
anova(mod_te3)
mod_te4<-update(mod_te3, .~. -rip_tree_cover)
anova(mod_te4)
mod_te5<-update(mod_te4, .~. -FDOM)
anova(mod_te5)
mod_te6<-update(mod_te5, .~. -rip_grass_cover)
anova(mod_te6)
summary(mod_te6)

mod_tp<-lm(tp_2_pp ~ log_sal, data = si)
anova(mod_tp)
summary(mod_tp)
plot(si$log_sal, si$tp_2_pp)
abline(3.43381, 0.85790)

mod_tp<-lm(tp_2_pp ~ temp, data = si)
anova(mod_tp)
summary(mod_tp)
plot(si$temp, si$tp_2_pp)
abline(1.94674, 0.05703)
#*

mod_tp<-lm(tp_2_pp ~ max_depth, data = si)
anova(mod_tp)
summary(mod_tp)
plot(si$max_depth, si$tp_2_pp)
abline(3.816546, -0.016092)

mod_tp<-lm(tp_2_pp ~ log_sal, data = si)
anova(mod_tp)
summary(mod_tp)
plot(si$log_sal, si$tp_2_pp)
abline(3.43381, 0.85790)


mod_tp<-lm(tp_2_pp ~ rip_tree_cover, data = si)
anova(mod_tp)
summary(mod_tp)
plot(si$rip_tree_cover, si$tp_2_pp, ylab = "trophic position", xlab = "percent tree cover")
abline(2.56678, 0.01359)

mod_fork <- lm(forkmm ~ rip_tree_cover, data = si)
summary(mod_fork)
plot(si$rip_tree_cover, si$forkmm, ylab = "fork length", xlab = "percent tree cover")
#*

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ rip_tree_cover, data = si)
anova(mod_te)
summary(mod_te)
plot(si$rip_tree_cover, qlogis(si$ter_energy_pp_0_1))
#abline(2.50201, -0.00457)

mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ rip_tree_cover, data = si)
anova(mod_te)
summary(mod_te)
plot(si$rip_tree_cover, qlogis(si$ter_energy_pp_0_1))
abline(2.50201, -0.00457)

mod_tr <- lm(rip_tree_cover ~ PC1, data = si)
summary(mod_tr)
plot(si$PC1, si$rip_tree_cover, ylab = "percent tree cover", xlab = "agricultural intensity")
abline(49.701, -3.448)
#**

mod_tr <- lm(rip_tree_cover ~ log_p_ag, data = si)
summary(mod_tr)
plot(si$log_p_ag, si$rip_tree_cover, ylab = "percent tree cover", xlab = "log agricultural land use")
abline(28.440, -190.868)

#si[is.na(si$ter_energy_sc), names(si) == "ter_energy_sc"] <- "x"
#si$ter_energy_sc_0_1 <- si$ter_energy_sc
#si[si$ter_energy_sc >= 1, names(si) == "ter_energy_sc_0_1"] <- 0.99
#si[si$ter_energy_sc <= 0, names(si) == "ter_energy_sc_0_1"] <- 0.01
#si[si$ter_energy_sc_0_1 == "x", names(si) == "ter_energy_sc_0_1"] <- NA
#si$ter_energy_sc_0_1<-as.numeric(si$ter_energy_sc_0_1)