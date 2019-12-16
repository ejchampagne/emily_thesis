#11_final figures
setwd("~/Desktop/Masters Analysis/analysis")
source("EmilyThesis/9_abundances.R")
source("EmilyThesis/5_analysis.R")

par(mfrow=c(2,1))
mod_te<-lm(qlogis(ter_energy_pp_0_1) ~ PC1 + forkmm, data = si)
anova(mod_te)
summary(mod_te)

avPlots(mod_te,"forkmm", xlab="Partial Forklength (mm)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-9, 7))
avPlots(mod_te,"PC1", xlab="Partial Agricuture Intensity", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-9, 7))



fig2<-lm(tp_2_pp ~ PC1 + forkmm, data = si)
anova(fig2)
summary(fig2)

avPlots(fig2,"forkmm", xlab="partial fork length (mm)", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l")

avPlots(fig2,"PC1", xlab="partial agriculture intensity", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "grey", bty ="l")

fig3 <- ggplot(si, aes(x = july.nitrogen, y = qlogis(ter_energy_pp_0_1))) + geom_point() +
  geom_smooth(method="lm") + labs(y="qlogis proportion terrestrial energy", x="July total Nitrogen (mg/L)") + 
  geom_point(aes(), size=3) + theme_classic()
fig3

fig4 <- ggplot(si, aes(x = july.nitrogen, y = tp_2_pp)) + geom_point() + geom_smooth(method="lm") + 
  labs(y = "trophic position", x = "July total Nitrogen (mg/L)") + geom_point(aes(), size = 3) + 
  theme_classic()
fig4

fig5 <- ggplot(sitedata, aes(x = agimpact, y = july.nitrogen)) +geom_point(aes(y = july.nitrogen)) + coord_cartesian (ylim=c(0, 16)) +
  geom_smooth(method="lm") +  labs(y = "July total Nitrogen (mg/L)", x = "agricultural intensity") + 
  geom_point(aes(), size = 3) + theme_classic()

fig5

fig6 <- ggplot(sitedata, aes(x = agimpact, y = qlogis(creek.chub/totalcount.y))) +geom_point() + coord_cartesian (xlim=c(4, 8)) +
  geom_smooth(method="lm") +  labs(y = "qlogis creek chub abundance/aquatic invertebrate abundance", x = "agricultural intensity") + 
  geom_point(aes(), size = 3) + theme_classic()
fig6

fig7 <- ggplot(si, aes(x = qlogis(creek.chub/totalcount.y), y = qlogis(ter_energy_pp_0_1))) + geom_point() +
  geom_smooth(method="lm") + labs(y="qlogis proportion terrestrial energy", x="qlogis creek chub abundance/aquatic invertebrate abundance") + 
  geom_point(aes(), size=3) + theme_classic()
fig7

fig8 <- ggplot(si, aes(x = terpred, y = tp_2_pp)) + geom_point() + geom_smooth(method="lm") + 
  labs(y = "trophic position", x = "proportion of predatory terrestrial invertebrates") + geom_point(aes(), size = 3) + 
  theme_classic()
fig8

fig9 <- ggplot(sitedata, aes(x = log_p_ag, y = terpred)) + geom_point() +
  geom_smooth(method="lm") +  labs(y = "proportion of predatory terrestrial invertebrates") + 
  geom_point(aes(), size = 3) + theme_classic()
fig9
