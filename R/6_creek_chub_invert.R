source("R/5_coupling_tp_regressions.R")

dat <- read.csv("data/2018mastersanalysis.csv", header = T)

dat = dat %>% pivot_longer(
  cols = blacknose.dace:bluegill,
  values_to = "count"
) 

richness = dat %>% mutate(presence = case_when(count > 0 ~ 1,
                                    count == 0 ~ 0)) %>%
  group_by(sitecode) %>%
  summarise(richness = sum(presence))



# creek chub abundance data
cc <- read.csv("data/2018fishwbulk.csv")
cc_count = cc %>% filter(species == "creek chub") %>%
  group_by(sitecode) %>%
  summarise(cc_n = n())

# read in aquatic invert data
aqinv <- read.csv("data/aq_invert18.csv")
# add edibles column
aqinv = aqinv %>% mutate(edibles = trichoptera + amphipoda + diptera + ephemeroptera + hemiptera + plecoptera + hirudinia + lepidoptera + coleoptera + isopoda + hymenoptera + megaloptera + odonata) %>%
  mutate(prop_edible = edibles/totalcount)

# read in terrestrial invert data
terinv <- read.csv("data/ter_invert18.csv")
# add predatory inv column
terinv = terinv %>% mutate(predatory = p_aranae + p_hymenoptera + p_opiliones + p_odonata + p_acari + p_neuroptera + p_mantodea + p_mecoptera)

# combine datasets
abun <- right_join(cc_count, aqinv)
abun <- right_join(abun, pcascores)
abun <- right_join(abun, pcadata)
abun <- right_join(abun, terinv, by = "sitecode")
abun <- right_join(abun, richness)

abun %>% filter(totalcount.x < 100)

plot(((abun$edibles+1)/abun$totalcount.x) ~ abun$log_ag_250)

richmod = lm(log10(abun$cc_n) ~ abun$log_ag_250 + abun$richness)


summary(richmod)

# calculate ratio of cc to edible inverts
abun$ratio_cc_edible <- abun$cc_n/abun$edibles
abun$ratio_cc_totalaq <- abun$cc_n/abun$totalcount.x

mod = lm(log10(abun$edibles+1) ~ abun$richness)
summary(mod)

# make some plots
mod_ag <- lm(ratio_cc_edible ~ log_ag_250, data = abun)
summary(mod_ag)

plot(log10(ratio_cc_edible) ~ log_ag_250, data = abun, pch = 19)
abline(mod_ag)

mod_buf <- lm(log10(ratio_cc_edible) ~ log_buf_width, data = abun)
summary(mod_buf)

plot(log10(ratio_cc_edible) ~ log_buf_width, data = abun, pch = 19)
abline(mod_buf)

mod_n <- lm(log_n ~ log_p_ag, data = abun)
summary(mod_n)

plot(log_n ~ log_p_ag, data = abun)
abline(mod_n)

mod_p <- lm(log_p ~ log_p_ag, data = abun)
summary(mod_p)

plot(log_p ~ log_p_ag, data = abun)
abline(mod_p)

cc %>% fnlter(species == "creek chub") %>% 
  group_by(sitecode) %>%
  summarise(count = n()) %>%
  right_join(pcadata) %>%
  ggplot(aes(x = log_ag_250, y = log10(count))) + geom_point() + geom_smooth(se = F, method = "lm")

cc %>% filter(species == "creek chub") %>% 
  group_by(sitecode) %>%
  summarise(mean_w = mean(weight, na.rm = T)) %>%
  right_join(pcadata) %>%
  ggplot(aes(x = log_ag_250, y = mean_w)) + geom_point() 

#coupling and TP vs invery densities
abun1 <- right_join(abun, si, by = "sitecode")

# model for terrestrial energy
hist(plogis(abun$ter_energy_pp))
mod_prop_ter <- lm(plogis(ter_energy_pp) ~ log_ag_250.x + ratio_cc_edible + forkmm, data = abun)
summary(mod_prop_ter)

# model for trophic position
hist(abun$tp_2_pp)
mod_tp <- lm(tp_2_pp ~ log_ag_250.x + predatory + forkmm, data = abun)
summary(mod_tp)
mod_tp <- update(mod_tp, .~. -forkmm)
summary(mod_tp)
mod_tp <- update(mod_tp, .~. -predatory)
summary(mod_tp)


# plots
# terrestrial energy
avPlots(mod_prop_ter,"forkmm", xlab="partial forklength (mm)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-0.18, 0.18))

avPlots(mod_prop_ter,"log_ag_250.x", xlab="log10 partial local % agriculture (250 m buffer)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-0.18, 0.18))

avPlots(mod_prop_ter,"ratio_cc_edible", xlab="partial creek chub:edible aquatic invertebrate ratio", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-0.18, 0.18))


# trophic position
avPlots(mod_tp,"log_ag_250.x",  ylab="trophic position", xlab="log10 partial local % agriculture (250 m buffer)",
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-5.5, 2))

avPlots(mod_tp,"predatory",  ylab="trophic position", xlab="partial proportion terrestrial predatory insects",
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-5.5, 2))

richmod = lm(log10(cc_n) ~ richness, data = abun)
summary(richmod)
xx=as.data.frame(avPlot(richmod, "richness"))
xx$sitecode = abun$sitecode

ediblemod = lm(log10(edibles) ~  richness, data = abun)
summary(ediblemod)
yy=as.data.frame(avPlot(ediblemod, "richness"))

zz = merge(xx, yy, by = "log_ag_250.x")
zz$ratio = zz$`log10(cc_n)`/zz$`log10(edibles)`
zz = zz %>% filter(! ratio < -50)
mod = lm(zz$ratio ~ zz$log_ag_250.x)

plot(zz$ratio ~ zz$log_ag_250.x)
