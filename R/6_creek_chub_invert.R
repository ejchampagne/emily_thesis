source("R/5_coupling_tp_regressions.R")

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


abun %>% filter(totalcount < 100) %>%

plot(((abun$edibles+1)/abun$totalcount) ~ abun$log_ag_250)

plot(log10(abun$cc_n+1) ~ abun$log_ag_250)

summary(lm(log10(abun$cc_n+1) ~ abun$log_ag_250))

# calculate ratio of cc to edible inverts
abun$ratio_cc_edible <- abun$cc_n/abun$edibles

plot(log10(abun$ratio_cc_edible+1) ~ abun$log_ag_250)
summary(lm(log10(abun$ratio_cc_edible+1) ~ abun$log_ag_250))

# make some plots
mod_ag <- lm(ratio_cc_edible ~ log_ag_250, data = abun)
summary(mod_ag)

plot(log10(ratio_cc_edible) ~ log_ag_250, data = abun, pch = 19)
abline(mod_ag)

mod_buf <- lm(log10(ratio_cc_edible) ~ log_buf_width, data = abun)
summary(mod_buf)

plot(log10(ratio_cc_edible) ~ log_buf_width, data = abun, pch = 19)
abline(mod_buf)

cc %>% filter(species == "creek chub") %>% 
  group_by(sitecode) %>%
  summarise(count = n()) %>%
  right_join(pcadata) %>%
  ggplot(aes(x = log_ag_250, y = log10(count))) + geom_point() + geom_smooth(se = F, method = "lm")

cc %>% filter(species == "creek chub") %>% 
  group_by(sitecode) %>%
  summarise(mean_w = mean(weight, na.rm = T)) %>%
  right_join(pcadata) %>%
  ggplot(aes(x = log_ag_250, y = mean_w)) + geom_point() 

