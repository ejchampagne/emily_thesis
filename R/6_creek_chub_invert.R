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

si <- right_join(si, cc_count)
si <- right_join(si, aqinv, by = "sitecode")

mod <- lm(log10(abun$cc_n/abun$edibles) ~ abun$log_ag_250)
summary(mod)

plot(log10(abun$cc_n/abun$edibles) ~ abun$log_ag_250, ylab="log10 creek chub:edible invertebrate abundance", xlab = "log10 250 m radial buffer % ag", pch = 19)
abline(mod)

abun1 <- abun %>% filter(!sitename == "Hawkcliff")

mod <- lm(log10(abun1$cc_n/abun1$edibles) ~ abun1$log_ag_250)
summary(mod)

plot(log10(abun1$cc_n/abun1$edibles) ~ abun1$log_ag_250)
abline(mod)
