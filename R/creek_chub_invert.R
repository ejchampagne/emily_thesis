source("R/5_coupling_tp_regressions.R")

# creek chub abundance data
cc <- read.csv("data/creek_chub_data.csv")
cc = cc %>% group_by(sitecode) %>%
  summarise(cc_n = n())

# read in aquatic invert data
aqinv <- read.csv("data/aq_invert18.csv")
# add edibles column
aqinv = aqinv %>% mutate(edibles = trichoptera + amphipoda + diptera + ephemeroptera + hemiptera + plecoptera + hirudinia + lepidoptera + coleoptera + isopoda + hymenoptera + megaloptera + odonata)

# read in terrestrial invert data
terinv <- read.csv("data/ter_invert18.csv")
# add predatory inv column
terinv = terinv %>% mutate(predatory = p_aranae + p_hymenoptera + p_opiliones + p_odonata + p_acari + p_neuroptera + p_mantodea + p_mecoptera)


# combine datasets
abun <- right_join(cc, aqinv)
abun <- right_join(abun, pcascores)

# calculate ratio of cc to edible inverts
abun$ratio_cc_edible <- abun$cc_n/abun$edibles

# make some plots
plot(log10(cc_n) ~ PC1, data = abun, pch = 19)
plot(log10(cc_n) ~ PC2, data = abun, pch = 19)

plot(log10(edibles) ~ PC1, data = abun, pch = 19)
plot(log10(edibles) ~ PC2, data = abun, pch = 19)


plot(log10(ratio_cc_edible) ~ PC1, data = abun, pch = 19)
plot(log10(ratio_cc_edible) ~ PC2, data = abun, pch = 19)


