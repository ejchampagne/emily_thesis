source("R/5_coupling_tp_regressions.R")

# calculate diversity and eveness metrics

# terrestrial invert data
terinv <- read.csv("data/ter_invert18.csv")

# calculates diversity of terrestrial inverts
div_terinv <- terinv[c(4:28)]
terinv$ter_div <- diversity(div_terinv, index = "shannon")
terinv$ter_div

# calculates evenness of terrestrial inverts
terinv$ter_even <- terinv$ter_div/log(specnumber(div_terinv))
terinv$ter_even

# calculate abundance of predatory terrestrial inverts
terinv$terpred <- terinv$p_aranae + terinv$p_hymenoptera + terinv$p_opiliones + terinv$p_odonata + terinv$p_acari + terinv$p_neuroptera + terinv$p_mantodea + terinv$p_mecoptera

# read in aquatic invert data
aqinv <- read.csv("data/aq_invert18.csv")

# calculates diversity of aquatic inverts 
div_aqinv <-aqinv[c(4:24)]
aqinv$aq_div<-diversity(div_aqinv, index = "shannon")
aqinv$aq_div

# calculates eveness of aquatic inverts 
aqinv$aq_even <- aqinv$aq_div/log(specnumber(div_aqinv))
aqinv$aq_even

# calculate abundance of edible aquatic inverts
aqinv$edibles <- aqinv$trichoptera + aqinv$amphipoda + aqinv$diptera + aqinv$ephemeroptera + 
  aqinv$hemiptera + aqinv$plecoptera + aqinv$hirudinia + aqinv$lepidoptera + aqinv$coleoptera + 
  aqinv$isopoda + aqinv$hymenoptera + aqinv$megaloptera + aqinv$odonata

#aquatic predatory inverts
aqinv$aqpred <- aqinv$oligochaeta + aqinv$hydrachnidia + aqinv$hirudinia + aqinv$megaloptera + aqinv$odonata



