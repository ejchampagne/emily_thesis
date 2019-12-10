library(tidyverse)

#2018data
cn <- read.csv("data/2018carbonnitrogen.csv", stringsAsFactors = F)
cn <- cn %>% filter(!replicate == 1) %>%
  select(-replicate, -old.sampleid)

h2 <- read.csv("data/2018deuterium.csv", stringsAsFactors = F)
h2 <- h2 %>% select(-old.sampleid)

si <- full_join(cn, h2)

#write.csv(si, "data/2018_isotopes.csv")

#2017data
#cn17 <- read.csv("data/2017cns.csv")
#h217 <- read.csv("data/2017deuterium.csv")
#view(cn17)

#si17 <- full_join(cn17, h217, by = "sampleid")

#write.csv(si17, "data/2017_isotopes.csv")
#view(si17)
