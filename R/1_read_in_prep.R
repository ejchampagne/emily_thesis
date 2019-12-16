library(tidyverse)

#2018data
cn <- read.csv("data/2018carbonnitrogen.csv", stringsAsFactors = F)
cn <- cn %>% filter(!replicate == 1) %>%
  select(-replicate,-old.sampleid)

h2 <- read.csv("data/2018deuterium.csv", stringsAsFactors = F)
h2 <- h2 %>% select(-old.sampleid)

si <- full_join(cn, h2)
