library(tidyverse)

source("R/4_run_pca.R")

dat <- read.csv("data/2018mastersanalysis.csv", stringsAsFactors = F)
spp <- read.csv("data/species_list_emily.csv", stringsAsFactors = F)

dat = dat %>% pivot_longer(
  cols = blacknose.dace:bluegill,
  values_to = "count" 
)
dat <- as.data.frame(dat)

dat1 = merge(dat, spp, by = "name")

dat2 = dat1 %>% filter(count > 0) %>%
  group_by(sitecode) %>%
  mutate(prop = count/totalfish) %>%
  mutate(propxtp = prop*tp) %>%
  summarise(sum_tp = sum(propxtp, na.rm = T)) %>%
  right_join(pcascores) %>%
  right_join(envdata)

mod <- lm(sum_tp ~ PC1 + PC2, data = dat2)
summary(mod)
mod <- update(mod, .~. -PC1)
summary(mod)

