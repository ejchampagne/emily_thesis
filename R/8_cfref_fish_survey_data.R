source("R/7_ontario_stream_fish_data.R")

dat <- read.csv("data/2018mastersanalysis.csv", header = T)

dat = dat %>% pivot_longer(
  cols = blacknose.dace:bluegill,
  values_to = "count"
)

dat <-