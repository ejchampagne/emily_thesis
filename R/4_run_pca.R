source("R/3_calculate_trophic_metrics.R")

library(car)
library(vegan)
library(corrgram)
library(Hmisc)

# run pca of environmental data for sites to get agriculture gradient score

# read in site data
envdata <- read.csv("data/2018PCAaggrad.csv")
hist(log10(envdata$turbidity))

# read in % ag data from radial buffers and convert to single df
buf <- readRDS("data/land_use_simplified.rds")
buf <- bind_rows(buf, .id = "buffer")

# look at mean ag at each buffer
buf %>% group_by(buffer) %>% 
  summarise(p_ag = mean(agricultural)) 

# subset out the xx m radial buffer data 
buf_250 <- buf %>% filter(buffer == "bdist_250") %>%
  select(sitecode, agricultural) %>%
  rename(p_ag_250 = agricultural)
hist(buf_250$p_ag_250)

# join radial buffer to env data
envdata <- right_join(envdata, buf_250)


# lat, lon, streamorder, wet width, max depth, summer velocity, summer water temp, summer discharge, conductivity, ph, 
# salinity, do, fdom, buffer width, sinucity, valley slope, grain size, drain area, % ag

# log transform variables to be used in pca
envdata = envdata %>% mutate(
  log_p_ag = log10(p_ag_watershed),
  log_sal = log10(salinity),
  log_sin = log10(sinuosity),
  log_D = log10(discharge),
  log_grain = log10(grain_size),
  log_drainarea = log10(drainage_area),
  log_turbidity = log10(turbidity),
  log_buf_width = log10(buf_width),
  log_p = log10(july.phosphorus),
  log_n = log10(july.nitrogen),
  log_ag_250 = log10(p_ag_250+1),
  log_cond = log10(conductivity),
  log_order = log10(order),
  log_water_t = log10(temp)
)


# keep only all sites for pca but not EP1 because no phos data
keep_pca <- c("AC", "AT", "BG", "BS", "BW", "DE", "EP2", "EP3", "EP4", "FP", "HC", "HT", "JC", "JW", "KC", "LEF", "LEST", "MT", "PF", "PVTF", "SC", "UH", "VN", "WA", "WH", "WW")

envdata = envdata %>% filter(sitecode %in% keep_pca)

# create a dataframe with data for pca
pcadata <- envdata %>% select(log_p_ag, log_buf_width, log_ag_250, log_p, log_n, log_sin, log_order, log_D, log_turbidity)
rownames(pcadata) <- envdata$sitecode  

# run pca
pca <- prcomp(pcadata, scale = TRUE, center = TRUE)
biplot(pca, xlim  = c(-0.5, 0.6))
pca$x
summary(pca)
pca$rotation

# turn pca scores into dataframe
pcascores <- as.data.frame(pca$x)
pcascores$sitecode = rownames(pcascores)
rownames(pcascores) = NULL
pcascores <- pcascores %>% select(sitecode, PC1, PC2)

# correlogram of variables in the pca
corrgram(pcadata, order=NULL, lower.panel=panel.conf, 
         upper.panel=panel.shade, text.panel=panel.txt, 
         diag.panel=NULL)
