source("R/3_calculate_trophic_metrics.R")

library(car)
library(vegan)
library(corrgram)
library(Hmisc)

# run pca of environmental data for sites to get agriculture gradient score

# read in site data
envdata <- read.csv("data/2018PCAaggrad.csv")
hist(log10(envdata$turbidity))


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
  log_velocity = log10(velocity+1), 
  log_p = log10(july.phosphorus),
  log_n = log10(july.nitrogen)
)

# keep only all sites for pca but not EP1 because no phos data
keep_pca <- c("AC", "AT", "BG", "BS", "BW", "DE", "EP2", "EP3", "EP4", "FP", "HC", "HT", "JC", "JW", "KC", "LEF", "LEST", "MT", "PF", "PVTF", "SC", "UH", "VN", "WA", "WH", "WW")

envdata = envdata %>% filter(sitecode %in% keep_pca)

rcorr(envdata$log_p_ag, envdata$log_buf_width)
rcorr(envdata$log_p_ag, envdata$log_turbidity)
rcorr(envdata$log_p_ag, envdata$log_drainarea)#no
rcorr(envdata$log_p_ag, envdata$log_grain)#no
rcorr(envdata$log_p_ag, envdata$log_D)#no
rcorr(envdata$log_p_ag, envdata$log_sin)
rcorr(envdata$log_p_ag, envdata$log_sal)#no

hist(log10(envdata$velocity))

# create a dataframe with data for pca
pcadata <- envdata %>% select(log_p_ag, log_turbidity, log_buf_width, log_sin, log_p, log_n)
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

# flip this score to make more sense with respect to increasing ag demand with larger score
#pcascores$PC1 <- pcascores$PC1 * (-1)
# add 5 to make all numbers positive to be a 'score'
#pcascores$PC1 <- pcascores$PC1 + 5

