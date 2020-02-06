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
#buf <- readRDS("data/land_use_simplified.rds")
#buf <- bind_rows(buf, .id = "buffer")

# look at mean ag at each buffer
#buf %>% group_by(buffer) %>% 
#  summarise(p_ag = mean(agricultural)) 

# subset out the xx m radial buffer data 
#buf_250 <- buf %>% filter(buffer == "bdist_250") %>%
#  select(sitecode, agricultural) %>%
#  rename(p_ag_250 = agricultural)
#hist(buf_250$p_ag_250)

#write.csv(buf_250, "data/buf_250.csv")
buf_250 <- read.csv("data/buf_250.csv")


# join radial buffer to env data
envdata <- right_join(envdata, buf_250)

# lat, lon, streamorder, wet width, max depth, summer velocity, summer water temp, summer discharge, conductivity, ph, 
# salinity, do, fdom, buffer width, sinucity, valley slope, grain size, drain area, % ag

# log transform variables to be used in pca
envdata = envdata %>% mutate(
  watershed_agriculture = log10(p_ag_watershed), #log_p_ag = log watershed agriculture
  salinity = log10(salinity), # log_sal = log salinity
  sinuosity = log10(sinuosity), # log_sin = sinuosity
  discharge = log10(discharge), # log_D = discharge m3/s
  avg_grain_size = log10(grain_size), #log_grain = avg grain size
  watershed_size = log10(drainage_area), #log_drainarea = watershed size
  turbidity = log10(turbidity), # log_turbidity = turbidity
  avg_buffer_w = log10(buf_width), # log_buf_width = avg buffer w
  phosphorus = log10(july.phosphorus), #log_p = phosphorus
  nitrogen = log10(july.nitrogen), # log_n = nitrogen
  agriculture_250 = log10(p_ag_250+1), # log_ag_250
  conductivity = log10(conductivity), #log_cond = conductivity
  stream_order = log10(order), #log_order = stream order
  water_temp = log10(temp) # log_water_t = water temp
)


# keep only all sites for pca but not EP1 because no phos data
keep_pca <- c("AC", "AT", "BG", "BS", "BW", "DE", "P2", "P3", "P4", "FP", "HC", "HT", "JC", "JW", "KC", "EF", "ST", "MT", "PF", "PV", "SC", "UH", "VN", "WA", "WH", "WW")

envdata = envdata %>% filter(sitecode %in% keep_pca)

# create a dataframe with data for pca
#pcadata <- envdata %>% select(log_p_ag, log_buf_width, log_ag_250, log_p, log_n, log_sin, log_order, log_D, log_turbidity)
pcadata <- envdata %>% select(watershed_agriculture, avg_buffer_w, agriculture_250, sinuosity, stream_order, turbidity, valley_slope, rip_tree_cover, avg_grain_size)
rownames(pcadata) <- envdata$sitecode  

# run pca
pca <- prcomp(pcadata, scale = TRUE, center = TRUE)
biplot(pca)
pca$x
summary(pca)
pca$rotation
pca$loadings
eigen<-eigenvals(pca)
eigen #amount of variation explained by the axis
scaled.eigen<-(eigen/sum(eigen))
scaled.eigen
screeplot(pca, main=NULL)
p<-ncol(pcadata) #p is just the number of variables. 
L<-matrix(ncol=p)
for (i in 1:p) {
  L[i]<-round(1/p*sum(1/seq(from=i, to=p)),2)
}
#now compare your scaled eigenvalues to L
L
scaled.eigen 

biplot(pca, xlim  = c(-0.5, 0.6), scale=1, cex=c(0.7, 1), arrow.len=0, main="Correlation biplot", xlab = "")
abline(h=0, lty=2)
abline(v=0, lty=2)

# turn pca scores into dataframe
pcascores <- as.data.frame(pca$x)
pcascores$sitecode = rownames(pcascores)
rownames(pcascores) = NULL
pcascores <- pcascores %>% select(sitecode, PC1, PC2)

# correlogram of variables in the pca
corrgram(pcadata, order=NULL, lower.panel=panel.conf, 
         upper.panel=panel.shade, text.panel=panel.txt, 
         diag.panel=NULL)
