#6_covariate testing
setwd("~/Desktop/Masters Analysis/analysis")

source("EmilyThesis/4_join_pc_gradient.R")

library(Hmisc)
library(corrgram)


sitedata <- right_join(envdata, pca, by = "sitecode.x")
sitedata <- right_join(sitedata, fishmeta, by = "sitecode.x")

sitedata$log_p_ag <- log10(sitedata$p_ag_watershed)
sitedata$log_sal <- log10(sitedata$salinity)
sitedata$log_sin <- log10(sitedata$sinuosity)
sitedata$log_D <- log10(sitedata$discharge)
sitedata$log_grain <- log10(sitedata$grain_size)
sitedata$log_drainarea <- log(sitedata$drainage_area)

#rcorr(agdata$log_p_ag,agdata$buf_width)
#rcorr(agdata$log_p_ag,agdata$log_sin)
#rcorr(agdata$log_p_ag,agdata$turbidity.x)
#rcorr(agdata$log_p_ag,agdata$log_D)

keeppca <- c("AC", "DE", "BW", "EP1", "EP2", "EP3", "HT", "JW", "KC", "LEST", "PF", "PVTF", "SC", "UH", "VN", "WA", "WW")
sitedata <- sitedata[sitedata$sitecode.x %in% keeppca,]

pcadata = select(sitedata,c("sitecode.x","log_p_ag","log_D", "turbidity","buf_width","log_sin"))
rownames(pcadata) = pcadata$sitecode.x
pcadata = select(pcadata,-"sitecode.x")

pcadata %>% 
  rename(
    log.percent.agriculture = log_p_ag,
    log.discharge = log_D,
    buffer.width = buf_width,
    log.sinuosity = log_sin
  )

corrgram(pcadata, order=NULL, lower.panel=panel.conf, 
         upper.panel=NULL, text.panel=panel.txt, 
         diag.panel=panel.density)
#need to make fill white and add value labels in the boxes

pca = prcomp(pcadata,scale = TRUE, center = TRUE)
biplot(pca, xlim  = c(-0.5, 0.6), scale = 1)
pca$x
summary(pca)


pcascores = as.data.frame(pca$x)
pcascores$sitecode.x = rownames(pcascores)
rownames(pcascores) = NULL
pcascores = select(pcascores,c("sitecode.x", "PC1", "PC2"))

sitedata <- right_join(sitedata,pcascores)
plot(sitedata$creek.chub~sitedata$PC1)










#write.csv(pcascores, "ag_pca_scores.csv", row.names = F)
