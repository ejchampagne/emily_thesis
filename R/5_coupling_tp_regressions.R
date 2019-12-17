source("R/4_run_pca.R")

# keep list for SC baselines
#keep_sc <- c("AC", "AT", "EP1", "EP2", "EP3", "EP4", "HC", "HT", "JW", "KC", "LEST", "PF", "PVTF", "SC", "UH", "VN", "WA", "WH", "WW")

# keep list for PP baselines
keep_pp <- c("AC", "AT", "BW", "DE","EP2", "EP3", "EP4", "HC", "HT", "JC", "JW", "KC", "LEST", "PF", "PVTF", "SC", "VN", "WA", "WW")

# keep only relevant sites as per list above
si <- si %>% filter(sitecode %in% keep_pp)

# merge fish data to si dataframe
fish <- read.csv("data/2018fishwbulk.csv")
fish <- fish %>% mutate(sitecode = as.character(sitecode), species = as.character(species), sampleid = as.character(sampleid))

# replace 0 and 1s with 
si <- si %>% right_join(fish, by = "sampleid", "sidecode") %>%
        filter(role == "cc") %>%
        mutate(ter_energy_pp = case_when(
                ter_energy_pp >= 1 ~ 0.99,
                ter_energy_pp <= 0 ~ 0.01,
                ter_energy_pp > 0 & ter_energy_pp < 1 ~ ter_energy_pp)) %>%
        mutate(ter_energy_sc = case_when(
                ter_energy_sc >= 1 ~ 0.99,
                ter_energy_sc <= 0 ~ 0.01,
                ter_energy_sc > 0 & ter_energy_sc < 1 ~ ter_energy_sc)) %>%
        rename(sitename = sitename.x, sitecode = sitecode.x, species = species.x) %>%
        select(-sitecode.y, -species.y, -sitename.y) %>%
        filter(!is.na(role))

# merge si data to pca gradient
si <- right_join(si, pcascores)

# merge si data to pca data
pcadata$sitecode <- row.names(pcadata)
row.names(pcadata) <- NULL
si <- right_join(si, pcadata)


# model for terrestrial energy
hist(plogis(si$ter_energy_pp))
mod_prop_ter <- lm(plogis(ter_energy_pp) ~ log_ag_250 + log_p_ag + forkmm, data = si)
summary(mod_prop_ter)
mod_prop_ter <- update(mod_prop_ter, .~. -log_p_ag)
summary(mod_prop_ter)

# model for trophic position
hist(si$tp_2_pp)
mod_tp <- lm(tp_2_pp ~ PC1 + PC2 + forkmm, data = si)
summary(mod_tp)

# plots

# terrestrial energy
avPlots(mod_prop_ter,"forkmm", xlab="Partial Forklength (mm)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-0.2, 0.1))

avPlots(mod_prop_ter,"log_ag_250", xlab="log local percent agriculture (250 m radial buffer)", ylab="logit proportion terrestrial energy", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-0.25, 0.15))


# trophic position
avPlots(mod_tp,"forkmm", xlab="Partial Forklength (mm)", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "grey", bty ="l")

avPlots(mod_tp,"PC1", xlab="Partial PC1", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l", ylim = c(-2.5, 2))

avPlots(mod_tp,"PC2", xlab="Partial PC2", ylab="trophic position", 
        grid = F, id = F, pch = 19, col.lines = "black", bty ="l",  ylim = c(-2.5, 2))
