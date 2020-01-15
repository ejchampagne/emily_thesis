# load required R packages
library(tidyverse)
library(cowplot)

# read in data
site <- read_csv("data/3060siteinfo.csv")
spp <- read_csv("data/3060speciesinfo.csv")

# select only columns we need for the species dataframe
spp = spp %>% select(sci_name, CommonLength, FoodTroph, Vulnerability, LongevityWild) %>%
  rename(name = sci_name)

# transpose site dataframe to long form
site = site %>% pivot_longer(
  cols = Alosa_pseudoharengus:Perca_flavescens,
  values_to = "presence" 
  )

# merge site and species dataframes and only keep rows where a species was present 
df = site %>% right_join(spp) %>%
  filter(presence == 1)

# some huge rivers to delete, likely will need to add more
delete <- c("Detroit River", "Niagra River", "St. Clair River", "Ottawa River", "St. Lawrence River", "Detroit River, Fleming Channel", "Niagara River")

# create a summary dataset with mean length and tp for each site and year
xx = df %>% group_by(Longitude, Latitude, Waterbody_Name, Capture_Year) %>%
  summarise(urban = mean(urban), natural = mean(natural), agricultural = mean(agricultural), 
            mean_length = mean(CommonLength), mean_tp = mean(FoodTroph), 
            mean_vulnerability = mean(Vulnerability), mean_long = mean(LongevityWild), 
            richness = sum(presence)) %>%
  filter(!Waterbody_Name %in% delete)  # filter out huge rivers mentioned above in 'delete'

write.csv(xx, "data_4.csv", row.names = F)

# regression testing influence of % agriculture on mean fish length
mod_length <- lm(log10(mean_length) ~ agricultural, data = xx)
summary(mod_length)

# regression testing influence of % agriculture on mean tp
mod_tp <- lm(mean_tp ~ agricultural, data = xx)
summary(mod_tp)

# make plots for length and tp vs % ag
a = ggplot(xx, aes(x = agricultural, y = mean_length)) + geom_point(alpha = 0.5) + geom_smooth(method = "lm") + theme_classic(base_size = 14) + xlab("% agriculture") + ylab("mean length (cm)") + scale_y_log10(limits = c(3, 70))
b = ggplot(xx, aes(x = agricultural, y = mean_tp)) + geom_point(alpha = 0.5) + geom_smooth(method = "lm") + theme_classic(base_size = 14) + xlab("% agriculture") + ylab("mean trophic position") + scale_y_continuous(limits = c(2.2, 4.7))

# put plots a and b in a grid
plot_grid(a, b, labels = c("A", "B"), ncol = 1)

