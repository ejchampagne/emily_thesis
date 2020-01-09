library(tidyverse)
library(cowplot)

# Ontario survey data (PWQA sampling)

# read in data
site <- read_csv("data/3060siteinfo.csv")
spp <- read_csv("data/3060speciesinfo.csv")

# select only columns we need for the species dataframe
spp = spp %>% select(sci_name, CommonLength, FoodTroph) %>%
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
  summarise(urban = mean(urban), natural = mean(natural), agricultural = mean(agricultural), mean_length = mean(CommonLength), mean_tp = mean(FoodTroph)) %>%
  filter(!Waterbody_Name %in% delete) %>% # filter out huge rivers mentioned above in 'delete'
  filter(! urban > 30) # filter out site with % urban >40%

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











# repeat analysis with Emily's data

#dat <- read.csv("data/2018mastersanalysis.csv", stringsAsFactors = F)
#species <- read.csv("data/species_list_emily.csv", stringsAsFactors = F)

#dat = dat %>% pivot_longer(
#  cols = blacknose.dace:bluegill,
#  values_to = "count" 
#)
#dat <- as.data.frame(dat)

#dat1 = merge(dat, spp, by = "name")

#dat2 = dat1 %>% filter(count > 0) %>%
#  group_by(sitecode) %>%
#  mutate(prop = count/totalfish) %>%
#  mutate(propxtp = prop*tp) %>%
#  summarise(sum_tp = sum(propxtp, na.rm = T)) %>%
#  right_join(pcascores) %>%
#  right_join(envdata)


