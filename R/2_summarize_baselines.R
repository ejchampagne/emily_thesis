source("R/1_read_in_prep.R")

keep <- c("algae", "det", "terbase", "aqbase", "cc")

si$delta13c_norm <- si$delta13c + (-3.32 + (0.99 * si$c_n_ratio))

si %>% filter(!is.na(sitecode)) %>%
  filter(role %in% keep) %>%
  group_by(sitecode, role) %>%
  summarise(
    d13c = mean(delta13c, na.rm = T),
    d13c_norm = mean(delta13c_norm, na.rm = T),
    d15n = mean(delta15n, na.rm = T),
    dh2 = mean(delta2h, na.rm = T)
  ) %>%
  ggplot(aes(
    x = dh2,
    y = d15n,
    color  = role,
    group = sitecode
  )) +
  geom_point(size = 3) +
  theme_bw() +
  facet_wrap( ~ sitecode)

baselines <- si %>% filter(!is.na(sitecode)) %>%
  filter(role %in% keep) %>%
  group_by(sitecode, role) %>%
  summarise(
    d13c = mean(delta13c, na.rm = T),
    d13c_norm = mean(delta13c_norm, na.rm = T),
    d15n = mean(delta15n, na.rm = T),
    dh2 = mean(delta2h, na.rm = T)
  ) %>%
  filter(!role == "cc") %>%
  filter(!is.na(dh2)) %>%
  gather(variable, value,-(sitecode:role)) %>%
  unite(temp, variable, role) %>%
  spread(temp, value)

# SC donations
baselines[2, 11] <- baselines[7 , 11] #N for AT aqbase from EP1
baselines[6, 11] <- baselines[5 , 11] #N for DE aqbase from BW
baselines[8, 11] <- baselines[7 , 11] #N for EP2 aqbase from EP1
baselines[9, 11] <- baselines[7 , 11] #N for EP3 aqbase from EP1
baselines[10, 11] <- baselines[7 , 11] #N for EP4 aqbase from EP1
baselines[17, 11] <- baselines[7 , 11] #N for KC aqbase from EP1
baselines[25, 11] <- baselines[4 , 11] #N for VN aqbase from BS
baselines[6, 15] <- baselines[5 , 15] #H for DE aqbase from BW
baselines[8, 15] <- baselines[7 , 15] #H for EP2 aqbase from EP1
baselines[9, 15] <- baselines[7 , 15] #H for EP3 aqbase from EP1
baselines[10, 15] <- baselines[7 , 15] #H for EP4 aqbase from EP1
baselines[17, 15] <- baselines[7 , 15] #H for KC aqbase from EP1
baselines[25, 15] <- baselines[4 , 15] #H for VN aqbase from BS
baselines[8, 13] <- baselines[7 , 13] #N for EP2 terbase from EP1
baselines[9, 13] <- baselines[7 , 13] #N for EP3 terbase from EP1
baselines[8, 12] <- baselines[7 , 12] #N for EP2 det from EP1
baselines[9, 12] <- baselines[7 , 12] #N for EP3 det from EP1
baselines[8, 16] <- baselines[7 , 16] #H for EP2 det from EP1
baselines[9, 16] <- baselines[7 , 16] #H for EP3 det from EP1

si <- left_join(si, baselines)
si <- filter(si, si$role == "cc")

#check sc baselines
cc_si = si %>% select(role, sitecode, delta2h, delta15n)
ter_base_sc = si %>% select(role, sitecode, dh2_terbase, d15n_terbase) %>% mutate(role = "terbase") %>% rename(delta2h = dh2_terbase, delta15n = d15n_terbase)
aq_base_sc = si %>% select(role, sitecode, dh2_aqbase, d15n_aqbase) %>% mutate(role = "aqbase") %>% rename(delta2h = dh2_aqbase, delta15n = d15n_aqbase)
si_sc <- rbind(cc_si, ter_base_sc, aq_base_sc)

ggplot(si_sc, aes(
  x = delta2h,
  y = delta15n,
  color = role,
  group = sitecode
)) +
  geom_point(size = 3) +
  theme_bw() +
  facet_wrap( ~ sitecode)

#check pp baselines
ter_base_pp = si %>% select(role, sitecode, dh2_det, d15n_det) %>% mutate(role = "det") %>% rename(delta2h = dh2_det, delta15n = d15n_det)
aq_base_pp = si %>% select(role, sitecode, dh2_algae, d15n_algae) %>% mutate(role = "algae") %>% rename(delta2h = dh2_algae, delta15n = d15n_algae)
si_pp <- rbind(cc_si, ter_base_pp, aq_base_pp)

ggplot(si_pp, aes(
  x = delta2h,
  y = delta15n,
  color = role,
  group = sitecode
)) +
  geom_point(size = 3) +
  theme_bw() +
  facet_wrap( ~ sitecode)
