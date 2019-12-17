source("R/2_summarize_baselines.R")

#Deuterium and Nitrogen
si$ter_energy_pp <- (si$delta2h - si$dh2_algae)/(si$dh2_det - si$dh2_algae)
si$ter_energy_sc <- (si$delta2h - si$dh2_aqbase)/(si$dh2_terbase - si$dh2_aqbase)

si$tp_1_algae <- 1 + (si$delta15n - si$d15n_algae)/3.4
si$tp_1_det <- 1 + (si$delta15n - si$d15n_det)/3.4
si$tp_1_terbase <- 2 + (si$delta15n - si$d15n_terbase)/3.4
si$tp_1_aqbase <- 2 + (si$delta15n - si$d15n_aqbase)/3.4

si$tp_2_pp <- 1 + (si$delta15n - ((si$d15n_algae*(1 - si$ter_energy_pp)) + (si$d15n_det*si$ter_energy_pp)))/3.4
si$tp_2_sc <- 2 + (si$delta15n - ((si$d15n_aqbase*(1 - si$ter_energy_sc)) + (si$d15n_terbase*si$ter_energy_sc)))/3.4

#Carbon and Nitrogen
#si$c_ter_energy_pp <- (si$delta13c_norm - si$d13c_norm_algae)/(si$d13c_norm_det - si$d13c_norm_algae)
#si$c_ter_energy_sc <- (si$delta13c_norm - si$d13c_norm_aqbase)/(si$d13c_norm_terbase - si$d13c_norm_aqbase)

#si$c_tp_1_algae <- 1 + (si$delta13c_norm - si$d13c_norm_algae)/3.4
#si$c_tp_1_det <- 1 + (si$delta13c_norm - si$d13c_norm_det)/3.4
#si$c_tp_1_terbase <- 2 + (si$delta13c_norm - si$d13c_norm_terbase)/3.4
#si$c_tp_1_aqbase <- 2 + (si$delta13c_norm - si$d13c_norm_aqbase)/3.4

#si$c_tp_2_pp <- 1 + (si$delta15n - ((si$d13c_norm_algae*(1 - si$c_ter_energy_pp)) + (si$d13c_norm_det*si$c_ter_energy_pp)))/3.4
#si$c_tp_2_sc <- 2 + (si$delta15n - ((si$d13c_norm_aqbase*(1 - si$c_ter_energy_sc)) + (si$d13c_norm_terbase*si$c_ter_energy_sc)))/3.4
