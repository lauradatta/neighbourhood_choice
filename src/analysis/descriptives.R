library(tidyverse)
library(data.table)


load("../../gen/analysis/temp/data_cluster.Rdata")
load("../../gen/analysis/input/nhchar.Rdata")
load("../../gen/analysis/temp/variables.Rdata")

moves_db <- moves_db_cl

VDI = T #set to True when working on VDI

if (VDI == T){
  #how many households have moved
  moves_db %>%
    distinct(PRSANR) %>%
    count()
  
  #how often people move
  moves <- moves_db %>%
    add_count(PRSANR) %>%
    rename(n_moves = n) %>%
    data.table
  
  #unique PRSANR
  moves_db_uniq <- moves_db %>%
    distinct(PRSANR, .keep_all = T)
  
  mean(moves_db_uniq$n_moves)
  sd(moves_db_uniq$n_moves)
  median(moves_db_uniq$n_moves)
  hist(moves_db_uniq$n_moves)
  
  # check moves respective to administrative units
  moves_db %>%
    count(move_type)
}

#### neighbourhood characteristics ######

buurtcodes_db <- moves_db %>% 
  filter(!buurtnaam %in% c("outside", "neighbour mun")) %>%
  distinct(buurtcode) %>%
  pull

sum_nh <- nhchar %>%
  filter(buurtcode %in% buurtcodes_db) %>%
  select(jaar, covar_nh) %>%
  filter(jaar == 2018) %>%
  group_by(jaar) %>%
  summarise_all(list(mean = mean, sd = sd, min = min, max = max), na.rm =T)

descr_nh <- sum_nh %>%
  pivot_longer(-jaar) %>%
  select(-jaar)%>%
  separate(name, into = c("name", "stat"), sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value)

descr_dh_df <- descr_nh %>%
  select(-name) %>%
  data.frame(row.names = labels_covar_nh)


##### on cluster level ########

descr_nh_cluster <- nhchar_cl %>%
  select(c(cluster,jaar,var_cluster)) %>%
  group_by(cluster)%>%
  filter(jaar == 2018 & cluster %in% 1:10)%>%
  select(-jaar) %>%
  pivot_longer(-cluster) %>%
  pivot_wider(names_from = cluster, values_from = value)


descr_nh_cluster_df <- descr_nh_cluster %>%
  select(-name) %>%
  data.frame(row.names = labels_cluster_var)

colnames(descr_nh_cluster_df) <- paste("Cluster", 1:10)


#### Household characterisitcs #######

t_hh_type <- moves_db %>%
  select(c(jaar,hh_type)) %>%
  filter(jaar == 2018) %>%
  count(hh_type)

t_ethni <- moves_db %>%
  select(c(jaar,ethnicity)) %>%
  filter(jaar == 2018) %>%
  count(ethnicity)

t_vrginh <- moves_db %>%
  select(c(jaar,vrginh)) %>%
  filter(jaar == 2018) %>%
  summarise(mean(vrginh, na.rm = T))

save(descr_dh_df, descr_nh_cluster_df, file = "../../gen/analysis/output/descriptives.Rdata")