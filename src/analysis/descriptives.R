#Prepare tables for descriptive statistics



##### set up
#set working directory to source file location

#load packages#
library(tidyverse)
library(data.table)

#load data
load("../../gen/analysis/output/data_cluster.Rdata")
load("../../gen/analysis/input/nhchar.Rdata")
source("../../src/analysis/variable_selection.R")

#number of clusters
no_clust <- 9

## take out NAs from moving data (this corresponds to the dataset used in the mlogit model)
moves_db <- moves_db_cl %>%
  drop_na(covar_indiv) %>%
  data.table

moves_db <- moves_db %>%
  left_join(nh_clusters, by = c("vrg_buurtcode" = "buurtcode"), suffix = c("", "_vrg")) %>%
  select(-vrg_buurtcode) %>%
  mutate(cluster_vrg = as.factor(cluster_vrg)) %>%
  data.table

#### neighbourhood characteristics ######

## Descriptive statistics on variables used for clustering ###

buurtcodes_db <- moves_db %>% 
  filter(!buurtnaam %in% c("outside mun", "neighbour mun")) %>%
  distinct(buurtcode) %>%
  pull

summary_nh <- nhchar %>%
  filter(buurtcode %in% buurtcodes_db) %>%
  select(jaar, var_cluster) %>%
  filter(jaar == 2018) %>%
  group_by(jaar) %>%
  summarise_all(list(Mean = mean, Sd = sd, Min = min, Max = max), na.rm =T)

summary_nh <- summary_nh %>%
  mutate_at(vars(matches("prop|perc")), list(~(. * 100))) %>%
  mutate_at(vars(GemiddeldeWoningwaarde_Mean : leefbh_Max), list(~round(.,1)))

descr_nh <- summary_nh %>%
  pivot_longer(-jaar) %>%
  select(-jaar)%>%
  separate(name, into = c("name", "stat"), sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value)


nhchar %>%
  filter(buurtcode %in% buurtcodes_db & jaar == 2018) %>%
  summarise(sum(AantalInwoners), sum(HuishoudensTotaal))



######### Cluster characteristics #############


#variables for complete table on clusters
var_nh <- c("AantalInwoners",
            "HuishoudensTotaal",
            "GemiddeldeWoningwaarde",
            "income.nh",
            "prop.dutch",
            "prop.nonwest",
            "prop.west",
            "prop.singles",
            "prop.couples",
            "prop.fam",
            "perc.property",
            "perc.priv.rent",
            "perc.soc.rent",
            "perc.after2000",
            "Omgevingsadressendichtheid",
            "AfstandTotOpritHoofdverkeersweg",
            "AfstandTotBelangrijkOverstapstation",
            "Restaurants3km",
            "leefbh",
            "hh_tot")

#order of variables
variable_order <- c("n",
                    "hh_tot",
                    "HuishoudensTotaal",
                    "AantalInwoners",
                    "mean_turnover",
                    "income.nh",
                    "prop.dutch",
                    "prop.nonwest",
                    "prop.west",
                    "prop.singles",
                    "prop.couples",
                    "prop.fam",
                    "GemiddeldeWoningwaarde",
                    "house_size",
                    "perc.property",
                    "perc.priv.rent",
                    "perc.soc.rent",
                    "perc.after2000",
                    "Omgevingsadressendichtheid",
                    "AfstandTotOpritHoofdverkeersweg",
                    "AfstandTotBelangrijkOverstapstation",
                    "Restaurants3km",
                    "leefbh"
                    )

#variables for smaller table
var_nh_redu <- c("leefbh",
                  "GemiddeldeWoningwaarde",
                  "prop.dutch",
                  "prop.fam",
                  "perc.soc.rent",
                  "AfstandTotBelangrijkOverstapstation",
                  "hh_tot")

variable_order_redu <- c("leefbh",
                    "GemiddeldeWoningwaarde",
                    "prop.dutch",
                    "prop.fam",
                    "perc.soc.rent",
                    "AfstandTotBelangrijkOverstapstation")

### Number of neighbourhoods per cluster ####
n_nh_clust <- moves_db_cl %>%
  filter(jaar == 2018) %>%
  filter(!buurtnaam %in% c("outside mun", "neighbour mun")) %>%
  distinct(buurtcode, cluster) %>%
  count(cluster)# %>%
  #pivot_wider(names_from = cluster, values_from = n)


##### Calculate Turnover rates #######
turnover_cl <- moves_db_cl %>%
  filter(jaar == 2018) %>%
  filter(!buurtnaam %in% c("outside mun", "neighbour mun"))%>%
  count(buurtcode, buurtnaam, cluster) %>%
  left_join(nhchar %>% 
              filter(jaar == 2018) %>%
              select(c(buurtcode, HuishoudensTotaal)), by = "buurtcode") %>%
  mutate(turnover = n / HuishoudensTotaal) %>%
  group_by(cluster) %>%
  summarise(mean_turnover = round(mean(turnover, na.rm = T)*100, 1), max(turnover), min(turnover)) #%>%
  #pivot_wider(names_from = cluster, values_from = mean_turnover)

#Housing size
house_size <- moves_db_cl %>%
  filter(jaar == 2018 & buurtcode %in% buurtcodes_db) %>%
  group_by(buurtcode, buurtnaam, cluster) %>%
  summarise(hous_size = round(mean(as.numeric(vloeropp), na.rm = T),2)) %>%
  group_by(cluster) %>%
  summarise(house_size = round(mean(hous_size, na.rm = T),1)) #%>%
  #pivot_wider(names_from = cluster, values_from = house_size)

add_var <- left_join(n_nh_clust, turnover_cl, by = "cluster") %>%
  left_join(house_size, by = "cluster") %>%
  mutate(cluster = as.numeric(cluster))


##### Table: descriptive stats for neighbourhood clusters ########
nhchar_cl_descr <- nhchar_cl %>%
  mutate_at(vars(matches("prop|perc")), .funs = funs(. * 100)) %>%
  mutate_at(vars(AantalInwoners : prop.dutch), .funs = funs(round(.,0))) %>%
  mutate(leefbh = round(leefbh, 1))
  
descr_nh_cluster <- nhchar_cl_descr %>%
  select(c(cluster,jaar,var_nh)) %>%
  group_by(cluster)%>%
  filter(jaar == 2018 & cluster %in% 1:no_clust)%>%
  left_join(add_var, by = "cluster") %>%
  select(-jaar) %>%
  select(c("cluster", variable_order)) %>%
  arrange(leefbh) %>%
  pivot_longer(-cluster) %>%
  pivot_wider(names_from = cluster, values_from = value)

## reduced tabled with one variable per dimension
descr_cluster_redu <- nhchar_cl_descr %>%
  select(c(cluster,jaar,var_nh_redu)) %>%
  group_by(cluster)%>%
  filter(jaar == 2018 & cluster %in% 1:no_clust)%>%
  left_join(add_var, by = "cluster") %>%
  select(-jaar) %>%
  select(c("cluster", variable_order_redu)) %>%
  arrange(leefbh)



#### get order of clusters in terms of nh reputation #####
cluster_ord <- nhchar_cl_descr %>%
  select(c(cluster,jaar,var_nh)) %>%
  group_by(cluster)%>%
  filter(jaar == 2018 & cluster %in% 1:no_clust)%>%
  left_join(add_var, by = "cluster") %>%
  select(-jaar) %>%
  select(c("cluster", variable_order)) %>%
  arrange(leefbh) %>%
  pull(cluster)


#### Descr: Household characterisitcs #######

#number of moves
moves_db[!cluster %in% c(10,11), .N]

#number of hh in moving data
moves_db[!cluster %in% c(10,11), .N , by = PRSANR]

#mean number of moves per HH
moves_db[!cluster %in% c(10,11), .N , by = PRSANR][,mean(N)]

#number of moves within
moves_db %>%
  filter(!cluster %in% c(10,11)) %>%
  group_by(move_type) %>%
  summarise(N = n())%>%
  mutate(freq = N/sum(N))

########## Table descriptives household characteristics


covar_hh <-c("move_type_gen_within",
              "income_hh",
             "ethnicity_dutch",
              "ethnicity_non_western",
              "ethnicity_western",
             "hh_type_single",
             "hh_type_couple",
             "hh_type_fam",
              "age_cat_25",
             "age_cat_25_44",
              "age_cat_45_64",
              "age_cat_65",
              "room_stress")

#mean household characteristics
hh_mean <- moves_db %>%
  filter(!cluster %in% c(10,11)) %>%
  summarise_at(vars(covar_hh), mean) %>%
  mutate_at(vars(-c(income_hh, room_stress)), ~(round(.*100,1))) %>%
  mutate(room_stress = round(room_stress,2)) %>%
  pivot_longer(move_type_gen_within:room_stress, names_to = "hh_char", values_to = "Mean")


#make sure to copy this file to "../../gen/paper/input/"
save(descr_nh, descr_nh_cluster, cluster_ord, file = "../../gen/analysis/output/descriptives.Rdata")
