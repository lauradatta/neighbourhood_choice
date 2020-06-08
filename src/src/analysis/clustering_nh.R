### k-means clustering ####

#script runs k-means clustering to cluster neighbourhoods

####### set up
#set working directory to source file location

#load packages
library(tidyverse)
library(data.table)

# number of clusters as defined by clustering_test.R
no_clust <- 9

# copy following files from "../../gen/data-prepartion/output/ first
load("../../gen/analysis/input/nhchar.Rdata")
load("../../gen/analysis/input/moves_db.Rdata")

#script with variables to be included in clustering
source("../../src/analysis/variable_selection.R")

############

#### select nh characteristics for nh of 's-Hertogenbosch

#buurtcodes s'H
buurtcodes_db <- moves_db %>%
  filter(gem == "'s-Hertogenbosch") %>%
  distinct(buurtcode) %>%
  pull

#chose neighbourhoods within s'H
nhchar_db <- nhchar %>%
  filter(buurtcode %in% buurtcodes_db)

#for year 2018
nhchar_db18 <- nhchar %>%
  filter(buurtcode %in% buurtcodes_db & jaar == 2018)

### add variable housing size####

# hous_size <- moves_db %>%
#   filter(buurtcode %in% buurtcodes_db) %>%
#   group_by(jaar, buurtcode) %>%
#   summarise(hous_size = round(mean(as.numeric(vloeropp), na.rm = T),2))
#
# nhchar_db18 <- nhchar_db18 %>%
#   left_join(hous_size, by = c("jaar", "buurtcode"))

### take out NAs ###


# take out the following 3 neighbourhoods because they are not really living neighbourhoods
take_out <- c(7960504,  7961111,  7961402)


#buurtcodes with NAs from nhchar_db18
buurtcode_mis <- c(nhchar_db18 %>%
                     filter_all(any_vars(is.na(.))) %>%
                     pull(buurtcode),
                   take_out)


#remove neighbourhoods with NAs from neighbourhood characteristics
nhchar_db18_cl <- nhchar_db18 %>%
  filter(!buurtcode %in% buurtcode_mis)

# also remove those neighbourhoods from moving data
moves_db_red <- moves_db %>%
  filter(!buurtcode %in% buurtcode_mis)

##### K means clustering #####
##############################

#scale variables
nhchar_scaled <- nhchar_db18_cl %>%
  select(var_cluster) %>%
  mutate_all( ~ (scale(.) %>% as.vector))

set.seed(1)

#k-means clustering
kmeans_out <- kmeans(nhchar_scaled, no_clust, nstart = 25)

#results
nh_clusters <-
  data.table(cbind(nhchar_db18_cl$buurtcode, kmeans_out$cluster))
nh_clusters <- nh_clusters %>%
  rename(buurtcode = V1, cluster = V2)

#assign cluster number according to mean household size of clusters

# predefine clusters for neighbourhoods outside mun Den Bosch
# neighbourhood municicpalities
buurtcodes_neighb <- moves_db_red %>%
  filter(buurtnaam == "neighbour mun") %>%
  distinct(buurtcode) %>%
  add_column(cluster = no_clust + 1)

#other municipalities
buurtcodes_outside <- moves_db_red %>%
  filter(buurtnaam == "outside mun") %>%
  distinct(buurtcode) %>%
  add_column(cluster = no_clust + 2)

#table with clusters and buurtcodes
nh_clusters <-
  rbind(nh_clusters, buurtcodes_outside, buurtcodes_neighb)

#merge cluster into neighbourhood char
nhchar_cluster <-
  left_join(nhchar, nh_clusters, by = "buurtcode") %>%
  filter(!is.na(cluster))

# mean neighbourhood char by cluster
#add total number of households by cluster
pop_tot <- nhchar_cluster %>%
  group_by(cluster, jaar) %>%
  summarise(hh_tot = sum(HuishoudensTotaal))

nhchar_cl <- nhchar_cluster %>%
  group_by(cluster, jaar) %>%
  summarize_at(vars(AantalInwoners:income.nh), mean, na.rm = T) %>%
  left_join(pop_tot, by = c("cluster", "jaar"))


#merge cluster (for 2018) into moving data
moves_db_cl <- moves_db_red %>%
  left_join(nh_clusters, by = "buurtcode") %>%
  left_join(
    nh_clusters,
    by = c("vrg_buurtcode" = "buurtcode"),
    suffix = c("", "_vrg")
  ) %>%
  mutate(cluster = as.factor(cluster),
         cluster_vrg = as.factor(cluster_vrg)) %>%
  data.table

#format cluster to factor
moves_db_cl[, cluster := as.factor(cluster)]

#make sure to copy this file to "../../gen/paper/input/"
save(moves_db_cl, nhchar_cl, nh_clusters, file = "../../gen/analysis/output/data_cluster.Rdata")
