library(tidyverse)
library(data.table)

#source("src/data-preparation/clean_cbs_data.R")

#load("gen/analysis/input/moves_db_anonym.Rdata")
source("src/data-preparation/clean_data_nh.R")

############# NH types #######################

#remove NAs from nhchar_db18
nhchar_db18_cl <- nhchar_db18 %>%
  filter(HuishoudensTotaal_28 > 60) %>%
  filter(!buurtcode %in% c(7961304, 7961303)) #take out Heeseind and De Lage Kant

#choose variables to build nh types

#variable selection for construction neighbourhood types

var_cluster <- c(#"jaar",
  #"buurtnaam",
  #"buurtcode",
  #"AantalInwoners_5",
  #"HuishoudensTotaal_28",
  "GemiddeldeWoningwaarde_35",
  "Omgevingsadressendichtheid_106",
  "AfstandTotZiekenhuis_11",
  "AfstandTotOpritHoofdverkeersweg_89",
  "Restaurants_3km",
  "AfstandTotBelangrijkOverstapstation_91",
  #"AfstandTotKinderdagverblijf_52",
  #"AfstandTotBuitenschoolseOpvang_56",
  "prop_nonwest",
  "prop_singles",
  #"prop_couples",
  "prop_fam",
  "prop_bijstand",
  #"prop_income_socialmin",
  #"perc_property",
  "perc_rent",
  "perc_after2000"
)

#select variables


nhchar_scaled <- nhchar_db18_cl %>%
  select(var_cluster) %>%
  mutate_all(~(scale(.) %>% as.vector))


####### PCA ###########
#######################

#pr_out <- prcomp(nh_db_scaled, scale = T)

#summary(pr_out)


#result <- cbind(nh_db$buurtcode,data.table(pr_out$x[,1:2]))

#plot PC1 and PC2
#ggplot(result, aes(x = PC1, y = PC2, label = V1)) +
#  geom_point()+
#  geom_text()

##### K means clustering #####
set.seed(1)

no_clust <- 10

kmeans_out <- kmeans(nhchar_scaled, no_clust, nstart = 40)

kmeans_out

#results
nh_clusters <- data.table(cbind(nhchar_db18_cl$buurtcode, kmeans_out$cluster))
nh_clusters <- nh_clusters %>%
  rename(buurtcode = V1, cluster = V2)

# clusters for neighbourhoods outside mun Den Bosch
#identify buurtcodes
buurtcodes_outside <- moves_db %>%
  filter(buurtnaam == "outside") %>%
  distinct(buurtcode) %>%
  add_column(cluster= no_clust + 2)

buurtcodes_neighb <- moves_db %>%
  filter(buurtnaam == "neighbour mun") %>%
  distinct(buurtcode) %>%
  add_column(cluster = no_clust + 1)

nh_clusters <- rbind(nh_clusters, buurtcodes_outside, buurtcodes_neighb)

#join cluster to neighbourhood char
nhchar_cluster<- left_join(nhchar, nh_clusters, by = "buurtcode") %>%
  filter(!is.na(cluster))

# mean neighbourhood char by cluster
nhchar_clus <- nhchar_cluster %>%
  group_by(cluster, jaar) %>%
  summarize_at(vars(GemiddeldeWoningwaarde_35:perc_after2000),mean)

#fill Omgevingsadress with 2018 data
nhchar_clus <- nhchar_clus %>%
  group_by(cluster)%>%
  fill(Omgevingsadressendichtheid_106, .direction = "up")

#merge cluster (2018) back into moving data

moves_db <- moves_db %>%
  left_join(nh_clusters, by = "buurtcode") %>%
  #left_join(nh_type_char, by = "cluster", suffix = c("_vrg", "_nh")) %>%
  data.table
moves_db[,cluster := as.factor(cluster)]
#moves_db[buurtnaam == "outside", cluster := "outside"]
#moves_db[buurtnaam == "neighbour mun", cluster := "neighbour mun"]


#### only keep data set
rm(list=(ls()[!ls() %in% c("moves_db", "nhchar_clus", "nhchar", "nh_clusters")]))


