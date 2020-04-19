#clustering

library(tidyverse)
library(data.table)

#define number of clusters
no_clust <- 10

load("../../gen/analysis/input/nhchar.Rdata")
load("../../gen/analysis/input/moves_db.Rdata")
load("../../gen/analysis/temp/variables.Rdata")


#### select nh characteristics for nh of 's-Hertogenbosch

buurtcodes_db <- moves_db %>%
  filter(!buurtnaam %in% c("outside", "neighbour mun")) %>%
  distinct(buurtcode) %>%
  pull

nhchar_db <- nhchar %>%
  filter(buurtcode %in% buurtcodes_db)

nhchar_db18 <- nhchar %>%
  filter(buurtcode %in% buurtcodes_db & jaar == 2018)


############# NH types #######################

#remove NAs from nhchar_db18
nhchar_db18_cl <- nhchar_db18 %>%
  filter(HuishoudensTotaal > 60) %>%
  filter(!buurtcode %in% c(7961304, 7961303)) #take out Heeseind and De Lage Kant

#scale variables
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
##############################
set.seed(1)

kmeans_out <- kmeans(nhchar_scaled, no_clust, nstart = 40)

#results
nh_clusters <- data.table(cbind(nhchar_db18_cl$buurtcode, kmeans_out$cluster))
nh_clusters <- nh_clusters %>%
  rename(buurtcode = V1, cluster = V2)

# predefine clusters for neighbourhoods outside mun Den Bosch
# neighbourhood municicpalities
buurtcodes_neighb <- moves_db %>%
  filter(buurtnaam == "neighbour mun") %>%
  distinct(buurtcode) %>%
  add_column(cluster = no_clust + 1)

#other municipalities
buurtcodes_outside <- moves_db %>%
  filter(buurtnaam == "outside") %>%
  distinct(buurtcode) %>%
  add_column(cluster= no_clust + 2)

#table with clusters and buurtcodes
nh_clusters <- rbind(nh_clusters, buurtcodes_outside, buurtcodes_neighb)

#merge cluster into neighbourhood char
nhchar_cluster<- left_join(nhchar, nh_clusters, by = "buurtcode") %>%
  filter(!is.na(cluster))

# mean neighbourhood char by cluster
nhchar_cl <- nhchar_cluster %>%
  group_by(cluster, jaar) %>%
  summarize_at(vars(GemiddeldeWoningwaarde:perc.after2000),mean)

#fill Omgevingsadress with 2018 data
nhchar_cl <- nhchar_cl %>%
  group_by(cluster)%>%
  fill(Omgevingsadressendichtheid, .direction = "up")

#merge cluster (for 2018) into moving data
moves_db_cl <- moves_db %>%
  left_join(nh_clusters, by = "buurtcode") %>%
  #left_join(nh_type_char, by = "cluster", suffix = c("_vrg", "_nh")) %>%
  data.table

#as.factor cluster
moves_db_cl[,cluster := as.factor(cluster)]

save(moves_db_cl, nhchar_cl, nh_clusters, file = "../../gen/analysis/temp/data_cluster.Rdata")




