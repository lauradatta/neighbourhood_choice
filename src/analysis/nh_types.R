library(tidyverse)
library(data.table)

source("src/data-preparation/clean_cbs_data.R")

#load("gen/analysis/input/moves_db_anonym.Rdata")
source("src/data-preparation/clean_data_nh.R")



############# NH types #######################


#choose variables to build nh types

#variable selection for construction neighbourhood types

var_nh_types <- c(#"jaar",
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
  "prop_income_socialmin",
  #"perc_property",
  "perc_rent",
  "perc_after2000"
)

#select variables

nh_db_scaled <- nh_db %>%
  select(var_nh_types) %>%
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

kmeans_out <- kmeans(nh_db_scaled, 5, nstart = 40)

kmeans_out

#results
nh_clusters <- data.table(cbind(nh_db$buurtcode, kmeans_out$cluster))
nh_clusters <- nh_clusters %>%
  rename(buurtcode = V1, cluster = V2)

nh_db_cl <- left_join(nh_db, nh_clusters, by = "buurtcode")

nh_db_cl %>%
  group_by(cluster) %>%
  summarize_at(vars(GemiddeldeWoningwaarde_35:perc_after2000),mean)

nh_type_char <- nh_db_cl %>%
  group_by(cluster) %>%
  summarize_at(vars(GemiddeldeWoningwaarde_35:perc_after2000),mean)

#merge cluster back into moving data

moves_db_18_cl <- moves_db_18 %>%
  left_join(nh_clusters, by = "buurtcode") %>%
  #left_join(nh_type_char, by = "cluster", suffix = c("_vrg", "_nh")) %>%
  data.table
moves_db_18_cl[,cluster := as.factor(cluster)]
moves_db_18_cl[buurtnaam == "outside", cluster := "outside"]
moves_db_18_cl[buurtnaam == "neighbour mun", cluster := "neighbour mun"]

moves_18 <- moves_db_18_cl


#### only keep data set
rm(list=(ls()[!ls() %in% c("moves_18", "nh_type_char")]))


