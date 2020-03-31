library(tidyverse)
library(data.table)

source("src/data-preparation/clean_data_2.R")


#choose variables to build nh types

#variable selection for construction neighbourhood types

var_nh_types <- c(#"jaar",
                  #"buurtnaam",
                  #"buurtcode",
                  #"AantalInwoners_5",
                  #"HuishoudensTotaal_28",
                  "GemiddeldeWoningwaarde_35",
                  "Omgevingsadressendichtheid_106",
                  #"AfstandTotZiekenhuis_11",
                  "AfstandTotOpritHoofdverkeersweg_89",
                  #"Restaurants_3km",
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
  
pr_out <- prcomp(nh_pca, scale = T)

summary(pr_out)


result <- cbind(nh_pca$buurtnaam,data.table(pr_out$x[,1:2]))

#plot PC1 and PC2
ggplot(result, aes(x = PC1, y = PC2, label = V1)) +
  geom_point()+
  geom_text()

##### K means clustering #####
set.seed(1)

kmeans_out <- kmeans(nh_db_scaled, 6, nstart = 40)

kmeans_out

#results
cbind(nh_db$buurtnaam, kmeans_out$cluster)


#plot results on map

#read cbs_buurten 2018 geodata, with geojson package
cbs_buurten_sp <- geojson_read("input/raw-data/cbs_buurten_2018.geojson", what = "sp")
#transform into 'sf' data format
cbs_buurten <- st_as_sf(cbs_buurten_sp)
#filter for only Den Bosch and buurtcodes
cbs_buurten_db <- cbs_buurten %>%
  filter(gemeentenaam == "'s-Hertogenbosch") %>%
  select(c(id, buurtnaam, buurtcode, geometry))
