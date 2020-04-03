library(tidyverse)
library(data.table)

source("src/data-preparation/clean_cbs_data.R")

#load("gen/analysis/input/moves_db_anonym.Rdata")
source("src/data-preparation/fill_anonym.R")

####### prepare data for clustering NHs ##########################

moves_db <- moves_db_anonym %>% data.table


#Idenitfy neighbourhood municipalities

moves_db[buurtnaam %in% c("Heusden", "Vught", "Sint Michielsgestel", "Bernheze", "Oss"), `:=` (buurtnaam = "neighbour mun", wijknaam = "neighbour mun")]

moves_db[buurtnaam_vrg %in% c("Heusden", "Vught", "Sint Michielsgestel", "Bernheze", "Oss"), `:=` (buurtnaam_vrg = "neighbour mun", wijknaam_vrg = "neighbour mun")]

#remove cases from outside regio
moves_db<-moves_db %>%
  filter(buurtnaam != 'Outside Region') %>%
  filter(buurtnaam_vrg != "Outside Region")%>%
  data.table


moves_db[gem != "'s-Hertogenbosch" & buurtnaam != "neighbour mun", `:=` (buurtnaam = "outside", wijknaam = "outside")]

moves_db[vrggem != "'s-Hertogenbosch" & buurtnaam_vrg != "neighbour mun", `:=` (buurtnaam_vrg = "outside", wijknaam_vrg = "outside")]


# take out neighbourhoods with no living space
moves_db <- moves_db %>%
  filter(!buurtnaam %in% c("Bedrijventerrein-Zuid",  "Bedrijventerrein De Herven", "De Lanen", "Zoggel - Berkt", "Kloosterstraat", "De Rietvelden-Oost", 
                           "Bedrijventerrein Nuland", "Bedrijventerrein Kruisstraat"))

### 2018 #####

moves_db_18 <- filter(moves_db, jaar == 2018)

#merge neighbourhood characteristics for 2018 moving data

moves_db_18 <- moves_db_18 %>%
  left_join(nhchar_18, by=c("vrg_buurtcode" = "code"))


#list of all neighbourhoods with moves
nh_db <- moves_db_18 %>%
  filter(!buurtnaam %in% c("neighbour mun", "outside")) %>%
  distinct(buurtnaam, .keep_all = T) %>%
  select(c(jaar, buurtnaam, buurtcode)) %>%
  left_join(nhchar_18, by = c("buurtcode" = "code"))


#remove neighbourhoods with missing data
# !!!! check how to solve this!
nh_db <- nh_db %>%
  filter(HuishoudensTotaal_28 > 50) %>%
  filter(!buurtnaam %in% c("De Lage Kant", "Heeseind", "A2 zone Rosmalen-Zuid")) #take out because of NAs in some variables

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


