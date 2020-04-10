library(data.table)
library(tidyverse)
library(mlogit)

#source("src/data-preparation/clean_data_nh.R")
source("src/analysis/nh_types.R")

#for now only take nh of Den Bosch
moves_db <- moves_db %>%
  filter(!buurtnaam %in% c("outside", "neighbour mun"))%>%
  filter(jaar %in% c(2017, 2018)) %>%
  data.table


covar_indiv <- c("move_id",
                 "jaar",
                 "cluster",
                 "buurtnaam",
                 "buurtcode",
                 "vrg_buurtcode",
                 "PRSGES", 
                 #"age", 
                 #"AANBEW", 
                 #"cdhhw", 
                 #"cbsetngr",
                 #"hsh_cat",
                 "etnikort", 
                 #"vrgwrd", 
                 #"vrghr", 
                 #"vrgpct", 
                 #"vrgopp", 
                 #"vrgkam", 
                 "vrginh" 
                 #"vrghrkp", 
                 #"vrgeigd", 
                 #"vrgbjr", 
                 #"vrgbwwyz", 
                 #"vrgwtyp", 
                 #"vrgvlopp", 
)

moves_subs <- moves_db %>%
  select(covar_indiv) %>%
  data.table

#"AantalInwoners_5", 
#"HuishoudensTotaal_28", 
#"GemiddeldeWoningwaarde_35_vrg", 
#"Omgevingsadressendichtheid_106_vrg", 
#"AfstandTotZiekenhuis_11_vrg", 
#"AfstandTotOpritHoofdverkeersweg_89_vrg", 
#"Restaurants_3km_vrg", 
#"AfstandTotBelangrijkOverstapstation_91_vrg", 
#"AfstandTotKinderdagverblijf_52_vrg", 
#"AfstandTotBuitenschoolseOpvang_56_vrg", 
#"prop_nonwest_vrg", 
#"prop_singles_vrg", 
#"prop_couples_vrg", 
#"prop_fam_vrg", 
#"prop_bijstand_vrg", 
#"prop_income_socialmin_vrg", 
#"perc_property_vrg", 
#"perc_rent_vrg", 
#"perc_after2000_vrg", 
#"cluster",

covar_nh <- c(
  "cluster",
  #"buurtcode",
  "jaar",
  "GemiddeldeWoningwaarde_35", 
  #"Omgevingsadressendichtheid_106", 
  #"AfstandTotZiekenhuis_11", 
  "AfstandTotOpritHoofdverkeersweg_89", 
  "Restaurants_3km", 
  "AfstandTotBelangrijkOverstapstation_91", 
  #"AfstandTotKinderdagverblijf_52",
  #"AfstandTotBuitenschoolseOpvang_56", 
  "prop_nonwest", 
  "prop_singles", 
  "prop_couples", 
  #"prop_fam", 
  "prop_bijstand",
  #"prop_income_socialmin", 
  #"perc_property", 
  "perc_rent", 
  "perc_after2000"
)

nhchar_subs <- nhchar_clus %>% 
  select(covar_nh) %>%
  filter(!is.na(cluster))


covar_nh_vrg <- c(
  #"cluster",
  "buurtcode",
  "jaar",
  "GemiddeldeWoningwaarde_35", 
  "Omgevingsadressendichtheid_106", 
  "AfstandTotZiekenhuis_11", 
  #"AfstandTotOpritHoofdverkeersweg_89", 
  #"Restaurants_3km", 
  #"AfstandTotBelangrijkOverstapstation_91", 
  #"AfstandTotKinderdagverblijf_52",
  #"AfstandTotBuitenschoolseOpvang_56", 
  "prop_nonwest", 
  "prop_singles", 
  #"prop_couples", 
  #"prop_fam", 
  "prop_bijstand"
  #"prop_income_socialmin", 
  #"perc_property", 
  #"perc_rent", 
  #"perc_after2000"
)

nhchar_vrg_subs <- nhchar %>%
  select(covar_nh_vrg) %>%
  group_by(buurtcode)%>%
  fill(Omgevingsadressendichtheid_106, .direction = "up")


##### get data in right shape for ML model

#list of alternatives (neighbourhoods)
alternatives <- nhchar_subs %>%
  distinct(cluster) %>%
  pull 

#take out nh outside Gemeente DB
alternatives <- alternatives[length(alternatives)-2 : length(alternatives)]

#take subset
subs <- moves_subs[,.(move_id, jaar, cluster)]

#subset of moves to try approach
#subs <- subs[jaar == 2018]


dt <- data.table()

for (row in 1: nrow(subs)){
  temp <- data.table(subs[row], alternatives)
  dt <- rbind(dt, temp)
}

dt[, choice := ifelse(cluster == alternatives, 1, 0)]
dt[, cluster := NULL]
dt_alt <- dt %>%
  left_join(moves_subs, by = c("move_id", "jaar"), suffix = c("_alt", "_choice")) %>%
  left_join(nhchar_subs, by = c("alternatives" = "cluster", "jaar")) %>%
  #left_join(nhchar_vrg_subs, by = c("vrg_buurtcode" = "buurtcode", "jaar"), suffix = c("_alt", "_vrg")) %>%
  mutate(vrginh = as.numeric(vrginh)) %>%
  mutate(vrgwrd = as.numeric(vrgwrd))

#dt_alt <- dt_alt %>%
#  select(-c(cluster, buurtnaam, buurtcode, vrg_buurtcode))

dt_alt_18 <- dt_alt %>%
  filter(jaar == 2018)

dt_mlogit <- mlogit.data(dt_alt, shape = "long", choice = "choice", alt.var = "alternatives", drop.index = T)

#specify covariates

#alternative specific variables

alt_specific <- covar_nh[-(1:2)]


indiv_specifc <- covar_indiv <- c(#"move_id",
                                  #"jaar",
                                  #"cluster",
                                  #"buurtnaam",
                                  #"buurtcode",
                                  #"vrg_buurtcode",
                                  #"PRSGES", 
                                  "age", 
                                  #"AANBEW", 
                                  #"cdhhw", 
                                  #"cbsetngr",
                                  #"hsh_cat",
                                  "etnikort", 
                                  #"vrgwrd", 
                                  #"vrghr", 
                                  #"vrgpct", 
                                  #"vrgopp", 
                                  #"vrgkam", 
                                  "vrginh" 
                                  #"vrghrkp", 
                                  #"vrgeigd", 
                                  #"vrgbjr", 
                                  #"vrgbwwyz", 
                                  #"vrgwtyp", 
                                  #"vrgvlopp", 
)

#define formulats
#alternatives only
f_alt <- as.formula(paste("choice", 
                          paste(c(alt_specific, -1), collapse = " + "), 
                          sep = " ~ "))





# add individual specific variables
f_altind <- as.formula(paste("choice",
                            paste(
                              paste(alt_specific, collapse = " + "),
                              paste(c(indiv_specifc, -1), collapse = " + "),
                              sep = " | "),
                            sep = " ~ "))

int <- c()

# add interactions
for (i in 1:length(alt_specific)){
  for (j in 1:length(indiv_specifc)){
    int <- c(int, paste(c(alt_specific[i], indiv_specifc[j]), collapse = " : "))
  }
}

f_int <- as.formula(paste("choice",
                          paste(c(int, - 1), collapse = " + "),
                          sep = " ~ "))

#run models
m1 <- mlogit(f_alt, dt_mlogit)
summary(m1)


m2 <- mlogit(f_altind, dt_mlogit)
summary(m2)

m3 <- mlogit(f_int, dt_mlogit)

summary(m3)
