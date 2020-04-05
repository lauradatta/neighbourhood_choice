library(data.table)
library(tidyverse)
library(mlogit)

ssource("src/data-preparation/clean_data_nh.R")

#for now only take nh of Den Bosch
moves_db <- moves_db %>%
  filter(!buurtnaam %in% c("outside", "neighbour mun"))%>%
  filter(jaar %in% c(2017, 2018))
  data.table


covar_indiv <- c("move_id",
              "jaar",
              "buurtnaam",
             "buurtcode",
             "vrg_buurtcode",
              "PRSGES", 
               "age", 
               #"AANBEW", 
               #"cdhhw", 
               #"cbsetngr", 
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
            "code",
            "jaar",
            "GemiddeldeWoningwaarde_35", 
            #"Omgevingsadressendichtheid_106", 
            #"AfstandTotZiekenhuis_11", 
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

nhchar_subs <- nhchar %>% 
  select(covar_nh)

##### get data in right shape for ML model

#list of alternatives (neighbourhoods)
alternatives <- moves_subs %>%
  distinct(buurtnaam, buurtcode) %>%
  rename(alternatives = buurtnaam)


a <- moves_subs[,.(move_id, jaar, buurtnaam)]

#subset of moves to try approach
a <- a[move_id %in% seq(33,533,50),]


dt <- data.table()

for (row in 1: nrow(a)){
  temp <- data.table(a[row], alternatives)
  dt <- rbind(dt, temp)
}

dt[, choice := ifelse(buurtnaam == alternatives, 1, 0)]
dt[, buurtnaam := NULL]
dt_alt <- dt %>%
  left_join(moves_subs, by = c("move_id", "jaar"), suffix = c("_alt", "_choice")) %>%
  left_join(nhchar_subs, by = c("buurtcode_alt" = "code", "jaar"))

dt_alt %>%
  select(-c(buurtnaam, buurtcode_choice, vrg_buurtcode))



#####

test_dt <- mlogit.data(dt_alt, shape = "long", choice = "choice", alt.var = "alternatives")
# choice ~ alternative specific var | individual specific var
test <- mlogit(choice ~ prop_nonwest + prop_singles | age + vrginh, test_dt)
