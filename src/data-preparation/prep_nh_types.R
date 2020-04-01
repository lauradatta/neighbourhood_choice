library(tidyverse)
library(data.table)

source("src/data-preparation/clean_cbs_data.R")

load("gen/analysis/input/moves_db_anonym.Rdata")

moves_db <- moves_db_anonym %>% data.table


#Idenitfy neighbourhood municipalities

moves_db[buurtnaam %in% c("Heusden", "Vught", "Sint Michielsgestel", "Bernheze", "Oss"), `:=` (buurtnaam = "neighbour mun", wijknaam = "neighbour mun")]

moves_db[buurtnaam_vrg %in% c("Heusden", "Vught", "Sint Michielsgestel", "Bernheze", "Oss"), `:=` (buurtnaam_vrg = "neighbour mun", wijknaam_vrg = "neighbour mun")]

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


