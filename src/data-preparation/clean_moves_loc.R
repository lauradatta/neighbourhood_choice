#continue clean moving data

#set up
library(tidyverse)
library(data.table)

load("../../gen/data-preparation/temp/moves_db.Rdata")

####### merge move data with neighbourhood data ##########################

moves_db[, jaar := factor(jaar)]

#Idenitfy neighbourhood municipalities

moves_db[buurtnaam %in% c("Heusden", "Vught", "Sint Michielsgestel", "Bernheze", "Oss"), `:=` (buurtnaam = "neighbour mun", wijknaam = "neighbour mun")]

moves_db[buurtnaam_vrg %in% c("Heusden", "Vught", "Sint Michielsgestel", "Bernheze", "Oss"), `:=` (buurtnaam_vrg = "neighbour mun", wijknaam_vrg = "neighbour mun")]

#remove cases from outside regio because no data available on neighbourhoods
moves_db<-moves_db %>%
  filter(buurtnaam != 'Outside Region') %>%
  filter(buurtnaam_vrg != "Outside Region")%>%
  data.table

#define neighbour and other municipalities
moves_db[gem != "'s-Hertogenbosch" & buurtnaam != "neighbour mun", `:=` (buurtnaam = "outside", wijknaam = "outside")]

moves_db[vrggem != "'s-Hertogenbosch" & buurtnaam_vrg != "neighbour mun", `:=` (buurtnaam_vrg = "outside", wijknaam_vrg = "outside")]

# take out neighbourhoods with no living space (i.e. bedrijventerreinen)
moves_db <- moves_db %>%
  filter(!buurtnaam %in% c("Bedrijventerrein-Zuid",  "Bedrijventerrein De Herven", "De Lanen", "Zoggel - Berkt", "Kloosterstraat", "De Rietvelden-Oost", 
                           "Bedrijventerrein Nuland", "Bedrijventerrein Kruisstraat", "Bedrijventerrein Maaspoort", "Brabantpoort", "Willemspoort")) %>%
  data.table

#create move id
moves_db[, move_id := seq_len(.N), by = jaar]

save(moves_db, file = )


