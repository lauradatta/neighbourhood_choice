library(tidyverse)
library(data.table)

source("src/data-preparation/clean_cbs_data.R")

#working on VDI
load("gen/analysis/input/moves_db.Rdata")

#uncomment working on local machine
#load("gen/analysis/input/moves_db_anonym.Rdata")
#source("src/data-preparation/fill_anonym.R")
#moves_db <- moves_db_anonym %>% data.table

####### merge move data with neighbourhood data ##########################


moves_db[, jaar := factor(jaar)]

nhchar[, jaar := factor(jaar)]


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
                           "Bedrijventerrein Nuland", "Bedrijventerrein Kruisstraat", "Bedrijventerrein Maaspoort", "Brabantpoort", "Willemspoort")) %>%
  data.table


moves_db[, move_id := seq_len(.N), by = jaar]

# #merge neighbourhood characteristics of previous neighbourhood with moving data (2017 and 2018)
# moves_db_nh <- moves_db %>%
#   filter(jaar %in% c("2017",  "2018")) %>%
#   left_join(nhchar, by=c("vrg_buurtcode" = "code", "jaar"))
# 
# 
# #list of all neighbourhoods with moves
# nh_db <- moves_db_nh %>%
#   filter(!buurtnaam %in% c("neighbour mun", "outside")) %>%
#   distinct(buurtnaam, jaar, .keep_all = T) %>%
#   select(c(jaar, buurtnaam, buurtcode)) %>%
#   left_join(nhchar, by = c("buurtcode" = "code", "jaar"))
# 
# # merge neighbourhood characteristics of current neighbourhood with moving data
# 
# moves_db_nh <- moves_db_nh %>%
#   left_join(nhchar, by = c("buurtcode" = "code", "jaar"), suffix = c("_vrg",""))

#### only keep data set
rm(list=(ls()[!ls() %in% c("moves_db", "nhchar")]))
            