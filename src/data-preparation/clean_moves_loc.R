#continue clean moving data

#set up
library(tidyverse)
library(data.table)

load("../../gen/data-preparation/temp/moves.Rdata")

#take out cases which didn't happen within Gemeente Den Bosch
moves_db <- moves %>%
  filter(gem == "'s-Hertogenbosch" | vrggem == "'s-Hertogenbosch" ) %>%
  data.table

#if the vrggem is NA, this is a move from Outside Region
moves_db[is.na(vrggem), `:=` (vrggem = "Outside Region", wijknaam_vrg = "Outside Region", 
                              buurtnaam_vrg = "Outside Region")]

#if gem is NA, this is a move out of the regio
moves_db[is.na(gem), `:=` (gem = "Outside Region", wijknaam = "Outside Region", buurtnaam = "Outside Region")]

# if gem is not Den Bosch and not Outside Region this is a move out of the Gem (but within region)
moves_db[gem != "'s-Hertogenbosch" & gem != "Outside Region", `:=` (wijknaam = gem, buurtnaam = gem)]

# if vrggem not Den Bosch nor Outside Region, but outside the Gemeente Den Bosch (but within region) --> Outside Municipality
moves_db[vrggem != "'s-Hertogenbosch" & vrggem != "Outside Region", `:=` 
         (wijknaam_vrg = vrggem, buurtnaam_vrg = vrggem)]

### determine verhuis type

#if buurtnaam = buurtnaam_vrg -> within nh
moves_db[buurtnaam == buurtnaam_vrg, move_type := "within"]

#if buurtnaam != buurtnaam_vrg & gem = 's-Hertogenbosch -> within mun
moves_db[buurtnaam != buurtnaam_vrg & gem == "'s-Hertogenbosch" & vrggem == "'s-Hertogenbosch", 
         move_type := "within"]

#if buurtnaam = "outside municipality"-> out mun
moves_db[gem != "'s-Hertogenbosch", move_type := "out"]

#if buurtnaam = "outside region" -> out regio
moves_db[buurtnaam == "Outside Region", move_type := "out"]

#if buurtnaam_vrg = "outside municipality" -> in mun
moves_db[vrggem != "'s-Hertogenbosch", move_type := "into"]

#if buurtnaam_vrg = "outside region" -> in regio
moves_db[buurtnaam_vrg == "Outside Region", move_type := "into"]

#take out moves where no information on old or new pkd available
moves_db <- moves_db[!is.na(move_type)]


# make categories

#categories ethnicity
moves_db[etnikort == "Autochtonen", ethnicity := "dutch"]
moves_db[etnikort == "Westerse allochtonen", ethnicity := "western"]
moves_db[!etnikort %in% c("Autochtonen", "Westerse allochtonen"), ethnicity := "non-western"]

#categories age
moves_db[age < 25, age_cat := "<25"]
moves_db[age >=25 & age <=65, age_cat := "25-65"]
moves_db[age > 65, age_cat := ">65"]

#categories household type
moves_db[cdhhw == "Tweepersoonshuishouden", hh_type := "couple"]
moves_db[cdhhw == "Gezin met kinderen" | cdhhw == "Eénoudergezin", hh_type := "fam with children"]
moves_db[!hh_type %in% c("couple", "fam with children"), hh_type := "single or other"]

#cleaning variables
moves_db[, jaar := factor(jaar)]
moves_db[, vrginh := as.numeric(vrginh)]

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

save(moves_db, file = "../../gen/data-preparation/output/moves_db.Rdata")





