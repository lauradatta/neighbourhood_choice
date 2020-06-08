# Load and clean moving data

############## set up
#set working directory to Source file location

#load packages
library(tidyverse)
library(data.table)
library(fastDummies)

#directory
dir <- "../../gen/data-preparation/"


#load moving and population data
load(paste0(dir,"input/verhuis1418.Rdata"))
load(paste0(dir,"input/pc6_gwb.Rdata")) # Buurtcodes as defined by CBS
load("../../gen/data-preparation/input/income.Rdata") # income data purchased from CBS
######################

#### pre-cleaning #####

#take out Gemeentenaam from GWB codes
pc6_wb <- pc6_gwb %>%
  select(-gemeentenaam)


#transform into data.tale
vhb1418_door <- data.table(vhb1418_door)
vhb1418_in <- data.table(vhb1418_in)
vhb1418_uit <- data.table(vhb1418_uit)

#merge moving 'In' and 'door' datasets together
vhb1418_door[,verhuis_type := "door"]
vhb1418_in[,verhuis_type := "in" ]
vhb1418_uit[,verhuis_type := "uit"]


#merge in and door
vhb_di_raw <- bind_rows(vhb1418_door, vhb1418_in, .id=NULL)


#### merge of in, door and uitverhuizing datasets #######

### in and uit verhuizingen ####

#### reduce dataset vhb_di to relevant columns
col_di <- c("jaar", "gem", "gemwpl", "HNR", "HLT", "HTV", "HAN", "PKD", "cat_vh", "ingdat", "vrggem", "vrggemwpl", "vrgHNR", "vrgHLT", "vrgHTV", 
            "vrgHAN", "vrgpkd", "PRSANR", "PRSGDTYY", "PRSGDTMM", "PRSGDTDD", "PRSGES", "age", "AANBEW", "cdhhw", 
            "cbsetngr", "etnikort", "hsh_cat",  "waarde",  "huur", "prscat", "opp", "kamers",  "inhoud",  "huurkoop",  
            "eigendom",   "bouwjaar",  "bouwwyze",   "wontype",  "vloeropp",  "vrgwrd", "vrghr",  "vrgpct",  "vrgopp",  "vrgkam",  "vrginh",  
            "vrghrkp", "vrgeigd", "vrgbjr",  "vrgbwwyz",   "vrgwtyp", "vrgvlopp", "verhuis_type")

vhb_di <- vhb_di_raw %>%
  select(col_di) %>%
  data.table


### clean "uit" dataset ###
#choose relevant columns which will be renamed
col_uit <- c("jaar", "gem", "HNR", "HLT", "HTV", "HAN", "PKD", "ingdat","cat_vh", "nwgem", "nwHNR", "nwHLT", "nwHTV", "nwHAN", "nwPKD", "PRSANR", "PRSGDTYY", 
             "PRSGDTMM", "PRSGDTDD", "PRSGES", "age", "AANBEW", "cdhhw", "cbsetngr", "etnikort", "hsh_cat", "gemwpl", "nwgemwpl", "opp", "waarde", 
             "huur", "prscat", "kamers", "inhoud", "huurkoop", "eigendom", "bouwjaar", "bouwwyze", "wontype", "vloeropp", "verhuis_type")

##add vrg
#old column names
col_vrg <- c("gem", "gemwpl", "HNR", "HLT", "HTV", "HAN", "PKD",  "waarde", "huur", "prscat", "opp", "kamers", "inhoud", "huurkoop", "eigendom", 
             "bouwjaar", "bouwwyze", "wontype", "vloeropp")
#new column names
col_vrg_new <- c("vrggem", "vrggemwpl","vrgHNR", "vrgHLT", "vrgHTV", "vrgHAN", "vrgpkd",  "vrgwrd", "vrghr",  "vrgpct",  "vrgopp",  "vrgkam",  "vrginh", "vrghrkp", "vrgeigd", 
                 "vrgbjr",  "vrgbwwyz",   "vrgwtyp", "vrgvlopp")

vhb1418_uit_cl <- vhb1418_uit %>%
  select(col_uit) %>%
  rename_at(vars(col_vrg), ~col_vrg_new) %>%
  set_names( ~ str_remove(.,"nw"))

#merge di with uit
vhb <- bind_rows(vhb_di, vhb1418_uit_cl)

### clean GWB codes using CBS data
vhb <- vhb %>%
  left_join(pc6_wb, by = c("PKD"= "PC6")) %>%
  rename(buurtcode = Buurt2018, wijkcode = Wijk2018, gemcode = Gemeente2018) %>%
  left_join(pc6_wb, by = c("vrgpkd"= "PC6"), suffix = c("", "_vrg")) %>%
  rename(vrg_buurtcode = Buurt2018, vrg_wijkcode = Wijk2018, vrg_gemcode = Gemeente2018)%>%
  data.table

moves <- vhb

### cleaning of merged dataset ###########

# take out double cases in terms of 'doorverhuizingen'
moves <- moves %>%
  distinct_at(vars(PRSANR,ingdat), .keep_all = T)

# take out cases of death
moves <- moves[cat_vh != "Verlater (overl)"]

#transform missing PKD into NAs
moves[PKD == "", PKD := NA]
moves[vrgpkd == "", vrgpkd := NA]

#### make categories

#categories ethnicity
moves[etnikort == "Autochtonen", ethnicity := "dutch"]
moves[etnikort == "Westerse allochtonen", ethnicity := "western"]
moves[!etnikort %in% c("Autochtonen", "Westerse allochtonen"), ethnicity := "non_western"]

#categories age
moves[age < 25, age_cat := "25"]
moves[age >= 25 & age < 45, age_cat := "25_44"]
moves[age >=45 & age < 65, age_cat := "45_64"]
moves[age >= 65, age_cat := "65"]

#categories household type
moves[cdhhw == "Tweepersoonshuishouden", hh_type := "couple"]
moves[cdhhw == "Gezin met kinderen" | cdhhw == "Eénoudergezin", hh_type := "fam"]
moves[!hh_type %in% c("couple", "fam"), hh_type := "single"]

#variable room stress
moves[, AANBEW := as.numeric(AANBEW)]
moves[, vrgvlopp := as.numeric(vrgvlopp)]

moves[!is.na(vrgvlopp) & vrgvlopp != 0, room_stress := round(AANBEW/vrgvlopp,4)]

#transform PRSANR in anonymous household ID
#prs_unique <- moves[,.(PRSANR = unique(PRSANR))]
#prs_unique[, hh_id := 1:.N]

#moves <- left_join(moves, prs_unique, by = "PRSANR") %>%
#  data.table

###### anonymisation to work on locally ########

# variables to be taken out
#anonym <- c("HNR", "HLT", "HTV", "HAN", "PKD", "AANBEW", "vrgHNR", "vrgHLT", "vrgHTV", "vrgHAN", "vrgpkd", "PRSANR", "PRSGDTYY", "PRSGDTMM", "PRSGDTDD", "PRSGES", 
#            "age", "cdhhw", "cbsetngr", "etnikort")

#replace_to_na <- function(column) {
#  column <- NA
#}

#moves_anonym <- moves %>%
#  mutate_at(vars(anonym),replace_to_na)

### reduce dataset to moves happening in Den Bosch ###

#take out cases which didn't happen to, within or from Gemeente Den Bosch
moves_db <- moves %>%
  filter(gem == "'s-Hertogenbosch" | vrggem == "'s-Hertogenbosch" ) %>%
  data.table

#take out cases where buurtcode is NA
moves_db <- moves_db[!is.na(buurtcode),]

# determine the location of the move with respect to inside or outside the region Noordoost Brabant
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

#remove moves which happened from or into Outside Region Noordoost Brabant, as no data on neighbourhoods available
moves_db <- moves_db %>%
  filter(buurtnaam != 'Outside Region') %>%
  filter(buurtnaam_vrg != "Outside Region") %>%
  data.table


##### define neighbour and other municipalities #######

### Neighbour municipalities
moves_db[buurtnaam %in% c("Heusden", "Vught", "Sint Michielsgestel", "Bernheze", "Oss"), `:=` (buurtnaam = "neighbour mun", wijknaam = "neighbour mun")]
moves_db[buurtnaam_vrg %in% c("Heusden", "Vught", "Sint Michielsgestel", "Bernheze", "Oss"), `:=` (buurtnaam_vrg = "neighbour mun", wijknaam_vrg = "neighbour mun")]

#### Other municipalities
moves_db[gem != "'s-Hertogenbosch" & buurtnaam != "neighbour mun", `:=` (buurtnaam = "outside mun", wijknaam = "outside mun")]
moves_db[vrggem != "'s-Hertogenbosch" & buurtnaam_vrg != "neighbour mun", `:=` (buurtnaam_vrg = "outside mun", wijknaam_vrg = "outside mun")]

## determine move type with respect to mun Den Bosch###

#within Den Bosch
moves_db[vrggem == "'s-Hertogenbosch" & gem == "'s-Hertogenbosch", move_type := "within"]

#from Den Bosch to neighbour municipality
moves_db[vrggem == "'s-Hertogenbosch" & buurtnaam == "neighbour mun", move_type := "to neighbour mun"]

#from Den Bosch to outside municipality
moves_db[vrggem == "'s-Hertogenbosch" & buurtnaam == "outside mun", move_type := "to outside mun"]

#from neighbour municipality to Den Bosch
moves_db[buurtnaam_vrg == "neighbour mun" , move_type := "from neighbour mun"]

#from outside municipality to Den Bosch
moves_db[buurtnaam_vrg == "outside mun", move_type := "from outside mun"]


#bigger categories move_type
moves_db[move_type == "within", move_type_gen := "within"]
moves_db[move_type == "to neighbour mun" | move_type == "to outside mun" , move_type_gen := "out"]
moves_db[move_type == "from neighbour mun" | move_type == "from outside mun" , move_type_gen := "into"]


##### clean variables for models #####

moves_db[, jaar := factor(jaar)]
moves_db[, vrginh := as.numeric(vrginh)]
moves_db[, vrgwrd := as.numeric(vrgwrd)]
moves_db[, vrghrkp := as.factor(vrghrkp)]
moves_db[, hh_type := as.factor(hh_type)]
moves_db[, ethnicity := as.factor(ethnicity)]
moves_db[, AANBEW := as.numeric(AANBEW)]
moves_db[, vrgvlopp := as.numeric(vrgvlopp)]



#transform categorical variables into dummy
#age cat
moves_db[age_cat == "25-44", age_cat := "25_44"]
moves_db <- dummy_cols(moves_db, "age_cat", ignore_na = T)


#hh type
moves_db <- dummy_cols(moves_db, "hh_type", ignore_na = T)

#ethnicity
moves_db <- dummy_cols(moves_db, "ethnicity", ignore_na = T)

#move type
moves_db <- dummy_cols(moves_db, "move_type_gen", ignore_na = T)


#### add income  data from CBS #####
moves_db <- moves_db %>%
  left_join(income, by = c("vrg_buurtcode" = "buurtcode", "jaar", "age_cat"))


# fill NA with neighbourhood totals
income_tot <- income %>%
  filter(age_cat == "Totaal") %>%
  select(-age_cat)

income_mis <- moves_db %>%
  filter(is.na(income)) %>%
  select(-income) %>%
  left_join(income_tot, by = c("vrg_buurtcode" =  "buurtcode", "jaar"))

moves_db <- moves_db %>% 
  filter(!is.na(income)) %>%
  bind_rows(income_mis) %>%
  rename(income_hh = income) %>%
  data.table

# transform age back to factor
moves_db[, age_cat := as.factor(age_cat)]

#create move id, this is needed for putting data in right format for the model 
moves_db[, move_id := seq_len(.N), by = jaar]


#make sure this file is copied to "../../gen/analyis/input/" to use it in analysis stage
save(moves_db, file = "../../gen/data-preparation/output/moves_db.Rdata") 


