#load 'verhuis data' from SPSS files


############# set up
#set working directory to Source file location

#load packages
library("tidyverse")
library("data.table")
library("haven")
library("purrr")
#####################


### Verhuis and bevolking data ########

#load SPSS files
mutatie18_sav <- read_sav("data/Bevolkingsmutaties 2018 - 2.sav")
gba18_sav <- read_sav("data/Gba19.sav")
vhb1418_door_sav <- read_sav("data/Vhb14-18 doorstromers woningen.sav")
vhb1418_in_sav <- read_sav("data/Vhb14-18 inverhuizingen woningen.sav")
vhb1418_uit_sav <- read_sav("data/Vhb14-18 uitverhuizingen woningen.sav")
# loading from SPSS didn't work so was transformed into csv
woningen18 <- read_csv2("data/Wkt2019 woningen.csv")

#convert SPSS labels to factors
mutatie18 <- mutatie18_sav %>% mutate_all(as_factor)
gba18 <- gba18_sav %>% mutate_all(as_factor)
vhb1418_door <- vhb1418_door_sav %>% mutate_at(vars(-ingdat),as_factor) #take out ingdt, couldn't be transformed into factor
vhb1418_in <- vhb1418_in_sav %>% mutate_at(vars(-ingdat),as_factor)
vhb1418_uit <- vhb1418_uit_sav %>% mutate_at(vars(-ingdat),as_factor)

#save data for further use

#save(mutatie18, gba18, file = "../../gen/data-preparation/input/bevolking18.Rdata")
save(vhb1418_door,vhb1418_in, vhb1418_uit, file = "../../gen/data-preparation/input/verhuis1418.Rdata")
#save(woningen18, file = "../../gen/data-preparation/input/woningen18.Rdata")


