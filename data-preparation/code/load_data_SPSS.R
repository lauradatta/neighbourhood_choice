library("tidyverse")
library("data.table")
library("haven")
library("purrr")

setwd("R:/os/project/Data bestanden/Woningmarktmonitor/Wmm2020/Laura scriptie/Master thesis Laura/data-preparation/input/")

mutatie18_sav <- read_sav("Bevolkingsmutaties 2018 - 2.sav")
gba18_sav <- read_sav("Gba19.sav")
vhb1418_door_sav <- read_sav("Vhb14-18 doorstromers woningen.sav")
vhb1418_in_sav <- read_sav("Vhb14-18 inverhuizingen woningen.sav")
vhb1418_uit_sav <- read_sav("Vhb14-18 uitverhuizingen woningen.sav")

woningen18 <- read_csv2("Wkt2019 woningen.csv")


#convert SPSS labels to factors

mutatie18 <- mutatie18_sav %>% mutate_all(as_factor)
gba18 <- gba18_sav %>% mutate_all(as_factor)
vhb1418_door <- vhb1418_door_sav %>% mutate_at(vars(-ingdat),as_factor)
vhb1418_in <- vhb1418_in_sav %>% mutate_at(vars(-ingdat),as_factor)
vhb1418_uit <- vhb1418_uit_sav %>% mutate_at(vars(-ingdat),as_factor)

save(mutatie18, gba18, file = "bevolking18.Rdata")
save(vhb1418_door,vhb1418_in, vhb1418_uit, file = "verhuis1418.Rdata")
save(woningen18, file = "woningen18.Rdata")


