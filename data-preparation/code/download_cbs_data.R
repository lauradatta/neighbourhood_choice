library(tidyverse)
library(readxl)
library(data.table)
library(cbsodataR)
library(sf)

setwd("~/Tilburg/Thesis/Master-thesis/")

cbs_buurten_2018 <- st_read("https://geodata.nationaalgeoregister.nl/wijkenbuurten2018/wfs?request=GetCapabilities")

save(cbs_buurten_2018, file = "input/cbs_buurten_2018.Rdata")

cbs_buurten_2018 %>%
  filter(gemeentenaam == "'s-Hertogenbosch") %>%
  ggplot() +
    geom_sf(aes(fill= aantal_inwoners))







