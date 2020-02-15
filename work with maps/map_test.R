library(tidyverse)
library(ggmap)
library(readxl)
library(data.table)
library(sp)

kwb <- data.table(read_excel("kwb-2017.xls"))

wijken_db <- kwb[recs=='Wijk' & gm_naam == "'s-Hertogenbosch", .(regio,a_inw)]

#wijken_db_latlong <- mutate_geocode(wijken_db, regio)

#save(wijken_db_latlong, file = "wijken_db_latlong.Rdata")

wijken_db_latlong

