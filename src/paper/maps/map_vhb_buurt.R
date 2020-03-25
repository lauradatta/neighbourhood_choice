library(tidyverse)
library(data.table)
library(geojsonio)
library(sf)
library(viridis)
library(leaflet)

source("code/help_functions.R")

#setwd("R:/os/project/Data bestanden/Woningmarktmonitor/Wmm2020/Laura scriptie/Master thesis Laura/data-preparation")

#load moving and population data
load("input/raw-data/verhuis1418.Rdata")
load("input/raw-data/bevolking18.Rdata")

#read cbs_buurten 2018 geodata, with geojson package
cbs_buurten_sp <- geojson_read("input/raw-data/cbs_buurten_2018.geojson", what = "sp")
#transform into 'sf' data format
cbs_buurten <- st_as_sf(cbs_buurten_sp)
#filter for only Den Bosch and buurtcodes
cbs_buurten_db <- cbs_buurten %>%
  filter(gemeentenaam == "'s-Hertogenbosch") %>%
    select(c(id, buurtnaam, buurtcode, geometry))

#transform into data.tale
vhb1418_door <- data.table(vhb1418_door)
vhb1418_in <- data.table(vhb1418_in)
vhb1418_uit <- data.table(vhb1418_uit)
gba18 <- data.table(gba18)

#merge moving 'In' and 'door' datasets together
vhb1418_door[,verhuis_type := "door"]
vhb1418_in[,verhuis_type := "in" ]
vhb1418_uit[,verhuis_type := "uit"]

vhb_di <- bind_rows(vhb1418_door, vhb1418_in, .id=NULL)

#vhb <- rbind(rbind(vhb1418_door, vhb1418_in, fill= T), vhb1418_uit, fill = T)

#### 2018 #######

#filter for year 2018
vhb18_di <- vhb_di[jaar == '2018']
#number of moves / Gemeente
vhb18_di[,.N, by = gem]

#merge buurt and wijk codes into 2018 data (they are missing), using 2016 data
## extract buurt and wijkcode based on PKD for 2016
vhb16_pkd_codes <- vhb_di %>%
  filter(jaar == "2016") %>%
  select(c(PKD, gemcw, gemcb)) %>%
  distinct(PKD, .keep_all = T)

### 2018 data ########

#left_join with 2018 data
vhb18_di <- left_join(vhb18_di,vhb16_pkd_codes, by = "PKD") %>%
  select(-c(gemcw.x,gemcb.x)) %>%
  rename(wijkcode = gemcw.y, buurtcode = gemcb.y) %>%
  data.table

#make buurtcode compatible with CBS geometry data
vhb18_di[!is.na(buurtcode), `:=` (buurtcode = paste0("BU",buurtcode), wijkcode = 
                                    paste0("WK", wijkcode))]


#calculate number of moves per buurt
vhb18_buurt <- vhb18_di %>%
  filter(gem == "'s-Hertogenbosch") %>%
  count(buurtcode, jaar) %>%
  rename(n_moves = n) %>%
  mutate(verhuis_type = "total")

#calculate number of moves per buurt
vhb18_buurt <- vhb18_di %>%
  filter(gem == "'s-Hertogenbosch") %>%
  count(buurtcode, verhuis_type, jaar) %>%
  rename(n_moves = n) %>%
  bind_rows(vhb18_buurt) %>%
  arrange(desc(verhuis_type))

vhb18_buurt_geo <- left_join(cbs_buurten_db, vhb18_buurt, by = 'buurtcode')

#prepare data for leaflet (reproject)
vhb18_buurt_leaflet <- st_transform(vhb18_buurt_geo, 4326)

# ggplot map count of moves on neighbourhood level
vhb18_buurt_geo %>%
  filter(verhuis_type == 'total' | is.na(n_moves)) %>%
  ggplot() +
    geom_sf(aes(fill = n_moves))
    #geom_sf_label(aes(label = buurtnaam))

moves <- vhb18_buurt_leaflet %>%
  filter(verhuis_type == "total" | is.na(n_moves))

moves_in <- vhb18_buurt_leaflet %>%
  filter(verhuis_type == "in" | is.na(n_moves))

moves_door <- vhb18_buurt_leaflet %>%
  filter(verhuis_type == "door" | is.na(n_moves))




#### complete data set #######


#left_join with 2018 data
vhb_di <- vhb_di %>%
  filter(jaar %in% c("2017", "2018")) %>%
  left_join(vhb16_pkd_codes, by = "PKD") %>%
  rename(gemcw = gemcw.y, gemcb = gemcb.y) %>%
  bind_rows(filter(vhb_di,!jaar %in% c("2017","2018"))) %>%
  rename(wijkcode = gemcw, buurtcode = gemcb)%>%
  data.table
  

#make buurtcode compatible with CBS geometry data
vhb_di[!is.na(buurtcode), `:=` (buurtcode = paste0("BU",buurtcode), wijkcode = 
                                    paste0("WK", wijkcode))]


#number of moves per buurt
moves1418 <- vhb_di %>%
  filter(gem == "'s-Hertogenbosch") %>%
  count(buurtcode) %>%
  rename(n_moves = n)

moves1418_geo <- left_join(cbs_buurten_db, moves1418, by = 'buurtcode')

#prepare data for leaflet (reproject)
moves1418_leaflet <- st_transform(moves1418_geo, 4326)


bins <-  c(0, 100, 200, 300, 500, 1000, 3000, 4000)
g_moves1418 <- make_leaflet(moves1418_leaflet, moves1418_leaflet$buurtnaam, moves1418_leaflet$n_moves, bins)



#number of moves per buurt / year
vhb_buurt_year <- vhb_di %>%
  filter(gem == "'s-Hertogenbosch") %>%
  count(buurtcode, jaar) %>%
  rename(n_moves = n) %>%
  mutate(verhuis_type = "total")

#calculate number of moves per buurt, per year, per verhuistype
vhb_buurt_yeah_vhtype <- vhb_di %>%
  filter(gem == "'s-Hertogenbosch") %>%
  count(buurtcode, verhuis_type, jaar) %>%
  rename(n_moves = n) %>%
  bind_rows(vhb_buurt) %>%
  arrange(desc(verhuis_type))


#prepare data for leaflet (reproject)
vhb_buurt_leaflet <- st_transform(vhb_buurt_geo, 4326)

# ggplot map count of moves on neighbourhood level
vhb_buurt_geo %>%
  filter(verhuis_type == 'total' | is.na(n_moves)) %>%
  ggplot() +
  geom_sf(aes(fill = n_moves))
#geom_sf_label(aes(label = buurtnaam))

moves <- vhb18_buurt_leaflet %>%
  filter(verhuis_type == "total" | is.na(n_moves))

moves_in <- vhb18_buurt_leaflet %>%
  filter(verhuis_type == "in" | is.na(n_moves))

moves_door <- vhb18_buurt_leaflet %>%
  filter(verhuis_type == "door" | is.na(n_moves))


######## Leaflet ############
bins <-  c(0, 10, 20, 50, 100, 200, 500, 700, 1000)
g_moves <- make_leaflet(moves, moves$buurtnaam, moves$n_moves, bins)

bins_in <- c(0, 10, 20, 50, 100, 200, 500, 700)

g_moves_in <- make_leaflet(moves_in, moves_in$buurtnaam, moves_in$n_moves, bins_in)

bins_door<- c(0, 10, 20, 50, 100, 150, 300)
g_moves_door <- make_leaflet(moves_door, moves_door$buurtnaam, moves_door$n_moves, bins_door)



  
  


