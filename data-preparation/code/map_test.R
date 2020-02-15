library(tidyverse)
library(ggmap)
library(readxl)
library(data.table)
library(sp)
library(rgdal)
library(cbsodataR)
library(sf)

kwb <- data.table(read_excel("kwb-2017.xls"))
kwb_subset <- kwb[recs == "Gemeente",.(gwb_code, a_inw)]
kwb_subset[,a_inw := as.numeric(a_inw)]

#wijken_db_latlong <- mutate_geocode(wijken_db, regio)

#save(wijken_db_latlong, file = "wijken_db_latlong.Rdata")

wijken_db_latlong

coords <- select(wijken_db_latlong,lon, lat)

points_sp <- SpatialPoints(coords = coords,
                           proj4string = CRS("+proj=longlat +datum=WGS84"))

points_spdf <- SpatialPointsDataFrame(coords = coords,
                                      data = wijken_db,  
                                      proj4string = CRS("+proj=longlat +datum=WGS84"))

spplot(points_spdf, "i_inw")

municipalBoundaries <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_gemeente_2017_gegeneraliseerd&outputFormat=json")

kwb_geo <- left_join(municipalBoundaries, kwb_subset, by  = c("statcode" = "gwb_code" ))

kwb_geo %>%
  ggplot()+
    geom_sf(aes(fill = a_inw))

cbs_buurten_2018 <- st_read("https://geodata.nationaalgeoregister.nl/wijkenbuurten2018/wfs?request=GetCapabilities")

cbs_buurten_2018 %>%
  filter(gemeentenaam == "'s-Hertogenbosch") %>%
  ggplot() +
    geom_sf(aes(fill= aantal_inwoners))





