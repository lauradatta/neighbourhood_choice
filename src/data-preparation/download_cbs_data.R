library(cbsodataR)

#download cbs_buurten 2018
geoUrl <- "https://geodata.nationaalgeoregister.nl/wijkenbuurten2018/wfs?request=GetFeature&service=WFS&version=1.1.0&typeName=wijkenbuurten2018:cbs_buurten_2018&outputFormat=json"
fileName <- "data/cbs_buurten_2018.geojson"
download.file(geoUrl, fileName)



#table of datasets cbs
toc <- cbs_get_toc()

#2018 data

#kerncijfers
kern_cbs_db <- cbs_get_data("84286NED") %>%
  filter(str_detect(Gemeentenaam_1, "'s-Hertogenbosch"))

#voorzieningen
voorzieningen_cbs <- cbs_get_data("84463NED") %>%
  filter(str_detect(Gemeentenaam_1, "'s-Hertogenbosch"))

save(voorzieningen_cbs, kern_cbs_db, file = "gen/data-preparation/input/cbs_data_18.Rdata")

