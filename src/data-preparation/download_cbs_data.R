library(cbsodataR)
library(tidyverse)

#download cbs_buurten 2018 geo data (geojson format)
#geoUrl <- "https://geodata.nationaalgeoregister.nl/wijkenbuurten2018/wfs?request=GetFeature&service=WFS&version=1.1.0&typeName=wijkenbuurten2018:cbs_buurten_2018&outputFormat=json"
#fileName <- "data/cbs_buurten_2018.geojson"
#download.file(geoUrl, fileName)



#table of datasets cbs
toc <- cbs_get_toc()

#municiaplities needed for thesis
gems <- "Haaren|Heusden|'s-Hertogenbosch|Sint-Michielsgestel|Vught|Boxtel|Oss|Grave|Landerd|Bernheze|Uden|Cuijk|Boxmeer|MillenSintHubert|Meierijstad|SintAnthonis"


#kerncijfers

#2018 data
kern_cbs_18_db <- cbs_get_data("84286NED") %>%
  filter(str_detect(Gemeentenaam_1, gems)) %>%
  add_column(jaar = 2018, .before = "WijkenEnBuurten")

#2017
kern_cbs_17_db <- cbs_get_data("83765NED")  %>%
  filter(str_detect(Gemeentenaam_1, gems)) %>%
  add_column(jaar = 2017, .before = "WijkenEnBuurten")

kern <- bind_rows(kern_cbs_17_db, kern_cbs_18_db)


#voorzieningen
#2018
voorzieningen_cbs_18 <- cbs_get_data("84463NED") %>%
  filter(str_detect(Gemeentenaam_1, gems)) %>%
  add_column(jaar = 2018, .before = "WijkenEnBuurten")

#2017
voorzieningen_cbs_17 <- cbs_get_data("84334NED") %>%
  filter(str_detect(Gemeentenaam_1, gems)) %>%
  add_column(jaar = 2017, .before = "WijkenEnBuurten")

voorzieningen <- bind_rows(voorzieningen_cbs_17, voorzieningen_cbs_18)

#save(voorzieningen, kern, file = "gen/data-preparation/input/cbs_data_18.Rdata")

