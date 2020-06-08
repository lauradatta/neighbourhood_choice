#download data from CBS

#this script downloads the relevant data from CBS using CBS package

############## set up
#set working directory to Source file location

#load packages
library(cbsodataR)
library(tidyverse)
library(data.table)
##################

#download cbs_buurten 2018 geo data (geojson format), this is to make the maps
#geoUrl <- "https://geodata.nationaalgeoregister.nl/wijkenbuurten2018/wfs?request=GetFeature&service=WFS&version=1.1.0&typeName=wijkenbuurten2018:cbs_buurten_2018&outputFormat=json"
#fileName <- "../../data/cbs_buurten_2018.geojson"
#download.file(geoUrl, fileName)

#table of datasets cbs
#toc <- cbs_get_toc()

#municiaplities needed for thesis
gems <- "Haaren|Heusden|'s-Hertogenbosch|Sint-Michielsgestel|Vught|Boxtel|Oss|Grave|Landerd|Bernheze|Uden|Cuijk|Boxmeer|MillenSintHubert|Meierijstad|SintAnthonis"

#kerncijfers
#2018 data
kern_cbs_18_db <- cbs_get_data("84286NED") %>%
  filter(str_detect(Gemeentenaam_1, gems)) 

#2017
kern_cbs_17_db <- cbs_get_data("83765NED")  %>%
  filter(str_detect(Gemeentenaam_1, gems)) 

#2016
kern_cbs_16_db <- cbs_get_data("83487NED")  %>%
  filter(str_detect(Gemeentenaam_1, gems)) 

#2015
kern_cbs_15_db <- cbs_get_data("83220NED")  %>%
  filter(str_detect(Gemeentenaam_1, gems)) 

#2014
kern_cbs_14_db <- cbs_get_data("82931NED")  %>%
  filter(str_detect(Gemeentenaam_1, gems)) 


## voorzieningen
#2018
voorzieningen_cbs_18 <- cbs_get_data("84463NED") %>%
  filter(str_detect(Gemeentenaam_1, gems))

#2017
voorzieningen_cbs_17 <- cbs_get_data("84334NED") %>%
  filter(str_detect(Gemeentenaam_1, gems)) 

#2016
voorzieningen_cbs_16 <- cbs_get_data("83749NED") %>%
  filter(str_detect(Gemeentenaam_1, gems))

#2015
voorzieningen_cbs_15 <- cbs_get_data("83304NED") %>%
  filter(str_detect(Gemeentenaam_1, gems)) 

#2014
voorzieningen_cbs_14 <- cbs_get_data("82829NED") %>%
  filter(str_detect(Gemeentenaam_1, gems)) 

#save file
save(kern_cbs_14_db, kern_cbs_15_db, kern_cbs_16_db, kern_cbs_17_db, kern_cbs_18_db, voorzieningen_cbs_14, voorzieningen_cbs_15, voorzieningen_cbs_16, 
     voorzieningen_cbs_17, voorzieningen_cbs_18, file = "../../gen/data-preparation/input/cbs_data.Rdata")

