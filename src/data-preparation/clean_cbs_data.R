#clean CBS data

############ set up
#set working directory to Source file location

#load packages
library(tidyverse)
library(data.table)

# load cbs data
load("../../gen/data-preparation/input/cbs_data.Rdata")
load("../../gen/data-preparation/input/leefbh.Rdata")
load("../../gen/data-preparation/input/income.Rdata")
###################

#kerncijfers
#2018 data
kern_cbs_18_db <- kern_cbs_18_db %>%
  add_column(jaar = 2018, .before = "WijkenEnBuurten")%>%
  filter(str_detect(SoortRegio_2, "Buurt")) %>%
  data.table

setnames(kern_cbs_18_db, names(kern_cbs_18_db), sub("_\\d.*","",names(kern_cbs_18_db)))

#2017
kern_cbs_17_db <- kern_cbs_17_db  %>%
  add_column(jaar = 2017, .before = "WijkenEnBuurten") %>%
  filter(str_detect(SoortRegio_2, "Buurt")) %>%
  data.table

setnames(kern_cbs_17_db, names(kern_cbs_17_db), sub("_\\d.*","",names(kern_cbs_17_db)))

#2016
kern_cbs_16_db <- kern_cbs_16_db  %>%
  add_column(jaar = 2016, .before = "WijkenEnBuurten") %>%
  filter(str_detect(SoortRegio_2, "Buurt")) %>%
  data.table

setnames(kern_cbs_16_db, names(kern_cbs_16_db), sub("_\\d.*","",names(kern_cbs_16_db)))

#2015
kern_cbs_15_db <- kern_cbs_15_db %>%
  add_column(jaar = 2015, .before = "WijkenEnBuurten")%>%
  filter(str_detect(SoortRegio_2, "Buurt")) %>%
  data.table

setnames(kern_cbs_15_db, names(kern_cbs_15_db), sub("_\\d.*","",names(kern_cbs_15_db)))

#2014
kern_cbs_14_db <- kern_cbs_14_db   %>%
  add_column(jaar = 2014, .before = "WijkenEnBuurten")%>%
  filter(str_detect(SoortRegio_2, "Buurt")) %>%
  data.table

setnames(kern_cbs_14_db, names(kern_cbs_14_db), sub("_\\d.*","",names(kern_cbs_14_db)))


# bind all kern_cbs data
kern <- bind_rows(kern_cbs_14_db,kern_cbs_15_db,kern_cbs_16_db, kern_cbs_17_db, kern_cbs_18_db)


# take out 's-Gravenhage
kern <- kern %>%
  filter(!str_detect(Gemeentenaam, "'s-Gravenhage"))

#voorzieningen
#2018
voorzieningen_cbs_18 <- voorzieningen_cbs_18 %>%
  add_column(jaar = 2018, .before = "WijkenEnBuurten")%>%
  filter(str_detect(SoortRegio_2, "Buurt")) %>%
  rename(Restaurants3km = Binnen3Km_46) %>%
  data.table

setnames(voorzieningen_cbs_18, names(voorzieningen_cbs_18), sub("_\\d.*","",names(voorzieningen_cbs_18)))

#2017
voorzieningen_cbs_17 <- voorzieningen_cbs_17 %>%
  add_column(jaar = 2017, .before = "WijkenEnBuurten")%>%
  filter(str_detect(SoortRegio_2, "Buurt")) %>%
  rename(Restaurants3km = Binnen3Km_46) %>%
  data.table

setnames(voorzieningen_cbs_17, names(voorzieningen_cbs_17), sub("_\\d.*","",names(voorzieningen_cbs_17)))

#2016
voorzieningen_cbs_16 <- voorzieningen_cbs_16 %>%
  add_column(jaar = 2016, .before = "WijkenEnBuurten")%>%
  filter(str_detect(SoortRegio_2, "Buurt")) %>%
  rename(Restaurants3km = Binnen3Km_46) %>%
  data.table

setnames(voorzieningen_cbs_16, names(voorzieningen_cbs_16), sub("_\\d.*","",names(voorzieningen_cbs_16)))

#2015
voorzieningen_cbs_15 <- voorzieningen_cbs_15 %>%
  filter(str_detect(SoortRegio_2, "Buurt")) %>%
  add_column(jaar = 2015, .before = "Regio") %>%
  rename(Restaurants3km = Binnen3Km_45) %>%
  data.table

setnames(voorzieningen_cbs_15, names(voorzieningen_cbs_15), sub("_\\d.*","",names(voorzieningen_cbs_15)))


#2014
voorzieningen_cbs_14 <- voorzieningen_cbs_14 %>%
  add_column(jaar = 2014, .before = "RegioS")%>%
  filter(str_detect(SoortRegio_2, "Buurt"))%>%
  rename(Restaurants3km = Binnen3Km_45) %>%
  data.table

setnames(voorzieningen_cbs_14, names(voorzieningen_cbs_14), sub("_\\d.*","",names(voorzieningen_cbs_14)))

# bind all voorzieningen_cbs together
voorzieningen <- bind_rows(voorzieningen_cbs_14, voorzieningen_cbs_15, voorzieningen_cbs_16, voorzieningen_cbs_17, voorzieningen_cbs_18)

# remove 's-Gravenhage
voorzieningen <- voorzieningen %>%
  filter(!str_detect(Gemeentenaam, "'s-Gravenhage"))

#### extract variables for current analysis

#voorzieningen

var_voorz <- c("jaar", "Codering", "AfstandTotZiekenhuis" , "AfstandTotOpritHoofdverkeersweg", "Restaurants3km", "AfstandTotBelangrijkOverstapstation")

facilities <- voorzieningen %>%
  select(var_voorz)%>%
  data.table

#kerncijfers
var_kern <- c("jaar", "Codering", "Gemeentenaam", "AantalInwoners", "WestersTotaal", "NietWestersTotaal", "HuishoudensTotaal", "Eenpersoonshuishoudens", "HuishoudensZonderKinderen", 
              "HuishoudensMetKinderen", "GemiddeldeWoningwaarde", "Koopwoningen", "InBezitOverigeVerhuurders", "InBezitWoningcorporatie", "BouwjaarVanaf2000", 
              "PersonenPerSoortUitkeringBijstand", "HuishOnderOfRondSociaalMinimum", "Omgevingsadressendichtheid")

indicators <- kern %>%
  select(var_kern)

#join data into neighbour characteristics
nhchar <- left_join(indicators, facilities, by = c("Codering", "jaar")) %>%
  data.table

## add propotions
nhchar[,`:=` (prop.nonwest = round(NietWestersTotaal/AantalInwoners,2),
            prop.west = round(WestersTotaal/AantalInwoners,2),
            prop.singles = round(Eenpersoonshuishoudens/HuishoudensTotaal,2), 
            prop.couples = round(HuishoudensZonderKinderen/HuishoudensTotaal,2),
            prop.fam = round(HuishoudensMetKinderen/HuishoudensTotaal,2),
            prop.bijstand = round(PersonenPerSoortUitkeringBijstand/ AantalInwoners,2),
            prop.income.socialmin = HuishOnderOfRondSociaalMinimum/100,
            perc.property = Koopwoningen/100,
            perc.priv.rent = InBezitOverigeVerhuurders/100, #private rental
            perc.soc.rent = InBezitWoningcorporatie/100,
            perc.after2000 = BouwjaarVanaf2000/100)]

nhchar[, prop.dutch := 1 - (prop.nonwest + prop.west)]

#remove variables not needed
nhchar <-   nhchar %>% 
  select(-c(NietWestersTotaal, WestersTotaal, Eenpersoonshuishoudens, HuishoudensZonderKinderen, HuishoudensMetKinderen, PersonenPerSoortUitkeringBijstand, 
            Koopwoningen, InBezitOverigeVerhuurders, BouwjaarVanaf2000, PersonenPerSoortUitkeringBijstand, HuishOnderOfRondSociaalMinimum, 
            InBezitWoningcorporatie)) %>%
  data.table

#prepare code variable for merging
nhchar[,code := as.numeric(substr(Codering, 3, 10))]
nhchar[,Codering:= NULL]

nhchar <- nhchar %>%
  rename(buurtcode = code)

nhchar[,jaar := factor(jaar)]

nhchar[, Gemeentenaam := sub("(\\S+).*", "\\1", Gemeentenaam)]


#### add Leefbarheid

nhchar <- nhchar %>%
  left_join(leefbh, by = c("buurtcode", "jaar")) %>%
  mutate(jaar = factor(jaar))


### add income
income_tot <- income %>%
  filter(age_cat == "Totaal") %>%
  select(-age_cat)

nhchar <- nhchar %>%
  left_join(income_tot, by = c("buurtcode", "jaar")) %>%
  rename(income.nh = income)
  data.table

save(nhchar, file = "../../gen/data-preparation/output/nhchar.Rdata")


