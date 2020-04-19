#clean CBS data

library(tidyverse)
library(data.table)

# load cbs data
load("../../gen/data-preparation/input/cbs_data.Rdata")

#use important variables
#voorzieningen
var_voorz <- c("jaar", "Codering_3", "AfstandTotZiekenhuis_11" , "AfstandTotOpritHoofdverkeersweg_89", "Binnen3Km_46", "AfstandTotBelangrijkOverstapstation_91", 
         "AfstandTotKinderdagverblijf_52", "AfstandTotBuitenschoolseOpvang_56")

facilities <- voorzieningen %>%
  select(var_voorz)%>%
  rename(Restaurants.3km = Binnen3Km_46)

#kerncijfers
var_kern <- c("jaar", "Codering_3", "AantalInwoners_5", "WestersTotaal_17", "NietWestersTotaal_18",  "HuishoudensTotaal_28" , "Eenpersoonshuishoudens_29", "HuishoudensZonderKinderen_30", "HuishoudensMetKinderen_31",  
              "GemiddeldeWoningwaarde_35", "Koopwoningen_40", "InBezitOverigeVerhuurders_43", "BouwjaarVanaf2000_46", "PersonenPerSoortUitkeringBijstand_74", "HuishOnderOfRondSociaalMinimum_73",
              "Omgevingsadressendichtheid_106")

indicators <- kern %>%
  select(var_kern)

#join data into neighbour characteristics
nhchar <- left_join(indicators, facilities, by = c("Codering_3", "jaar")) %>%
  data.table

setnames(nhchar, names(nhchar), sub("_.*","",names(nhchar)))



## add propotions
nhchar[,`:=` (prop.nonwest = round(NietWestersTotaal/AantalInwoners,2),
            prop.west = round(WestersTotaal/AantalInwoners,2),
            prop.singles = round(Eenpersoonshuishoudens/HuishoudensTotaal,2), 
            prop.couples = round(HuishoudensZonderKinderen/HuishoudensTotaal,2),
            prop.fam = round(HuishoudensMetKinderen/HuishoudensTotaal,2),
            prop.bijstand = round(PersonenPerSoortUitkeringBijstand/ AantalInwoners,2),
            prop.income.socialmin = HuishOnderOfRondSociaalMinimum/100,
            perc.property = Koopwoningen/100,
            perc.rent = InBezitOverigeVerhuurders/100, #private rental
            perc.after2000 = BouwjaarVanaf2000/100)]

nhchar[, prop.dutch := 1 - (prop.nonwest + prop.west)]

#remove variables not needed
nhchar <-   nhchar %>% 
  select(-c(NietWestersTotaal, WestersTotaal, Eenpersoonshuishoudens, HuishoudensZonderKinderen, HuishoudensMetKinderen, PersonenPerSoortUitkeringBijstand, Koopwoningen,
            InBezitOverigeVerhuurders, BouwjaarVanaf2000, PersonenPerSoortUitkeringBijstand, HuishOnderOfRondSociaalMinimum)) %>%
  data.table

#prepare code variable for merging
nhchar[,code := as.numeric(substr(Codering, 3, 10))]
nhchar[,Codering:= NULL]

nhchar <- nhchar %>%
  rename(buurtcode = code)

nhchar[,jaar := factor(jaar)]

save(nhchar, file = "../../gen/data-preparation/output/nhchar.Rdata")


