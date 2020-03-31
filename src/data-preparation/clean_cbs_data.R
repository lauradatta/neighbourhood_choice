library(tidyverse)
library(data.table)


load("gen/data-preparation/input/cbs_data_18.Rdata")

#take out important variables

var_voorz <- c("Codering_3", "AfstandTotZiekenhuis_11" , "AfstandTotOpritHoofdverkeersweg_89", "Binnen3Km_46", "AfstandTotBelangrijkOverstapstation_91", 
         "AfstandTotKinderdagverblijf_52", "AfstandTotBuitenschoolseOpvang_56")

facilities <- voorzieningen_cbs %>%
  select(var_voorz)%>%
  rename(Restaurants_3km = Binnen3Km_46)


var_kern <- c("Codering_3", "AantalInwoners_5", "NietWestersTotaal_18","HuishoudensTotaal_28" , "Eenpersoonshuishoudens_29", "HuishoudensZonderKinderen_30", "HuishoudensMetKinderen_31",  
              "GemiddeldeWoningwaarde_35", "Koopwoningen_40", "HuurwoningenTotaal_41", "BouwjaarVanaf2000_46", "PersonenPerSoortUitkeringBijstand_74", "HuishOnderOfRondSociaalMinimum_73",
              "Omgevingsadressendichtheid_106")

kern <- kern_cbs_db %>%
  select(var_kern)

nhchar_18 <- left_join(kern, facilities, by = "Codering_3") %>%
  data.table

## add propotions

nhchar_18[,`:=` (prop_nonwest = round(NietWestersTotaal_18/AantalInwoners_5,2), 
            prop_singles = round(Eenpersoonshuishoudens_29/HuishoudensTotaal_28,2), 
            prop_couples = round(HuishoudensZonderKinderen_30/HuishoudensTotaal_28,2),
            prop_fam = round(HuishoudensMetKinderen_31/HuishoudensTotaal_28,2),
            prop_bijstand = round(PersonenPerSoortUitkeringBijstand_74 / AantalInwoners_5,2),
            prop_income_socialmin = HuishOnderOfRondSociaalMinimum_73/100,
            perc_property = Koopwoningen_40/100,
            perc_rent = HuurwoningenTotaal_41/100,
            perc_after2000 = BouwjaarVanaf2000_46/100)]

nhchar_18 <-   nhchar_18 %>% 
  select(-c(NietWestersTotaal_18, Eenpersoonshuishoudens_29, HuishoudensZonderKinderen_30, HuishoudensMetKinderen_31, PersonenPerSoortUitkeringBijstand_74, Koopwoningen_40,
            HuurwoningenTotaal_41, BouwjaarVanaf2000_46, PersonenPerSoortUitkeringBijstand_74, HuishOnderOfRondSociaalMinimum_73)) %>%
  data.table

nhchar_18[,code := as.numeric(substr(Codering_3, 3, 10))]
nhchar_18[,Codering_3:= NULL]


