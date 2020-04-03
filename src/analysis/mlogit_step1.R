library(data.table)
library(tidyverse)

source("src/analysis/nh_types.R")

mod_var <- c("buurtnaam",
        "PRSGES", 
         "age", 
         #"AANBEW", 
         #"cdhhw", 
         #"cbsetngr", 
         "etnikort", 
         #"vrgwrd", 
         #"vrghr", 
         #"vrgpct", 
         #"vrgopp", 
         #"vrgkam", 
         "vrginh", 
         #"vrghrkp", 
         #"vrgeigd", 
         #"vrgbjr", 
         #"vrgbwwyz", 
         #"vrgwtyp", 
         #"vrgvlopp", 
         #"AantalInwoners_5", 
         #"HuishoudensTotaal_28", 
        #"GemiddeldeWoningwaarde_35_vrg", 
        #"Omgevingsadressendichtheid_106_vrg", 
        #"AfstandTotZiekenhuis_11_vrg", 
        #"AfstandTotOpritHoofdverkeersweg_89_vrg", 
        #"Restaurants_3km_vrg", 
        #"AfstandTotBelangrijkOverstapstation_91_vrg", 
        #"AfstandTotKinderdagverblijf_52_vrg", 
        #"AfstandTotBuitenschoolseOpvang_56_vrg", 
        #"prop_nonwest_vrg", 
        #"prop_singles_vrg", 
        #"prop_couples_vrg", 
        #"prop_fam_vrg", 
        #"prop_bijstand_vrg", 
        #"prop_income_socialmin_vrg", 
        #"perc_property_vrg", 
        #"perc_rent_vrg", 
        #"perc_after2000_vrg", 
        "cluster"
        #"GemiddeldeWoningwaarde_35_nh", 
        #"Omgevingsadressendichtheid_106_nh", 
        #"AfstandTotZiekenhuis_11_nh", 
        #"AfstandTotOpritHoofdverkeersweg_89_nh", 
        #"Restaurants_3km_nh", 
        #"AfstandTotBelangrijkOverstapstation_91_nh", 
        #"AfstandTotKinderdagverblijf_52_nh", 
        #"AfstandTotBuitenschoolseOpvang_56_nh", 
        #"prop_nonwest_nh", 
        #"prop_singles_nh", 
        #"prop_couples_nh", 
        #"prop_fam_nh", 
        #"prop_bijstand_nh", 
        #"prop_income_socialmin_nh", 
        #"perc_property_nh", 
        #"perc_rent_nh", 
        #"perc_after2000_nh"
)

moves_18_log <- moves_18 %>%
  select(mod_var)

moves_18_log <- moves_18_log %>%
  filter(!cluster %in% c( "outside", "neighbour mun"))%>%
  data.table

wide <- moves_18_log %>%
  mutate(var = 1) %>%
  pivot_wider(names_from = cluster, values_from = var, names_prefix = "cluster_", values_fn = list(var = min), values_fill = list(var = 0))

