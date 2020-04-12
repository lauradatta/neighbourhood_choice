library(data.table)
library(tidyverse)
library(mlogit)

source("src/data-preparation/mlogit_clust_prep.R")

save(dt_mlogit, file = "gen/analysis/temp/dt_mlogit.Rdata")

load("gen/analysis/temp/dt_mlogit.Rdata")

covar_nh <- c(
              #"cluster",
              #"buurtcode",
              #"jaar",
              "GemiddeldeWoningwaarde_35", 
              #"Omgevingsadressendichtheid_106", 
              #"AfstandTotZiekenhuis_11", 
              "AfstandTotOpritHoofdverkeersweg_89", 
              "Restaurants_3km", 
              "AfstandTotBelangrijkOverstapstation_91", 
              #"AfstandTotKinderdagverblijf_52",
              #"AfstandTotBuitenschoolseOpvang_56", 
              "prop_nonwest", 
              "prop_singles", 
              "prop_couples", 
              #"prop_fam", 
              "prop_bijstand",
              #"prop_income_socialmin", 
              #"perc_property", 
              "perc_rent", 
              "perc_after2000"
            )

covar_indiv <- c(#"move_id",
                  #"jaar",
                  #"cluster",
                  #"buurtnaam",
                  #"buurtcode",
                  #"vrg_buurtcode",
                  #"PRSGES", 
                  #"age",
                  "age_cat",
                  #"AANBEW", 
                  "cdhhw", 
                  #"cbsetngr",
                  #"hsh_cat",
                  #"etnikort",
                  "ethnicity",
                  "vrgwrd"
                  #"vrghr", 
                  #"vrgpct", 
                  #"vrgopp", 
                  #"vrgkam", 
                  #"vrginh" 
                  #"vrghrkp", 
                  #"vrgeigd", 
                  #"vrgbjr", 
                  #"vrgbwwyz", 
                  #"vrgwtyp", 
                  #"vrgvlopp", 
                  )

covar_nh_vrg <- c(
                #"cluster",
                #"buurtcode",
                #"jaar",
                "GemiddeldeWoningwaarde_35_vrg", 
                "Omgevingsadressendichtheid_106_vrg", 
                "AfstandTotZiekenhuis_11_vrg", 
                #"AfstandTotOpritHoofdverkeersweg_89", 
                #"Restaurants_3km", 
                #"AfstandTotBelangrijkOverstapstation_91", 
                #"AfstandTotKinderdagverblijf_52",
                #"AfstandTotBuitenschoolseOpvang_56", 
                "prop_nonwest_vrg", 
                "prop_singles_vrg", 
                #"prop_couples", 
                #"prop_fam", 
                "prop_bijstand_vrg"
                #"prop_income_socialmin", 
                #"perc_property", 
                #"perc_rent", 
                #"perc_after2000"
              )


#specify covariates

#alternative specific variables

alt_specific <- covar_nh


indiv_specifc <- covar_indiv

#define formulats
#alternatives only
f_alt <- as.formula(paste("choice", 
                          paste(c(alt_specific, -1), collapse = " + "), 
                          sep = " ~ "))


# add individual specific variables
f_altind <- as.formula(paste("choice",
                             paste(
                               paste(alt_specific, collapse = " + "),
                               paste(c(indiv_specifc, -1), collapse = " + "),
                               sep = " | "),
                             sep = " ~ "))

int <- c()

# add interactions
for (i in 1:length(alt_specific)){
  for (j in 1:length(indiv_specifc)){
    int <- c(int, paste(c(alt_specific[i], indiv_specifc[j]), collapse = " : "))
  }
}

f_int <- as.formula(paste("choice",
                          paste(c(int, - 1), collapse = " + "),
                          sep = " ~ "))

#run models
m1 <- mlogit(f_alt, dt_mlogit)
summary(m1)


m2 <- mlogit(f_altind, dt_mlogit)
summary(m2)

m3 <- mlogit(f_int, dt_mlogit)

summary(m3)