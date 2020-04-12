#fill anonymsed data with random values

#set up
library(data.table)
library(tidyverse)

anonymize = T # set to FALSE IF not wanting to anonymise

#load anonymised data
load("../../gen/data-preparation/input/moves_db_anonym.Rdata")

moves_db_anonym <- data.table(moves_db_anonym)

if (anonymize==T) {
  
  #fill anonymsed columns with random values
  moves_db_anonym[, age := sample(seq(18,90),nrow(moves_db_anonym), replace = T)]
  moves_db_anonym[, AANBEW := sample(seq(1:6),nrow(moves_db_anonym), replace = T)]
  moves_db_anonym[, etnikort := factor(sample(seq(1:3),nrow(moves_db_anonym), replace = T), labels = c("dutch", "non_west", "west"))]
  moves_db_anonym[, PRSGES := factor(sample(seq(1:2),nrow(moves_db_anonym), replace = T), labels = c("male", "female"))]
}

#save 
save(moves_db_anonym, file = "../../gen/data-preparation/temp/moves_db.Rdata")
