#fill anonymsed data with random values

#set up
library(data.table)
library(tidyverse)
library(Rlab)

anonymize = F # set to FALSE IF not wanting to anonymise

#load anonymised data
load("../../gen/data-preparation/temp/moves_anonym.Rdata")

moves_anonym <- data.table(moves_anonym)

if (anonymize==T) {
  
  #fill anonymsed columns with random values
  moves_anonym[, age := round(rnorm(nrow(moves_anonym),mean = 43.22, sd=18.2),0)]
  moves_anonym[, AANBEW := sample(seq(1:6),nrow(moves_db_anonym), replace = T)]
  moves_anonym[, etnikort := factor(sample(seq(1:8),nrow(moves_db_anonym), replace = T), labels = c("Autochtonen", "Westerse allochtonen", 
                                                                                                    "Overige niet-westerse allochtonen", "Surinamers", 
                                                                                                    "Turken", "Marokkanen", "Antillianen en Arubanen", "0", "NA"))]
  moves_anonym[, PRSGES := factor(rbern(nrow(moves_anonym),prob = 0.5), labels = c("male", "female"))]
}

#save 
save(moves_anonym, file = "../../gen/data-preparation/temp/moves.Rdata")
