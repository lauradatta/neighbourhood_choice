#fill anonymsed data with random values

#set up
library(data.table)
library(tidyverse)
library(Rlab)

anonymize = T # set to FALSE IF not wanting to anonymise

#load anonymised data
load("../../gen/data-preparation/temp/moves_anonym.Rdata")

moves_anonym <- data.table(moves_anonym)

if (anonymize==T) {
  
  #fill anonymsed columns with random values
  moves_anonym[, age := round(rnorm(nrow(moves_anonym),mean = 43.22, sd=18.2),0)]
  moves_anonym[, AANBEW := sample(seq(1:6),nrow(moves_anonym), replace = T)]
  moves_anonym[, etnikort := factor(sample(seq(1:9),nrow(moves_anonym), replace = T), labels = c("Autochtonen", "Westerse allochtonen", 
                                                                                                    "Overige niet-westerse allochtonen", "Surinamers", 
                                                                                                    "Turken", "Marokkanen", "Antillianen en Arubanen", "0", "NA"))]
  moves_anonym[, PRSGES := factor(rbern(nrow(moves_anonym),prob = 0.5), labels = c("man", "vrouw"))]
  moves_anonym[, cdhhw := factor(sample(seq(1:5), nrow(moves_anonym), replace = T), labels = c("single", "couple", "fam with children", "single parent", "several pers hh" ))]
}

moves <- moves_anonym

#save 
save(moves, file = "../../gen/data-preparation/temp/moves.Rdata")
