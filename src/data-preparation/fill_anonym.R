library(data.table)
library(tidyverse)


#fill anonymsed data with random values

load("gen/analysis/input/moves_db_anonym.Rdata")

moves_db_anonym <- data.table(moves_db_anonym)

moves_db_anonym[, age := sample(seq(18,90),nrow(moves_db_anonym), replace = T)]
moves_db_anonym[, AANBEW := sample(seq(1:6),nrow(moves_db_anonym), replace = T)]
moves_db_anonym[, etnikort := factor(sample(seq(1:3),nrow(moves_db_anonym), replace = T), labels = c("dutch", "non_west", "west"))]
moves_db_anonym[, PRSGES := factor(sample(seq(1:2),nrow(moves_db_anonym), replace = T), labels = c("male", "female"))]
