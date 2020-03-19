library(tidyverse)
library(data.table)

source("code/data-preparation/clean_data.R")


#how many households have moved
moves_db %>%
  distinct(PRSANR) %>%
  count()

#how often people move
moves_db <- moves_db %>%
  add_count(PRSANR) %>%
  rename(n_moves = n) %>%
  data.table

#unique PRSANR
moves_db_uniq <- moves_db %>%
  distinct(PRSANR, .keep_all = T)

mean(moves_db_uniq$n_moves)
sd(moves_db_uniq$n_moves)
median(moves_db_uniq$n_moves)
hist(moves_db_uniq$n_moves)

# check moves respective to administrative units
moves_db %>%
  count(move_type)

#### look neighbourhood characteristics

