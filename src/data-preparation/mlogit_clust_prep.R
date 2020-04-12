library(data.table)
library(tidyverse)
library(mlogit)

#source("src/data-preparation/clean_data_nh.R")
source("src/analysis/nh_types.R")

#for now only take nh of Den Bosch
moves_db <- moves_db %>%
  filter(!buurtnaam %in% c("outside", "neighbour mun"))%>%
  filter(jaar %in% c(2017, 2018)) %>%
  data.table

# nhchar<- nhchar %>%
#   group_by(buurtcode)%>%
#   fill(Omgevingsadressendichtheid_106, .direction = "up")



##### get data in right shape for multinomial logit model

#list of alternatives (neighbourhoods)
alternatives <- nhchar_clus %>%
  distinct(cluster) %>%
  pull 

#take out nh outside Gemeente DB
alternatives <- alternatives[length(alternatives)-2 : length(alternatives)]

#take subset
subs <- moves_db[,.(move_id, jaar, cluster)]

#subset of moves to try approach
#subs <- subs[jaar == 2018]


dt <- data.table()

for (row in 1: nrow(subs)){
  temp <- data.table(subs[row], alternatives)
  dt <- rbind(dt, temp)
}

dt[, choice := ifelse(cluster == alternatives, 1, 0)]
dt[, cluster := NULL]
dt_alt <- dt %>%
  left_join(moves_db, by = c("move_id", "jaar"), suffix = c("_alt", "_choice")) %>%
  left_join(nhchar_clus, by = c("alternatives" = "cluster", "jaar")) %>%
  left_join(nhchar, by = c("vrg_buurtcode" = "buurtcode", "jaar"), suffix = c("", "_vrg")) %>%
  mutate(vrginh = as.numeric(vrginh))

#dt_alt <- dt_alt %>%
#  select(-c(cluster, buurtnaam, buurtcode, vrg_buurtcode))

dt_alt_18 <- dt_alt %>%
  filter(jaar == 2018)

dt_mlogit <- mlogit.data(dt_alt, shape = "long", choice = "choice", alt.var = "alternatives", drop.index = T)


#### only keep data set
rm(list=(ls()[!ls() %in% c("dt_mlogit")]))

