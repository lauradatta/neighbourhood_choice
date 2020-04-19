#prepare data for mlogit

#set up
library(data.table)
library(tidyverse)
library(mlogit)

load("../../gen/analysis/temp/data_cluster.Rdata")
load("../../gen/analysis/input/nhchar.Rdata")

#for now only take nh of Den Bosch
moves_db_cl <- moves_db_cl %>%
  filter(!buurtnaam %in% c("outside", "neighbour mun"))%>%
  filter(jaar %in% c(2017, 2018)) %>%
  data.table


##### get data in right shape for multinomial logit model

#list of alternatives (neighbourhoods)
alternatives <- nhchar_cl %>%
  distinct(cluster) %>%
  pull 

#take out nh outside Gemeente DB
alternatives <- alternatives[length(alternatives)-2 : length(alternatives)]

#take subset
subs <- moves_db_cl[,.(move_id, jaar, cluster)]

#subset of moves to try approach
#subs <- subs[jaar == 2018]

##### build data for mlogit #####
#initialise empty data.table
dt <- data.table()

#loop over rows and expand by alternatives
for (row in 1: nrow(subs)){
  temp <- data.table(subs[row], alternatives)
  dt <- rbind(dt, temp)
}

#determine the alternative chosen
dt[, choice := ifelse(cluster == alternatives, 1, 0)]
dt[, cluster := NULL]
#merge move, char with of clusters and char of previous nhs into data
dt_alt <- dt %>%
  left_join(moves_db_cl, by = c("move_id", "jaar"), suffix = c("_alt", "_choice")) %>%
  left_join(nhchar_cl, by = c("alternatives" = "cluster", "jaar")) %>%
  left_join(nhchar, by = c("vrg_buurtcode" = "buurtcode", "jaar"), suffix = c("", "_vrg"))


#transform dt into mlogit format (as required by package)
dt_mlogit <- mlogit.data(dt_alt, shape = "long", choice = "choice", alt.var = "alternatives", drop.index = T)

save(dt_mlogit, file = "../../gen/analysis/temp/dt_mlogit.Rdata")

