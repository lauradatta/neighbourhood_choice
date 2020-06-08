#prepare data for mlogit



##### set up
#set working directory to source file location

#load packages
library(data.table)
library(tidyverse)
library(mlogit)

# copy following files from "../../gen/data-prepartion/output/ first
load("../../gen/analysis/output/data_cluster.Rdata")
load("../../gen/analysis/input/nhchar.Rdata")

#script with variables to be included in conditional logit model
source("../../src/analysis/variable_selection.R")
########### 


#take neighbourhoods of s'H
moves_db_cl <- moves_db_cl %>%
   filter(!buurtnaam %in% c("outside mun", "neighbour mun"))%>%
   #filter(jaar %in% c(2017, 2018)) %>%
   data.table

#relevant households characteristics
covar_moves <- c("cluster", "jaar", "move_id", covar_indiv)

#remove variables with NAs in hh characteristics used in the model
moves_db_cl_na <- moves_db_cl  %>% 
  drop_na(covar_indiv) %>%
  data.table

##### get data in right shape for multinomial logit model ##

#list of alternatives (neighbourhoods)
alternatives <- nhchar_cl %>%
  distinct(cluster) %>%
  pull 

#take out nh outside Gemeente DB
alternatives <- rev(alternatives[length(alternatives)-2 : length(alternatives)])

#take subset
subs <- moves_db_cl_na[,.(move_id, jaar, cluster)]


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
  left_join(moves_db_cl_na, by = c("move_id", "jaar"), suffix = c("_alt", "_choice")) %>%
  left_join(nhchar_cl, by = c("alternatives" = "cluster", "jaar")) %>%
  data.table

#standardise data
dt_alt_stand <- dt_alt %>%
  mutate_at(covar_stand, ~(scale(.) %>% as.vector)) %>%
  select(c("alternatives", "choice", covar_nh, covar_indiv))

#transform dt into mlogit format (as required by mlogit package)
#non-standardised --> not used in model
dt_mlogit <- mlogit.data(dt_alt, shape = "long", choice = "choice", alt.var = "alternatives", drop.index = T)

#standardised
dt_mlogit_stand <- mlogit.data(dt_alt_stand, shape = "long", choice = "choice", alt.var = "alternatives", drop.index = T)

#save temporary folder for further use
save(dt_mlogit_stand, dt_alt, dt_alt_stand, file = "../../gen/analysis/temp/dt_mlogit.Rdata")

