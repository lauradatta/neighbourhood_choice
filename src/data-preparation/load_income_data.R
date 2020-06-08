## Load income data ###

#set working directory to sources file location
#this script loads the csv income files in the data folder. They have been manually created from the CBS files to fit our purposes

############# set up
#set working directory to Source file location

#load packages
library(tidyverse)
library(data.table)

#load data
dir <- "../../data/"
####################

#relevant columns to use from csv files
col <- c("Gemeentecode", "Wijkcode", "Buurtcode", "Totaal", "age_cat_25", "age_cat_24_44", "age_cat_45_64", "age_cat_65")


#2014
inc_raw_14 <- data.table(read.csv(paste0(dir, "VSO Tabel 4.2 2014.csv"), skip = 5, col.names = col))
inc_raw_14[,jaar := 2014]


#2015
inc_raw_15 <- data.table(read.csv(paste0(dir, "VSO Tabel 4.2 2015.csv"), skip = 5, col.names = col))
inc_raw_15[,jaar := 2015]

#2016
inc_raw_16 <- data.table(read.csv(paste0(dir, "VSO Tabel 4.2 2016.csv"), skip = 5, col.names = col))
inc_raw_16[,jaar := 2016]
inc_raw_16 <- inc_raw_16[-11634,]

#2017
inc_raw_17 <- data.table(read.csv(paste0(dir, "VSO Tabel 4.2 2017.csv"), skip = 5, col.names = col))
inc_raw_17[,jaar := 2017]
inc_raw_17 <- inc_raw_17[-11888,]

#2018: use 2018 data
inc_raw_18 <- inc_raw_17 %>%
  mutate(jaar = 2018)

#put all years together
inc_raw <- bind_rows(inc_raw_14, inc_raw_15, inc_raw_16, inc_raw_17, inc_raw_18)

#only take neighbourhood levels
inc <- inc_raw %>%
  mutate_at(vars(Totaal : age_cat_65), as.numeric) %>%
  filter(Buurtcode != "Totaal" & Gemeentecode != "Nederland") %>%
  mutate(buurtcode = paste0(Gemeentecode, Wijkcode, Buurtcode)) 

#transform into long format
income <- pivot_longer(inc, Totaal:age_cat_65)

#prepare for merging with moving data
income <- income %>%
  mutate(buurtcode = as.numeric(buurtcode)) %>%
  mutate(jaar = as.character(jaar)) %>%
  rename(income = value, age_cat = name) %>%
  data.table

income[, age_cat := sub("age_cat_","", age_cat)]


save(income, file = "../../gen/data-preparation/input/income.Rdata")
            