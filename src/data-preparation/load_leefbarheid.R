### Load Data Leefbarheid ####

############# set up
#set working directory to Source file location

#load packages
library(tidyverse)
library(data.table)

#load leefbh scores, have been downloaded from internet
leef_raw <- fread("../../data/scorebuurt/Score_Buurt.csv")
##############


leef <- leef_raw %>%
  select(CODE, KL14, KL16, KL18) %>%
  mutate(buurtcode = as.numeric(substr(CODE, 3, 10))) %>%
  select(-CODE) %>%
  add_column(KL15 = NA, .before = "KL16") %>%
  add_column(KL17 = NA, .before = "KL18")


leef_long <- pivot_longer(leef, KL14:KL18) %>%
  data.table

leef_long[, jaar := sub("KL", "20", name)]

leefbh <- leef_long %>%
  select(-name) %>%
  rename(leefbh = value) %>%
  group_by(buurtcode) %>%
  fill(leefbh, .direction = c("down"))


save(leefbh, file = "../../gen/data-preparation/input/leefbh.Rdata")
