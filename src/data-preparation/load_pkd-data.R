library(tidyverse)
library(data.table)
library(foreign)

##### Labels and codes buurten, wijken, gemeente based on postcode ######

#Gemeentes in regio Noord Oost Brabant
gems <- vhb1418_door %>% distinct(gem) %>% pull

#load codes downloaded from CBS
pc6_gwb_code <- read_csv2("input/2018-cbs-pc6huisnr20180801_buurt -vs2/pc6hnr20180801_gwb-vs2.csv")
gemeentenaam <- read_csv2("input/2018-cbs-pc6huisnr20180801_buurt -vs2/gemeentenaam2018.csv")
wijknaam <- read_csv2("input/2018-cbs-pc6huisnr20180801_buurt -vs2/wijknaam2018.csv")
buurtnaam <- read_csv2("input/2018-cbs-pc6huisnr20180801_buurt -vs2/buurtnaam2018.csv")

#filter for Regio Noordoost Brabant
gem_db <- gemeentenaam %>%
  filter(GWBlabel %in% gems) %>%
  select(GWBcode8) %>%
  pull

pc6_gwb_code_db <-pc6_gwb_code %>%
  select(-Huisnummer) %>%
  filter(Gemeente2018 %in% gem_db) %>%
  distinct(PC6, .keep_all =T)

#join names
pc6_gwb <- pc6_gwb_code_db %>%
  left_join(gemeentenaam, by  = c("Gemeente2018" =  "GWBcode8")) %>%
  rename(gemeentenaam = GWBlabel) %>%
  left_join(wijknaam, by  = c("Wijk2018" =  "GWBcode8")) %>%
  rename(wijknaam = GWBlabel) %>%
  left_join(buurtnaam, by  = c("Buurt2018" =  "GWBcode8")) %>%
  rename(buurtnaam = GWBlabel)


## missing postcodes
#load CBS postcode data for 2015

pkd_mis <- c("5231XJ", "5231XK", "5231EK", "5222AB", "5231WD", "5212XB", "5223XE", "5231VA", "5231XL", 
             "5244JG", "5391LJ", "5216VG", "5222AE", "5223RL", "5223VT", "5223XH", "5223XJ", "5231VB", 
             "5235TX", "5247WE", "5224EE", "5213BL", "5212GC", "5216JH", "5241CS", "5233HE", "5221GB", 
             "5223GX", "5242HN", "5211AA", "5235DP", "5241EH", "5235ED", "5223DE")


pkd_2015 <- read.dbf("input/2015-cbs-pc6huisnummer-buurt/2015-cbs-pc6huisnummer-buurt.dbf")
pkd_2015 <- pkd_2015 %>%
  distinct(POSTCODE, .keep_all = T)

pkd_2015_mis <- pkd_2015 %>%
  filter(POSTCODE %in% pkd_mis) %>%
  select(-HUISNUMMER) %>%
  rename(PC6 = POSTCODE, Buurt2018 = BUURTCODE, Wijk2018 = WIJKCODE, Gemeente2018 = GEMEENTECO, 
         gemeentenaam = GEMEENTENA, wijknaam = WIJKNAAM, buurtnaam = BUURTMNAAM)

pc6_gwb <- bind_rows(pc6_gwb, pkd_2015_mis) %>% data.table



#save file for further use
save(pc6_gwb, file = "gen/data-preparation/input/pc6_gwb.Rdata")
