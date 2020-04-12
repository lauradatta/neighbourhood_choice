#load moves data

#set up
library(tidyverse)
library(data.table)

#working on VDI
vdi = T #set to TRUE if working on VDI

if (vdi == T){
  dir <- "../../gen/data-preparation/"
  
  #load moving and population data
  load(paste0(dir,"input/verhuis1418.Rdata"))
  load(paste0(dir,"input/bevolking18.Rdata"))
  load(paste0(dir,"input/pc6_gwb.Rdata"))
  
  #take out Gemeentenaam from GWB codes
  pc6_wb <- pc6_gwb %>%
    select(-gemeentenaam)
  
  
  #transform into data.tale
  vhb1418_door <- data.table(vhb1418_door)
  vhb1418_in <- data.table(vhb1418_in)
  vhb1418_uit <- data.table(vhb1418_uit)
  gba18 <- data.table(gba18)
  
  #merge moving 'In' and 'door' datasets together
  vhb1418_door[,verhuis_type := "door"]
  vhb1418_in[,verhuis_type := "in" ]
  vhb1418_uit[,verhuis_type := "uit"]
  
  
  #merge in and door
  vhb_di_raw <- bind_rows(vhb1418_door, vhb1418_in, .id=NULL)
  
  
  #### cleaning #######
  
  
  #### reduce dataset vhb_di to relevant columns
  col_di <- c("jaar", "gem", "gemwpl", "HNR", "HLT", "HTV", "HAN", "PKD", "cat_vh", "ingdat", "vrggem", "vrggemwpl", "vrgHNR", "vrgHLT", "vrgHTV", 
              "vrgHAN", "vrgpkd", "PRSANR", "PRSGDTYY", "PRSGDTMM", "PRSGDTDD", "PRSGES", "age", "AANBEW", "cdhhw", 
              "cbsetngr", "etnikort", "hsh_cat",  "waarde",  "huur", "prscat", "opp", "kamers",  "inhoud",  "huurkoop",  
              "eigendom",   "bouwjaar",  "bouwwyze",   "wontype",  "vloeropp",  "vrgwrd", "vrghr",  "vrgpct",  "vrgopp",  "vrgkam",  "vrginh",  
              "vrghrkp", "vrgeigd", "vrgbjr",  "vrgbwwyz",   "vrgwtyp", "vrgvlopp", "verhuis_type")
  
  vhb_di <- vhb_di_raw %>%
    select(col_di) %>%
    data.table
  
  
  ### clean "uit" dataset ###
  col_uit <- c("jaar", "gem", "HNR", "HLT", "HTV", "HAN", "PKD", "ingdat","cat_vh", "nwgem", "nwHNR", "nwHLT", "nwHTV", "nwHAN", "nwPKD", "PRSANR", "PRSGDTYY", 
               "PRSGDTMM", "PRSGDTDD", "PRSGES", "age", "AANBEW", "cdhhw", "cbsetngr", "etnikort", "hsh_cat", "gemwpl", "nwgemwpl", "opp", "waarde", 
               "huur", "prscat", "kamers", "inhoud", "huurkoop", "eigendom", "bouwjaar", "bouwwyze", "wontype", "vloeropp", "verhuis_type")
  
  
  #columns to be renamed
  #add vrg
  #old column names
  col_vrg <- c("gem", "gemwpl", "HNR", "HLT", "HTV", "HAN", "PKD",  "waarde", "huur", "prscat", "opp", "kamers", "inhoud", "huurkoop", "eigendom", 
               "bouwjaar", "bouwwyze", "wontype", "vloeropp")
  #new column names
  col_vrg_new <- c("vrggem", "vrggemwpl","vrgHNR", "vrgHLT", "vrgHTV", "vrgHAN", "vrgpkd",  "vrgwrd", "vrghr",  "vrgpct",  "vrgopp",  "vrgkam",  "vrginh", "vrghrkp", "vrgeigd", 
                   "vrgbjr",  "vrgbwwyz",   "vrgwtyp", "vrgvlopp")
  
  vhb1418_uit_cl <- vhb1418_uit %>%
    select(col_uit) %>%
    rename_at(vars(col_vrg), ~col_vrg_new) %>%
    set_names( ~ str_remove(.,"nw"))
  
  #merge di with uit
  vhb <- bind_rows(vhb_di, vhb1418_uit_cl)
  


  
  ### clean GWB codes using CBS data
  
  vhb <- vhb %>%
    left_join(pc6_wb, by = c("PKD"= "PC6")) %>%
    rename(buurtcode = Buurt2018, wijkcode = Wijk2018, gemcode = Gemeente2018) %>%
    left_join(pc6_wb, by = c("vrgpkd"= "PC6"), suffix = c("", "_vrg")) %>%
    rename(vrg_buurtcode = Buurt2018, vrg_wijkcode = Wijk2018, vrg_gemcode = Gemeente2018)%>%
    data.table
  
  moves <- vhb
  
  ### cleaning moves
  
  # take out double cases in terms of 'doorverhuizingen'
  moves <- moves %>%
    distinct_at(vars(PRSANR,ingdat), .keep_all = T)
  
  # take out cases of death
  
  moves <- moves[cat_vh != "Verlater (overl)"]
  
  #transform missing PKD into NAs
  moves[PKD == "", PKD := NA]
  moves[vrgpkd == "", vrgpkd := NA]
  
  ###### for anonymisation take out the following variables ########
  
  anonym <- c("HNR", "HLT", "HTV", "HAN", "PKD", "vrgHNR", "vrgHLT", "vrgHTV", "vrgHAN", "vrgpkd", "PRSANR", "PRSGDTYY", "PRSGDTMM", "PRSGDTDD", "PRSGES", 
              "age", "AANBEW", "cdhhw", "cbsetngr", "etnikort")
  
  replace_to_na <- function(column) {
    column <- NA
  }
  
  moves_anonym <- moves %>%
    mutate_at(vars(anonym),replace_to_na)
  
  save(moves, file = "../../gen/data-preparation/temp/moves.Rdata")
  
  save(moves_anonym, file = "../../gen/data-preparation/temp/moves_anonym.Rdata")

}
