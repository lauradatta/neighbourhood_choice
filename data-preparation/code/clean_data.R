library("tidyverse")
library("data.table")

load('Data/bevolking18.Rdata')
load('Data/verhuis1418.Rdata')
load('Data/woningen18.Rdata')

labels_prct19 <- c("huur < € 424" , "huur € 424-607", "huur € 607-720", "huur > € 720", "huur onbekend", 
                   "koop < € 185.000", "koop € 185-225.000", "koop € 225-275.000", "koop € 275-350.000", 
                   "koop € 350-425.000", "koop > € 425.000", "koop onbekend")  

woningen18$prscat19 <- factor(woningen18$prscat19, levels = rev(labels_prct19))

woningen18 %>%
  count(gem, wontype) %>%
  ggplot(aes(x = gem, y = n, fill = wontype)) + 
    geom_col()

woningen18 %>%
  filter(huurkoop == "Koopwoning") %>%
  count(gem, prscat19) %>%
  group_by(gem) %>%
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(x = gem, y = perc, fill = prscat19))+
    geom_col()

vhb1418_door %>%
  filter(PKD == '5216SE' & HNR == 17) %>% data.frame

  