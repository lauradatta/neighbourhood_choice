library(tidyverse)
library(data.table)
library(plotly)

source("code/data-preparation/clean_data.R")

col <- c("jaar", "PRSANR", "gem", "vrggem", "wijknaam", "wijknaam_vrg", "buurtnaam", "buurtnaam_vrg", "move_type")

#remove Buitengebied Heesch
moves_db <- moves_db %>%
  filter(wijknaam != "Buitengebied Heesch" & wijknaam_vrg != "Buitengebied Heesch")

#filter for 2018 only
moves_db_2018 <- moves_db %>%
  filter(jaar == "2018")


wijk_sorted <- c(moves_db %>%
                   distinct(wijknaam) %>%
                   filter(str_detect(wijknaam, "Outside", negate = T)) %>% 
                   pull, moves_db %>%
                   distinct(wijknaam) %>%
                   filter(str_detect(wijknaam, "Outside")) %>% 
                   pull)

moves_db <- moves_db %>%
  mutate_at(vars(contains("wijk")), ~(factor(., levels = wijk_sorted)))


#### Transition matrix on 'wijk' level
#vector with all current wijks
wijk <- moves_db %>% 
  select(wijknaam) %>%
  #filter(!is.na(wijknaam)) %>%
  pull

# vector with previous wijks
wijk_vrg <- moves_db %>%
  select(wijknaam_vrg) %>%
  #filter(!is.na(wijknaam_vrg)) %>%
  pull



#transition matrix
trans_matrix <- as.data.frame.matrix(prop.table(wijk_vrg, wijk))

#heat map
trans_matrix_dt <- data.table(table(wijk, wijk_vrg))


g_trans_matrix <- trans_matrix_dt %>%
  group_by(wijk_vrg) %>%
  mutate(p = round(N/sum(N),2)) %>%
  ggplot(aes(factor(wijk, levels = wijk_sorted), factor(wijk_vrg, levels = rev(wijk_sorted)))) +
    geom_tile(aes(fill = log(p))) +
    geom_text(aes(label = scales::percent(p)))+
    #ggtitle("Household relocations between districts (wijken) in Gemeente 's-Hertogenbosch for 2014 - 2018") + 
    scale_fill_distiller(palette = "RdYlGn") +
    scale_y_discrete(name = "Previous district (wijk)") +
    scale_x_discrete(name = "New district (wijk)", position = "top", expand = c(0,0,0.1,0)) +
    annotate("text", y = 1:16, x = 17, label = paste0(100, "%")) + 
    theme(legend.position = "None", 
          panel.background = element_blank(),
          panel.spacing=unit(c(10,10,10,10), "cm"))

g_trans_matrix
    
ggsave(g_trans_matrix, file = "gen/analysis/output/g_trans_matrix.jpg", width = 14, height = 8)
    


trans_matrix_dt %>%
  group_by(wijk_vrg) %>%
  mutate(p_in = N/sum(N)) %>%
  ggplot(aes(wijk_vrg, wijk)) +
  geom_tile(aes(fill = p_in)) +
  scale_fill_distiller(palette = "RdYlGn")


#plotly heat map
ggplotly(g_trans_matrix)

                     