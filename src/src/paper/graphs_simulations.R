#Make simulation graphs

#this script makes latex code for those graphs, you can also simply use ggsave, if you want to have them in jpeg format

###### set up
#set working directory to source file location

#load packages
library(tidyverse)
library(data.table)
#library(tikzDevice) #package to render latex graphs
library(ggthemes)


# copy following files from "../../gen/analysis/output/" first
load("../../gen/paper/input/simulation.Rdata")

#nice labels for covariates
source("../../src/paper/labels_covariates.R")
#################

### simulation 1: Change in economic situation of the household (income decrease)

#Add percentage differences for the labels
prob_cl_inc <- prob_cl %>%
  filter(shock %in% c("average", "income_neg")) %>%
  pivot_wider(names_from = shock, values_from = prob) %>%
  mutate(diff_inc = (income_neg - average)/average)


#tikz(file = "../../gen/paper/output/g_sim_inc.tex", width =10, height = 5)

#Graph income
g_sim_inc <- prob_cl %>%
  filter(shock %in%  c("average", "income_neg")) %>%
  ggplot(aes(x = cluster, y = prob, fill = shock)) + 
  geom_bar(stat = "identity", position="dodge") + 
  geom_text(aes(label = paste0(round(prob_cl_inc$diff_inc * 100,1), "\\%")), data = prob_cl %>% filter(shock == "income_neg"), 
            position = position_dodge(0.9), vjust = - 1.5) +
  scale_fill_hc(name = "Income level", labels = c("Average income", "5\\% decrease")) + 
  scale_x_discrete(name = "New neighbourhood cluster") + 
  scale_y_continuous(name = 'Probability of neighbourhood choice\n', limits = c(0,0.5), labels = function(x) paste0(round(x*100,0), "\\%")) +
  #annotate("text", y = 0.3, x = 1:9, label = paste0(round(perc_inc * 100,1), "\\%")) + 
  theme_hc()+
  theme(panel.background = element_blank(),
        text = element_text(face = "plain", size = 14),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black")) #,
        #axis.title.x = element_blank())

g_sim_inc

#dev.off()

#### Simulation 1b: Test white flight theory (postive income shock to hh in vulnerable cluster based on ethnicity)

#prepare data
prob_cl8_ethn <- prob_cl8 %>%
  filter(cluster == "Vulnerable" & shock != "soc_housing_cl5") %>%
  add_column(ethnicity = c(rep("Native Dutch",2), rep("Western",2), rep("Non-western",2))) %>%
  mutate(shock = rep(c("average", "incpos"),3)) %>%
  mutate(ethnicity = factor(ethnicity, levels = c("Native Dutch", "Western", "Non-western")))

#percentage changes
perc_cl8_ethn <- prob_cl8_ethn %>%
  pivot_wider(names_from = shock, values_from = prob) %>%
  mutate(diff = (incpos - average)/average)

#tikz(file = "../../gen/paper/output/g_cl8_ethn.tex", width = 10, height = 5)

#Graph
g_cl8_ethn <- prob_cl8_ethn %>%
  ggplot(aes(x = ethnicity, y = prob, fill = shock)) + 
    geom_col(position = "dodge") + 
    geom_text(aes(label = paste0(round(perc_cl8_ethn$diff * 100,1), "\\%")), data = prob_cl8_ethn %>% filter(shock == "incpos"), 
            position = position_dodge(0.9), vjust = - 1.5, hjust = 0.5) +
    scale_fill_hc(name = "Income", labels = c("Baseline", "5\\% increase" )) + 
    scale_x_discrete(name = "Ethnicity") + 
    scale_y_continuous(name = "Probability to choose vulnerable cluster", limits = c(0,0.5), labels = function(x) paste0(round(x*100,0), "\\%")) +
    theme_hc()+
    theme(panel.background = element_blank(),
          text = element_text(face = "plain", size = 14),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 0, hjust = 1),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"))

g_cl8_ethn

dev.off()


#########Simulation on room stress

#tikz(file = "../../gen/paper/output/g_room_stress.tex", width = 10, height = 5)

#Graph
g_room_stress <- prob_cl %>%
  filter(shock %in%  c("average", "room_stress", "room_stress_incpos")) %>%
  ggplot(aes(x = cluster, y = prob, fill = shock)) + 
  geom_bar(stat = "identity", position="dodge")+ 
  geom_text(aes(label = paste0(round(prob * 100,1), "\\%")), position = position_dodge(0.9), vjust = - 1.5) +
  scale_fill_hc(name = "", labels = c("Average income, average room-stress", "Average income, increased room-stress ", "Increased income, increased room-stress")) + 
  scale_x_discrete(name = "New neighbourhood cluster") + 
  scale_y_continuous(name = 'Probability of neighbourhood choice\n', limits = c(0,0.5), labels = function(x) paste0(round(x*100,0), "\\%")) +
  theme_hc()+
  theme(panel.background = element_blank(),
        text = element_text(face = "plain", size = 14),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"))


g_room_stress

dev.off()

##### Staying power of neighbourhoods (not included in thesis)

prob_sticky_avg <- prob_sticky[shock == "average"]

prob_stay_avg <- data.table(shock = "average", prob_stay = diag(as.matrix(prob_sticky_avg)), cluster = factor(1:9, levels = cluster_ord, labels = labels_clust))

prob_stay_avg[, prob_stay := as.numeric(prob_stay)]

#tikz(file = "../../gen/paper/output/g_sticky.tex", width = 10, height = 5)

g_sticky <- prob_stay_avg %>%
  #filter(shock == "average") %>%
  ggplot(aes(x = cluster, y = prob_stay)) +  #, fill = shock)) + 
    geom_col(position = "dodge") +
    geom_text(aes(label = paste0(round(prob_stay * 100,1), "\\%")), position = position_dodge(0.9), vjust = - 1.5) +
    #scale_fill_hc(name = "\nIncome level", labels = c("Average", "5\\% increase")) + 
    scale_x_discrete(name = "New neighbourhood cluster") + 
    scale_y_continuous(name = 'Probability of stay\n', limits = c(0,0.5), labels = function(x) paste0(round(x*100,0), "\\%")) +
    theme_hc()+
    theme(panel.background = element_blank(),
          text = element_text(face = "plain", size = 14),
          legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black")) #,

g_sticky

#dev.off()

########## Simulation 2a: Increase social housing in cluster 5 (new family houses) ###########

#percentage changes
perc_cl5 <- prob_cl8 %>%
  filter(shock %in% c("average", "soc_housing_cl5")) %>%
  pivot_wider(names_from = "shock", values_from = "prob") %>%
  mutate(diff = (soc_housing_cl5 - average)/average)


#tikz(file = "../../gen/paper/output/g_cl8_sochos.tex", width = 10, height = 5)
#Graph
g_cl8_sochos <- prob_cl8 %>%
  filter(shock %in% c("average", "soc_housing_cl5")) %>%
  ggplot(aes(x = cluster, y = prob, fill = shock)) +
  geom_col(position = "dodge") + 
  geom_text(aes(label = paste0(round(perc_cl5$diff * 100,1), "\\%")), data = prob_cl8 %>% filter(shock == "soc_housing_cl5"),position = position_dodge(0.9), vjust = - 1.5) +
  scale_fill_hc(name = "Measures", labels = c("Current share of social housing", "Increase social housing 'new family houses' cluster")) + 
  scale_x_discrete(name = "New neighbourhood cluster") + 
  scale_y_continuous(name = "Probability of neighbourhood choice\n", 
                     limits = c(0,0.5), labels = function(x) paste0(round(x*100,0), "\\%")) +
  theme_hc()+
  theme(panel.background = element_blank(),
        text = element_text(face = "plain", size = 14),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black")) #,

g_cl8_sochos

#dev.off()

######## Simulation 2b: increase in neighbourhood reputation for cluster 8

#percentage changes
perc_rep_cl8 <- prob_sticky_long %>%
  filter(cluster == "Vulnerable") %>%
  pivot_wider(names_from = "shock", values_from = "prob") %>%
  mutate(diff = (reputation - average)/average)

#tikz(file = "../../gen/paper/output/g_rep_cl8.tex", width = 10, height = 5)

g_rep_cl8 <- prob_sticky_long %>%
  filter(cluster == "Vulnerable") %>%
  ggplot(aes(x = cluster_vrg, y = prob, fill = shock)) +
  geom_col(position = "dodge") +
  coord_flip() + 
  geom_text(aes(label = paste0(round(perc_rep_cl8$diff * 100,1), "\\%")), data = prob_sticky_long %>% filter(shock == "reputation" & cluster == "Vulnerable"),
            position = position_dodge(0.9), vjust = 0.5, hjust = - 0.3) +
  scale_fill_hc(name = "Neighbourhood reputation of 'vulnerable' cluster", labels = c("Current (4.8)", "Increased (6.8)")) + 
  scale_x_discrete(name = "Previous neighbourhood cluster") + 
  scale_y_continuous(name = 'Probability to move to "vulnerable" cluster\n', limits = c(0,0.3), labels = function(x) paste0(round(x*100,0), "\\%")) +
  theme_hc()+
  theme(panel.background = element_blank(),
        text = element_text(face = "plain", size = 14),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black")) #,


g_rep_cl8

#dev.off()
