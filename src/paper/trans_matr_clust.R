#Transition matrix on cluster levels

##### Set up

#set working directory to source file location

#load pacakages
library(tidyverse)
library(data.table)
#library(tikzDevice)

# copy following files from "../../gen/analysis/output/" first
load("../../gen/paper/input/data_cluster.Rdata")
load("../../gen/paper/input/descriptives.Rdata")

#nice labels for covariates
source("../../src/paper/labels_covariates.R")

#choose relevent columns
col <- c("jaar", "buurtnaam", "buurtnaam_vrg", "vrg_buurtcode", "cluster", "cluster_vrg")
############

#prepare data
moves_trans <- moves_db_cl %>%
  select(col)

cluster <- pull(moves_trans, cluster)

#change order of cluster according to neighbourhood reputation
cluster_ord_all <- c(cluster_ord, 10, 11)

clust_label <- c(labels_clust, "Neighbour municipality", "Outside municipality")

cluster <- factor(cluster, levels = cluster_ord_all, labels = clust_label)

cluster_vrg <- factor(pull(moves_trans, cluster_vrg),  levels = cluster_ord_all,labels = clust_label)


##### Outward mobility ###########

#transition matrix
trans_matrix <- as.data.frame.matrix(table(cluster_vrg, cluster))

#prepare of heat map
trans_matrix_dt <- data.table(table(cluster, cluster_vrg))

#tikz(file = "../../gen/paper/output/trans_matr_clust.tex", width =10, height = 5)

#Graph
g_trans_matrix_out <- trans_matrix_dt %>%
  group_by(cluster_vrg) %>%
  mutate(p_out = round(N/sum(N),2)) %>%
  ggplot(aes(factor(cluster, levels = clust_label), factor(cluster_vrg, levels = rev(clust_label)))) +
    geom_tile(aes(fill = 1-log(p_out))) +
    geom_text(aes(label = p_out*100))+
    #ggtitle("Household relocations between neighbourhood clusters within 's-Hertogenbosch (2014 - 2018)") + 
    scale_fill_distiller("Greys") +
    scale_y_discrete(name = "Previous neighbourhood cluster \n")+
    scale_x_discrete(name = "New neighbourhood cluster \n", position = "top", expand = c(0,0,0.2,0)) +
    annotate("text", y = 1:11, x = 12, label = paste0(100, "\\%")) + 
    theme(legend.position = "None", 
          axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(angle = 45, hjust = 0),
          panel.background = element_blank(),
          panel.spacing=unit(c(10,10,10,10), "cm"))

g_trans_matrix_out

ggsave(g_trans_matrix_out, file = "../../gen/paper/output/trans_matrix_clust_out.jpg", width = 10, height = 7)

#dev.off()


########## Inward mobility ##########

#transition matrix
trans_matrix <- as.data.frame.matrix(table(cluster_vrg, cluster))

#heat map
trans_matrix_dt <- data.table(table(cluster_vrg, cluster))

#tikz(file = "../../gen/paper/output/trans_matr_clust.tex", width =10, height = 5)

g_trans_matrix_in <- trans_matrix_dt %>%
  group_by(cluster) %>%
  mutate(p_in = round(N/sum(N),2)) %>%
  ggplot(aes(factor(cluster, levels = clust_label), factor(cluster_vrg, levels = rev(clust_label)))) +
    geom_tile(aes(fill = 1-log(p_in))) +
    geom_text(aes(label = p_in*100))+
    #ggtitle("Household relocations between neighbourhood clusters within 's-Hertogenbosch (2014 - 2018)") +
    scale_fill_distiller("Greys") +
    scale_y_discrete(name = "Previous neighbourhood cluster")+
    scale_x_discrete(name = "New neighbourhood cluster \n", position = "top", expand = c(0,0,0,0)) +
    annotate("text", x = 1:11, y = 0, label = paste0(100, "\\%")) +
    theme(legend.position = "None",
          axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 10, l = 0)),
          axis.text.x = element_text(angle = 45, hjust = 0),
          panel.background = element_blank(),
          panel.spacing=unit(c(10,10,10,10), "cm"))

g_trans_matrix_in

ggsave(g_trans_matrix_in, file = "../../gen/paper/output/trans_matrix_clust_in.jpg", width = 10, height = 7)

#dev.off()
