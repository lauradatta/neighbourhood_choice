#Test optimal number of clusters

library(tidyverse)
library(data.table)
library(cluster)
library(factoextra)

#run clustering script
source("../../src/analysis/clustering_nh.R")

###### Test optimal number of clusters ####

#Elbow method

set.seed(1)

fviz_nbclust(nhchar_scaled, kmeans, method = "wss", k.max = 15)

# Silhouette
set.seed(1)
fviz_nbclust(nhchar_scaled, kmeans, method = "silhouette", k.max = 15)

#Gap statistic
set.seed(1)

gap_stat <- clusGap(nhchar_scaled, FUN = kmeans, nstart = 25,
                    K.max = 15, B = 50)
fviz_gap_stat(gap_stat)

# we set optimal number of cluster to ()

####### some testing with PCA ###########

#evetually not used
pr_out <- prcomp(nhchar_scaled, scale = T)

summary(pr_out)

pr_out$rotation

pca_var <- get_pca_var(pr_out)

pca_var$contrib


fviz_pca_var(pr_out, col.var = "black")

#result <- cbind(nh_db$buurtcode,data.table(pr_out$x[,1:2]))

#plot PC1 and PC2
#ggplot(result, aes(x = PC1, y = PC2, label = V1)) +
#  geom_point()+
#  geom_text()


pca_res <- pr_out$x[,1:10]

#Elbow method

set.seed(1)

fviz_nbclust(pca_res, kmeans, method = "wss", k.max = 15)

#Gap statistic
set.seed(1)
gap_stat <- clusGap(pca_res , FUN = kmeans, nstart = 25,
                    K.max = 15, B = 50)
fviz_gap_stat(gap_stat)

set.seed(1)

kmeans_out <- kmeans(pca_res , 9, nstart = 25)

#results
nh_clusters <- data.table(cbind(nhchar_db18_cl$buurtcode, kmeans_out$cluster))

nh_clusters <- nh_clusters %>%
  rename(buurtcode = V1, cluster = V2) %>%
  mutate(buurtnaam = nhchar_db18_cl$buurtnaam)



