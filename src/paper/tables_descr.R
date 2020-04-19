### Tables Descriptive statistics #####

library(stargazer)

load("../../gen/paper/input/descriptives.Rdata")

#table nh characteristics

stargazer(descr_dh_df, out = "../../gen/paper/output/table_descr_nh.tex", summary = F, digits = 1)

stargazer(descr_nh_cluster_df, out = "../../gen/paper/output/table_descr_nh_clust.tex", summary = F, digits = 1)
