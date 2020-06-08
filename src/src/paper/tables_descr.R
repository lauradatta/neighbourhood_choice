### Tables Descriptive statistics #####

library(stargazer)

load("../../gen/paper/input/descriptives.Rdata")
source("../../src/paper/labels_covariates.R")

#table nh characteristics

labels_nh_descr <- c("Average Dwelling Values (x 1000, in €)", 
                     "Mean Household Income (x 1000, in €)",
                     "Share of Non-Western Minorities (in %)", 
                     "Share of Western Minorities (in %)",
                     "Share of Singles or Other (in %)", 
                     "Share of Families with Children (in %)",
                     "Share of Private Rental (in %)",
                     "Share of Social Housing (in %)", 
                     "Share of Dwellings Built after 2000",
                     "Housing Density (adresses / km\\textsuperscript{2})", 
                     "Distance to Highway Access Lane (in km)", 
                     "Distance to Important Train Station (in km)",
                     "Restaurants Within 3 km", 
                     "Neighbourhood Reputation")


descr_nh_df <- descr_nh %>%
  select(-name) %>%
  data.frame(row.names = labels_nh_descr)

stargazer(descr_nh_df, out = "../../gen/paper/output/table_descr_nh.tex", type = "latex", title = "Descriptive statistics: Neighbourhood characteristics, (2018, N = 72)",
          summary = F, digits = 1, label = "tab:nh_descr")


# Household characteristics

labels_hh_descr <- c("Move within municipality",
                     "Household Income",
                     "Native Dutch",
                     "Non-Western minority",
                     "Western minority",
                     "Single or other",
                     "Couples",
                     "Families with children",
                     "Younger than  25 years",
                     "25 - 64 years",
                     "45 - 64 years",
                     "Older than 65 Years",
                     "Room-stress")

hh_mean_df <- hh_mean %>%
  select(-hh_char) %>%
  data.frame(row.names = labels_hh_descr)

stargazer(hh_mean_df, out = "../../gen/paper/output/table_descr_hh.html", title = "Descriptive statistics: Household characteristics, (N = 18,318)",
          summary = F, digits = 1, label = "tab:hh_descr")


#Cluster characteristics


#labels
labels_var_nh <- c("Number of Neighbourhoods",
                   "Total number of households",
                   "Mean number of households",
                   "Mean number of inhabitants",
                   "Turnover rate (in %)",
                   "Mean Household Income (x 1000, in €)",
                   "Share of Native Dutch (in %)",
                   "Share of Non-Western Minorities (in %)", 
                   "Share of Western Minorities (in %)",
                   "Share of Singles or Other (in %)", 
                   "Share of Couples (in %)",
                   "Share of Families with Children (in %)",
                   "Average Dwelling Values (x 1000, in €)",
                   "Mean housing size (in m\\textsuperscript{2})",
                   "Share of Home Ownership (in %)", 
                   "Share of Private Rental (in %)",
                   "Share of Social Housing (in %)", 
                   "Share of New Houses (in %)",
                   "Housing Density (addresses / km\\textsuperscript{2})", 
                   "Distance to Highway Access Lane (in km)", 
                   "Distance to Train Station (in km)",
                   "Restaurants Within 3 km", 
                   "Neighbourhood Reputation")

descr_nh_cluster_df <- descr_nh_cluster %>%
  select(-name) %>%
  data.frame(row.names = labels_var_nh)

colnames(descr_nh_cluster_df) <- labels_clust



stargazer(descr_nh_cluster_df, out = "../../gen/paper/output/table_descr_nh_clust.html",  summary = F, label = "tab:nh_clust",
          title = "Mean characteristics of neighbourhood clusters (2018, Number of Neighbourhoods = 72)", digits = 1, no.space = T, align = F, column.sep.width = "0.5pt")


#labels
labels_var_nh_redu <- c("Reputation",
                        "Dwelling Values (x 1000)",
                        "Native Dutch",
                        "Families with Children",
                        "Social Housing", 
                        "Distance to Train Station")

descr_cluster_redu_df <- descr_cluster_redu %>%
  ungroup() %>%
  select(-cluster) %>%
  data.frame(row.names = labels_clust)

colnames(descr_cluster_redu_df) <- c(labels_var_nh_redu)



stargazer(descr_cluster_redu_df, out = "../../gen/paper/output/table_descr_nh_clust_redu.html",  summary = F, label = "tab:nh_clust_red",
          title = "Mean characteristics of neighbourhood clusters for one variable per dimension (2018, Number of Neighbourhoods = 72)", 
          digits = 1, no.space = T, align = F, column.sep.width = "0.5pt")


