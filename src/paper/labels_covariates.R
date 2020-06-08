

labels_indiv <- c("Moves within \\textsuperscript{a}",
  "Household Income",
  "Non-western minority \\textsuperscript{c}",
  "Western minority \\textsuperscript{c}",
  "Singles or other \\textsuperscript{b}",
  "Families with children \\textsuperscript{b}",
  "< 25 years \\textsuperscript{d}",
  "45 - 64 years \\textsuperscript{d}",
  "> 65 years \\textsuperscript{d}",
  "Room-stress")

labels_nh_short <- c("Dwelling  values",
               "Neighbourhood income",
               "Non-western  minorities",
               "Western minorities",
               "Singles or other", 
               "Families with children",
               "Social housing",
               "New houses",
               "Distance to highway", 
               "Distance to  train station",
               "Neighbourhood reputation")


labels_nh <- c("Average dwelling values",
               "Mean neighbourhood income",
               "Share of non-western minorities",
               "Share of western minorities",
               "Share of singles or other",
               "Share of families with children",
               #"Share of Private Rental",
               "Share of social housing",
               "Share of new houses",
               #"Housing Density", 
               "Distance to highway", 
               "Distance to train station",
               #"Restaurants Within 3 km", 
               "Neighbourhood reputation")



labels_nh_int <- paste("Interactions with",
                   labels_nh)



labels_mlogit <- rep(c(
                    "Moves within \\textit{(Reference: Moves into)}",
                     "Household income",
                     "Non-western minority \\textit{(Reference: Native Dutch)}",
                     "Western minority \\textit{(Reference: Native Dutch)}",
                    "Singles or other \\textit{(Reference: Couples)}",
                      "Families with children \\textit{(Reference: Couples)}",
                     "< 25 years \\textit{(Reference: 25 - 44 years)}",
                     "45 - 64 years \\textit{(Reference: 25 - 44 years)}",
                     "> 65 Years \\textit{(Reference: 25 - 44 years)}",
                     "Room-stress"), length(labels_nh_int))


labels_clust <- c(
                  "Vulnerable",
                  "Melting pot",
                  "The average",
                  "Diversity outside centre",
                  "Urban expansion",
                  "Rural and spacious", 
                  "City centre",
                  "New family houses",
                  "High-end"
                 )

cluster_ord <- c(8, 9, 6, 7, 3, 1, 2, 5, 4)
