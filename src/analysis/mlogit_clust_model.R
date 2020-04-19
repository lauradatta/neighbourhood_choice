#Mlogit models

#set up
library(data.table)
library(tidyverse)
library(mlogit)

load("../../gen/analysis/temp/dt_mlogit.Rdata")
load("../../gen/analysis/temp/variables.Rdata")

###### build models ########

#define alternative and individual specific variables
alt_specific <- covar_nh
indiv_specifc <- c(covar_indiv, covar_nh_vrg)


#### Model: interactions between individual and alternative specific covariates

#build interactions
int <- c()

# add interactions
for (i in 1:length(alt_specific)){
  for (j in 1:length(indiv_specifc)){
    int <- c(int, paste(c(alt_specific[i], indiv_specifc[j]), collapse = " : "))
  }
}

#formula
f_interact <- as.formula(paste("choice",
                          paste(c(int, - 1), collapse = " + "),
                          sep = " ~ "))


head(model.matrix(mFormula(f_interact), dt_mlogit), n = 15)

#model
m_interact <- mlogit(f_interact, dt_mlogit)

#results
summary(m_interact)

save(m_interact, file = "../../gen/analysis/output/model_results.Rdata")


#### Model 1: alternatives only

# # formula
# f_alt <- as.formula(paste("choice", 
#                           paste(c(alt_specific, -1), collapse = " + "), 
#                           sep = " ~ "))
# 
# #model
# m1 <- mlogit(f_alt, dt_mlogit)
# 
# #results
# summary(m1)


#### Model 2: add individual specific variables --> this model is not identified

#formula
#f_altind <- as.formula(paste("choice",
#                             paste(
#                               paste(alt_specific, collapse = " + "),
#                               paste(c(indiv_specifc, -1), collapse = " + "),
#                               sep = " | "),
#                             sep = " ~ "))


#model
#m2 <- mlogit(f_altind, dt_mlogit)


#results
#summary(m2)
