# Estimate conditional logit model using standardised data



###### set up
#set working directory to source file location

#load packages
library(data.table)
library(tidyverse)
library(mlogit)

#load data prepared for mlogit
load("../../gen/analysis/temp/dt_mlogit.Rdata")

#script with variables to be included in conditional logit model
source("../../src/analysis/variable_selection.R")

#use standardised data
dt_mlogit <- dt_mlogit_stand
#####################

###### build models ########

###### Model 1: Baseline model

m_interact_basic <- mlogit(choice ~ income.nh:income_hh + prop.nonwest:ethnicity_non_western + prop.nonwest:ethnicity_western + prop.west:ethnicity_non_western + 
                            prop.west:ethnicity_western + prop.singles:hh_type_single + prop.singles:hh_type_fam + prop.fam:hh_type_single + 
                            prop.fam:hh_type_fam - 1, dt_mlogit)

######## Model 2: Full model

#define alternative and individual specific variables
alt_specific <- covar_nh
indiv_specifc <- covar_indiv

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
                               paste(c(int, -1), collapse = " + "),
                               sep = " ~ "))


#model
m_interact <- mlogit(f_interact, dt_mlogit)

#summary(m_interact)

###### NULL model to calculate pseudo R2
m_null <- mlogit(choice ~ income.nh:income_hh - 1, dt_mlogit)


#make sure to copy this file to "../../gen/paper/input/"
save(m_interact_basic, m_interact, m_null, file = "../../gen/analysis/output/model_results_stand.Rdata")
