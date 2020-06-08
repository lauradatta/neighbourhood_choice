## Simulations ###

###### set up
#set working directory to source file location

#load packages
library(data.table)
library(tidyverse)
library(mlogit)


load("../../gen/analysis/temp/dt_mlogit.Rdata")
# copy following files from "../../gen/data-prepartion/output/ first
load("../../gen/analysis/output/data_cluster.Rdata")
load("../../gen/analysis/output/model_results_stand.Rdata")

#scripts defining variables
source("../../src/analysis/variable_selection.R")
#help functions
source("../help_functions.R")
##################

## take out NAs from moving data (this corresponds to the dataset used in the mlogit model)
moves_db <- moves_db_cl %>%
  filter(!cluster %in% c(10,11)) %>%
  drop_na(covar_indiv) %>%
  data.table

#### prepare standardisation of simulation data ####

#get mean and std from non-standardised data
#mean
mean_train <- dt_alt %>%
  select(covar_stand) %>%
  colMeans

#std
sd_train <- dt_alt %>%
  select(covar_stand) %>%
  data.table

sd_train <- as.matrix(dt_alt[,lapply(.SD, sd), .SDcol = covar_stand])

######## simulations ######


######### 1) Simualtions to average houshold ######

#### Step 1) generate simluated data

#### a) average mover
#average nh characteristics
nh_avg <- nhchar_cl %>%
  filter(jaar == 2018 & !cluster %in% c(10,11)) %>%
  summarise_at(vars(covar_nh), mean) %>%
  data.table

#avg hh characteristics
hh_avg <- moves_db %>%
  summarise_at(vars(covar_indiv), mean)

#set categorical variables to ethnicity: Dutch, hh_type: single, age: 25-44 years
hh_avg <- hh_avg %>%
  mutate(move_type_gen_within = 1, ethnicity_non_western = 0, ethnicity_western = 0, hh_type_single = 1, hh_type_fam = 0, 
         age_cat_25 = 0, age_cat_45_64 = 0, age_cat_65 = 0)

#### b) simulate data

#### households char ########

## Income
# positive income shock
hh_avg_incpos <- hh_avg %>%
  mutate(income_hh = income_hh * 1.05)

#negative income shock
hh_avg_incneg <- hh_avg %>%
  mutate(income_hh = income_hh * 0.95)


##### Ethnicity in combination with income
#make hh non-western
hh_avg_nwest <- hh_avg %>%
  mutate(ethnicity_non_western = 1, ethnicity_western = 0)

#positve income shock to non-wester
hh_avg_nwest_incpos <- hh_avg_incpos %>%
  mutate(ethnicity_non_western = 1, ethnicity_western = 0)

#make hh western
hh_avg_west <- hh_avg %>%
  mutate(ethnicity_non_western = 0, ethnicity_western = 1)

#negative income shock to western
hh_avg_west_incpos <- hh_avg_incpos %>%
  mutate(ethnicity_non_western = 0, ethnicity_western = 1)

###### room_stress
#increased room stress
hh_avg_rs <- hh_avg %>%
  mutate(room_stress = room_stress * 1.25)

#increased stress in combination with more income
hh_avg_rs_inc <- hh_avg_rs %>%
  mutate(income_hh = income_hh * 1.1)

##### concatenate the different new hh data ###
hh_avg_sim <- data.table(bind_rows(hh_avg, hh_avg_incpos, hh_avg_incneg, hh_avg_nwest, hh_avg_nwest_incpos, hh_avg_west, hh_avg_west_incpos, 
                                   hh_avg_rs, hh_avg_rs_inc))

#names for shocks
shock_hh <- c("average", "income_pos", "income_neg", "ethn_nwest",  "ethn_nwest_incpos","ethn_west",  "ethn_west_incpos", 
              "room_stress", "room_stress_incpos")

#### Possible simulations to neighbourhoods (not included in thesis)

### shock decrease average dwelling value ####
nh_avg_val <- nh_avg %>%
  mutate(GemiddeldeWoningwaarde = GemiddeldeWoningwaarde * 0.95)

### shock decreased value with new buildings
nh_avg_val_new <- nh_avg_val %>%
  mutate(perc.after2000 = perc.after2000 * 1.1)

### shock only new buildings####
nh_avg_new <- nh_avg %>%
  mutate(perc.after2000 = perc.after2000 * 1.1)

# shock social housing in cluster 5
nh_avg_sochos <- nh_avg %>%
  mutate(perc.soc.rent = replace(perc.soc.rent, cluster == 5, perc.soc.rent [cluster == 5] * 1.1))
         
# reputation
nh_avg_leefbh <- nh_avg %>%
  mutate(leefbh = leefbh + 1.5)

#new build and social housing
nh_avg_new_sochos <- nh_avg_new %>%
  mutate(perc.soc.rent = perc.soc.rent * 1.1)

#merge simulated household and neighbourhood characteristics and standardise
dt_sim <- conc_dt(hh_avg, hh_avg_sim, nh_avg, nh_avg, "id") # see help_functions

#standardise simulated data
dt_sim_stand <- stand(dt_sim, mean_train, sd_train, covar_stand)

##### Step 2: predict the probabilities on the new data using mlogit predict function
pred <- data.table(predict(m_interact, newdata = dt_sim_stand))

#name different simulations
pred[, shock:= c(shock_hh, NA)] 

pred <- pred[!is.na(shock),] #take out last row which is same as baseline

#long format for making graphs
prob_cl <- pred %>%
  pivot_longer(1:9, names_to = "cluster", values_to = "prob") %>%
  mutate(cluster = factor(cluster, levels = cluster_ord, labels = labels_clust))


#### 2) Simulations on "vulnerable cluster" #########

#vulnerable cluster is cluster 8

#average mover of cluster 8
hh_avg_cl8 <- moves_db %>%
  filter(!is.na(cluster_vrg)) %>%
  #filter(!cluster_vrg %in% c(10,11, NA)) %>%
  group_by(cluster_vrg) %>%
  summarise_at(vars(covar_indiv), mean) %>%
  mutate(cluster_vrg = as.numeric(cluster_vrg)) %>%
  filter(cluster_vrg == 8) %>%
  data.table

#set ethnicity of average household to dutch
hh_avg_cl8<- hh_avg_cl8 %>%
  mutate(ethnicity_non_western = 0, ethnicity_western = 0) %>%
  mutate(hh_type_single = 1, hh_type_fam = 0) %>%
  mutate(age_cat_25 = 0, age_cat_45_64 = 0, age_cat_65 = 0)

#### 2.1) Income shock based on ethnicity ####

#income shock
hh_avg_cl8_incpos <- hh_avg_cl8  %>%
  mutate(income_hh = income_hh * 1.05)

#ethnicity non western
hh_avg_cl8_nwest <- hh_avg_cl8  %>%
  mutate(ethnicity_non_western = 1, ethnicity_western = 0)

#income_shock non western
hh_avg_cl8_nwest_incpos <- hh_avg_cl8_incpos  %>%
  mutate(ethnicity_non_western = 1, ethnicity_western = 0)

#ethnicity western
hh_avg_cl8_west <- hh_avg_cl8  %>%
  mutate(ethnicity_non_western = 0, ethnicity_western = 1)

# income shock to western
hh_avg_cl8_west_incpos <- hh_avg_cl8_incpos %>%
  mutate(ethnicity_non_western = 0, ethnicity_western = 1)

# concatenate hh char
hh_cl8_sim <- data.table(bind_rows(hh_avg_cl8, hh_avg_cl8_incpos,hh_avg_cl8_west, hh_avg_cl8_west_incpos,  hh_avg_cl8_nwest, hh_avg_cl8_nwest_incpos))

####### 2.2) increase social housing to 25% in cluster 5 (new family houses)
nhg_avg_sochos <-  nh_avg %>% 
  mutate(perc.soc.rent = replace(perc.soc.rent, cluster == 5, 0.25)) #%>%
  #mutate(perc.soc.rent = replace(perc.soc.rent, cluster == 8, 0.67))

#create nh sim
nh_sim_cl8 <- data.table(bind_rows(nhg_avg_sochos))

######concatenate
cl8_sim <- conc_dt(hh_avg_cl8, hh_cl8_sim, nh_avg, nh_sim_cl8, "id")

#standardise
cl8_sim_stand<- stand(cl8_sim, mean_train, sd_train, covar_stand)

######## predict
prob_cl8 <- data.table(predict(m_interact, cl8_sim_stand))

#names
prob_cl8[, shock := c("average", "incpos", "ethn_nwest",  "ethn_nwest_incpos","ethn_west", "ethn_west_incpos", "soc_housing_cl5")]

#long format
prob_cl8 <- prob_cl8 %>%
  pivot_longer(1:9, names_to = "cluster", values_to = "prob") %>%
  mutate(cluster = factor(cluster, levels = cluster_ord, labels = labels_clust))

###### 3) increase reputation of vulnerable cluster ###########

#hh average characteristics based on previous cluster
hh_avg_vrg <- moves_db %>%
  #filter(!is.na(cluster_vrg)) %>%
  filter(!cluster_vrg %in% c(10,11, NA)) %>%
  group_by(cluster_vrg) %>%
  summarise_at(vars(covar_indiv), mean) %>%
  mutate(cluster_vrg = as.numeric(cluster_vrg)) %>%
  #mutate(ethnicity_non_western = 0, ethnicity_western = 0)
  data.table

#simulate that we increase neighbourhood reputation to 6.8
nh_avg_sticky <- nh_avg %>%
  mutate(leefbh = replace(leefbh, cluster %in% c(8,9), 6.8))

nh_avg_sticky_sim <- data.table(do.call("rbind", replicate(9, nh_avg, simplify = FALSE)))

#concatenate hh and nh char
dt_sticky <- conc_dt(hh_avg_vrg, hh_avg_vrg, nh_avg, nh_avg_sticky_sim , "cluster_vrg")

#standardise
dt_sticky_stand <- stand(dt_sticky, mean_train, sd_train, covar_stand)


### Step 2: predict using mlogit
prob_sticky <- data.table(predict(m_interact, dt_sticky_stand))

#nameing
prob_sticky[, shock := c(rep("average",9), rep("reputation",9))]

prob_sticky[, cluster_vrg := seq(1:.N), by = shock]

#long format
prob_sticky_long <- prob_sticky %>%
  pivot_longer(1:9, names_to = "cluster", values_to = "prob") %>%
  mutate(cluster = factor(cluster, levels = cluster_ord, labels = labels_clust)) %>%
  mutate(cluster_vrg = factor(cluster_vrg, levels = cluster_ord, labels = labels_clust))

#make sure to copy this file to "../../gen/paper/input/"
save(prob_cl, prob_cl8, prob_sticky_long, file = "../../gen/analysis/output/simulation.Rdata")
