#########################################
###### makefile analysis ################
#########################################

simulation: ../../gen/analysis/output/simulation.Rdata

mlogit: ../../gen/analysis/output/model_results_stand.Rdata

mlogit_prep: get ../../gen/analysis/temp/mlogit.Rdata

descr: ../../gen/analysis/output/descriptives.Rdata

cluster_nh: 	get ../../gen/analysis/temp/data_cluster.Rdata

#copy files from data-preparation output to analysis input
get: 
	copy ..\..\gen\data-preparation\output\*.* ..\..\gen\analysis\input\

####### Step 1: clustering of neighbourhoods #########
../../gen/analysis/temp/data_cluster.Rdata: clustering_nh.R
	R --vanilla --args "" < "clustering_nh.R"
	
#### Step 2: Descriptive Statistics ######

../../gen/analysis/output/descriptives.Rdata: ../../gen/analysis/temp/data_cluster.Rdata descriptives.R
	R --vanilla --args "" < "descriptives.R"
	
##### Step 3: Estimate mlogit ########

#Step 1: prepare dataset
../../gen/analysis/temp/mlogit.Rdata: ../../gen/analysis/temp/data_cluster.Rdata mlogit_clust_prep.R
	R --vanilla --args "" < "mlogit_clust_prep.R"
	
#Step 2: run the model
#../../gen/analysis/output/model_results.Rdata: ../../gen/analysis/temp/mlogit.Rdata  mlogit_clust_model.R
../../gen/analysis/output/model_results_stand.Rdata: ../../gen/analysis/temp/mlogit.Rdata mlogit_clust_model.R 
	R --vanilla --args "" < "mlogit_clust_model.R"
	
##### Step 3: Simulation ########

../../gen/analysis/output/simulation.Rdata: ../../gen/analysis/temp/mlogit.Rdata ../../gen/analysis/output/model_results_stand.Rdata simulation.R 
	R --vanilla --args "" < "simulation.R"
