#########################################
###### makefile analysis ################
#########################################


models: mlogit

#copy files from data-pre output to analysis input
get: 
	copy ..\..\gen\data-preparation\output\*.* ..\..\gen\analysis\input\

mlogit: cluster ../../gen/analysis/output/model_results.Rdata

mlogit_prep: ../../gen/analysis/temp/mlogit.Rdata

descr: cluster ../../gen/analysis/output/descriptives.Rdata

cluster: 	var ../../gen/analysis/temp/data_cluster.Rdata

var: ../../gen/analysis/temp/variables.Rdata

##### Step 1: select variables used in the model #####

../../gen/analysis/temp/variables.Rdata: variable_selection.R
	R --vanilla --args "" < "variable_selection.R"
	
####### Step 2: clustering of neighbourhoods #########
../../gen/analysis/temp/data_cluster.Rdata: ../../gen/analysis/temp/variables.Rdata clustering_nh.R
	R --vanilla --args "" < "clustering_nh.R"
	
	
#### Step 3: Descriptive Statistics ######

../../gen/analysis/output/descriptives.Rdata: ../../gen/analysis/temp/data_cluster.Rdata descriptives.R
	R --vanilla --args "" < "descriptives.R"
	
##### Step 4: Estimate mlogit ########

#Step 1: prepare dataset
../../gen/analysis/temp/mlogit.Rdata: ../../gen/analysis/temp/data_cluster.Rdata ../../gen/analysis/temp/variables.Rdata mlogit_clust_prep.R
	R --vanilla --args "" < "mlogit_clust_prep.R"
	
#Step 2: run the models
../../gen/analysis/output/model_results.Rdata: ../../gen/analysis/temp/mlogit.Rdata mlogit_clust_model.R
	R --vanilla --args "" < "mlogit_clust_model.R"

