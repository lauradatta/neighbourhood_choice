#########################################
###### makefile data-preparation ########
#########################################

prep: prep_nhchar prep_moves


##### Step 1: load all necessary data #########

#load moving data (verhuis data) into R from SPSS
../../gen/data-preparation/input/verhuis1418.Rdata: load_verhuis-data.R
	R --vanilla --args "" < "load_verhuis-data.R"

#load postcode data
../../gen/data-preparation/input/pc6_gwb.Rdata: load_pcd-data.R
	R --vanilla --args "" < "load_pcd-data.R"
	
#load leefbarheid data
../../gen/data-preparation/input/leefbh.Rdata: load_leefbarheid.R
	R --vanilla --args "" < "load_leefbarheid.R"
	
#load income data
../../gen/data-preparation/input/income.Rdata: load_income_data.R
	R --vanilla --args "" < "load_income_data.R"


####### Step 2a: load and clean neighbourhood data ########

#make sure to copy this file "../../gen/analysis/input"  for next stage
prep_nhchar: ../../gen/data-preparation/output/nhchar.Rdata

#Step 1: download CBS data
../../gen/data-preparation/input/cbs_data.Rdata: download_cbs_data.R
	R --vanilla --args "" < "download_cbs_data.R"

#Step 2: clean CBS data
../../gen/data-preparation/output/nhchar.Rdata: clean_cbs_data.R
	R --vanilla --args "" < "clean_cbs_data.R"


######## Step 2b: load and clean moving data #########

#make sure to copy this file "../../gen/analysis/input"  for next stage
prep_moves: ../../gen/data-preparation/output/moves_db.RData

#Step 1: clean moving data
../../gen/data-preparation/output/moves_db.RData: ../../gen/data-preparation/input/verhuis1418.Rdata ../../gen/data-preparation/input/pc6_gwb.Rdata  ../../gen/data-preparation/input/leefbh.Rdata  ../../gen/data-preparation/input/income.Rdata clean_moves.R 
	R --vanilla --args "" < "clean_moves.R"