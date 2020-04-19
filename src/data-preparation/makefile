#########################################
###### makefile data-preparation ########
#########################################

prep: prep_nhchar prep_moves

####### neighbourhood data ########

prep_nhchar: ../../gen/data-preparation/output/nhchar.Rdata

#Step 1: download CBS data
../../gen/data-preparation/input/cbs_data.Rdata: download_cbs_data.R
	R --vanilla --args "" < "download_cbs_data.R"

#Step 2: clean CBS data
../../gen/data-preparation/output/nhchar.Rdata: ../../gen/data-preparation/input/cbs_data.Rdata clean_cbs_data.R
	R --vanilla --args "" < "clean_cbs_data.R"


######## moving data #########

#fill anonymous data with random values
prep_moves: ../../gen/data-preparation/output/moves_db.RData


#Step 1: fill anonymised data with fake data
../../gen/data-preparation/temp/moves.RData: fill_moves_anonym.R
	R --vanilla --args "" < "fill_moves_anonym.R"
	
#Step 2: clean moving data
../../gen/data-preparation/output/moves_db.RData: fill_moves_anonym.R clean_moves_loc.R 
	R --vanilla --args "" < "clean_moves_loc.R"