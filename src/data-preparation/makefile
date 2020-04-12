# TEMPLATE FOR A "RULE"

#[which files are made by this rule?]: [which files do I need to make them - can be more than one]
#	welchen command muss ich ausfuehren um es zu runnen

### neighbourhood data ###
../../gen/data-preparation/temp/cbs_data.Rdata



### moving data ####
#fill anonymous data with random values
all: ../../gen/data-preparation/temp/moves_db_anonym_filled.RData
	
../../gen/data-preparation/temp/moves_db_anonym_filled.RData: fill_anonym.R
	R --vanilla --args "" < "fill_anonym.R"