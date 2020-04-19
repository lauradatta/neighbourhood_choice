#########################################
###### makefile paper ###################
#########################################

t_descr: get ../../gen/paper/output/table_descr_nh.tex ../../gen/paper/output/table_descr_nh_clust.tex
t_models: ../../gen/paper/output/res_m_interact.tex

#copy files from data-pre output to analysis input
get: 
	copy ..\..\gen\analysis\output\*.* ..\..\gen\paper\input\
	
../../gen/paper/output/res_m_interact.tex: tables_mlogit.R
	R --vanilla --args "" < "tables_mlogit.R"
	
../../gen/paper/output/table_descr_nh.tex ../../gen/paper/output/table_descr_nh_clust.tex: tables_descr.R
	R --vanilla --args "" < "tables_descr.R"