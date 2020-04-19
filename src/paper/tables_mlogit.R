# make tables from mlogit

# Set up
library(stargazer)
library(mlogit)

# Load results
load("../../gen/paper/input/model_results.RData")

# Export to html table (omits f-stat since messes up table)
stargazer(m_interact,out="../../gen/paper/output/res_m_interact.tex",omit.stat=c("f"))
