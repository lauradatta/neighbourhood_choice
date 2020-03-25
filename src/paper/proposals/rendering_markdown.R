library(knitr)
library(rmarkdown)

#source R script for generating objects
source("code/data-preparation/EDA_vhb.R")

#render markdown
render("code/output/proposal_1.Rmd", output_dir = "output/proposal/", output_format = "word_document")

render("code/output/proposal_1.Rmd", output_dir = "output/proposal/")
