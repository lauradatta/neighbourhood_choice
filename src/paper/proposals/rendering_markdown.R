library(knitr)
library(rmarkdown)

#source R script for generating objects
source("src/analysis/map_nh_types.R")

#render markdown

render("src/paper/proposals/nh_types_map_markdown.Rmd", output_dir = "gen/paper/output")
