# make tables for conditional logit model 

# this is mainly to render latex code of tables

##### Set up

#set working directory to source file location

#load pacakages
library(tidyverse)
library(data.table)
library(stargazer)
library(mlogit)
library(xtable)

#Load results from conditional logit model
# copy following files from "../../gen/analysis/output/" first
load("../../gen/paper/input/model_results_stand.RData")

#source additional scripts needed
source("../../src/analysis/variable_selection.R")
source("../../src/paper/labels_covariates.R")
source("../help_functions.R")
###########

### Change coefficient labels

# baseline model
names_coef_bas <- data.table(str_split(names(m_interact_basic$coefficients) , ":", simplify = T)) %>%
  mutate(V3 = ifelse(V1 %in% c("income.nh", "prop.nonwest", "prop.singles"), V1, V2))%>%
  mutate(V4 = ifelse(V1 %in% c("income.nh", "prop.nonwest", "prop.singles"), V2, V1))%>%
  unite("V5", V3, V4, sep = ":") %>%
  pull(V5)

names(m_interact_basic$coefficients) <- names_coef_bas

#summary of results
summary(m_interact_basic)

### Full model
names_coef <- data.table(str_split(names(m_interact$coefficients) , ":", simplify = T)) %>%
  mutate(V3 = ifelse(V1 == covar_nh[1], V1, V2))%>%
  mutate(V4 = ifelse(V1 == covar_nh[1], V2, V1))%>%
  unite("V5", V3, V4, sep = ":") %>%
  pull(V5)

names(m_interact$coefficients) <- names_coef

#summary of results
summary(m_interact)

##### Ratio tests

#Baseline model
1 - m_interact_basic$logLik / m_null$logLik

#Full model
1 - m_interact$logLik / m_null$logLik


### Long table

# Export to html table 
star_mlogit  <- stargazer(m_interact_basic, m_interact, omit.stat=c("f"), title = "Neighbourhood choice model based on interactions
                    between neighbourhood and household characteristics", dep.var.labels = "Neighbourhood choice", notes.append = T, align = T, no.space = T,
                    single.row=F, covariate.labels = labels_mlogit, model.names = F, model.numbers = F, column.labels=c("Basic model", 'Full model'),
                    label = "tab:res", out="../../gen/paper/output/res_m_interact.html")

#change Latex code
no_clust <- 9
no_hh_var <- length(labels_mlogit) / length(labels_nh_int)
j = 17

start <- c(star_mlogit[1:(j-1)])

string <- c()


for (i in 1:length(labels_nh_int)){
  substr <- c(paste0("\\textit{",labels_nh_int[i],"} $ $ \\\\"), "& & \\\\" ,paste('\\hspace{3mm}',star_mlogit[(j):(j+no_hh_var*2-1)]))
  string <- c(string, substr)
  j = j + no_hh_var*2
}

start_end <- length(start) + length(string) - length(labels_nh_int)*2 + 1

end <- star_mlogit[start_end :length(star_mlogit)]

t_mlogit <- cat(c(start, string, end), sep = "\n")


#### matrix table

coeftable <- data.table(summary(m_interact)$CoefTable)
coeftable[, varname:=rownames(summary(m_interact)$CoefTable)]

for (v in covar_nh) coeftable[grepl(v, varname), nh:= v]
for (v in covar_indiv) coeftable[grepl(v, varname), indiv:= v]

roundf <- function(x) format(round(x, 3), nsmall = 3)
signif.num <- function(x) {
  symnum(x, corr = FALSE, na = FALSE, legend = FALSE,
         cutpoints = c(0, 0.01, 0.05, 0.1, 1), 
         symbols = c("***", "**", "*", " "))
}

coeftable[, text := paste0(roundf(Estimate), ' ', signif.num(`Pr(>|z|)`))]


tmp = dcast(coeftable, indiv~nh, value.var='text')
setcolorder(tmp, c('indiv', covar_nh))
tmp = tmp[match(covar_indiv, indiv),]

tmp$indiv <- labels_indiv

colnames(tmp) <- c("Household characteristics", labels_nh_short)

table <- xtable(tmp)

print(table,
      include.rownames = F,
      sanitize.text.function = function(x){x})
