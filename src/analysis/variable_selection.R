##### select covariates to be put into clustering and conditional logit model ########

#### Variables used clustering ########
#variable selection for construction neighbourhood types
var_cluster <- c(
  "GemiddeldeWoningwaarde",
  "income.nh",
  "prop.nonwest",
  "prop.west",
  "prop.singles",
  "prop.fam",
  "perc.priv.rent",
  "perc.soc.rent",
  "perc.after2000",
  "Omgevingsadressendichtheid",
  "AfstandTotOpritHoofdverkeersweg",
  "AfstandTotBelangrijkOverstapstation",
  "Restaurants3km",
  "leefbh"
)

### Variabels used for conditional logit model
#characteristics current neighbourhood
#Full model
covar_nh <- c(
  "GemiddeldeWoningwaarde",
  "income.nh",
  "prop.nonwest",
  "prop.west",
  "prop.singles",
  "prop.fam",
  #"perc.priv.rent", #taken out of model because no additional explanatory power
  "perc.soc.rent",
  "perc.after2000",
  #"Omgevingsadressendichtheid", #taken out of model because no additional explanatory power
  "AfstandTotOpritHoofdverkeersweg",
  "AfstandTotBelangrijkOverstapstation",
  #"Restaurants3km", #taken out of model because no additional explanatory power
  "leefbh"
)

# Baseline model
covar_nh_basic <- c(
  "GemiddeldeWoningwaarde",
  "income.nh",
  "prop.west",
  "prop.nonwest",
  "prop.singles",
  "prop.fam"
)


# household characteristics

#Full model
covar_indiv <- c(
  "move_type_gen_within",
  "income_hh",
  "ethnicity_non_western",
  "ethnicity_western",
  "hh_type_single",
  "hh_type_fam",
  "age_cat_25",
  "age_cat_45_64",
  "age_cat_65",
  "room_stress"
)

#Baseline model
covar_indiv_basic <- c(
  "income_hh",
  "hh_type_single",
  "hh_type_fam",
  "ethnicity_non_western",
  "ethnicity_western"
)


#Variables to be standardised
covar_stand <- c(covar_nh,
                 "income_hh",
                 "room_stress")
