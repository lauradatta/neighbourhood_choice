# select covariates to be put into the model ########


#### clustering ########

#variable selection for construction neighbourhood types
var_cluster <- c(#"jaar",
  #"buurtnaam",
  #"buurtcode",
  #"AantalInwoners",
  #"HuishoudensTotaal",
  "GemiddeldeWoningwaarde",
  "Omgevingsadressendichtheid",
  "AfstandTotZiekenhuis",
  "AfstandTotOpritHoofdverkeersweg",
  "Restaurants.3km",
  "AfstandTotBelangrijkOverstapstation",
  #"AfstandTotKinderdagverblijf",
  #"AfstandTotBuitenschoolseOpvang",
  #"prop.dutch",
  "prop.west",
  "prop.nonwest",
  "prop.west",
  "prop.singles",
  #"prop.couples",
  "prop.fam",
  "prop.bijstand",
  #"prop.income.socialmin",
  #"perc.property",
  "perc.rent",
  "perc.after2000"
)

labels_cluster_var <- c(
  #"Cluster",
  #"buurtcode",
  #"Year",
  "Average Dwelling Values (x 1000)", 
  "Housing density", 
  "Distance to Hospital", 
  "Distance to Highway Access Lane", 
  "Restaurants Within 3 km", 
  "Distance to Important Train Station", 
  #"Distance to Day Care",
  #"Distance to After School Day Care", 
  #"Share of Native Dutch",
  "Share of Western Minorities",
  "Share of Non-Western Minorities", 
  "Share of Singles", 
  #"Share of Couples", 
  "Share of Families with Children", 
  "Share of People Living From Social Benefits",
  #"Share of People Living below Social Minimum", 
  #"Share of Property", 
  "Share of Private Rental", 
  "Share of Dwellings Built > 2000"
)

##### characteristics current neighbourhood

covar_nh <- c(
  #"cluster",
  #"buurtcode",
  #"jaar",
  "GemiddeldeWoningwaarde", 
  #"Omgevingsadressendichtheid", 
  #"Distance to Hospital", 
  #"AfstandTotOpritHoofdverkeersweg", 
  #"Restaurants.3km", 
  #"AfstandTotBelangrijkOverstapstation", 
  #"AfstandTotKinderdagverblijf",
  #"AfstandTotBuitenschoolseOpvang", 
  #"prop.dutch",
  "prop.west",
  "prop.nonwest", 
  "prop.singles", 
  "prop.couples" 
  #"prop.fam", 
  #"prop.bijstand",
  #"prop.income.socialmin", 
  #"perc.property", 
  #"perc.rent", 
  #"perc.after2000"
)

labels_covar_nh <- c(
    #"Cluster",
    #"buurtcode",
    #"Year",
    "Average Dwelling Values (x 1000)", 
    #"Omgevingsadressendichtheid", 
    #"AfstandTotZiekenhuis", 
    #"Distance to Highway Access Lane", 
    #"Restaurants Within 3 km", 
    #"Distance to Important Train Station", 
    #"Distance to Day Care",
    #"Distance to After School Day Care", 
    #"Share of Native Dutch",
    "Share of Western Minorities",
    "Share of Non-Western Minorities",
    "Share of Singles", 
    "Share of Couples" 
    #"Share of Families with Children", 
    #"Share of People Living From Social Benefits",
    #"Share of People Living below Social Minimum", 
    #"Share of Property", 
    #"Share of Private Rental", 
    #"Share of Dwellings Built > 2000"
  )

###### household characteristics

covar_indiv <- c(#"move_id",
  #"jaar",
  #"cluster",
  #"buurtnaam",
  #"buurtcode",
  #"vrg_buurtcode",
  #"PRSGES", 
  #"age",
  #"age_cat",
  #"AANBEW",
  "hh_type",
  #"cdhhw", 
  #"cbsetngr",
  #"hsh_cat",
  #"etnikort",
  "ethnicity",
  #"vrgwrd"
  #"vrghr", 
  #"vrgpct", 
  #"vrgopp", 
  #"vrgkam", 
  "vrginh" 
  #"vrghrkp", 
  #"vrgeigd", 
  #"vrgbjr", 
  #"vrgbwwyz", 
  #"vrgwtyp", 
  #"vrgvlopp", 
)

##### chacteristics preivous neighbourhood

covar_nh_vrg <- c(
  #"cluster",
  #"buurtcode",
  #"jaar",
  #"GemiddeldeWoningwaarde_vrg", 
  #"Omgevingsadressendichtheid_vrg", 
  #"AfstandTotZiekenhuis_vrg", 
  #"AfstandTotOpritHoofdverkeersweg_vrg", 
  #"Restaurants.3km", 
  #"AfstandTotBelangrijkOverstapstation_vrg", 
  #"AfstandTotKinderdagverblijf_vrg",
  #"AfstandTotBuitenschoolseOpvang_vrg", 
  #"prop.nonwest_vrg" 
  #"prop.singles_vrg" 
  #"prop.couples_vrg", 
  #"prop.fam_vrg", 
  #"prop.bijstand_vrg"
  #"prop.income_socialmin_vrg", 
  #"perc.property_vrg", 
  #"perc.rent_vrg", 
  #"perc.after2000_vrg"
)


save(covar_nh, labels_covar_nh, covar_indiv, covar_nh_vrg, var_cluster, labels_cluster_var, file = "../../gen/analysis/temp/variables.Rdata")
