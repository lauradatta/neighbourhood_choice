library(geojsonio)
library(sf)
library(viridis)
library(leaflet)

load("../../gen/analysis/temp/data_cluster.Rdata")
#source("src/help_functions.R")

# data preparation for showing clusters on map
#read cbs_buurten 2018 geodata, with geojson package
cbs_buurten_sp <- geojson_read("../../data/cbs_buurten_2018.geojson", what = "sp")
#transform into 'sf' data format
cbs_buurten <- st_as_sf(cbs_buurten_sp)
#filter for only Den Bosch and buurtcodes
cbs_buurten_db <- cbs_buurten %>%
  filter(gemeentenaam == "'s-Hertogenbosch") %>%
  select(c(id, buurtnaam, buurtcode, geometry))

####### MAP ###############
nh_clusters_geo <- nh_clusters %>%
  mutate(buurtcode = paste0("BU0", buurtcode))


#plot results on map
nh_clusters_geo <- cbs_buurten_db %>%
  left_join(nh_clusters_geo, by = "buurtcode")

ggplot(nh_clusters_geo) +
  geom_sf(aes(fill = cluster))


#prepare data for leaflet (reproject)
nh_clusters_leaflet <- st_transform(nh_clusters_geo, 4326)

labels <- sprintf("<strong>%s</strong>", nh_clusters_leaflet$buurtnaam) %>% lapply(htmltools::HTML)
pal <- colorFactor(
  palette = "RdYlBu",
  domain = nh_clusters_leaflet$cluster)

map <- leaflet(nh_clusters_leaflet) %>%
  addPolygons(
    fillColor = ~ pal(nh_clusters_leaflet$cluster),
    weight = 0.2,
    opacity = 1,
    color = "black",
    dashArray = "",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7, 
      bringToFront = T),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend("bottomright", pal = pal, values = ~nh_clusters_leaflet$cluster,
            title = "Neighbourhood clusters",
            opacity = 1)

