#Map with clusters

# 1 with ggplot, 1 with leaflet (dynamic)

##### Set up

#set working directory to source file location

#load pacakages
library(geojsonio)
library(sf)
library(viridis)
library(leaflet)
library(mapview)
library(tidyverse)
library(ggthemes)
#library(tikzDevice)

# copy following files from "../../gen/analysis/output/" first
load("../../gen/paper/input/data_cluster.Rdata")

#nice labels for covariates
source("../../src/paper/labels_covariates.R")


#### preparation
#add names to clusters
nh_clusters$cluster <- factor(nh_clusters$cluster, levels = cluster_ord, labels = labels_clust)

#read cbs_buurten 2018 geodata, with geojson package
cbs_buurten_sp <- geojson_read("../../data/cbs_buurten_2018.geojson", what = "sp")

#transform into 'sf' data format
cbs_buurten <- st_as_sf(cbs_buurten_sp)

#filter for only Den Bosch and buurtcodes
cbs_buurten_db <- cbs_buurten %>%
  filter(gemeentenaam == "'s-Hertogenbosch") %>%
  select(c(id, buurtnaam, buurtcode, geometry))

# adjust buurtcode format by adding "BU0"
nh_clusters_geo <- nh_clusters %>%
  mutate(buurtcode = paste0("BU0", buurtcode))


#join clusters to geo data
nh_clusters_geo <- cbs_buurten_db %>%
  left_join(nh_clusters_geo, by = "buurtcode")


### ggplot
#tikz(file = "../../gen/paper/output/map.tex", width =10, height = 5)

g_map <- ggplot(nh_clusters_geo) +
  geom_sf(aes(fill = cluster)) + 
  scale_fill_hc(name ="Neighbourhood clusters", palette = "darkunica")+
  theme_hc() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

g_map

#dev.off()

### Leaflet
#prepare data for leaflet (reproject)
nh_clusters_leaflet <- st_transform(nh_clusters_geo, 4326)

labels <- sprintf("<strong>%s</strong>", nh_clusters_leaflet$buurtnaam) %>% lapply(htmltools::HTML)
pal <- colorFactor(
  palette = "Set3",
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

mapshot(map, url = "../../gen/paper/output/map_clusters.html")

#mapshot(map, url = "../../gen/paper/output/map_clusters.png", remove_controls = c("zoomControl", "layersControl", "homeButton",
#                                                                                  "scaleBar"))

