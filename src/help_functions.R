
#make leaflet graph

make_leaflet <- function(data, label, var, bins){
  #bins <- quantile(log(var), seq(0,1,1/6), na.rm = T)
  pal <- colorBin("YlOrRd", domain = var, bins = bins)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g moves",
    label, var
  ) %>% lapply(htmltools::HTML)
  
  map <- leaflet(data) %>%
    addPolygons(
      fillColor = ~pal(var),
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
    addLegend(pal = pal, values = ~var, opacity = 0.7, title = NULL,
              position = "bottomright")
  
  return(map)
} 
