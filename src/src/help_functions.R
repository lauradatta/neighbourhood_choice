
### standardise data


stand <- function(data, mean, std, variables){
  data_to_stand <- data %>%
    select(variables)
  
  #make matrix
  mean_tr_m <- matrix(rep(mean,each=nrow(data_to_stand)),nrow=nrow(data_to_stand))
  sd_tr_m <- matrix(rep(std,each=nrow(data_to_stand)),nrow=nrow(data_to_stand))
  
  ### standardise simulated data
  data_stand <- (data_to_stand - mean_tr_m) / sd_tr_m
  
  dt_stand <- data %>%
    select(-variables) %>%
    bind_cols(data_stand)
  
  return(dt_stand)
  
}


conc_dt <- function(hh_avg, hh_sim, nh_avg, nh_sim, id_ct){
  
  no_sim_hh <- nrow(hh_sim) #number of simulation at household level
  
  no_sim_nh <- nrow(nh_sim) #number of simulations at neighbourhood level
  
  ## household
  #repeat hh_avg as many as simulations on nh level
  hh_avg_rep <- data.table(do.call("rbind", replicate(no_sim_nh, hh_avg, simplify = FALSE)))
  
  #bind simulated and average hh
  sim_hh <- data.table(bind_rows(hh_sim, hh_avg_rep))
  
  #id hh
  sim_hh[, id := seq(1:.N)]
  
  
  ## neighbourhood
  #repeat nh_avg as many as simulations on hh level
  nh_avg_rep <- data.table(do.call("rbind", replicate(no_sim_hh, nh_avg, simplify = FALSE)))
  
  #bind simulated and average nh but the other way around
  sim_nh <- data.table(bind_rows(nh_avg_rep, nh_sim))
  
  id <- id_ct
  
  #id_nh
  sim_nh[, id := seq(1:.N), by = cluster]
  
  
  ## concatenate
  data_sim <- sim_nh %>%
    left_join(sim_hh, by = "id")
  
  return(data_sim)
}



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


longtable.stargazer = function(..., float = T, longtable.float = F, 
                               longtable.head = T, filename = NULL){
  # Capturing stargazer to hack it
  require(stargazer)
  res = capture.output(
    stargazer(..., float = float)
  )
  # Changing tabulare environment for longtable
  res = gsub("tabular", "longtable", res)
  # removing floating environment
  if(float == T & longtable.float == F){
    res[grep("table", res)[1]] = res[grep("longtable", res)[1]]
    # Removing extra longtable commands
    res = res[-grep("longtable", res)[2]]
    res = res[-length(res)]
  }
  # Adding page headings
  if(longtable.head == T){
    res = c(res[1:which(res == "\\hline \\\\[-1.8ex] ")[1] - 1], "\\endhead", res[which(res == "\\hline \\\\[-1.8ex] ")[1]:length(res)])
  }
  # Exporting
  cat(res, sep = "\n")
  # Exporting
  if(!is.null(filename)){
    cat(res, file = filename, sep = "\n")
    # Message
    cat(paste("\nLaTeX output printed to", filename, "\n", sep = " ", 
              collapse = ""))
  }else{
    cat(res, sep = "\n")
  }
}

star_insert_row <- function(star, string, insert.after) {
  
  if (length(string) < length(insert.after)) {
    stop("Error: in star_insert_row() insert.after has more elements than string")
  }
  
  if (length(insert.after) == 1) {
    ##If the length of insert.after is 1, use the append function
    return(append(star, string, after = insert.after))
  }
  ##c(star[1:insert.after], string, star[(insert.after + 1):length(star)]))
  
  if (length(string) > length(insert.after)) {
    ##string has more elements than Insert after
    ##fill insert.after with its last element so its length
    ##matches string
    length.diff <- length(string) - length(insert.after)
    insert.after <- c(insert.after,
                      rep(insert.after[length(insert.after)], length.diff))
  }
  
  ##From http://stackoverflow.com/a/1495204/1317443
  id <- c(seq_along(star), insert.after + 0.5)
  star <- c(star, string)
  star <- star[order(id)]
  return(star)
}
