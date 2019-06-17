library(tidyselect)
library(tidymodels)
library(leaflet)
library(recipes)
library(htmltools)
library(RColorBrewer)
library(rlang)

chicago_map <- function(dat, target = "html", plot_var = NULL) {
  map_data <- 
    dat %>%
    mutate(value = !!sym(plot_var)) %>% 
    dplyr::filter(!is.na(value))
  p <- nrow(map_data)
  
  map_data <-
    map_data %>% 
    mutate(
      value = !!sym(plot_var),
      radius = abs(value),
      rank = rank(-radius),
      label = paste0(description, " (abs rank=", rank, " of ", p, ")")
    )
  
  map_data <- recipe( ~ ., data = map_data) %>%
    step_range(radius, max = 300) %>%
    prep(training = map_data, retain = TRUE) %>%
    juice()
  val_range <- max(abs(map_data$value), na.rm = TRUE)
  point_cols <- colorNumeric("RdBu", domain = c(-val_range, val_range))
  
  target_station <- 
    dat %>%
    filter(station_id == "s_40380")
  
  if (target == "html") {
    chi_map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels)
  } else {
    chi_map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels)
    # see http://leaflet-extras.github.io/leaflet-providers/preview/
    # Esri.WorldGrayCanvas is minimal
    # CartoDB.Positron is good with streets and some names
  }
  
  chi_map <- 
    chi_map %>%
    addMarkers(
      data = target_station,
      lng = ~ lon,
      lat = ~ lat,
      popup = "Clark and Lake Station"
    ) %>%
    addCircles(
      data = map_data,
      lng = ~ lon,
      lat = ~ lat,
      color = ~ point_cols(value),
      fillColor = ~ point_cols(value),
      fill = TRUE,
      opacity = .01,
      fillOpacity = 1,
      radius = ~ radius,
      popup = htmlEscape(map_data$label)
    ) %>%
    setView(lng = -87.7, lat = 41.91, zoom = 10)
  chi_map
}