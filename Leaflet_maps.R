# load libraries ----------------------------------------------------------

library(readr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(dbscan)
library(dplyr)
library(geosphere)
library(magrittr)
library(webshot)
library(mapview)


# load data - domestic wells urban ---------------------------------------------------------------

urban_domestic_wells <- read_sf("C:/Users/SShroder/OneDrive - Water Boards/R/Infiltration/Communication_Plan/Raw_data/Domestic_wells_urban/domestic_wells_urban.shp")

mapview(urban_domestic_wells)

urban_domestic_wells <- st_transform(urban_domestic_wells, 4326)

class(urban_domestic_wells$dwr_dm_)

binpal <- colorBin("RdYlBu", urban_domestic_wells$dwr_dm_, 9, pretty = FALSE)


urban_wells_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels)  %>% 
  setView(lng = -119.7871, lat = 36.7378, zoom = 7) %>% 
  addPolygons(data = urban_domestic_wells,
              weight = 1,
              stroke = FALSE, 
              smoothFactor = 0.2, 
              fillOpacity = 1,
              color = ~binpal(dwr_dm_)) %>%
addLegend(position = "bottomright",
          values = urban_domestic_wells$dwr_dm_, # data frame column for legend
          opacity = .7, # alpha of the legend
          pal = binpal, # palette declared earlier
          title = "Number of Wells/PLSS Section")

urban_wells_map


# lots of domestic wells (CES) ---------------------------------------------------------------

# ces for urban areas with at least 10 wells/section
CES_many_wells_urban <- read_sf("C:/Users/SShroder/OneDrive - Water Boards/R/Infiltration/Communication_Plan/Raw_data/CES_many_domestic_wells_urba/CES_domestic.shp")
CES_many_wells_urban <- st_transform(CES_many_wells_urban, 4326)


mapview(CES_many_wells_urban)

bin_ces <- colorBin("YlOrRd", CES_many_wells_urban$ciscorep, 9, pretty = FALSE)

# combined risk map urban areas
combined_risk <- read_sf("C:/Users/SShroder/OneDrive - Water Boards/R/Infiltration/Communication_Plan/Raw_data/Combined_risk_score_2024.shp")
combined_risk <- st_transform(combined_risk, 4326)
mapview(combined_risk)

palrisk <- leaflet::colorFactor(palette = c("red", "green", "yellow"), 
                               domain = combined_risk$crbin)





big_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, group = "No Labels")  %>%
  addTiles(group = "OSM (default)") %>%
  setView(lng = -119.7871, lat = 36.7378, zoom = 7) %>%
  addPolygons(data = urban_domestic_wells,
              weight = 1,
              stroke = FALSE, 
              smoothFactor = 0.2, 
              fillOpacity = 1,
              color = ~binpal(dwr_dm_),
              group = "Urban Wells") %>%
  addPolygons(data = CES_many_wells_urban,
              weight = 1, 
              stroke = FALSE, 
              smoothFactor = 0.2, 
              fillOpacity = 1,
              color = ~bin_ces(ciscorep),
              fillColor = ~bin_ces(ciscorep),
              group = "CES Urban Wells") %>%
  addPolygons(data = combined_risk,
              weight = 1, 
              stroke = FALSE, 
              smoothFactor = 0.2, 
              fillOpacity = 1,
              color = ~palrisk(crbin),
              fillColor = ~palrisk(crbin),
              group = "Combined Risk") %>%
  addLegend(position = "bottomright",
            values = urban_domestic_wells$dwr_dm_, # data frame column for legend
            opacity = .7, # alpha of the legend
            pal = binpal, # palette declared earlier
            title = "Number of Wells/PLSS Section") %>%
  addLegend(position = "bottomright",
            values = CES_many_wells_urban$ciscorep,
            opacity = .7,
            pal = bin_ces,
            title = "Cumulative Environmental Impact Score (Percentile)") %>%
  addLegend(position = "bottomleft",
            values = combined_risk$crbin,
            opacity = .7,
            pal = palrisk,
            title = "Combined Risk") %>%
  addLayersControl(
    baseGroups = c("No Labels", "OSM (default)"),
    overlayGroups = c("Urban Wells", "CES Urban Wells", "Combined Risk"),
    options = layersControlOptions(collapsed = FALSE)
  )

big_map
