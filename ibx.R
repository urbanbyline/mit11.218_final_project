### Libraries
library(tidyverse)
library(tidycensus)
library(ggplot2)
library(leaflet)
library(mapview)
library(sf)
library(scales)
library(units)
library(htmltools)

### Read in all the data files needed

ibx_stations<- st_read("data/IBX Shape File/IBX_Stations.shp")
###mta_lines <- st_read("data/MTA_Subway_Service_Lines_20251116.geojson")
###lirr_lines <- st_read("data/MTA_Rail_Branches_20251116.geojson")

complete_stations <-st_read("data/all_stations_final.geojson")

mta_stations <- st_read("data/MTA_Subway_Stations_and_Complexes_20251201.csv")

### Clean and Merge Datasets

ibx_stations <- ibx_stations %>%
  mutate(
    SubwayStPr = replace_na(SubwayStPr, "False")
  )

ibx_clean <- ibx_stations %>%
  mutate(
    stop_name_clean = str_to_upper(str_trim(Name)),
    IBX = "Yes",
    Existing = ifelse(SubwayStPr == "True", "Yes", "No"),
    route_names = Connecting
  ) %>%
  select(stop_name_clean, IBX, Existing, route_names)

ibx_clean <- ibx_clean %>%
  mutate(route_names = ifelse(
    is.na(route_names),
    "IBX",
    paste0(route_names, ",IBX")
  ))

mta_clean <- mta_stations %>%
  mutate(
    stop_name_clean = str_to_upper(str_trim(Stop.Name)),
    borough = recode(Borough,
                     "M" = "Manhattan",
                     "B" = "Brooklyn",
                     "Bk" = "Brooklyn",
                     "Q" = "Queens",
                     "S" = "Staten Island",
                     "SI" = "Staten Island",
                     "Bx" = "Bronx")
  ) %>%
  select(stop_name_clean, borough, route_names = Daytime.Routes, Latitude, Longitude)

mta_clean <- mta_clean %>%
  mutate(route_names = gsub(" ", ",", route_names))

mta_clean <- mta_clean %>%
  mutate(
    IBX = "No",         # every row gets "No"
    Existing= "Yes"  # every row gets "Existing"
  ) %>%
  filter(borough %in% c("Queens", "Brooklyn"))


ibx_clean <- ibx_clean %>%
  mutate(
    borough = case_when(
      stop_name_clean %in% c(
        "4TH AVE", "8TH AVE", "NEW UTRECHT AVE", "MCDONALD AVE",
        "E 16TH ST", "FLATBUSH AVE", "UTICA AVE", "REMSEN AVE",
        "LINDEN BLVD", "LIVONIA AVE", "SUTTER AVE", "ATLANTIC AVE",
        "WILSON AVE", "BROOKLYN ARMY TERMINAL"
      ) ~ "Brooklyn",
      stop_name_clean %in% c(
        "MYRTLE AVE", "METROPOLITAN AVE", "ELIOT AVE",
        "GRAND AVE", "ROOSEVELT AVE"
      ) ~ "Queens",
      TRUE ~ NA_character_
    )
  )

ibx_clean <- ibx_clean %>%
  st_transform(4326)

ibx_clean <- ibx_clean %>%
  st_zm(drop = TRUE, what = "ZM")

mta_clean_sf <- mta_clean %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  ) %>%
  st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326,         # WGS84
    remove = FALSE      # keep original lat/long columns
  )

mta_clean_sf <- mta_clean_sf %>%
  select(-Latitude, - Longitude)

all_stations <- bind_rows(mta_clean_sf, ibx_clean)


add_route_count <- function(df) {
  df %>%
    mutate(
      # Normalize separators
      route_clean = str_replace_all(route_names, ",", " "),
      route_clean = str_squish(route_clean),
      
      # Split into vector
      route_list = str_split(route_clean, " "),
      
      # Count lines
      line_count = sapply(route_list, function(x) {
        if (is.null(x) || all(is.na(x))) return(0)
        length(unique(x[x != ""]))
      })
    ) %>%
    select(-route_clean, -route_list)
}

all_stations <- all_stations |> add_route_count()

mapview(all_stations)

st_write(all_stations, "data/all_stations_final.geojson")

st_read("data/all")

###### create buffer
half_mile_meters <- 0.5 * 1609.34  # ≈ 804.67 meters
all_stations_buffer <- st_buffer(complete_stations, dist = half_mile_meters)

st_write(all_stations_buffer, "data/all_stations_buffers_final.geojson")

mapview(all_stations_buffer)


# check result
plot(st_geometry(all_stations_buffer), col = 'lightblue')
plot(st_geometry(complete_stations), add = TRUE, pch = 16, col = 'red')


B01001_total <- get_acs(
  geography = "tract",
  state = "NY",
  county = c( "Kings", "Queens"),
  variables = "B01001_001E",  # total population
  survey = "acs5",
  year = 2023,
  output = "wide",
  geometry = TRUE
)

### data transformations
  
B01001_total <- st_transform(B01001_total, st_crs(all_stations_buffer))

mapview(B01001_total)
mapview(all_stations_buffer)

# 1. Pre-identify only the intersecting tract IDs using the spatial index
idx <- st_intersects(all_stations_buffer, B01001_total)

# 2. For each buffer, run st_intersection only on the tracts that matter
tracts_within_buffer <- map_dfr(
  seq_along(idx),
  ~ {
    st_intersection(
      all_stations_buffer[.x, ] %>% mutate(buffer_id = .x),
      B01001_total[idx[[.x]], ]
    )
  }
)


###tracts_within_buffer <- st_intersection(
### all_stations_buffer %>% mutate(buffer_id = row_number()),
### B01001_total
###)

tracts_within_buffer <- tracts_within_buffer %>%
  mutate(
    intersect_area = st_area(geometry),
    tract_area = st_area(st_make_valid(B01001_total)[match(GEOID, B01001_total$GEOID), ]),
    pop_weighted = (B01001_001E * as.numeric(intersect_area / tract_area))
  )

buffer_pop <- tracts_within_buffer %>%
  group_by(stop_name_clean, route_names) %>%
  summarize(
    total_pop   = sum(pop_weighted, na.rm = TRUE),
    route_names = first(route_names),
    IBX         = first(IBX),
    Existing    = first(Existing),
    line_count  = first(line_count),
    borough     = first(borough),
    geometry    = st_union(geometry),
    .groups = "drop"
  )

buffer_pop <- st_transform(buffer_pop, 4326)



tracts_elmhurst <- tracts_within_buffer %>%
  filter(stop_name_clean == "90 ST-ELMHURST AV")

leaflet(tracts_elmhurst) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "blue",
    weight = 1,
    fillOpacity = 0.4,
    popup = ~paste0(
      "<b>Tract:</b> ", GEOID, "<br>",
      "<b>Population (weighted):</b> ", round(pop_weighted, 1)
    )
  )
#####


ggplot() +
  geom_sf(data = buffer_pop, aes(fill = total_pop), color = "grey40", alpha = 0.8) +
  geom_sf_text(data = buffer_pop, aes(label = stop_name_clean), size = 3, color = "black") +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  theme_minimal() +
  labs(
    title = "Population within 0.5 Mile of Queens/Brooklyn Stations",
    fill = "Estimated Population"
  )

buffer_pop_no_geom <- buffer_pop %>% 
  st_drop_geometry()

ggplot(buffer_pop_no_geom, aes(x = factor(line_count), y = total_pop)) +
  geom_boxplot() +
  labs(
    title = "Population Served by Stations, by Number of Lines",
    x = "Number of Subway Lines at Station",
    y = "Total Population in Buffer"
  ) +
  theme_minimal()

buffer_pop_no_geom %>%
  group_by(line_count) %>%
  summarize(avg_pop = mean(total_pop)) %>%
  ggplot(aes(x=factor(line_count), y=avg_pop)) +
  geom_col() +
  labs(
    title="Average Population by Line Count",
    x="Line Count",
    y="Average Population"
  ) +
  theme_minimal()

buffer_pop_no_geom %>%
  group_by(borough) %>%
  summarize(avg_pop = mean(total_pop)) %>%
  ggplot(aes(x = reorder(borough, avg_pop), y = avg_pop)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Population Served per Station by Borough",
    x = "Borough",
    y = "Average Population"
  ) +
  theme_minimal()

ggplot(buffer_pop_no_geom, aes(x = borough, y = total_pop)) +
  geom_boxplot() +
  labs(
    title = "Population Distribution by Borough",
    x = "Borough",
    y = "Total Population"
  ) +
  theme_minimal()

st_write(buffer_pop, "all_stations_pop.geojson")
# make sure CRS is WGS84 for Leaflet
buffer_pop <- st_transform(buffer_pop, 4326)
complete_stations <- st_transform(complete_stations, 4326)

# create a color palette based on total_pop
library(paletteer)



pal <- colorNumeric(
  palette = RColorBrewer::brewer.pal(9, "YlOrRd"),
  domain = buffer_pop$total_pop
)
leaflet(buffer_pop) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  
  setView(lng = -73.944, lat = 40.6782, zoom = 12)  %>%# grey map
  addPolygons(
    fillColor = ~pal(total_pop),
    color = "black",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    label = ~paste0(
      stop_name_clean, "<br>",
      "Population within 0.5 mile: ", round(total_pop)
    ),
    labelOptions = labelOptions(direction = "auto")
  ) %>%
  addCircles(
    data = complete_stations,
    radius = 1,
    color = "red",
    stroke = TRUE,
    fillOpacity = 0.9,
    label = ~stop_name_clean
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~total_pop,
    title = "Population"
  )


## layers

# --- Create hover label once ---
buffer_pop <- buffer_pop %>%
  mutate(hover_label = paste0(stop_name_clean, "<br>Population: ", round(total_pop)))

# --- Subset layers ---
buffer_ibx_yes  <- buffer_pop %>% filter(IBX == "Yes")
buffer_ibx_no   <- buffer_pop %>% filter(IBX == "No")

buffer_1line    <- buffer_pop %>% filter(line_count == 1)
buffer_2lines   <- buffer_pop %>% filter(line_count == 2)
buffer_3lines   <- buffer_pop %>% filter(line_count == 3)
buffer_4plus    <- buffer_pop %>% filter(line_count >= 4)

buffer_bk       <- buffer_pop %>% filter(borough == "Brooklyn")
buffer_qns      <- buffer_pop %>% filter(borough == "Queens")

# --- Color palette ---
pal <- colorNumeric("YlOrRd", buffer_pop$total_pop)

# --- Leaflet map ---
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -73.94, lat = 40.70, zoom = 11) %>%
  
  ### --- IBX Layers ---
  addPolygons(
    data = buffer_ibx_yes,
    fillColor = ~pal(total_pop),
    color = "black", weight = 1, fillOpacity = 0.7,
    group = "IBX – Yes",
    label = ~hover_label,
    labelOptions = labelOptions(direction = "auto")
  ) %>%
  addPolygons(
    data = buffer_ibx_no,
    fillColor = ~pal(total_pop),
    color = "black", weight = 1, fillOpacity = 0.7,
    group = "IBX – No",
    label = ~hover_label,
    labelOptions = labelOptions(direction = "auto")
  ) %>%
  
  ### --- Line Count Layers ---
  addPolygons(
    data = buffer_1line,
    fillColor = ~pal(total_pop),
    color = "black", weight = 1, fillOpacity = 0.7,
    group = "1 Line",
    label = ~hover_label,
    labelOptions = labelOptions(direction = "auto")
  ) %>%
  addPolygons(
    data = buffer_2lines,
    fillColor = ~pal(total_pop),
    color = "black", weight = 1, fillOpacity = 0.7,
    group = "2 Lines",
    label = ~hover_label,
    labelOptions = labelOptions(direction = "auto")
  ) %>%
  addPolygons(
    data = buffer_3lines,
    fillColor = ~pal(total_pop),
    color = "black", weight = 1, fillOpacity = 0.7,
    group = "3 Lines",
    label = ~hover_label,
    labelOptions = labelOptions(direction = "auto")
  ) %>%
  addPolygons(
    data = buffer_4plus,
    fillColor = ~pal(total_pop),
    color = "black", weight = 1, fillOpacity = 0.7,
    group = "4+ Lines",
    label = ~hover_label,
    labelOptions = labelOptions(direction = "auto")
  ) %>%
  
  ### --- Borough Layers ---
  addPolygons(
    data = buffer_bk,
    fillColor = ~pal(total_pop),
    color = "black", weight = 1, fillOpacity = 0.7,
    group = "Brooklyn",
    label = ~hover_label,
    labelOptions = labelOptions(direction = "auto")
  ) %>%
  addPolygons(
    data = buffer_qns,
    fillColor = ~pal(total_pop),
    color = "black", weight = 1, fillOpacity = 0.7,
    group = "Queens",
    label = ~hover_label,
    labelOptions = labelOptions(direction = "auto")
  ) %>%
  
  ### --- Legend ---
  addLegend(
    "bottomright",
    pal = pal,
    values = buffer_pop$total_pop,
    title = "Population"
  ) %>%
  
  ### --- Layers Control ---
  addLayersControl(
    overlayGroups = c(
      "IBX – Yes", "IBX – No",
      "1 Line", "2 Lines", "3 Lines", "4+ Lines",
      "Brooklyn", "Queens"
    ),
    options = layersControlOptions(collapsed = FALSE)
  )

#### let's do the interactive app next

#### play around with open space

nyc_open_space <- st_read("data/NYC_Planimetric_Database__Open_Space_(Parks)_20251202.csv")

nyc_open_space_sf <- nyc_open_space %>%
  st_as_sf(wkt = "the_geom", crs = 4326)

nyc_open_space_sf <- nyc_open_space_sf %>%
  mutate(
    SHAPE_Leng = as.numeric(gsub(",", "", SHAPE_Leng)),
    SHAPE_Area = as.numeric(gsub(",", "", SHAPE_Area))
  )
nyc_open_space_sf <- nyc_open_space_sf %>%
  st_make_valid()

large_parks <- nyc_open_space_sf %>%
  st_transform(st_crs(buffer_pop)) %>%   # match CRS
  mutate(area_m2 = st_area(the_geom)) %>%
  filter(area_m2 > units::set_units(500000, m^2))  # threshold for large parks

