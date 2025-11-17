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
mta_lines <- st_read("data/MTA_Subway_Service_Lines_20251116.geojson")
mta_stations <- st_read("data/MTA_Subway_Stations_20251116.geojson")
lirr_lines <- st_read("data/MTA_Rail_Branches_20251116.geojson")

### mta stations clean up

queens_mta_stations <- mta_stations |>
  filter(borough == "Q")

bk_mta_stations<- mta_stations |>
  filter(borough == "Bk")

#### ibx station clean up

ibx_stations_2d <- st_zm(ibx_stations)  # drop Z (and M) dimension if present
# convert 0.5 miles to meters
half_mile_meters <- 0.5 * 1609.34  # ≈ 804.67 meters

# create buffer
ibx_stations_buffer <- st_buffer(ibx_stations_2d, dist = half_mile_meters)
bk_mta_stations_buffer <- st_buffer(bk_mta_stations, dist = half_mile_meters)
queens_mta_stations_buffer <- st_buffer(queens_mta_stations, dist = half_mile_meters)

# check result
plot(st_geometry(ibx_stations_buffer), col = 'lightblue')
plot(st_geometry(ibx_stations_2d), add = TRUE, pch = 16, col = 'red')


mapview(ibx_stations_buffer)
mapview(mta_lines)
mapview(mta_stations)
mapview(bk_mta_stations_buffer)
mapview(bk_mta_stations)

B01001_total <- get_acs(
  geography = "tract",
  state = "NY",
  county = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
  variables = "B01001_001E",  # total population
  survey = "acs5",
  year = 2023,
  output = "wide",
  geometry = TRUE
)

### data transformations
  
B01001_total <- st_transform(B01001_total, st_crs(ibx_stations_buffer))

tracts_within_buffer <- st_intersection(
  ibx_stations_buffer %>% mutate(buffer_id = row_number()),
  B01001_total
)

tracts_within_buffer <- tracts_within_buffer %>%
  mutate(
    intersect_area = st_area(geometry),
    tract_area = st_area(st_make_valid(B01001_total)[match(GEOID, B01001_total$GEOID), ]),
    pop_weighted = (B01001_001E * as.numeric(intersect_area / tract_area))
  )

buffer_pop <- tracts_within_buffer %>%
  group_by(Name, Connecting) %>%
  summarize(
    total_pop = sum(pop_weighted, na.rm = TRUE),
    geometry = st_union(geometry),
    .groups ="drop"
  )

buffer_pop <- st_transform(buffer_pop, 4326)


ggplot() +
  geom_sf(data = buffer_pop, aes(fill = total_pop), color = "grey40", alpha = 0.8) +
  geom_sf_text(data = buffer_pop, aes(label = Name), size = 3, color = "black") +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  theme_minimal() +
  labs(
    title = "Population within 0.5 Mile of IBX Stations",
    fill = "Estimated Population"
  )


# make sure CRS is WGS84 for Leaflet
buffer_pop <- st_transform(buffer_pop, 4326)
ibx_stations_2d <- st_transform(ibx_stations_2d, 4326)

# create a color palette based on total_pop
pal <- colorNumeric(palette = "viridis", domain = buffer_pop$total_pop)

leaflet(buffer_pop) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  # grey map
  addPolygons(
    fillColor = ~pal(total_pop),
    color = "black",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    label = ~paste0(
      Name, "<br>",
      "Connecting:", Connecting, "<br>",
      "Population within 0.5 mile: ", round(total_pop)
    ),
    labelOptions = labelOptions(direction = "auto")
  ) %>%
  addCircleMarkers(
    data = ibx_stations_2d,
    radius = 5,
    color = "red",
    stroke = TRUE,
    fillOpacity = 0.9,
    label = ~Name
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~total_pop,
    title = "Population",
    opacity = 0.8
  )


### county comparison

# make sure same CRS as stations (projected!)
acs_proj <- st_transform(B01001_total, st_crs(ibx_stations_buffer))

# Extract county names from NAME field
acs_counties <- acs_proj  %>%
  filter(str_detect(NAME, "Kings County|Queens County")) %>%
  mutate(county = case_when(
    str_detect(NAME, "Kings County") ~ "Kings",
    str_detect(NAME, "Queens County") ~ "Queens"
  ))

# Dissolve by county
county_summary <- acs_counties %>%
  group_by(county) %>%
  summarise(
    total_pop = sum(B01001_001E, na.rm = TRUE),
    geometry = st_union(geometry),
    .groups = "drop"
  ) %>%
  mutate(
    area_m2 = st_area(geometry),
    area_mi2 = set_units(area_m2, mi^2),
    density_pop_per_mi2 = total_pop / area_mi2
  )
county_summary

buffer_pop <- buffer_pop %>%
  mutate(
    buffer_area_m2 = st_area(geometry),
    buffer_area_mi2 = set_units(buffer_area_m2, mi^2),
    pop_density_mi2 = total_pop / buffer_area_mi2
  )

comparison <- county_summary %>%
  st_drop_geometry() %>%
  mutate(
    area_mi2 = as.numeric(area_mi2),
    density_pop_per_mi2 = as.numeric(density_pop_per_mi2)
  ) %>%
  bind_rows(
    buffer_pop %>%
      st_drop_geometry() %>%
      transmute(
        county = paste0("IBX - ", Name),
        total_pop = total_pop,
        area_mi2 = as.numeric(buffer_area_mi2),
        density_pop_per_mi2 = as.numeric(pop_density_mi2)
      )
  )
comparison

### updated leaflet
# Convert county_summary to leaflet CRS (WGS84)
county_leaflet <- county_summary %>%
  st_transform(4326) %>%
  mutate(
    density_pop_per_mi2 = as.numeric(density_pop_per_mi2)
  )
pal_county <- colorNumeric(
  palette = "inferno",
  domain = county_leaflet$density_pop_per_mi2
)
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  
  
  # --- County polygons ---
  addPolygons(
    data = county_leaflet,
    color = "black",
    weight = 1.5,
    popup = lapply(1:nrow(buffer_pop), function(i) {
      HTML(paste0(
        "<div style='font-size:14px; line-height:1.4;'>",
        "<strong style='font-size:16px;'>", county_leaflet$county[i], "</strong><br>",
        "<em style='color:#555;'>Density 0.5:</em> ", county_leaflet$density_pop_per_mi2[i], "<br><br>",
        "</span>",
        "</div>"
      ))
    }))%>%
  
  # --- IBX buffer polygons ---
  addPolygons(
    data = buffer_pop,
    fillColor = ~pal(total_pop),
    color = "black",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    popup = lapply(1:nrow(buffer_pop), function(i) {
      HTML(paste0(
        "<div style='font-size:14px; line-height:1.4;'>",
        "<strong style='font-size:16px;'>", buffer_pop$Name[i], "</strong><br>",
        "<em style='color:#555;'>Connecting:</em> ", buffer_pop$Connecting[i], "<br><br>",
        
        "<strong>Population within 0.5 miles:</strong><br>",
        "<span style='font-size:18px; color:#4A1486;'>", 
        format(round(buffer_pop$total_pop[i]), big.mark=","),
        "</span>",
        "</div>"
      ))
    })
  ) %>%
  
  # --- Station point markers ---
  addCircleMarkers(
    data = ibx_stations_2d,
    radius = 5,
    color = "red",
    stroke = TRUE,
    fillOpacity = 0.9,
    label = ~Name
  ) %>%
  
  # --- Legend for IBX buffers ---
  addLegend(
    "bottomright",
    pal = pal,
    values = buffer_pop$total_pop,
    title = "Population (0.5 mi buffer)",
    opacity = 0.8
  ) 
