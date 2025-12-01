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
mta_stations <- st_read("data/MTA_Subway_Stations_20251116.geojson")
###lirr_lines <- st_read("data/MTA_Rail_Branches_20251116.geojson")

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
  select(stop_name = Name, stop_name_clean, IBX, Existing, route_names)

ibx_clean <- ibx_clean %>%
  mutate(route_names = ifelse(
    is.na(route_names),
    "IBX",
    paste0(route_names, ",IBX")
  ))

mta_clean <- mta_stations %>%
  mutate(
    stop_name_clean = str_to_upper(str_trim(stop_name)),
    borough = recode(borough,
                     "M" = "Manhattan",
                     "B" = "Brooklyn",
                     "Bk" = "Brooklyn",
                     "Q" = "Queens",
                     "S" = "Staten Island",
                     "SI" = "Staten Island",
                     "Bx" = "Bronx")
  ) %>%
  select(stop_name, stop_name_clean, borough, route_names = daytime_routes)

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

all_stations <- bind_rows(mta_clean, ibx_clean)


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

###### create buffer
half_mile_meters <- 0.5 * 1609.34  # ≈ 804.67 meters
all_stations_buffer <- st_buffer(all_stations, dist = half_mile_meters)

mapview(mta_stations)


# check result
plot(st_geometry(all_stations_buffer), col = 'lightblue')
plot(st_geometry(all_stations), add = TRUE, pch = 16, col = 'red')


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
  group_by(stop_name_clean) %>%
  summarize(
    total_pop = sum(pop_weighted, na.rm = TRUE),
    geometry = st_union(geometry),
    .groups ="drop"
  )

buffer_pop <- st_transform(buffer_pop, 4326)


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


# make sure CRS is WGS84 for Leaflet
buffer_pop <- st_transform(buffer_pop, 4326)
all_stations <- st_transform(all_stations, 4326)

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
      stop_name_clean, "<br>",
      "Population within 0.5 mile: ", round(total_pop)
    ),
    labelOptions = labelOptions(direction = "auto")
  ) %>%
  addCircleMarkers(
    data = all_stations,
    radius = 5,
    color = "red",
    stroke = TRUE,
    fillOpacity = 0.9,
    label = ~stop_name_clean
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~total_pop,
    title = "Population",
    opacity = 0.8
  )

# Create population bins for buffers
buffer_pop <- buffer_pop %>%
  mutate(
    pop_bin = cut(total_pop,
                  breaks = c(0, 5000, 10000, 20000, Inf),
                  labels = c("0-5k", "5-10k", "10-20k", "20k+"))
  )

# Split stations by borough
stations_brooklyn <- all_stations %>% filter(borough == "Brooklyn")
stations_queens  <- all_stations %>% filter(borough == "Queens")

# Define color palette for buffer population
pal <- colorNumeric(palette = "viridis", domain = buffer_pop$total_pop)

leaflet(buffer_pop) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Add population buffers
  addPolygons(
    fillColor = ~pal(total_pop),
    color = "black",
    weight = 0.5,
    opacity = 1,
    fillOpacity = 0.4,
    popup = ~paste0("<b>", stop_name_clean, "</b><br>",
                    "Population: ", round(total_pop)),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "blue",
      fillOpacity = 0.6,
      bringToFront = FALSE
    )
  ) %>%
  
  # Add stations by borough as separate layer groups
  addCircleMarkers(
    data = stations_brooklyn,
    radius = 5,
    color = "yellow",
    stroke = TRUE,
    fillOpacity = 0.9,
    label = ~stop_name_clean,
    group = "Stations - Brooklyn"
  ) %>%
  addCircleMarkers(
    data = stations_queens,
    radius = 5,
    color = "red",
    stroke = TRUE,
    fillOpacity = 0.9,
    label = ~stop_name_clean,
    group = "Stations - Queens"
  ) %>%
  
  # Add stations by line count (already in line_count column)
  addCircleMarkers(
    data = all_stations %>% filter(line_count == 1),
    radius = 5,
    color = "blue",
    group = "1 line"
  ) %>%
  addCircleMarkers(
    data = all_stations %>% filter(line_count == 2),
    radius = 5,
    color = "green",
    group = "2 lines"
  ) %>%
  addCircleMarkers(
    data = all_stations %>% filter(line_count >= 3),
    radius = 5,
    color = "purple",
    group = "3+ lines"
  ) %>%
  
  # Add layer control for toggling
  addLayersControl(
    overlayGroups = c(
      "Stations - Brooklyn",
      "Stations - Queens",
      "1 line", "2 lines", "3+ lines",
      "Population 0-5k", "Population 5-10k"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Add legend for buffer population
  addLegend(
    "bottomright",
    pal = pal,
    values = ~total_pop,
    title = "Population",
    opacity = 0.8
  )

### Make it into an App So it's easier to read

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
    fill= FALSE,
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
