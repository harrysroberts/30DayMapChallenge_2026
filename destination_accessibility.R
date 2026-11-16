library(tidyverse)
library(sf)
library(ggspatial)

#Install Java if not already installed
if (!(rJavaEnv::java_check_version_rjava()>0)){
  rJavaEnv::java_quick_install(version = 21)
}

#Detach r5r if it is already loaded, in order to pass new memory allowance
if("package:r5r" %in% search()){detach("package:r5r")}

#Allocate 16gb memory to Java
options(java.parameters = "-Xmx16G")

#Load r5r once memory is allocated
library(r5r)

## Set walk and bike speeds

# Walk and bike speeds on level ground and age/gender factors taken from 
# MatSim GitHub repository, Based on MatSim documentation by Horni et al.
#  (2016) available from https://www.ubiquitypress.com/books/e/10.5334/baw
# Weidmann reference speed of 1.34 m/s (4.824 km/h) used by MatSim

level_walk_speed = 1.34

leeds_oa_populations <- read_csv("input/30463753490639.csv", skip = 7) |>
  filter(!is.na(`Age 16 to 17`)) |>
  mutate(
    population = rowSums(
      pick(`Age 16 to 17`:`Age 60 to 64`),
      na.rm = TRUE
    )
  ) |>
  select(mnemonic, population)

leeds_oa_centroids <- read_csv("input/Output_Areas_Dec_2011_PWC_2022_-806071369276555851.csv") |>
  st_as_sf(coords = c("x", "y"), crs = 27700) |>
  st_transform(crs = 4326) |>
  mutate(
    id = OA11CD,
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2]
  ) |>
  st_drop_geometry() |>
  select(id,lon,lat) |>
  right_join(leeds_oa_populations, by = join_by(id == mnemonic))

r5r_network <- build_network(normalizePath("input/r5r"))

walk_times <- travel_time_matrix(
  r5r_network = r5r_network,
  origins = leeds_oa_centroids,
  destinations = leeds_oa_centroids,
  mode = c("WALK"),
  max_trip_duration = 1440,
  walk_speed = level_walk_speed * 3600/1000
  )

attraction_accessibilities <- walk_times |>
  filter(travel_time_p50 < 30) %>%
  left_join(leeds_oa_populations, by = join_by(`from_id` == `mnemonic`)) %>%
  group_by(to_id) %>%
  summarise(pop_within_30m_walk = sum(population, na.rm = TRUE))

output_areas <- st_read("input/Output_Areas_Dec_2011_Boundaries_EW_BGC_2022_-4410048714460462685.gpkg")

p <- output_areas |>
  right_join(attraction_accessibilities, by = join_by(OA11CD == to_id)) |>
  st_transform(crs = 4326) |>
  ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(
    aes(fill = pop_within_30m_walk),
    colour = NA,
    alpha = 0.75
  ) +
  scale_fill_viridis_c(
    option = "mako",
    direction = -1,
    name = "Population within\n30 min walk",
    labels = scales::label_comma(),
    guide = guide_colourbar(
      barheight = unit(4, "cm"),
      barwidth = unit(0.4, "cm"),
      ticks = FALSE
    )
  ) +
  coord_sf(
    xlim = c(-1.59, -1.50),
    ylim = c(53.78, 53.83),
    expand = FALSE
  ) +
  labs(
    title = "Number of residents aged 16-64 within a 30-minute walk",
    subtitle = "Census output area level",
    caption = "Basemap: © OpenStreetMap contributors"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 2)),
    plot.subtitle = element_text(colour = "grey35", margin = margin(b = 8)),
    plot.caption = element_text(colour = "grey50", size = 7, margin = margin(t = 8)),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  "walking_access_population.png",
  plot = p,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)