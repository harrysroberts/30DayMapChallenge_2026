library(tidyverse)
library(sf)
library(ggrepel)
library(rnaturalearth)

data <- read_csv("european_cities_transport.csv") |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  mutate(
    top_pt_mode = factor(
      top_pt_mode,
      levels = c("Metro", "Tram", "None"),
      labels = c("Metro", "Tram but no metro", "Neither metro nor tram")
    )
  )

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)
p <- ggplot() +
  geom_sf(
    data = world,
    fill = "#F2F2F2",
    colour = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = data,
    aes(fill = top_pt_mode),
    shape = 21,
    colour = "white",
    stroke = 0.7,
    size = 5
  ) +
  geom_text_repel(
    data = data,
    aes(
      x = st_coordinates(data)[, 1],
      y = st_coordinates(data)[, 2],
      label = city
    ),
    size = 4,
    colour = "grey15",
    fontface = "bold",
    box.padding = 0.5,
    point.padding = 0.35,
    min.segment.length = 0,
    segment.colour = "grey50",
    segment.linewidth = 0.3,
    max.overlaps = Inf,
    seed = 123
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Neither metro nor tram" = "#FF0000",  # bright red
      "Tram but no metro"      = "#0000FF",  # blue
      "Metro"                  = "#006400"   # dark green
    )
  ) +
  coord_sf(
    xlim = c(-20,55),
    ylim = c(36, 63),
    expand = FALSE
  ) +
  labs(
    title = "Public transport provision of European urban areas",
    subtitle = "Urban areas with more than 800,000 inhabitants"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(
      size = 22,
      face = "bold",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 15,
      colour = "grey35",
      margin = margin(b = 12)
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 13),
    legend.key.spacing.x = unit(5, "mm"),
    plot.margin = margin(15, 15, 10, 15)
  )

ggsave(
  "pt_europe.png",
  plot = p,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)
