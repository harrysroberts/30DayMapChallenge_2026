# Residential frontage coverage per street (tidyverse + osmextract)
# For each named street: sample every STEP m, cast a ray sideways on both sides,
# classify the first building hit (residential / non-residential / nothing).
#
# install.packages(c("tidyverse", "sf", "osmextract"))

library(tidyverse)
library(sf)
library(osmextract)

# ---- parameters -------------------------------------------------------------
geofabrik_region <- "West Yorkshire"   # oe_match("Leeds") suggests a region name
bb <- c(xmin = -1.60, ymin = 53.81, xmax = -1.56, ymax = 53.83)  # WGS84 lon/lat
CRS  <- 27700   # local metric CRS (British National Grid)
STEP <- 5       # sample spacing along street (m)
MAXD <- 25      # ray length (m); try 40 for wide or set-back streets
YES_AS_RES <- TRUE  # building=yes counts as residential if inside landuse=residential
# and with no commercial tags

RES_TYPES <- c("house", "detached", "semidetached_house", "terrace", "terraced",
               "bungalow", "residential", "apartments", "flats", "dormitory")
NONRES_KEYS <- c("shop", "amenity", "office", "tourism", "craft", "industrial")
EXCLUDE_HIGHWAY <- c("motorway", "motorway_link", "proposed", "construction",
                     "raceway", "bus_guideway", "busway", "escape",
                     "corridor", "elevator", "services", "rest_area")

# ---- download (cached after first run) --------------------------------------
bbox_sfc <- st_as_sfc(st_bbox(bb, crs = 4326))

bbox_sfc_buffered <- st_buffer(bbox_sfc,100)

polys_raw <- oe_get(geofabrik_region, layer = "multipolygons",
                    extra_tags = "industrial",
                    boundary = bbox_sfc_buffered, boundary_type = "spat", quiet = TRUE)

lines_raw <- oe_get(geofabrik_region, layer = "lines",
                    boundary = bbox_sfc_buffered, boundary_type = "spat", quiet = TRUE)

# ---- streets ----------------------------------------------------------------
streets <- lines_raw |>
  filter(!is.na(highway), !highway %in% EXCLUDE_HIGHWAY) |>
  st_transform(CRS) |>
  st_cast("LINESTRING", warn = FALSE) |>
  mutate(line_id  = row_number(),
         length_m = as.numeric(st_length(geometry)))

street_union <- st_union(streets)

# ---- buildings --------------------------------------------------------------
lu <- polys_raw |>
  filter(landuse == "residential") |>
  st_transform(CRS) |>
  st_make_valid()

bld <- polys_raw |>
  filter(!is.na(building)) |>
  st_transform(CRS) |>
  st_make_valid() |>
  mutate(bid = row_number()) |>
  filter(lengths(st_is_within_distance(geometry, street_union, dist = MAXD)) > 0)

in_lu <- if (nrow(lu) > 0) {
  lu_union <- st_union(lu)
  lengths(st_intersects(suppressWarnings(st_point_on_surface(bld)), lu_union)) > 0
} else {
  rep(FALSE, nrow(bld))
}

bld <- bld |>
  mutate(in_lu  = in_lu,
         nonres = if_any(any_of(NONRES_KEYS), ~ !is.na(.x)),
         res    = (building %in% RES_TYPES | (YES_AS_RES & building == "yes" & in_lu)) & !nonres) |>
  select(bid, res)

# ---- sample points + perpendicular rays -------------------------------------
coords_at <- function(geom, s) {
  st_line_sample(geom, sample = s) |>
    st_coordinates() |>
    as_tibble() |>
    select(x = X, y = Y)
}

make_rays <- function(geom, line_id) {
  len <- as.numeric(st_length(geom))
  d <- seq(0, len, by = STEP)
  g <- st_sfc(geom, crs = CRS)
  p <- coords_at(g, d / len)
  q <- coords_at(g, pmin((d + 1) / len, 1))
  
  tibble(line_id = line_id,
         x0 = p$x, y0 = p$y, tx = q$x - p$x, ty = q$y - p$y) |>
    mutate(n = sqrt(tx^2 + ty^2)) |>
    filter(n > 0) |>
    mutate(tx = tx / n, ty = ty / n) |>
    (\(df) map_dfr(c(1, -1), \(s) mutate(df,
                                         side = s,
                                         x1 = x0 + (-ty * s) * MAXD,
                                         y1 = y0 + ( tx * s) * MAXD)))()
}

rays_df <- pmap_dfr(list(st_geometry(streets), streets$line_id), make_rays) |>
  mutate(ray_id = row_number())

rays <- rays_df |>
  mutate(geometry = st_sfc(
    pmap(list(x0, y0, x1, y1),
         \(a, b, c, d) st_linestring(matrix(c(a, c, b, d), ncol = 2))),
    crs = CRS)) |>
  st_as_sf() |>
  select(ray_id)

origins <- rays_df |>
  st_as_sf(coords = c("x0", "y0"), crs = CRS, remove = FALSE) |>
  st_geometry()

# ---- first building hit per ray ---------------------------------------------
first_hit <- suppressWarnings(st_intersection(rays, bld)) |>
  mutate(dist = as.numeric(st_distance(geometry, origins[ray_id], by_element = TRUE))) |>
  st_drop_geometry() |>
  group_by(ray_id) |>
  slice_min(dist, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(ray_id, res)

# ---- compute results --------------------------------------------------------

result <- rays_df |>
  left_join(first_hit, by = "ray_id") |>
  mutate(built = !is.na(res),
         res   = coalesce(res, FALSE)) |>
  group_by(line_id) |>
  summarise(n_samples        = n(),
            pct_residential  = 100 * mean(res),
            pct_built        = 100 * mean(built),
            pct_res_of_built = if_else(sum(built) > 0, 100 * sum(res) / sum(built), NA_real_),
            .groups = "drop")

# ---- plot results -----------------------------------------------------------

tmap_mode("plot")

m <- tm_shape(bld, bbox = bbox_sfc) +
  tm_fill(fill = "res", fill_alpha = 0.5,
          fill.scale  = tm_scale_categorical(values = c("grey80", "#d95f02"),
                                             labels = c("Other", "Residential")),
          fill.legend = tm_legend("Buildings")) +
  tm_shape(streets |> left_join(result, by = join_by(line_id))) +
  tm_lines(col = "pct_residential", lwd = 2,
           col.scale  = tm_scale_continuous(limits = c(0, 100)),
           col.legend = tm_legend("Residential\nfrontage (%)")) +
  tm_title("Residential frontage along roads and paths in Headingley, Leeds") +
  tm_layout(frame = FALSE, legend.position = tm_pos_out("right", "center"))

tmap_save(m, "frontage_map.png", width = 8, height = 6, units = "in", dpi = 300)
