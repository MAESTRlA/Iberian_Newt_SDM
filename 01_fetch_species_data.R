# ==============================================================================
# Step 1: Biodiversity Data Pipeline - Pleurodeles waltl
# ==============================================================================

# 1. Load Libraries
library(tidyverse)
library(rgbif)
library(sf)
library(rnaturalearth)

# 2. Set Analysis Parameters
SPECIES_NAME <- "Pleurodeles waltl"
FETCH_LIMIT  <- 2500

IBERIA_BBOX  <- c(
  "lon_min" = -10.0,
  "lon_max" =   4.0,
  "lat_min" =  35.0,
  "lat_max" =  44.0
)

# Set timeout option for GBIF API download
options(gbif_curl_options = list(timeout = 300))

# 3. Fetch Data from GBIF API
message(paste0("Querying GBIF API for species: ", SPECIES_NAME, "..."))
gbif_raw <- occ_data(
  scientificName = SPECIES_NAME,
  hasCoordinate = TRUE,
  limit = FETCH_LIMIT
)

# 4. Clean & Filter Data
clean_species_df <- gbif_raw$data %>%
  select(scientificName, decimalLongitude, decimalLatitude, year, countryCode) %>%
  drop_na(decimalLongitude, decimalLatitude) %>%
  distinct(decimalLongitude, decimalLatitude, .keep_all = TRUE) %>%
  filter(
    decimalLongitude >= IBERIA_BBOX["lon_min"] & decimalLongitude <= IBERIA_BBOX["lon_max"],
    decimalLatitude  >= IBERIA_BBOX["lat_min"] & decimalLatitude  <= IBERIA_BBOX["lat_max"]
  )

# 5. Convert to Spatial (sf) & Apply Terrestrial Polygon Clipping
species_sf_raw <- st_as_sf(
  clean_species_df,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)

iberia_poly <- ne_countries(
  scale = "medium", 
  country = c("Portugal", "Spain"), 
  returnclass = "sf"
) %>% 
  st_union()

species_sf <- st_intersection(species_sf_raw, iberia_poly)
message(paste("Final clipped terrestrial records:", nrow(species_sf)))

# 6. Save Clean Spatial Output (.gpkg)
output_file <- "pleurodeles_waltl_points.gpkg"
st_write(species_sf, output_file, append = FALSE, quiet = TRUE)
message(paste("Spatial layer saved locally as:", output_file))

# 7. Diagnostic Visualization
world_map <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world_map, fill = "#f2f2ef", color = "#b8b8b8") +
  geom_sf(data = species_sf, color = "#1b9e77", alpha = 0.6, size = 1.3) +
  coord_sf(
    xlim = c(IBERIA_BBOX["lon_min"], IBERIA_BBOX["lon_max"]),
    ylim = c(IBERIA_BBOX["lat_min"], IBERIA_BBOX["lat_max"]),
    expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = expression(paste("Terrestrial Occurrences of ", italic("Pleurodeles waltl"))),
    subtitle = paste("Source: GBIF API | Clipped Points:", nrow(species_sf)),
    x = "Longitude", y = "Latitude"
  )
