# ==============================================================================
# Step 2: Environmental Predictor Pipeline - WorldClim Bioclimatic Rasters
# ==============================================================================

# 1. Load required libraries
library(terra)
library(geodata)
library(sf)
library(tidyverse)

# 2. define spatial extent and directory structure
dir.create("data/env", recursive = TRUE, showWarnings = FALSE)

# iberian peninsula study bounding box
IBERIA_BBOX <- ext(-10.0, 4.0, 35.0, 44.0)

# 3. fetch global WorldClim 2.1 Bioclimatic Rasters (2.5 arc-min resolution)
message("Downloading WorldClim 2.1 bioclimatic variables...")
worldclim_global <- worldclim_global(
  var = "bio",
  res = 2.5,
  path = "data/env"
)

# 4. crop and mask rasters to iberian extent
message("Cropping predictor rasters to Iberian bounding box...")
worldclim_iberia <- crop(worldclim_global, IBERIA_BBOX)

# 5. save processed spatraster stack locally
output_raster_path <- "data/env/worldclim_iberia_2.5m.tif"
writeRaster(worldclim_iberia, output_raster_path, overwrite = TRUE)
message(paste("Iberian bioclimatic raster stack saved to:", output_raster_path))

# 6. sanity plot of annual mean temperature (BIO1)
plot(worldclim_iberia[[1]], main = "WorldClim BIO1 - Annual Mean Temperature (ºC)")