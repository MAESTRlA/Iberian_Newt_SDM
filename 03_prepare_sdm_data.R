# ==============================================================================
# Step 3: SDM Dataset Preparation, Background Sampling & VIF Collinearity Analysis
# ==============================================================================

# 1. load required packages
install.packages("usdm")
library(sf)
library(terra)
library(tidyverse)
library(usdm) #for variance inflation factor (VIF) analysis

# 2. load processed spatial inputs
species_sf <- st_read("pleurodeles_waltl_points.gpkg", quiet = TRUE)
env_stack <- rast("data/env/worldclim_iberia_2.5m.tif")
names(env_stack) <- paste0("BIO", 1:19)

# 3. extract environmental values at occurrence locations(presences)
message("Extracting environmental values for presence points...")
presence_coords <- st_coordinates(species_sf)
presence_env <- terra::extract(env_stack, presence_coords) %>% drop_na()
presence_data <- presence_env %>%
mutate(presence = 1)

# 4. sample random background / pseudo-absence points (10,000 points)
set.seed(42)
message("Generating 10,000 random background points across Iberia...")

bg_points <- spatSample(
  x = env_stack[[1]],
  size = 10000,
  method = "random",
  na.rm = TRUE,
  xy = TRUE
)

# extract climate variables at background point coordinates
bg_coords <- bg_points[, c("x","y")]
bg_env <- terra::extract(env_stack, bg_coords) %>%
  drop_na()

# add target response variable (0 = background)
bg_data <- bg_env %>%
  mutate(presence = 0)

# combine presence and background into a unified modeling dataset
sdm_full_df <- bind_rows(presence_data, bg_data)

# 5. perform collinearity analysis via variance inflation factor (VIF)

# isolate predictor matrix (excluding coordinates/ID columns and presence target)
predictors_only <- sdm_full_df %>%
  select(starts_with("BIO"))

# 6. run stepwise vif selection (threshold vif < 10)
vif_result <- vifstep(predictors_only, th=10)
print(vif_result)

# 7. filter dataset to only keep uncorrelated predictors
uncorrelated_vars <- vif_result@results$Variables

sdm_clean_df <- sdm_full_df %>%
  select(presence, all_of(uncorrelated_vars))

# 8. save clean dataset for modelling
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

write.csv(
  sdm_clean_df,
  "data/processed/sdm_data_uncorrelated.csv",
  row.names = FALSE
)

# 9. save subsetted raster stack for future model predictions and projections
env_stack_reduced <- env_stack[[uncorrelated_vars]]
writeRaster(
  env_stack_reduced,
  "data/env/worldclim_iberia_uncorrelated.tif",
  overwrite = TRUE
)