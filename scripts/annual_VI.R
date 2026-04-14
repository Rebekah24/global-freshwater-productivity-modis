# Set python system to the virtual environment
Sys.setenv(RETICULATE_PYTHON = "~/earthengine-env/bin/python")

# Load Python packages 
library(reticulate)
use_python("~/earthengine-env/bin/python", required = TRUE)

# Check if it's using the virtual environment
py_config()

ee <- import("ee")

# Authenticate and initialize
#ee$Authenticate()
ee$Initialize(project = "ee-rebekahchen24")
#ee_Initialize("rebekahchen24@gmail.com", drive = TRUE)


# Test if EE is connected and works. It should print "Hello Earth Engine"
print(ee$String("Hello Earth Engine")$getInfo())

# Load packages
library(rgee)
library(sf)
library(stars)
library(dplyr)
library(rnaturalearth)
library(ggplot2)
library(tidyr)
library(stringr)


# # THESE STEPS ARE NOT NECESSARY. IT IS REALLY FOR CHECKING THE CORRECT PORJECTION BEFORE RUNNING THE REST OF THE SCRIPT
# mcd43a4 <- ee$ImageCollection("MODIS/061/MCD43A4")
# 
# start_date <- "2024-01-01"
# end_date <- "2024-01-31"
# 
# filtered_collection <- mcd43a4$
#   filterDate(start_date, end_date)$
#   first()  # Get the first image as an example
# 
# original_projection <- filtered_collection$projection()
# print(original_projection$getInfo()) # This will show it's in Sinusoidal (SR-ORG:6974)
# 
# # Define the target projection (WGS 84) to match with the species richness grid
# target_projection <- ee$Projection("EPSG:4326")
# 
# # Reproject the image
# reprojected_img <- filtered_collection$
#   reproject(crs = "EPSG:4326")
# 
# reprojected_img$getInfo() # mke sure the projection is WGS 84
# 
# 
# # Get global river dataset
# global_river <- st_read("/Users/rebekahchen/Desktop/NDVI/HydroRIVERS_v10_shp/HydroRIVERS_v10_shp")
# 
# global_rivers <- global_river |>
#   group_by(MAIN_RIV, HYRIV_ID ) |>
#   tidyterra::filter(ORD_STRA > 3)
# 
# global_rivers_filtered <- global_rivers %>%
#   ungroup() %>%  # Make sure no previous grouping
#   mutate(RIVER_SEGMENT_ID = row_number()) %>%
#   select(c("HYRIV_ID","MAIN_RIV","RIVER_SEGMENT_ID","geometry"))
# 
# global_rivers_filtered <- global_rivers %>% group_by("HYRIV_ID")%>%
#   mutate(RIVER_SEGMENT_ID = row_number()) %>%
#   ungroup() #%>%
#   #select(c("HYRIV_ID","MAIN_RIV","geometry"))
# 
# 
# write_sf(global_rivers_filtered, "/Users/rebekahchen/Desktop/NDVI/global_rivers_filtered.shp")
# 

# NDVI calculation function -------------------------------------------------------------------

calculate_ndvi <- function(start_date, end_date) {
  modis <- ee$ImageCollection('MODIS/061/MCD43A4')$
    filter(ee$Filter$date(start_date, end_date))
  
  # Function to calculate NDVI for each image
  add_ndvi <- function(image) {
    # Scale the reflectance values (MODIS uses a scale factor of 0.0001)
    red <- image$select("Nadir_Reflectance_Band1")$multiply(0.0001)
    nir <- image$select("Nadir_Reflectance_Band2")$multiply(0.0001)
    
    # NDVI = (NIR - Red) / (NIR + Red)
    ndvi <- nir$subtract(red)$divide(nir$add(red))$rename("NDVI")
    return(image$addBands(ndvi))
  }
  
  # Apply NDVI calculation to all images
  modis_with_ndvi <- modis$map(add_ndvi)
  
  # Get the mean NDVI over the year (average of all daily values)
  annual_ndvi <- modis_with_ndvi$select("NDVI")$mean()
  
  # Reproject to WGS 84 to match with the species richness grid
  annual_ndvi_reproject <- annual_ndvi$reproject(crs = "EPSG:4326", scale = 500)
  
  return(annual_ndvi_reproject)
}



## Lakes (NDVI) -------------------------------------------------------------------

richness_ee <- ee$FeatureCollection("projects/ee-rebekahchen24/assets/NDVI/Global_fwplant_richness")
lakes_ee <- ee$FeatureCollection("projects/ee-rebekahchen24/assets/NDVI/Global_LAKES_polys_v10_shp")
water_mask <- ee$Image('projects/ee-rebekahchen24/assets/NDVI/MOD44W_2024_global_WGS84')

# # Checking richness data
# total_richness <- richness_ee$size()$getInfo()
# print(paste("Total richness cells:", total_richness))
# 
# # Get a sample to see the structure
# first_cell <- richness_ee$first()$getInfo() # cell refers to the first species richness grid cell
# print("First cell structure:")
# print(str(first_cell))
# print("Properties available:")
# print(names(first_cell$properties))
# 
# # Filter for cells with species (some cells contain 0 species)
# richness_with_species <- richness_ee$filter(ee$Filter$gt("Fwplant_ri", 0))
# n_with_species <- richness_with_species$size()$getInfo()
# print(paste("Cells with species:", n_with_species))
# 
# # Calculate NDVI
# annual_ndvi <- calculate_ndvi('2024-01-01', '2024-12-31') # set the dates
# 
# # Test with one species richness grid cell
# test_cell <- richness_with_species$first()
# cell_info <- test_cell$getInfo()
# 
# 
# lakes_in_cell <- lakes_ee$filterBounds(test_cell$geometry())
# n_lakes <- lakes_in_cell$size()$getInfo()
# print(paste("Number of lakes in this cell:", n_lakes))
# 
# 
# # Test with a small batch
# test_batch <- richness_with_species$limit(50)
# 
# results <- test_batch$map(function(cell) {
#   # Find lakes
#   lakes_in_cell <- lakes_ee$filterBounds(cell$geometry())
#   
# 
#   # We need to define this INSIDE the map function
#   lakes_geometry <- lakes_in_cell$geometry()$intersection(cell$geometry())
#   
#   # Calculate NDVI and std - explicitly select NDVI band
#   ndvi_stats <- annual_ndvi$select('NDVI')$reduceRegion(
#     reducer = ee$Reducer$mean()$combine(
#       reducer2 = ee$Reducer$stdDev(),
#       sharedInputs = TRUE
#     ),
#     geometry = lakes_geometry,  
#     scale = 500,
#     maxPixels = 1e9
#   )
# 
#   
#   return(cell$set(list(
#     lake_ndvi_annual_mean = ndvi_stats$get('NDVI_mean'),     
#     lake_ndvi_annual_sd = ndvi_stats$get('NDVI_stdDev'),
#     n_lakes = lakes_in_cell$size()
#   )))
# })



# # Get the results
# results_data <- results$getInfo()

# # Convert to dataframe with proper error handling
# results_df <- do.call(rbind, lapply(results_data$features, function(f) {
#   props <- f$properties
#   
#   data.frame(
#     Grid_ID = props$Grid_ID,
#     latitude = props$latitude,
#     longitude = props$longitude,
#     Fwplant_ri = props$Fwplant_ri,
#     lake_annual_ndvi_mean = ifelse(is.null(props$lake_ndvi_annual_mean), NA, props$lake_ndvi_annual_mean),
#     lake_annual_ndvi_sd = ifelse(is.null(props$lake_ndvi_annual_sd), NA, props$lake_ndvi_annual_sd),
#     n_lakes = ifelse(is.null(props$n_lakes), 0, props$n_lakes),
#     stringsAsFactors = FALSE
#   )
# }))


# Process all cells (remove the limit)
# all_results <- richness_with_species$map(function(cell) {
#   # Find lakes
#   lakes_in_cell <- lakes_ee$filterBounds(cell$geometry())
#   
#   # Get intersection
#   lakes_geometry <- lakes_in_cell$geometry()$intersection(cell$geometry())
#   
#   # Calculate NDVI and std
#   ndvi_stats <- annual_ndvi$select('NDVI')$reduceRegion(
#     reducer = ee$Reducer$mean()$combine(
#       reducer2 = ee$Reducer$stdDev(),
#       sharedInputs = TRUE
#     ),
#     geometry = lakes_geometry,
#     scale = 500,
#     maxPixels = 1e9
#   )
#   
#   # Return cell with new properties 
#   return(cell$set(list(
#     lk_ndvi_mn = ndvi_stats$get('NDVI_mean'),      
#     lk_ndvi_sd = ndvi_stats$get('NDVI_stdDev'),    
#     n_lakes = lakes_in_cell$size()                  
#   )))
# })
# 
# 
# export_task <- ee$batch$Export$table$toDrive(
#   collection = all_results,
#   description = 'lake_ndvi_global_final_2024',
#   folder = 'EarthEngine_Exports', # This folder will be created in your Google Drive
#   fileNamePrefix = 'global_lake_ndvi_richness_2024',
#   fileFormat = 'SHP'
# )
# 
# # Start the export
# export_task$start()
# print(paste("Export started! Task ID:", export_task$id)) # Check it started

# New Method with water mask
calculate_lake_nvdi_richness <- function(year = 2024) {
  
  # Calculate NDVI
  annual_ndvi <- calculate_ndvi(paste0(year, '-01-01'), paste0(year, '-12-31'))
  
  # Resample water mask
  water_mask_500m <- water_mask$
    reduceResolution(reducer = ee$Reducer$mean(), maxPixels = 20)$
    reproject(crs = "EPSG:4326", scale = 500)
  
  combined <- annual_ndvi$addBands(water_mask_500m$rename('water_mask'))
  
  # Process richness cells directly
  richness_with_species <- richness_ee$filter(ee$Filter$gt("Fwplant_ri", 0))
  
  results <- richness_with_species$map(function(cell) {
    
    # Find lakes in cell
    lakes_in_cell <- lakes_ee$filterBounds(cell$geometry())
    
    # Get combined lake geometry
    lakes_geometry <- lakes_in_cell$geometry()$intersection(cell$geometry())
    
    # Calculate stats for all lake areas at once
    stats <- combined$reduceRegion(
      reducer = ee$Reducer$mean()$combine(
        ee$Reducer$stdDev(), "", TRUE
      ),
      geometry = lakes_geometry,
      scale = 500,
      maxPixels = 1e7,
      tileScale = 8,
      bestEffort = TRUE  
    )
    
    # Water-only stats
    water_only <- combined$updateMask(water_mask_500m$gt(0.5))
    water_stats <- water_only$select('NDVI')$reduceRegion(
      reducer = ee$Reducer$mean(),
      geometry = lakes_geometry,
      scale = 500,
      maxPixels = 1e7,
      tileScale = 8,
      bestEffort = TRUE
    )
    
    return(cell$
             set('lk_ndvi_mn', stats$get('NDVI_mean'))$
             set('lk_ndvi_sd', stats$get('NDVI_stdDev'))$
             set('water_prop', stats$get('water_mask_mean'))$
             set('ndvi_wtr', water_stats$get('NDVI'))$
             set('n_lakes', lakes_in_cell$size())
    )
  })
  
  return(results)
}

# Run the direct calculation
results <- calculate_richness_direct(2024)

# Export with higher tileScale
task <- ee$batch$Export$table$toDrive(
  collection = results,
  description = 'richness_direct_2024',
  folder = 'EarthEngine_Exports',
  fileNamePrefix = 'richness_lake_ndvi_direct_2024',
  fileFormat = 'SHP',
  selectors = list('Grid_ID', 'Fwplant_ri', 'latitude', 'longitude',
                   'lk_ndvi_mn', 'lk_ndvi_sd', 'water_prop', 
                   'ndvi_wtr', 'n_lakes')  # Specify fields to export
)
task$start()



## Rivers (NDVI) ------------------------------------------------------------------

rivers_ee<- ee$FeatureCollection("projects/ee-rebekahchen24/assets/NDVI/Global_rivers_filtered")
richness_ee <- ee$FeatureCollection("projects/ee-rebekahchen24/assets/NDVI/Global_fwplant_richness")

# # Calculate annual NDVI
# annual_ndvi <- calculate_ndvi('2024-01-01', '2024-12-31')
# 
# annual_ndvi$getInfo() # check is the projection is EPSG:4326
# 
# 
# # Get a sample river to check the data structure
# first_river <- rivers_ee$first()$getInfo()
# print(str(first_river))
# print(names(first_river$properties))
# 
# # Filter for cells with species (some cells contain 0 species). Same steps as shown in Lakes
# richness_with_species <- richness_ee$filter(ee$Filter$gt("Fwplant_ri", 0))
# n_with_species <- richness_with_species$size()$getInfo()
# 
# 
# # Test with a small batch
# test_batch <- richness_with_species$limit(50)
# 
# results <- test_batch$map(function(cell) {
#   # Find rivers in cell
#   rivers_in_cell <- rivers_ee$filterBounds(cell$geometry())
#   
#   # Calculate NDVI for river 
#   rivers_ndvi <- rivers_in_cell$geometry()$intersection(cell$geometry())
#   
#   ndvi_stats_river <- annual_ndvi$select('NDVI')$reduceRegion(
#     reducer = ee$Reducer$mean()$combine(
#       reducer2 = ee$Reducer$stdDev(),
#       sharedInputs = TRUE
#     ),
#     geometry = rivers_ndvi,
#     scale = 500,
#     maxPixels = 1e9
#   )
# 
#   unique_rivers <- rivers_in_cell$aggregate_count_distinct('MAIN_RIV')
#   
#   return(cell$set(list(
#     rv_ndvi_mn = ndvi_stats_river$get('NDVI_mean'),      
#     rv_ndvi_sd = ndvi_stats_river$get('NDVI_stdDev'),    
#     n_riv_seg = rivers_in_cell$size(),                   
#     n_uniq_riv = unique_rivers                           
#   )))
# })
# 
# results_data <- results$getInfo()
# 
# results_df <- do.call(rbind, lapply(results_data$features, function(f) {
#   props <- f$properties
#   
#   data.frame(
#     Grid_ID = props$Grid_ID,
#     latitude = props$latitude,
#     longitude = props$longitude,
#     Fwplant_ri = props$Fwplant_ri,
#     river_ndvi_mean = ifelse(is.null(props$rv_ndvi_mn), NA, props$rv_ndvi_mn),  
#     river_ndvi_sd = ifelse(is.null(props$rv_ndvi_sd), NA, props$rv_ndvi_sd),    
#     n_river_segments = ifelse(is.null(props$n_riv_seg), 0, props$n_riv_seg),    
#     n_unique_rivers = ifelse(is.null(props$n_uniq_riv), 0, props$n_uniq_riv),  
#     stringsAsFactors = FALSE
#   )
# }))
# 
# 
# head(results_df)
# print(paste("Grid cells with rivers:", sum(results_df$n_river_segments > 0)))
# 
# 
# # Process all cells in the cloud
# all_results_river <- richness_with_species$map(function(cell) {
#   # Find rivers in cell
#   rivers_in_cell <- rivers_ee$filterBounds(cell$geometry())
#   
#   # Calculate NDVI for each river segment
#   rivers_ndvi <- rivers_in_cell$geometry()$intersection(cell$geometry())
#   
#   ndvi_stats_river <- annual_ndvi$select('NDVI')$reduceRegion(
#     reducer = ee$Reducer$mean()$combine(
#       reducer2 = ee$Reducer$stdDev(),
#       sharedInputs = TRUE
#     ),
#     geometry = rivers_ndvi,
#     scale = 500,
#     maxPixels = 1e9
#   )
#   
#   unique_rivers <- rivers_in_cell$aggregate_count_distinct('MAIN_RIV')
#   
#   return(cell$set(list(
#     rv_ndvi_mn = ndvi_stats_river$get('NDVI_mean'),      
#     rv_ndvi_sd = ndvi_stats_river$get('NDVI_stdDev'),    
#     n_riv_seg = rivers_in_cell$size(),                   
#     n_uniq_riv = unique_rivers                           
#   )))
# })
# 
# 
# # Export results
# export_task <- ee$batch$Export$table$toDrive(
#   collection = all_results_river,  # <- Fixed: use all_results_river
#   description = 'river_ndvi_global_final',
#   folder = 'EarthEngine_Exports',
#   fileNamePrefix = 'global_river_ndvi_richness_2024',
#   fileFormat = 'SHP'
# )
# 
# # Start the export
# export_task$start()
# print(paste("Export started! Task ID:", export_task$id))
# 
# print(export_task$status())

#New method with water mask
calculate_river_ndvi_richness <- function(year = 2024) {
  
  # Calculate NDVI
  annual_ndvi <- calculate_ndvi(paste0(year, '-01-01'), paste0(year, '-12-31'))
  
  # Resample water mask
  water_mask_500m <- water_mask$
    reduceResolution(reducer = ee$Reducer$mean(), maxPixels = 20)$
    reproject(crs = "EPSG:4326", scale = 500)
  
  combined <- annual_ndvi$addBands(water_mask_500m$rename('water_mask'))
  
  # Process richness cells directly
  richness_with_species <- richness_ee$filter(ee$Filter$gt("Fwplant_ri", 0))
  
  results <- richness_with_species$map(function(cell) {
    
    # Find river segments in cell
    rivers_in_cell <- rivers_ee$filterBounds(cell$geometry())
    
    # Get combined river geometry
    rivers_geometry <- rivers_in_cell$geometry()$intersection(cell$geometry())
    
    # Calculate stats for all river areas
    stats <- combined$reduceRegion(
      reducer = ee$Reducer$mean()$combine(
        ee$Reducer$stdDev(), "", TRUE
      ),
      geometry = rivers_geometry,
      scale = 500,
      maxPixels = 1e7,
      tileScale = 8,
      bestEffort = TRUE
    )
    
    # Water-only stats
    water_only <- combined$updateMask(water_mask_500m$gt(0.5))
    water_stats <- water_only$select('NDVI')$reduceRegion(
      reducer = ee$Reducer$mean(),
      geometry = rivers_geometry,
      scale = 500,
      maxPixels = 1e7,
      tileScale = 8,
      bestEffort = TRUE
    )
    
    # Count both segments and unique rivers
    n_segments <- rivers_in_cell$size()
    n_unique_rivers <- rivers_in_cell$aggregate_count_distinct('MAIN_RIV')
    
    return(cell$
             set('rv_ndvi_mn', stats$get('NDVI_mean'))$
             set('rv_ndvi_sd', stats$get('NDVI_stdDev'))$
             set('water_prop', stats$get('water_mask_mean'))$
             set('ndvi_wtr', water_stats$get('NDVI'))$
             set('n_segments', n_segments)$           # Total river segments
             set('n_rivers', n_unique_rivers)         # Unique rivers
    )
  })
  
  return(results)
}

river_ndvi<- calculate_river_ndvi_richness(2024)

river_ndvi_task <- ee$batch$Export$table$toDrive(
  collection = river_ndvi,
  description = 'river_richness_ndvi_water_2024',
  folder = 'EarthEngine_Exports',
  fileNamePrefix = 'river_richness_ndvi_water_2024',
  fileFormat = 'SHP',
  selectors = list('Grid_ID', 'Fwplant_ri', 'latitude', 'longitude',
                   'rv_ndvi_mn', 'rv_ndvi_sd', 'water_prop', 
                   'ndvi_wtr', 'n_rivers','n_segments')  # Specify fields to export
)

river_ndvi_task $start()


# NDAVI calculation function -----------------------------------------------------------------

calculate_ndavi <- function(start_date, end_date) {
  modis <- ee$ImageCollection('MODIS/061/MCD43A4')$
    filter(ee$Filter$date(start_date, end_date))
  
  # Function to calculate NDAVI for each image
  add_ndavi <- function(image) {
    # Scale the reflectance values (MODIS uses a scale factor of 0.0001)
    blue <- image$select("Nadir_Reflectance_Band3")$multiply(0.0001)  # Blue band
    nir <- image$select("Nadir_Reflectance_Band2")$multiply(0.0001)   # NIR band
    
    # NDAVI = (NIR - Blue) / (NIR + Blue)
    ndavi <- nir$subtract(blue)$divide(nir$add(blue))$rename("NDAVI")
    return(image$addBands(ndavi))
  }
  
  # Apply NDAVI calculation to all images
  modis_with_ndavi <- modis$map(add_ndavi)
  
  # Get the mean NDAVI over the year (average of all daily values)
  annual_ndavi <- modis_with_ndavi$select("NDAVI")$mean()
  
  # Reproject to WGS 84 to match with the species richness grid
  annual_ndavi_reproject <- annual_ndavi$reproject(crs = "EPSG:4326", scale = 500)
  
  return(annual_ndavi_reproject)
}

annual_ndavi <- calculate_ndavi('2024-01-01', '2024-12-31')









## Rivers (NDAVI) -----------------------------------------------------------

# rivers_ee <- ee$FeatureCollection("projects/ee-rebekahchen24/assets/NDVI/Global_rivers_filtered")
# richness_ee <- ee$FeatureCollection("projects/ee-rebekahchen24/assets/NDVI/Global_fwplant_richness")
# 
# # Filter for cells with species
# richness_with_species <- richness_ee$filter(ee$Filter$gt("Fwplant_ri", 0))
# 
# # Create a test batch
# test_batch_ndavi <- richness_with_species$limit(50)
# 
# test_results_ndavi <- test_batch_ndavi$map(function(cell) {
#   # Find rivers in cell
#   rivers_in_cell <- rivers_ee$filterBounds(cell$geometry())
#   
#   # Calculate NDAVI for each river segment
#   rivers_ndavi <- rivers_in_cell$geometry()$intersection(cell$geometry())
#   
#   ndavi_stats_river <- annual_ndavi$select('NDAVI')$reduceRegion(
#     reducer = ee$Reducer$mean()$combine(
#       reducer2 = ee$Reducer$stdDev(),
#       sharedInputs = TRUE
#     ),
#     geometry = rivers_ndavi,
#     scale = 500,
#     maxPixels = 1e9
#   )
#   
#   unique_rivers <- rivers_in_cell$aggregate_count_distinct('MAIN_RIV')
#   
#   return(cell$set(list(
#     rv_ndavi_mn = ndavi_stats_river$get('NDAVI_mean'),      
#     rv_ndavi_sd = ndavi_stats_river$get('NDAVI_stdDev'),    
#     n_riv_seg = rivers_in_cell$size(),                   
#     n_uniq_riv = unique_rivers                           
#   )))
# })
# 
# ndavi_results_data <- test_results_ndavi$getInfo()
# 
# ndavi_results_df <- do.call(rbind, lapply(ndavi_results_data$features, function(f) {
#   props <- f$properties
#   
#   data.frame(
#     Grid_ID = props$Grid_ID,
#     latitude = props$latitude,
#     longitude = props$longitude,
#     Fwplant_ri = props$Fwplant_ri,
#     river_nadvi_mean = ifelse(is.null(props$rv_ndavi_mn), NA, props$rv_ndavi_mn),  
#     river_nadvi_sd = ifelse(is.null(props$rv_ndavi_sd), NA, props$rv_ndavi_sd),    
#     n_river_segments = ifelse(is.null(props$n_riv_seg), 0, props$n_riv_seg),    
#     n_unique_rivers = ifelse(is.null(props$n_uniq_riv), 0, props$n_uniq_riv),  
#     stringsAsFactors = FALSE
#   )
# }))
# 
# 
# 
# # Process all richness cells with NDAVI
# all_results_river_ndavi <- richness_with_species$map(function(cell) {
#   # Find rivers in cell
#   rivers_in_cell <- rivers_ee$filterBounds(cell$geometry())
#   
#   # Calculate NDAVI for each river segment
#   rivers_ndavi <- rivers_in_cell$geometry()$intersection(cell$geometry())
#   
#   ndavi_stats_river <- annual_ndavi$select('NDAVI')$reduceRegion(
#     reducer = ee$Reducer$mean()$combine(
#       reducer2 = ee$Reducer$stdDev(),
#       sharedInputs = TRUE
#     ),
#     geometry = rivers_ndavi,
#     scale = 500,
#     maxPixels = 1e9
#   )
#   
#   unique_rivers <- rivers_in_cell$aggregate_count_distinct('MAIN_RIV')
#   
#   return(cell$set(list(
#     rv_ndavi_mn = ndavi_stats_river$get('NDAVI_mean'),      
#     rv_ndavi_sd = ndavi_stats_river$get('NDAVI_stdDev'),    
#     n_riv_seg = rivers_in_cell$size(),                   
#     n_uniq_riv = unique_rivers                           
#   )))
# })
# 
# # Export results
# export_task_ndavi <- ee$batch$Export$table$toDrive(
#   collection = all_results_river_ndavi,
#   description = 'river_ndavi_global_final',
#   folder = 'EarthEngine_Exports',
#   fileNamePrefix = 'global_river_ndavi_richness_2024',
#   fileFormat = 'SHP'
# )
# 
# export_task_ndavi$start()
# print(paste("Export started! Task ID:",export_task_ndavi$id))


# New method with water mask
calculate_river_ndavi_richness <- function(year = 2024) {
  
  # Calculate NDAVI
  annual_ndavi <- calculate_ndavi(paste0(year, '-01-01'), paste0(year, '-12-31'))
  
  # Resample water mask
  water_mask_500m <- water_mask$
    reduceResolution(reducer = ee$Reducer$mean(), maxPixels = 20)$
    reproject(crs = "EPSG:4326", scale = 500)
  
  combined <- annual_ndavi$addBands(water_mask_500m$rename('water_mask'))
  
  # Process richness cells directly
  richness_with_species <- richness_ee$filter(ee$Filter$gt("Fwplant_ri", 0))
  
  results <- richness_with_species$map(function(cell) {
    
    # Find river segments in cell
    rivers_in_cell <- rivers_ee$filterBounds(cell$geometry())
    
    # Get combined river geometry
    rivers_geometry <- rivers_in_cell$geometry()$intersection(cell$geometry())
    
    # Calculate stats for all river areas
    stats <- combined$reduceRegion(
      reducer = ee$Reducer$mean()$combine(
        ee$Reducer$stdDev(), "", TRUE
      ),
      geometry = rivers_geometry,
      scale = 500,
      maxPixels = 1e7,
      tileScale = 8,
      bestEffort = TRUE
    )
    
    # Water-only stats
    water_only <- combined$updateMask(water_mask_500m$gt(0.5))
    water_stats <- water_only$select('NDAVI')$reduceRegion(
      reducer = ee$Reducer$mean(),
      geometry = rivers_geometry,
      scale = 500,
      maxPixels = 1e7,
      tileScale = 8,
      bestEffort = TRUE
    )
    
    # Count both segments and unique rivers
    n_segments <- rivers_in_cell$size()
    n_unique_rivers <- rivers_in_cell$aggregate_count_distinct('MAIN_RIV')
    
    return(cell$
             set('rv_ndavi_mn', stats$get('NDAVI_mean'))$
             set('rv_ndavi_sd', stats$get('NDAVI_stdDev'))$
             set('water_prop', stats$get('water_mask_mean'))$
             set('ndavi_wtr', water_stats$get('NDAVI'))$
             set('n_segments', n_segments)$           # Total river segments
             set('n_rivers', n_unique_rivers)         # Unique rivers
    )
  })
  
  return(results)
}

river_ndavi<- calculate_river_ndavi_richness(2024)

river_ndavi_task <- ee$batch$Export$table$toDrive(
  collection = river_ndavi,
  description = 'water_mask_river_ndavi_sr_2024',
  folder = 'EarthEngine_Exports',
  fileNamePrefix = 'water_mask_river_ndavi_sr_2024',
  fileFormat = 'SHP',
  selectors = list('Grid_ID', 'Fwplant_ri', 'latitude', 'longitude',
                   'rv_ndavi_mn', 'rv_ndavi_sd', 'water_prop', 
                   'ndavi_wtr', 'n_rivers', 'n_segments') 
)

river_ndavi_task $start()


## Lakes (NDAVI) -----------------------------------------------------------

# lakes_ee <- ee$FeatureCollection("projects/ee-rebekahchen24/assets/NDVI/Global_LAKES_polys_v10_shp")
# richness_ee <- ee$FeatureCollection("projects/ee-rebekahchen24/assets/NDVI/Global_fwplant_richness")
# 
# # Filter for cells with species
# richness_with_species <- richness_ee$filter(ee$Filter$gt("Fwplant_ri", 0))
# 
# # Create a test batch
# test_batch_ndavi <- richness_with_species$limit(50)
# 
# test_results_ndavi <- test_batch_ndavi$map(function(cell) {
#   # Find lakes in cell
#   lakes_in_cell <- lakes_ee$filterBounds(cell$geometry())
#   
#   # Calculate NDAVI for lakes in the cell
#   lakes_ndavi <- lakes_in_cell$geometry()$intersection(cell$geometry())
#   
#   ndavi_stats_lake <- annual_ndavi$select('NDAVI')$reduceRegion(
#     reducer = ee$Reducer$mean()$combine(
#       reducer2 = ee$Reducer$stdDev(),
#       sharedInputs = TRUE
#     ),
#     geometry = lakes_ndavi,
#     scale = 500,
#     maxPixels = 1e9
#   )
#   
#   unique_lakes <- lakes_in_cell$aggregate_count_distinct('Hylak_id')
#   
#   return(cell$set(list(
#     lk_ndavi_mn = ndavi_stats_lake$get('NDAVI_mean'),      
#     lk_ndavi_sd = ndavi_stats_lake$get('NDAVI_stdDev'),    
#     n_uniq_lk = unique_lakes                           
#   )))
# })
# 
# ndavi_results_data <- test_results_ndavi$getInfo()
# 
# ndavi_results_df <- do.call(rbind, lapply(ndavi_results_data$features, function(f) {
#   props <- f$properties
#   
#   data.frame(
#     Grid_ID = props$Grid_ID,
#     latitude = props$latitude,
#     longitude = props$longitude,
#     Fwplant_ri = props$Fwplant_ri,
#     lake_nadvi_mean = ifelse(is.null(props$lk_ndavi_mn), NA, props$lk_ndavi_mn),  
#     lake_nadvi_sd = ifelse(is.null(props$lk_ndavi_sd), NA, props$lk_ndavi_sd),    
#     n_unique_lakes = ifelse(is.null(props$n_uniq_lk), 0, props$n_uniq_lk),  
#     stringsAsFactors = FALSE
#   )
# }))
# 
# 
# 
# # Process all richness cells with NDAVI
# all_results_lake_ndavi <- richness_with_species$map(function(cell) {
#   # Find rivers in cell
#   lakes_in_cell <- lakes_ee$filterBounds(cell$geometry())
#   
#   # Calculate NDAVI for each river segment
#   lakes_ndavi <- lakes_in_cell$geometry()$intersection(cell$geometry())
#   
#   ndavi_stats_lake <- annual_ndavi$select('NDAVI')$reduceRegion(
#     reducer = ee$Reducer$mean()$combine(
#       reducer2 = ee$Reducer$stdDev(),
#       sharedInputs = TRUE
#     ),
#     geometry = lakes_ndavi,
#     scale = 500,
#     maxPixels = 1e9
#   )
#   
#   unique_lakes <- lakes_in_cell$size()
#   
#   return(cell$set(list(
#     lk_ndavi_mn = ndavi_stats_lake$get('NDAVI_mean'),      
#     lk_ndavi_sd = ndavi_stats_lake$get('NDAVI_stdDev'),    
#     n_uniq_lk = unique_lakes                           
#   )))
# })
# 
# # Export results
# export_task_ndavi <- ee$batch$Export$table$toDrive(
#   collection = all_results_lake_ndavi,
#   description = 'lake_ndavi_global_final',
#   folder = 'EarthEngine_Exports',
#   fileNamePrefix = 'global_lake_ndavi_richness_2024',
#   fileFormat = 'SHP'
# )
# 
# export_task_ndavi$start()
# print(paste("Export started! Task ID:",export_task_ndavi$id))


# New mathod with water mask

calculate_lake_ndavi_richness <- function(year = 2024) {
  
  # Calculate NDAVI
  annual_ndavi <- calculate_ndavi(paste0(year, '-01-01'), paste0(year, '-12-31'))
  
  # Resample water mask
  water_mask_500m <- water_mask$
    reduceResolution(reducer = ee$Reducer$mean(), maxPixels = 20)$
    reproject(crs = "EPSG:4326", scale = 500)
  
  combined <- annual_ndavi$addBands(water_mask_500m$rename('water_mask'))
  
  # Process richness cells directly
  richness_with_species <- richness_ee$filter(ee$Filter$gt("Fwplant_ri", 0))
  
  results <- richness_with_species$map(function(cell) {
    
    # Find lakes in cell
    lakes_in_cell <- lakes_ee$filterBounds(cell$geometry())
    
    # Get combined lake geometry
    lakes_geometry <- lakes_in_cell$geometry()$intersection(cell$geometry())
    
    # Calculate stats for all lake areas at once
    stats <- combined$reduceRegion(
      reducer = ee$Reducer$mean()$combine(
        ee$Reducer$stdDev(), "", TRUE
      ),
      geometry = lakes_geometry,
      scale = 500,
      maxPixels = 1e7,
      tileScale = 8,
      bestEffort = TRUE
    )
    
    # Water-only stats
    water_only <- combined$updateMask(water_mask_500m$gt(0.5))
    water_stats <- water_only$select('NDAVI')$reduceRegion(
      reducer = ee$Reducer$mean(),
      geometry = lakes_geometry,
      scale = 500,
      maxPixels = 1e7,
      tileScale = 8,
      bestEffort = TRUE
    )
    
    return(cell$
             set('lk_ndavi_mn', stats$get('NDAVI_mean'))$
             set('lk_ndavi_sd', stats$get('NDAVI_stdDev'))$
             set('water_prop', stats$get('water_mask_mean'))$
             set('ndavi_wtr', water_stats$get('NDAVI'))$
             set('n_lakes', lakes_in_cell$size())
    )
  })
  
  return(results)
}

lake_nadvi <- calculate_lake_ndavi_richness(2024)

# Export with higher tileScale
lake_ndavi_task <- ee$batch$Export$table$toDrive(
  collection = lake_nadvi,
  description = 'lake_richness_direct_ndavi_2024',
  folder = 'EarthEngine_Exports',
  fileNamePrefix = 'richness_lake_ndavi_direct_2024',
  fileFormat = 'SHP',
  selectors = list('Grid_ID', 'Fwplant_ri', 'latitude', 'longitude',
                   'lk_ndavi_mn', 'lk_ndavi_sd', 'water_prop', 
                   'ndavi_wtr', 'n_lakes')  # Specify fields to export
)

lake_ndavi_task$start()


#Plotting ----------------------------------------------------------------

# Load datasets
river_ndvi_wm <- st_read("/Users/rebekahchen/Desktop/NDVI/water_mask_river_ndvi_sr_2024")
river_ndavi_wm <- st_read("/Users/rebekahchen/Desktop/NDVI/water_mask_river_ndavi_sr_2024")
lake_ndvi_wm <- st_read("/Users/rebekahchen/Desktop/NDVI/water_mask_lake_ndvi_sr_2024")
lake_ndavi_wm <- st_read("/Users/rebekahchen/Desktop/NDVI/water_mask_lake_ndavi_sr_2024")

names(river_ndvi_wm)

world <- ne_countries(scale = "medium", returnclass = "sf")

world_regions <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(continent, subregion)

st_crs(world_regions)  # make usre the projection aligns



## Lake NDVI ---------------------------------------------------------------

lake_ndvi_wm <- st_read("/Users/rebekahchen/Desktop/NDVI/water_mask_lake_ndvi_sr_2024")

names(lake_ndvi_wm)

# Load required packages
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# Read lake NDVI data
lake_ndvi_wm <- st_read("/Users/rebekahchen/Desktop/NDVI/water_mask_river_ndvi_sr_2024")

ggplot() +
  # First layer: world map background
  geom_sf(data = world, fill = "lightgray", color = "white", size = 0.1) +
  # Second layer: lake NDVI data
  geom_sf(data = lake_ndvi_wm, aes(color = ndvi_wtr), size = 1) +
  scale_color_gradient(
    low = "white",
    high = "#228B22",
    name = "Mean Lake NDVI with Water Mask"
  ) +
  theme_minimal() +
  labs(title = "Global Lake NDVI Distribution within Freshwater Species Richness Grid")


lake_ndvi_continents <- lake_ndvi_wm %>%
  # Join with world regions to get continent info for each lake
  st_join(world_regions) %>%
  # Remove spatial geometry to work with regular data
  st_drop_geometry() %>%
  # Group by continent
  group_by(continent) %>%
  # Calculate summary statistics for each continent
  summarise(
    # Mean and median NDVI with water mask
    mean_ndvi_water = mean(ndvi_wtr, na.rm = TRUE),
    median_ndvi_water = median(ndvi_wtr, na.rm = TRUE),
    
    # Mean and median NDVI without water mask (lake-specific column)
    mean_ndvi = mean(lk_ndvi_mn, na.rm = TRUE),
    median_ndvi = median(lk_ndvi_mn, na.rm = TRUE),
    
    # Standard deviation (measures how spread out the values are)
    sd_ndvi = sd(lk_ndvi_mn, na.rm = TRUE),
    
    # Total number of lakes in each continent
    total_lakes = sum(n_lakes, na.rm = TRUE),
    
    # Count of missing values
    n_missing = sum(is.na(lk_ndvi_mn)),
    
    .groups = 'drop'
  ) %>%
  # Remove ocean data (not relevant for lake analysis)
  filter(continent != "Seven seas (open ocean)")


lake_ndvi_long <- lake_ndvi_continents %>%
  pivot_longer(
    cols = c(mean_ndvi_water, mean_ndvi, median_ndvi_water, median_ndvi),
    names_to = "metric_type",
    values_to = "value"
  ) %>%
  # Create clean labels for our plots
  mutate(
    stat_type = case_when(
      str_detect(metric_type, "mean") ~ "Mean",
      str_detect(metric_type, "median") ~ "Median"
    ),
    water_mask = case_when(
      str_detect(metric_type, "_water") ~ "With Water Mask",
      TRUE ~ "Without Water Mask"
    ),
    # Combine for clean facet labels
    facet_label = paste(stat_type, "-", water_mask)
  )


# Order continents by their NDVI values
continent_order <- lake_ndvi_continents %>%
  arrange(desc(mean_ndvi_water)) %>%
  pull(continent)


lake_ndvi_long %>%
  mutate(continent = factor(continent, levels = continent_order)) %>%
  ggplot(aes(x = continent, y = value)) +
  geom_bar(stat = "identity", fill = "darkblue") +  # Changed to blue for lakes
  # Create separate panels for each metric type
  facet_wrap(~ facet_label, scales = "free_y") +  
  labs(
    title = "Annual Lake NDVI by Continent",
    subtitle = "Continents ordered by mean Lake NDVI with water mask", 
    x = "Continent",
    y = "Lake NDVI Value"
  ) +
  theme_minimal() +
  # Rotate continent names for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


continent_stats <- lake_ndvi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  group_by(continent) %>%
  summarise(
    mean_ndvi = mean(lk_ndvi_mn, na.rm = TRUE),
    mean_ndvi_water = mean(ndvi_wtr, na.rm = TRUE),
    sd_ndvi = sd(lk_ndvi_mn, na.rm = TRUE),
    n_cells = n(),
    total_lakes = sum(n_lakes, na.rm = TRUE)
  ) %>%
  filter(continent != "Seven seas (open ocean)")


lake_ndvi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  filter(continent != "Seven seas (open ocean)") %>%
  ggplot(aes(x = continent, y = ndvi_wtr)) +
  geom_boxplot(fill = "lightblue") +  # Light blue fill for lakes
  # Add individual points (slightly spread out to avoid overlap)
  geom_jitter(alpha = 0.1, color = "darkblue") +
  labs(
    title = "Lake NDVI Distribution by Continent",
    x = "Continent",
    y = "Lake NDVI (with water mask)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


lake_ndvi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  filter(continent != "Seven seas (open ocean)") %>%
  ggplot(aes(x = lk_ndvi_mn, y = ndvi_wtr)) +
  # Scatter plot with semi-transparent points
  geom_point(alpha = 0.5, color = "darkblue") +
  # Add trend line (red line shows the relationship)
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  # Separate panel for each continent
  facet_wrap(~ continent, scales = "free") +
  labs(
    title = "Lake NDVI Comparison: Water Mask vs No Water Mask by Continent",
    x = "Lake NDVI (no water mask)",
    y = "Lake NDVI (with water mask)"
  ) +
  theme_minimal()


lake_ndvi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  filter(continent != "Seven seas (open ocean)") %>%
  group_by(continent) %>%
  mutate(index = row_number()) %>%
  ggplot(aes(x = Fwplant_ri, y = log(1 + ndvi_wtr))) +
  geom_point(alpha = 0.5, color = "darkblue") +
  facet_wrap(~ continent, scales = "free") +
  labs(
    title = "Lake NDVI vs Freshwater Plant Species Richness by Continent",
    x = "Freshwater Plant Species Richness",
    y = "Log-transformed Lake NDVI (with water mask)"
  ) +
  theme_minimal()


## Lake NDAVI ---------------------------------------------------------------

ggplot() +
  # First layer: world map
  geom_sf(data = world, fill = "lightgray", color = "white", size = 0.1) +
  # Second layer: lake NDAVI
  geom_sf(data = lake_ndavi_wm, aes(color = ndavi_wtr), size = 1) +
  scale_color_gradient(
    low = "white",
    high = "#228B22",
    name = "mean NDAVI with water mask"
  ) +
  theme_minimal() +
  labs(title = "Global Lake NDAVI Distribution within Freshwater Species Richness Grid")


lake_ndavi_continents <- lake_ndavi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  group_by(continent) %>%
  summarise(
    mean_ndavi_water = mean(ndavi_wtr, na.rm = TRUE),
    mean_ndavi = mean(lk_ndavi_m, na.rm = TRUE),
    median_ndavi_water = median(ndavi_wtr, na.rm= TRUE),
    median_ndavi = median(lk_ndavi_m, na.rm = TRUE),
    sd_ndvi = sd(lk_ndavi_m, na.rm = TRUE),
    total_lakes = sum(n_lakes, na.rm = TRUE),
    n_missing = sum(is.na(lk_ndavi_m)),
    .groups = 'drop'
  ) %>%
  filter(continent != "Seven seas (open ocean)")



lake_ndavi_long <- lake_ndavi_continents %>%
  pivot_longer(
    cols = c(mean_ndavi_water, mean_ndavi, median_ndavi_water, median_ndavi),
    names_to = "metric_type",
    values_to = "value"
  ) %>%
  mutate(
    stat_type = case_when(
      str_detect(metric_type, "mean") ~ "Mean",
      str_detect(metric_type, "median") ~ "Median"
    ),
    water_mask = case_when(
      str_detect(metric_type, "_water") ~ "With Water Mask",
      TRUE ~ "Without Water Mask"
    ),
    # Create combined labels for facets
    facet_label = paste(stat_type, "-", water_mask)
  )


continent_order <- lake_ndavi_continents %>%
  arrange(desc(median_ndavi_water)) %>%
  pull(continent)

# Plot with consistent ordering
lake_ndavi_long %>%
  mutate(continent = factor(continent, levels = continent_order)) %>%
  ggplot(aes(x = continent, y = value)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  facet_wrap(~ facet_label, scales = "free_y") +  # Only y-axis varies
  labs(
    title = "Annual Lake NDAVI by Continent",
    subtitle = "Continents ordered by median NDAVI with water mask",
    x = "Continent",
    y = "NDAVI Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


continent_stats <- lake_ndavi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  group_by(continent) %>%
  summarise(
    mean_ndvi = mean(lk_ndavi_m, na.rm = TRUE),
    mean_ndvi_water = mean(ndavi_wtr, na.rm = TRUE),
    sd_ndvi = sd(lk_ndavi_m, na.rm = TRUE),
    n_cells = n(),
    total_lakes = sum(n_lakes, na.rm = TRUE)
  ) %>%
  filter(continent != "Seven seas (open ocean)")


lake_ndvi_wm%>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  group_by(continent) %>%
  filter(continent != "Seven seas (open ocean)") %>% 
  ggplot(aes(x = continent, y = ndvi_wtr)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1)


lake_ndavi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  filter(continent != "Seven seas (open ocean)") %>%
  # Create an index for spreading points horizontally
  group_by(continent) %>%
  mutate(index = row_number()) %>%
  ggplot(aes(x = Fwplant_ri, y = log(1+ndavi_wtr))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ continent, scales = "free") +
  labs(
    title = "NDVI Distribution by Continent",
    x =  "Species Richness (within continent)",
    y = "NDAVI Water"
  ) +
  theme_minimal()

lake_ndavi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  filter(continent != "Seven seas (open ocean)") %>%
  ggplot(aes(x = lk_ndavi_m, y = ndavi_wtr)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add trend line
  facet_wrap(~ continent, scales = "free") +
  labs(
    title = "NDAVI Comparison: Water Mask vs No Water Mask by Continent",
    x = "Lake NDAVI (no water mask)",
    y = "NDAVI (with water mask)"
  ) +
  theme_minimal()


## River NDVI --------------------------------------------------------------

river_ndvi_wm <- st_read("/Users/rebekahchen/Desktop/NDVI/water_mask_river_ndvi_sr_2024")

ggplot() +
  # First layer: world map
  geom_sf(data = world, fill = "lightgray", color = "white", size = 0.1) +
  # Second layer: river NDVI
  geom_sf(data = river_ndvi_wm, aes(color = ndvi_wtr), size = 1) +
  scale_color_gradient(
    low = "white",
    high = "#228B22",
    name = "mean River NDVI with water mask"
  ) +
  theme_minimal() +
  labs(title = "Global River NDVI Distribution within Freshwater Species Richness Grid")


# Continential summaries
river_ndvi_continents <- river_ndvi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  group_by(continent) %>%
  summarise(
    mean_ndvi_water = mean(ndvi_wtr, na.rm = TRUE),
    mean_ndvi = mean(rv_ndvi_mn, na.rm = TRUE),             
    median_ndvi_water = median(ndvi_wtr, na.rm= TRUE),
    median_ndvi = median(rv_ndvi_mn, na.rm = TRUE),       
    sd_ndvi = sd(rv_ndvi_mn, na.rm = TRUE),                 
    total_rivers = sum(n_rivers, na.rm = TRUE),             
    n_missing = sum(is.na(rv_ndvi_mn)),                     
    .groups = 'drop'
  ) %>%
  filter(continent != "Seven seas (open ocean)")


river_ndvi_long <- river_ndvi_continents %>%
  pivot_longer(
    cols = c(mean_ndvi_water, mean_ndvi, median_ndvi_water, median_ndvi),
    names_to = "metric_type",
    values_to = "value"
  ) %>%
  # Create cleaner labels for faceting
  mutate(
    stat_type = case_when(
      str_detect(metric_type, "mean") ~ "Mean",
      str_detect(metric_type, "median") ~ "Median"
    ),
    water_mask = case_when(
      str_detect(metric_type, "_water") ~ "With Water Mask",
      TRUE ~ "Without Water Mask"
    ),
    # Create combined labels for facets
    facet_label = paste(stat_type, "-", water_mask)
  )


continent_order <- river_ndvi_continents %>%
  arrange(desc(mean_ndvi_water)) %>%
  pull(continent)


river_ndvi_long %>%
  mutate(continent = factor(continent, levels = continent_order)) %>%
  ggplot(aes(x = continent, y = value)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  facet_wrap(~ facet_label, scales = "free_y") +  
  labs(
    title = "Annual River NDVI by Continent",                
    subtitle = "Continents ordered by mean River NDVI with water mask", 
    x = "Continent",
    y = "River NDVI Value"                           
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



continent_stats <- river_ndvi_wm %>%                         
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  group_by(continent) %>%
  summarise(
    mean_ndvi = mean(rv_ndvi_mn, na.rm = TRUE),             
    mean_ndvi_water = mean(ndvi_wtr, na.rm = TRUE),          
    sd_ndvi = sd(rv_ndvi_mn, na.rm = TRUE),                 
    n_cells = n(),
    total_rivers = sum(n_rivers, na.rm = TRUE)          
  ) %>%
  filter(continent != "Seven seas (open ocean)")



river_ndvi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  group_by(continent) %>%
  filter(continent != "Seven seas (open ocean)") %>% 
  ggplot(aes(x = continent, y = ndvi_wtr)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1) +
  labs(title = "River NDVI Distribution by Continent") +    
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



river_ndvi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  filter(continent != "Seven seas (open ocean)") %>%
  ggplot(aes(x = rv_ndvi_mn, y = ndvi_wtr)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  facet_wrap(~ continent, scales = "free") +
  labs(
    title = "River NDVI Comparison: Water Mask vs No Water Mask by Continent",
    x = "River NDVI (no water mask)",                         
    y = "River NDVI (with water mask)"                        
  ) +
  theme_minimal()



river_ndvi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  filter(continent != "Seven seas (open ocean)") %>%
  # Create an index for spreading points horizontally
  group_by(continent) %>%
  mutate(index = row_number()) %>%
  ggplot(aes(x = Fwplant_ri, y = log(1+ndvi_wtr))) +        
  geom_point(alpha = 0.5) +
  facet_wrap(~ continent, scales = "free") +
  labs(
    title = "River NDVI Distribution by Continent",           
    x = "Species Richness (within continent)",
    y = "River NDVI Water"                                  
  ) +
  theme_minimal()


## River NDAVI -------------------------------------------------------------

river_ndavi_wm <- st_read("/Users/rebekahchen/Desktop/NDVI/water_mask_river_ndavi_sr_2024")

names(river_ndavi_wm)


ggplot() +
  # First layer: world map background
  geom_sf(data = world, fill = "lightgray", color = "white", size = 0.1) +
  # Second layer: river NDVI data
  geom_sf(data = river_ndavi_wm, aes(color = ndavi_wtr), size = 1) +
  # Color scale: white = low vegetation, forest green = high vegetation
  scale_color_gradient(
    low = "white",
    high = "#228B22",
    name = "Mean River NDVI with Water Mask"
  ) +
  theme_minimal() +
  labs(title = "Global River NDAVI Distribution within Freshwater Species Richness Grid")


river_ndavi_continents <- river_ndavi_wm %>%
  # Join with world regions to get continent info for each river
  st_join(world_regions) %>%
  # Remove spatial geometry to work with regular data
  st_drop_geometry() %>%
  # Group by continent (like sorting into piles)
  group_by(continent) %>%
  # Calculate summary statistics for each continent
  summarise(
    mean_ndavi_water = mean(ndavi_wtr, na.rm = TRUE),
    median_ndavi_water = median(ndavi_wtr, na.rm = TRUE),
    mean_ndavi = mean(rv_ndavi_m, na.rm = TRUE),
    median_ndavi = median(rv_ndavi_m, na.rm = TRUE),
    sd_ndavi = sd(rv_ndavi_m, na.rm = TRUE),
    total_rivers = sum(n_rivers, na.rm = TRUE),
    n_missing = sum(is.na(rv_ndavi_m)),
    
    .groups = 'drop'
  ) %>%
  filter(continent != "Seven seas (open ocean)")



river_ndavi_long <- river_ndavi_continents %>%
  pivot_longer(
    cols = c(mean_ndavi_water, mean_ndavi, median_ndavi_water, median_ndavi),
    names_to = "metric_type",
    values_to = "value"
  ) %>%
  mutate(
    # Determine if it's mean or median
    stat_type = case_when(
      str_detect(metric_type, "mean") ~ "Mean",
      str_detect(metric_type, "median") ~ "Median"
    ),
    water_mask = case_when(
      str_detect(metric_type, "_water") ~ "With Water Mask",
      TRUE ~ "Without Water Mask"
    ),
    facet_label = paste(stat_type, "-", water_mask)
  )


continent_order <- river_ndavi_continents %>%
  arrange(desc(mean_ndavi_water)) %>%
  pull(continent)


river_ndavi_long %>%
  mutate(continent = factor(continent, levels = continent_order)) %>%
  ggplot(aes(x = continent, y = value)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  facet_wrap(~ facet_label, scales = "free_y") +  
  labs(
    title = "Annual River NDVI by Continent",
    subtitle = "Continents ordered by mean River NDVI with water mask", 
    x = "Continent",
    y = "River NDVI Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


continent_stats <- river_ndavi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  group_by(continent) %>%
  summarise(
    mean_ndavi = mean(rv_ndavi_m, na.rm = TRUE),
    mean_ndavi_water = mean(ndavi_wtr, na.rm = TRUE),
    sd_ndavi = sd(rv_ndavi_m, na.rm = TRUE),
    # Count of grid cells (sample size)
    n_cells = n(),
    # Total rivers across all cells in continent
    total_rivers = sum(n_rivers, na.rm = TRUE)
  ) %>%
  filter(continent != "Seven seas (open ocean)")



river_ndavi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  filter(continent != "Seven seas (open ocean)") %>%
  ggplot(aes(x = continent, y = ndavi_wtr)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1) +
  labs(title = "River NDVI Distribution by Continent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



river_ndavi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  filter(continent != "Seven seas (open ocean)") %>%
  ggplot(aes(x = rv_ndavi_m, y = ndavi_wtr)) +
  geom_point(alpha = 0.5) +
  # Add trend line
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  # Separate panel for each continent
  facet_wrap(~ continent, scales = "free") +
  labs(
    title = "River NDVI Comparison: Water Mask vs No Water Mask by Continent",
    x = "River NDAVI (no water mask)",
    y = "River NDAVI (with water mask)"
  ) +
  theme_minimal()


river_ndavi_wm %>%
  st_join(world_regions) %>%
  st_drop_geometry() %>%
  filter(continent != "Seven seas (open ocean)") %>%
  group_by(continent) %>%
  mutate(index = row_number()) %>%
  ggplot(aes(x = Fwplant_ri, y = log(1 + ndavi_wtr))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ continent, scales = "free") +
  labs(
    title = "River NDVI vs Freshwater Plant Species Richness by Continent",
    x = "Freshwater Plant Species Richness",
    y = "Log-transformed River NDAVI (with water mask)"
  ) +
  theme_minimal()



