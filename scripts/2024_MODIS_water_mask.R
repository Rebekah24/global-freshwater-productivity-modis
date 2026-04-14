# Load libraries
library(terra)
library(sf)
library(dplyr)
library(stringr)

# Get unique tiles only
hdf_files <- list.files("MOD44W_061-20250819_153851", 
                        pattern = "\\.hdf$", 
                        full.names = TRUE, 
                        recursive = TRUE)

print(basename(hdf_files[1:5]))

filenames <- basename(hdf_files)
tile_ids <- str_extract(filenames, pattern = "h[0-9][0-9]v[0-9][0-9]")
print(tile_ids)

# Create dataframe to inspect
df <- data.frame(
  file = hdf_files,
  tile = tile_ids,
  filename = basename(hdf_files),
  stringsAsFactors = FALSE
)


df_unique <- df %>%
  group_by(tile) %>%
  slice(1) %>%  # Keep first occurrence of each tile
  ungroup()

nrow(df_unique)# 318 tiles, but MODIS should give 460 non-fill tiles

unique_file_paths <- df_unique$file

# Create the global mosaic (this takes a long time but only needs to be done once)
create_global_water_mask <- function(hdf_files, output_file = "MOD44W_2024_global.tif") {
  
  cat(sprintf("Found %d HDF files to merge\n", length(hdf_files)))
  
  # Process in batches to manage memory
  batch_size <- 50
  n_batches <- ceiling(length(hdf_files) / batch_size)
  temp_files <- character()
  
  for(batch in 1:n_batches) {
    cat(sprintf("\nBatch %d/%d\n", batch, n_batches))
    
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, length(hdf_files)) # This is so smart!
    batch_files <- hdf_files[start_idx:end_idx]
    
    # Read all rasters in batch
    batch_rasters <- list()
    for(i in seq_along(batch_files)) {
      if(i %% 10 == 0) cat("nothing looking good") # just in case the script run forever, it helps to know something is going wrong by printing "not looking good"
      r <- rast(batch_files[i], subds = 1)  # get water mask values (0 or 1)
      batch_rasters[[i]] <- r
    }
    
    # merge this batch (mosaic is the equivalent function for merge in this case)
    batch_mosaic <- mosaic(sprc(batch_rasters), fun = "first")
    
    # Save temporary file
    temp_file <- tempfile(pattern = sprintf("batch_%03d_", batch), fileext = ".tif")
    writeRaster(batch_mosaic, temp_file, 
                datatype = "INT1U",
                gdal = c("COMPRESS=LZW", "TILED=YES"))
    temp_files <- c(temp_files, temp_file)
    
    cat(" done\n")
    rm(batch_rasters, batch_mosaic)
    gc(verbose = FALSE)
  }
  
  # Final global water mask
  cat("\nCreating final global water mask...")
  temp_rasters <- lapply(temp_files, rast)
  global_mosaic <- mosaic(sprc(temp_rasters), fun = "first")
  
  # Save final global water mask
  writeRaster(global_mosaic, output_file,
              datatype = "INT1U",
              gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
              overwrite = TRUE)
  
  # Remove the temporary file 
  file.remove(temp_files)
  
  cat(sprintf("\n\nGlobal water mask saved as: %s\n", output_file))
  
  return(global_mosaic)
}

river_ndvi_sf <- st_read("/Users/rebekahchen/Desktop/NDVI/global_river_ndvi_sr_2024")
hdf_folder <- "/Users/rebekahchen/Desktop/NDVI/MOD44W_061-20250819_153851"


global_water_mask <- create_global_water_mask(unique_file_paths, "MOD44W_2024_global_clean.tif")




