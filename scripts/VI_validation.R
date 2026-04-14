# Load required libraries
library(pointblank)
library(tidyverse)
library(sf)
library(here)
library(gt)  # For nicer table outputs

# Load data -----------------------------------------------------------------------------
river_ndavi <- st_read("water_mask_river_ndavi_sr_2024")
river_ndvi <- st_read("water_mask_river_ndvi_sr_2024")

lake_ndavi <- st_read("water_mask_lake_ndavi_sr_2024")
lake_ndvi <- st_read("water_mask_lake_ndvi_sr_2024")

 
# lake_ndvi<- lake_ndvi %>%
#    rename(c("latitude" = "longitude", "longitude" = "latitude"))
# 
# names(lake_ndvi)
# 
# # Fix abbreviated names and reorder
# lake_ndvi <- lake_ndvi%>%
# 
#    select(
#      # Core identification
#     Grid_ID,
#     latitude,
#     longitude,
# 
#     # Species data
#     Fwplant_ri,
# 
#     # NDAVI metrics (grouped together)
#     lk_ndvi_mn,
#     lk_ndvi_sd,
#     ndvi_wtr,
# 
#     # Environmental metrics
#     water_prop,
# 
#     # River network metrics
#     n_lakes
#   )
# 
# # Check the new order
# names(lake_ndvi)

# # Save corrected shapefile
#st_write(lake_ndvi, "water_mask_lake_ndvi_sr_2024_fixed.shp")




# RIVERS ------------------------------------------------------------------

# Convert to regular dataframe for validation
river_ndavi_df <- river_ndavi %>% st_drop_geometry()
river_ndvi_df <- river_ndvi %>% st_drop_geometry()

## Create data dictionary --------------------------------------------------

create_river_data_dictionary<- function(data, index_type = NULL) {
  
  # Auto-detect index type
  if(is.null(index_type)) {
    if("rv_ndavi_mn" %in% names(data)) {
      index_type <- "NDAVI"
    } else if("rv_ndvi_mn" %in% names(data)) {
      index_type <- "NDVI"
    } else {
      stop("Cannot determine index type")
    }
  }
  
  # Create informant with dynamic column selection
  data_dict <- create_informant(
    tbl = data,
    label = paste("River", index_type, "Dataset (2024)")
  ) %>%
    info_tabular(
      description = paste("Annual", index_type, "vegetation indices for global river segment")
    )
  
  # Document all columns that exist in the data
  column_descriptions <- list(
    Grid_ID = "Unique grid cell identifier",
    Fwplant_ri = "Freshwater plant species richness",
    latitude = "Latitude (WGS84)",
    longitude = "Longitude (WGS84)",
    rv_ndavi_m = "Mean NDAVI across river pixels",
    rv_ndavi_s = "NDAVI standard deviation",
    rv_ndvi_mn = "Mean NDVI across river pixels",
    rv_ndvi_sd = "NDVI standard deviation",
    water_prop = "Water coverage proportion",
    ndavi_wtr = "NDAVI for water pixels",
    ndvi_wtr = "NDVI for water pixels",
    n_rivers = "Unique river count",
    n_segment = "River segment count",
    Ecorgn = "Ecoregion name",
    MHT_ID = "Major Habitat Type identifier",
    MHT_Name = "Major Habitat Type name"
  )
  
  # Add info for each column that exists
  for(col_name in names(data)) {
    if(col_name %in% names(column_descriptions)) {
      # Use all_of() instead of vars() for string-based selection
      data_dict <- data_dict %>%
        info_columns(
          columns = all_of(col_name),
          `Description` = column_descriptions[[col_name]]
        )
    }
  }
  
  return(data_dict)
}


# For NDAVI data
river_ndavi_dictionary <- create_river_data_dictionary(river_ndavi_df, "NDAVI")

# For NDVI data  
river_ndvi_dictionary <- create_river_data_dictionary(river_ndvi_df, "NDVI")

# Get the report
river_ndvi_dictionary %>% 
  get_informant_report(title = "Global Rivers NDVI Dataset Documentation")

river_ndavi_dictionary %>% 
  get_informant_report(title = "Global Rivers NDAVI Dataset Documentation")




## Create VALIDATION AGENT-------------------------------------------------

create_river_validation_agent <- function(data, index_type = "NDAVI") {
  
  # Determine column names based on index type
  index_mean_col <- if(index_type == "NDAVI") "rv_ndavi_m" else "rv_ndvi_mn"
  index_sd_col <- if(index_type == "NDAVI") "rv_ndavi_s" else "rv_ndvi_sd"
  index_water_col <- if(index_type == "NDAVI") "ndavi_wtr" else "ndvi_wtr"
  
  agent <- create_agent(
    tbl = data,
    label = paste("River", index_type, "Validation"),
    actions = action_levels(
      warn_at = 0.05,    # Warn if >5% of rows fail
      stop_at = 0.10     # Stop if >10% of rows fail
    )
  ) %>%
  # Check for unique Grid IDs
  rows_distinct(
    columns = vars(Grid_ID),
    brief = "Grid IDs must be unique"
  ) %>%
    
    # Ensure all required columns exist
    col_exists(
      columns = vars(Grid_ID, Fwplant_ri, latitude, longitude, water_prop, n_rivers, n_segment),
      brief = "Core columns must be present"
    ) %>%
    
    # Check for complete cases in critical columns
    col_vals_not_null(
      columns = vars(Grid_ID, Fwplant_ri, latitude, longitude),
      brief = "Core identifying fields cannot be NULL"
    ) %>%
  
  # Validate latitude bounds (assuming global coverage)
  col_vals_between(
    columns = vars(latitude),
    left = -90, 
    right = 90,
    brief = "Latitude must be between -90 and 90"
  ) %>%
    
    # Validate longitude bounds
    col_vals_between(
      columns = vars(longitude),
      left = -180, 
      right = 180,
      brief = "Longitude must be between -180 and 180"
    ) %>%
  
  # Water proportion must be between 0 and 1
  col_vals_between(
    columns = vars(water_prop),
    left = 0, 
    right = 1, 
    inclusive = c(TRUE, TRUE),
    brief = "Water proportion must be between 0 and 1"
  ) %>%
  
  # Species richness must be non-negative
  col_vals_gte(
    columns = vars(Fwplant_ri),
    value = 0,
    brief = "Species richness cannot be negative"
  ) %>%
  
    
    # River counts must be positive integers
    col_vals_gte(
      columns = vars(n_rivers),
      value = 0,
      brief = "River count cannot be negative"
    ) %>%
    
    col_vals_gte(
      columns = vars(n_segment),
      value = 0,
      brief = "Segment count cannot be negative"
    )
  
  # Add index-specific validations only if columns exist
  if(index_mean_col %in% names(data)) {
    agent <- agent %>%
      col_vals_between(
        columns = vars(!!sym(index_mean_col)),
        left = -1, 
        right = 1,
        brief = paste(index_type, "mean must be between -1 and 1")
      )
  }
  
  if(index_water_col %in% names(data)) {
    agent <- agent %>%
      col_vals_between(
        columns = vars(!!sym(index_water_col)),
        left = -1, 
        right = 1,
        brief = paste(index_type, "water values must be between -1 and 1")
      )
  }
  
  if(index_sd_col %in% names(data)) {
    agent <- agent %>%
      col_vals_gte(
        columns = vars(!!sym(index_sd_col)),
        value = 0,
        brief = "Standard deviation cannot be negative"
      ) %>%
      col_vals_lte(
        columns = vars(!!sym(index_sd_col)),
        value = 1,
        brief = "Standard deviation should not exceed theoretical maximum"
      )
  }
  
  return(agent)
}



## Run the validation ------------------------------------------------------

river_ndavi_agent <- create_river_validation_agent (river_ndavi_df, "NDAVI") %>%
  interrogate()

river_ndvi_agent <- create_river_validation_agent (river_ndvi_df, "NDVI") %>%
  interrogate()

## Validation results ---------------------------------------------------------

river_ndavi_report <- get_agent_report(
  river_ndavi_agent,
  title = "NDAVI Dataset Validation Report",
  arrange_by = "severity"  # Show most critical issues first
)

river_ndavi_report

river_ndvi_report <- get_agent_report(
  river_ndvi_agent,
  title = "NDVI Dataset Validation Report",
  arrange_by = "severity"  # Show most critical issues first
)

river_ndvi_report



# Create a summary table ------------------------------------------------

create_river_summary <- function(data, index_type) {
  
  # Define index columns based on type
  index_cols <- if(index_type == "NDAVI") {
    c("rv_ndavi_m", "rv_ndavi_s", "ndavi_wtr")
  } else {
    c("rv_ndvi_mn", "rv_ndvi_sd", "ndvi_wtr")
  }
  
  # Check which columns actually exist in the data
  existing_cols <- index_cols[index_cols %in% names(data)]
  
  # Add other columns that should exist
  other_cols <- c("water_prop", "Fwplant_ri", "n_rivers", "n_segment")
  existing_other_cols <- other_cols[other_cols %in% names(data)]
  
  all_cols <- c(existing_cols, existing_other_cols)
  
  # Calculate summary statistics
  summary_stats <- data %>%
    select(all_of(all_cols)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    group_by(variable) %>%
    summarise(
      n_valid = sum(!is.na(value)),
      n_missing = sum(is.na(value)),
      min = min(value, na.rm = TRUE),
      mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE)
    ) %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  # Add expected ranges for validation - ONLY for the current index type
  if(index_type == "NDAVI") {
    expected_ranges <- tribble(
      ~variable, ~expected_min, ~expected_max, ~notes,
      "rv_ndavi_m", -1, 1, "Theoretical NDAVI range",
      "rv_ndavi_s", 0, 2, "SD for bounded variable",
      "ndavi_wtr", -1, 1, "Water-only NDAVI",
      "water_prop", 0, 1, "Proportion (0-100%)",
      "Fwplant_ri", 0, Inf, "Species count",
      "n_rivers", 0, Inf, "River count",
      "n_segment", 0, Inf, "Segment count"
    )
  } else {
    expected_ranges <- tribble(
      ~variable, ~expected_min, ~expected_max, ~notes,
      "rv_ndvi_mn", -1, 1, "Theoretical NDVI range",
      "rv_ndvi_sd", 0, 2, "SD for bounded variable",
      "ndvi_wtr", -1, 1, "Water-only NDVI",
      "water_prop", 0, 1, "Proportion (0-100%)",
      "Fwplant_ri", 0, Inf, "Species count",
      "n_rivers", 0, Inf, "River count",
      "n_segment", 0, Inf, "Segment count"
    )
  }
  
  # Join and validate
  validation_summary <- summary_stats %>%
    left_join(expected_ranges, by = "variable") %>%
    mutate(
      within_range = min >= expected_min & max <= expected_max
    )
  
  return(validation_summary)
}

# Generate data summary
river_ndavi_summary <- create_river_summary(river_ndavi_df, "NDAVI")
river_ndvi_summary <- create_river_summary(river_ndvi_df, "NDVI") 

# Data summary table
create_data_summary_table <- function(summary_data, index_type = "NDAVI") {
  
  summary_data %>%
    select(variable, n_valid, n_missing, min, max) %>%
    gt() %>%
    cols_label(
      variable = "Variable",
      n_valid = "N",
      n_missing = "Missing",
      min = "Min",
      max = "Max"
    ) %>%
    fmt_number(
      columns = c(min, max),
      decimals = 3
    ) %>%
    tab_style(
      style = cell_fill(color = "white"),
      locations = cells_body(
        columns = everything()
      )
    )
}

## Create the tables
river_ndavi_summary_table <- create_data_summary_table(river_ndavi_summary, "NDAVI")
river_ndavi_summary_table

river_ndvi_summary_table <- create_data_summary_table(river_ndvi_summary, "NDVI")
river_ndvi_summary_table



# LAKES -------------------------------------------------------------------

lake_ndavi_df <- lake_ndavi %>% st_drop_geometry()
lake_ndvi_df <- lake_ndvi %>% st_drop_geometry()


create_lake_data_dictionary <- function(data, index_type = NULL) {
  
  # Auto-detect index type
  if(is.null(index_type)) {
    if("lk_ndavi_mn" %in% names(data)) {
      index_type <- "NDAVI"
    } else if("lk_ndvi_mn" %in% names(data)) {
      index_type <- "NDVI"
    } else {
      stop("Cannot determine index type")
    }
  }
  
  # Create informant with dynamic column selection
  data_dict <- create_informant(
    tbl = data,
    label = paste("Lake", index_type, "Dataset (2024)")
  ) %>%
    info_tabular(
      description = paste("Annual", index_type, "vegetation indices for global lake segment")
    )
  
  # Document all columns that exist in the data
  # Updated for lake-specific columns
  column_descriptions <- list(
    Grid_ID = "Unique grid cell identifier",
    Fwplant_ri = "Freshwater plant species richness",
    latitude = "Latitude (WGS84)",
    longitude = "Longitude (WGS84)",
    lk_ndavi_mn = "Mean NDAVI across lake pixels",
    lk_ndavi_sd = "NDAVI standard deviation for lake pixels",
    lk_ndvi_mn = "Mean NDVI across lake pixels",
    lk_ndvi_sd = "NDVI standard deviation for lake pixels",
    water_prop = "Water coverage proportion",
    ndavi_wtr = "NDAVI for water pixels",
    ndvi_wtr = "NDVI for water pixels",
    n_lakes = "Number of unique lakes in grid cell",
    Ecorgn = "Ecoregion name",
    MHT_ID = "Major Habitat Type identifier",
    MHT_Name = "Major Habitat Type name"
  )
  
  # Add info for each column that exists
  for(col_name in names(data)) {
    if(col_name %in% names(column_descriptions)) {
      # Use all_of() instead of vars() for string-based selection
      data_dict <- data_dict %>%
        info_columns(
          columns = all_of(col_name),
          `Description` = column_descriptions[[col_name]]
        )
    }
  }
  
  return(data_dict)
}

# Create data dictionaries for lake data
lake_ndavi_dictionary <- create_lake_data_dictionary(lake_ndavi_df, "NDAVI")
lake_ndvi_dictionary <- create_lake_data_dictionary(lake_ndvi_df, "NDVI")

# Get the reports
lake_ndvi_dictionary %>% 
  get_informant_report(title = "Global Lakes NDVI Dataset Documentation")

lake_ndavi_dictionary %>% 
  get_informant_report(title = "Global Lakes NDAVI Dataset Documentation")


## Create VALIDATION AGENT -------------------------------------------------

create_lake_validation_agent <- function(data, index_type = "NDAVI") {
  
  # Determine column names based on index type (updated for lakes)
  index_mean_col <- if(index_type == "NDAVI") "lk_ndavi_mn" else "lk_ndvi_mn"
  index_sd_col <- if(index_type == "NDAVI") "lk_ndavi_sd" else "lk_ndvi_sd"
  index_water_col <- if(index_type == "NDAVI") "ndavi_wtr" else "ndvi_wtr"
  
  agent <- create_agent(
    tbl = data,
    label = paste("Lake", index_type, "Validation"),
    actions = action_levels(
      warn_at = 0.05,    # Warn if >5% of rows fail
      stop_at = 0.10     # Stop if >10% of rows fail
    )
  ) %>%
    # Check for unique Grid IDs
    rows_distinct(
      columns = vars(Grid_ID),
      brief = "Grid IDs must be unique"
    ) %>%
    
    # Ensure all required columns exist (updated for lakes)
    col_exists(
      columns = vars(Grid_ID, Fwplant_ri, latitude, longitude, water_prop, n_lakes),
      brief = "Core columns must be present"
    ) %>%
    
    # Check for complete cases in critical columns
    col_vals_not_null(
      columns = vars(Grid_ID, Fwplant_ri, latitude, longitude),
      brief = "Core identifying fields cannot be NULL"
    ) %>%
    
    # Validate latitude bounds (assuming global coverage)
    col_vals_between(
      columns = vars(latitude),
      left = -90, 
      right = 90,
      brief = "Latitude must be between -90 and 90"
    ) %>%
    
    # Validate longitude bounds
    col_vals_between(
      columns = vars(longitude),
      left = -180, 
      right = 180,
      brief = "Longitude must be between -180 and 180"
    ) %>%
    
    # Water proportion must be between 0 and 1
    col_vals_between(
      columns = vars(water_prop),
      left = 0, 
      right = 1, 
      inclusive = c(TRUE, TRUE),
      brief = "Water proportion must be between 0 and 1"
    ) %>%
    
    # Species richness must be non-negative
    col_vals_gte(
      columns = vars(Fwplant_ri),
      value = 0,
      brief = "Species richness cannot be negative"
    ) %>%
    
    # Lake counts must be positive integers
    col_vals_gte(
      columns = vars(n_lakes),
      value = 0,
      brief = "Lake count cannot be negative"
    )
  
  # Add index-specific validations only if columns exist
  if(index_mean_col %in% names(data)) {
    agent <- agent %>%
      col_vals_between(
        columns = vars(!!sym(index_mean_col)),
        left = -1, 
        right = 1,
        brief = paste(index_type, "mean must be between -1 and 1")
      )
  }
  
  if(index_water_col %in% names(data)) {
    agent <- agent %>%
      col_vals_between(
        columns = vars(!!sym(index_water_col)),
        left = -1, 
        right = 1,
        brief = paste(index_type, "water values must be between -1 and 1")
      )
  }
  
  if(index_sd_col %in% names(data)) {
    agent <- agent %>%
      col_vals_gte(
        columns = vars(!!sym(index_sd_col)),
        value = 0,
        brief = "Standard deviation cannot be negative"
      ) %>%
      col_vals_lte(
        columns = vars(!!sym(index_sd_col)),
        value = 1,
        brief = "Standard deviation should not exceed theoretical maximum"
      )
  }
  
  return(agent)
}


## Run the validation for lakes ------------------------------------------------------

lake_ndavi_agent <- create_lake_validation_agent(lake_ndavi_df, "NDAVI") %>%
  interrogate()

lake_ndvi_agent <- create_lake_validation_agent(lake_ndvi_df, "NDVI") %>%
  interrogate()

## Validation results ---------------------------------------------------------

lake_ndavi_report <- get_agent_report(
  lake_ndavi_agent,
  title = "Lake NDAVI Dataset Validation Report",
  arrange_by = "severity"  # Show most critical issues first
)

lake_ndavi_report

lake_ndvi_report <- get_agent_report(
  lake_ndvi_agent,
  title = "Lake NDVI Dataset Validation Report",
  arrange_by = "severity"  # Show most critical issues first
)

lake_ndvi_report



## Create a summary table for lakes ------------------------------------------------

create_lake_summary <- function(data, index_type) {
  
  # Updated column names for lakes
  index_cols <- if(index_type == "NDAVI") {
    c("lk_ndavi_m", "lk_ndavi_s", "ndavi_wtr")
  } else {
    c("lk_ndvi_mn", "lk_ndvi_sd", "ndvi_wtr")
  }
  
  # Check which columns actually exist in the data
  existing_cols <- index_cols[index_cols %in% names(data)]
  
  # Add other columns that should exist
  other_cols <- c("water_prop", "Fwplant_ri", "n_lakes")
  existing_other_cols <- other_cols[other_cols %in% names(data)]
  
  all_cols <- c(existing_cols, existing_other_cols)
  
  summary_stats <- data %>%
    select(all_of(all_cols)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    group_by(variable) %>%
    summarise(
      n_valid = sum(!is.na(value)),
      n_missing = sum(is.na(value)),
      min = min(value, na.rm = TRUE),
      mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE)
    ) %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  if(index_type == "NDAVI") {
    expected_ranges <- tribble(
      ~variable, ~expected_min, ~expected_max, ~notes,
      "lk_ndavi_m", -1, 1, "Theoretical NDAVI range",
      "lk_ndavi_s", 0, 1, "SD for bounded variable",
      "ndavi_wtr", -1, 1, "Water-only NDAVI",
      "water_prop", 0, 1, "Proportion (0-100%)",
      "Fwplant_ri", 0, Inf, "Species count",
      "n_lakes", 0, Inf, "Lake count"
    )
  } else {
    expected_ranges <- tribble(
      ~variable, ~expected_min, ~expected_max, ~notes,
      "lk_ndvi_mn", -1, 1, "Theoretical NDVI range",
      "lk_ndvi_sd", 0, 1, "SD for bounded variable",
      "ndvi_wtr", -1, 1, "Water-only NDVI",
      "water_prop", 0, 1, "Proportion (0-100%)",
      "Fwplant_ri", 0, Inf, "Species count",
      "n_lakes", 0, Inf, "Lake count"
    )
  }
  
  validation_summary <- summary_stats %>%
    left_join(expected_ranges, by = "variable") %>%
    mutate(
      within_range = min >= expected_min & max <= expected_max
    )
  
  return(validation_summary)
}

# Generate data summary for lakes
lake_ndavi_summary <- create_lake_summary(lake_ndavi_df, "NDAVI")
lake_ndvi_summary <- create_lake_summary(lake_ndvi_df, "NDVI")



# #Save as RData
# save(river_ndavi_dictionary,
#      river_ndvi_dictionary,
#      river_ndvi_report,
#      river_ndavi_report,
#      river_ndavi_summary,
#      river_ndvi_summary,
#      lake_ndavi_dictionary,
#      lake_ndvi_dictionary,
#      lake_ndvi_report,
#      lake_ndavi_report,
#      lake_ndavi_summary,
#      lake_ndvi_summary,
#      file = "validation_reports.RData")
# 
# load("validation_reports.RData")




