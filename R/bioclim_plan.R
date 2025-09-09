bioclim_plan <- list(

  # =============================================================================
  # BIOCLIM DATA EXTRACTION PLAN
  # =============================================================================
  # This plan downloads and extracts climate variables from two sources:
  # 1. WorldClim bioclim variables (19 standard variables)
  # 2. CHELSA bioclim+ variables (7 additional variables)
  #
  # WorldClim Variables (bio1-bio19):
  # - Annual mean temperature (°C)
  # - Mean diurnal range (°C) 
  # - Isothermality (%)
  # - Temperature seasonality (standard deviation ×100)
  # - Max temperature of warmest month (°C)
  # - Min temperature of coldest month (°C)
  # - Temperature annual range (°C)
  # - Mean temperature of wettest quarter (°C)
  # - Mean temperature of driest quarter (°C)
  # - Mean temperature of warmest quarter (°C)
  # - Mean temperature of coldest quarter (°C)
  # - Annual precipitation (mm)
  # - Precipitation of wettest month (mm)
  # - Precipitation of driest month (mm)
  # - Precipitation seasonality (coefficient of variation)
  # - Precipitation of wettest quarter (mm)
  # - Precipitation of driest quarter (mm)
  # - Precipitation of warmest quarter (mm)
  # - Precipitation of coldest quarter (mm)
  #
  # CHELSA Bioclim+ Variables:
  # - swb: Soil Water Balance (mm)
  # - gsl: Growing Season Length (days)
  # - gsp: Growing Season Precipitation (mm)
  # - gst: Growing Season Temperature (°C)
  # - pet_penman_mean: Potential Evapotranspiration - Penman method (mm/month)
  # - vpd_mean: Vapor Pressure Deficit (Pa)
  # - bio2: Mean Diurnal Range (°C) - CHELSA version
  # =============================================================================

  # download WorldClim data (file target - doesn't load into memory)
  tar_target(
    name = worldclim_files,
    command = {
      tryCatch(
        {
          bioclim_data <- geodata::worldclim_global(
            var = "bio",
            res = 0.5,
            path = "WorldClimData",
            download = TRUE
          )
          
          if (is.null(bioclim_data)) {
            stop("WorldClim data download failed")
          }
          
          # Return the file paths instead of the data
          list.files("WorldClimData/climate/wc2.1_30s", pattern = "wc2.1_30s_bio_.*tif$", full.names = TRUE)
        },
        error = function(e) {
          message("WorldClim download failed: ", e$message)
          return(character(0))
        }
      )
    },
    format = "file"
  ),

  # Extract bioclim variables at all coordinates using existing WorldClim data
  tar_target(
    name = bioclim_all_extracted,
    command = {
      if (length(worldclim_files) == 0) {
        return(NULL)
      }
      
      # Get coordinates from all_coordinates
      coords_all <- all_coordinates |>
        dplyr::distinct(longitude_e, latitude_n)
      
      if (nrow(coords_all) == 0) {
        return(NULL)
      }
      
      bioclim_raster <- terra::rast(worldclim_files)
      coords_df <- data.frame(lon = coords_all$longitude_e, lat = coords_all$latitude_n)
      
      extracted <- terra::extract(bioclim_raster, coords_df)
      
      # Combine coordinates with extracted values
      dplyr::bind_cols(coords_all, extracted)
    }
  ),

  # Format bioclim data for all coordinates
  tar_target(
    name = bioclim,
    command = {
      if (is.null(bioclim_all_extracted)) {
        return(tibble())
      }
      
      # Rename columns to match your existing bioclim format
      bioclim_all_extracted |>
        dplyr::rename(
          annual_temperature = wc2.1_30s_bio_1,
          diurnal_range = wc2.1_30s_bio_2,
          isothermality = wc2.1_30s_bio_3,
          temperature_seasonality = wc2.1_30s_bio_4,
          max_temperture_warmest_month = wc2.1_30s_bio_5,
          min_temperture_coldest_month = wc2.1_30s_bio_6,
          temperature_annual_range = wc2.1_30s_bio_7,
          mean_temperture_wettest_quarter = wc2.1_30s_bio_8,
          mean_temperture_driest_quarter = wc2.1_30s_bio_9,
          mean_temperture_warmest_quarter = wc2.1_30s_bio_10,
          mean_temperture_coldest_quarter = wc2.1_30s_bio_11,
          annual_precipitation = wc2.1_30s_bio_12,
          precipitation_wettest_month = wc2.1_30s_bio_13,
          precipitation_driest_month = wc2.1_30s_bio_14,
          precipitation_seasonality = wc2.1_30s_bio_15,
          precipitation_wettest_quarter = wc2.1_30s_bio_16,
          precipitation_driest_quarter = wc2.1_30s_bio_17,
          precipitation_warmest_quarter = wc2.1_30s_bio_18,
          precipitation_coldest_quarter = wc2.1_30s_bio_19
        ) |>
        # Join back with all_coordinates to get country, site, plot_id info
        tidylog::left_join(all_coordinates, by = c("longitude_e" = "longitude_e", "latitude_n" = "latitude_n")) |>
        # Ensure proper column order
        dplyr::select(country, region, gradient, site, plot_id, elevation_m, longitude_e, latitude_n, ecosystem, dplyr::everything())
    }
  ),

  # CHELSA Bioclim+ variables from envidatS3paths.txt
  tar_target(
    name = chelsa_urls,
    command = chelsa_read_txt_urls("data/envidatS3paths.txt")
  ),

  # Download all CHELSA files from the URL list
  tar_target(
    name = chelsa_files,
    command = {
      if (length(chelsa_urls) == 0) return(character())
      
      cat("Downloading", length(chelsa_urls), "CHELSA files:\n")
      for (i in seq_along(chelsa_urls)) {
        cat(sprintf("%d. %s\n", i, basename(chelsa_urls[i])))
      }
      
      # Download all files to cache
      chelsa_download_batch(chelsa_urls)
    }
  ),

  # Extract all CHELSA variables at study coordinates
  tar_target(
    name = chelsa_extracted,
    command = {
      if (length(chelsa_files) == 0) return(tibble())
      
      # Get coordinates from all_coordinates
      coords_all <- all_coordinates |>
        dplyr::distinct(longitude_e, latitude_n)
      
      if (nrow(coords_all) == 0) {
        cat("No coordinates found\n")
        return(tibble())
      }
      
      cat("Extracting from", length(chelsa_files), "CHELSA files for", nrow(coords_all), "unique coordinates\n")
      
      # Direct extraction approach - more reliable than helper function
      all_results <- list()
      
      for (i in seq_along(chelsa_files)) {
        file_path <- chelsa_files[i]
        
        if (!file.exists(file_path)) {
          cat("File not found:", basename(file_path), "\n")
          next
        }
        
        cat("Processing:", basename(file_path), "\n")
        
        tryCatch({
          # Load raster
          raster <- terra::rast(file_path)
          
          # Create coordinate matrix for extraction
          coords_matrix <- as.matrix(coords_all[, c("longitude_e", "latitude_n")])
          
          # Extract values
          extracted_values <- terra::extract(raster, coords_matrix)
          
          # Get variable name from filename
          var_name <- tools::file_path_sans_ext(basename(file_path))
          var_name <- gsub("CHELSA_", "", var_name)
          var_name <- gsub("_V\\.2\\.1$", "", var_name)
          
          # Get the data column (excluding ID)
          data_cols <- names(extracted_values)[names(extracted_values) != "ID"]
          if (length(data_cols) == 0) {
            cat("  No data columns found\n")
            next
          }
          
          values <- extracted_values[[data_cols[1]]]
          
          # Debug output
          cat("  Raw values class:", class(values), "\n")
          cat("  Sample values:", paste(head(values, 5), collapse = ", "), "\n")
          
          # Clean values - handle both character and numeric
          if (is.character(values)) {
            # Handle character values like "Invalid nr"
            values <- ifelse(grepl("Invalid|invalid|NaN|Inf|^\\s*$", values), NA, values)
            values <- suppressWarnings(as.numeric(values))
          } else {
            # Handle numeric values
            values[is.infinite(values) | is.nan(values) | !is.finite(values)] <- NA
            values[abs(values) > 1e8] <- NA
            values[values %in% c(-9999, 9999, -999, 999)] <- NA
          }
          
          # Store result
          result_df <- coords_all
          result_df[[var_name]] <- values
          
          all_results[[var_name]] <- result_df
          
          # Report extraction success
          valid_count <- sum(!is.na(values))
          cat("  Extracted", valid_count, "valid values out of", length(values), "\n")
          
        }, error = function(e) {
          cat("  Error:", e$message, "\n")
        })
      }
      
      if (length(all_results) == 0) {
        cat("No data extracted from any files\n")
        return(tibble())
      }
      
      # Combine all results
      result <- coords_all
      for (var_name in names(all_results)) {
        var_data <- all_results[[var_name]][[var_name]]
        result[[var_name]] <- var_data
      }
      
      # Simple cleaning - just convert "Invalid number" to NA
      result_cleaned <- result
      for (var_name in names(result)) {
        if (var_name %in% c("longitude_e", "latitude_n")) next
        
        var_data <- result[[var_name]]
        
        # If it's character, replace "Invalid number" with NA, then convert to numeric
        if (is.character(var_data)) {
          var_data[var_data == "Invalid Number"] <- NA
          var_data <- suppressWarnings(as.numeric(var_data))
        }
        
        result_cleaned[[var_name]] <- var_data
      }
      
      # Join with all_coordinates to get site metadata
      result_final <- all_coordinates |>
        dplyr::left_join(result_cleaned, by = c("longitude_e", "latitude_n")) |>
        # Rename CHELSA variables to descriptive names
        dplyr::rename_with(~ dplyr::case_when(
          .x == "swb_1981" ~ "soil_water_balance",
          .x == "gsl_1981-2010" ~ "growing_season_length", 
          .x == "gsp_1981-2010" ~ "growing_season_precipitation",
          .x == "gst_1981-2010" ~ "growing_season_temperature",
          .x == "pet_penman_mean_1981-2010" ~ "potential_evapotranspiration",
          .x == "vpd_mean_1981-2010" ~ "vapor_pressure_deficit",
          .x == "bio2_1981-2010" ~ "mean_diurnal_range_chelsa",
          TRUE ~ .x
        )) |>
        # Reorder columns
        dplyr::select(country, region, gradient, site, plot_id, elevation_m, longitude_e, latitude_n, ecosystem, dplyr::everything())
      
      # Print summary
      var_names <- setdiff(names(result_final), c("country", "region", "gradient", "site", "plot_id", "elevation_m", "longitude_e", "latitude_n", "ecosystem"))
      cat("\nFinal CHELSA dataset:\n")
      cat("Total records:", nrow(result_final), "\n")
      cat("Variables extracted:", length(var_names), "\n")
      
      # Define units
      variable_units <- list(
        "soil_water_balance" = "mm",
        "growing_season_length" = "days",
        "growing_season_precipitation" = "mm", 
        "growing_season_temperature" = "°C",
        "potential_evapotranspiration" = "mm/month",
        "vapor_pressure_deficit" = "Pa",
        "mean_diurnal_range_chelsa" = "°C"
      )
      
      # Summary by variable
      for (var in var_names) {
        if (var %in% names(result_final) && is.numeric(result_final[[var]])) {
          var_range <- range(result_final[[var]], na.rm = TRUE)
          na_count <- sum(is.na(result_final[[var]]))
          unit <- ifelse(var %in% names(variable_units), paste0(" (", variable_units[[var]], ")"), "")
          cat(sprintf("- %s%s: %.1f to %.1f (%d missing)\n", var, unit, var_range[1], var_range[2], na_count))
        }
      }
      
      return(result_final)
    }
  )
  
)


