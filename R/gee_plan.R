gee_plan <- list(

  # =============================================================================
  # GOOGLE EARTH ENGINE DATA EXTRACTION PLAN
  # =============================================================================
  # This plan extracts climate and vegetation data from Google Earth Engine
  # 
  # Available datasets to explore:
  # - NOAA/VIIRS/001/VNP22Q2: Vegetation phenology (growing season length, etc.)
  # - MODIS/006/MOD13Q1: Vegetation indices (NDVI, EVI)
  # - MODIS/006/MOD11A2: Land surface temperature
  # - ERA5-Land: Climate reanalysis data
  # - WorldClim: Bioclimatic variables
  # =============================================================================

  # Note: GEE authentication is handled in run.R (outside the pipeline)

  # Extract growing season length from VIIRS VNP22Q2
  tar_target(
    name = gee_growing_season_length,
    command = {
      # Ensure GEE is initialized in this targets process (non-interactive).
      if (!requireNamespace("rgee", quietly = TRUE)) {
        stop("Package rgee is not installed. Install it and try again.")
      }
      ok <- tryCatch({ rgee::ee$Date$now()$getInfo(); TRUE }, error = function(e) FALSE)
      if (!ok) {
        # Attempt a silent initialization using cached credentials (no Drive/GCS needed here).
        tryCatch({
          rgee::ee_Initialize(drive = FALSE, gcs = FALSE)
        }, error = function(e) {
          stop("Google Earth Engine is not initialized in this process, and auto-init failed. Run rgee::ee_Initialize() once interactively to cache credentials, then re-run.")
        })
      }
      
      # Get coordinates from all_coordinates
      coords_all <- all_coordinates |>
        distinct(longitude_e, latitude_n) |>
        filter(!is.na(longitude_e), !is.na(latitude_n))
      
      if (nrow(coords_all) == 0) {
        cat("No valid coordinates found\n")
        return(tibble())
      }
      
      cat("Extracting growing season length for", nrow(coords_all), "coordinates\n")
      
      tryCatch({
        # Load the VIIRS vegetation phenology dataset
        viirs_collection <- rgee::ee$ImageCollection("NOAA/VIIRS/001/VNP22Q2")
        
        # Build a composite (wider date window to be safe)
        viirs_image <- viirs_collection$filterDate("2018-01-01", "2023-12-31")$median()

        # Inspect available bands for debugging
        band_names <- tryCatch(viirs_image$bandNames()$getInfo(), error = function(e) NULL)
        if (is.null(band_names)) band_names <- character()
        cat("VNP22Q2 bands in composite:\n", paste(band_names, collapse = ", "), "\n")

        # Prefer direct band if available; otherwise compute from onset bands
        if ('Growing_Season_Length_1' %in% band_names) {
          cat("Using direct Growing_Season_Length_1 band.\n")
          gsl_image <- viirs_image$select('Growing_Season_Length_1')$rename('growing_season_length')
        } else if (all(c('Onset_Greenness_Decrease_1','Onset_Greenness_Increase_1') %in% band_names)) {
          cat("Computing GSL from onset bands.\n")
          gsl_image <- viirs_image$expression(
            'Onset_Greenness_Decrease_1 - Onset_Greenness_Increase_1',
            list(
              'Onset_Greenness_Decrease_1' = viirs_image$select('Onset_Greenness_Decrease_1'),
              'Onset_Greenness_Increase_1' = viirs_image$select('Onset_Greenness_Increase_1')
            )
          )$rename('growing_season_length')
        } else {
          cat("Required bands not found. Returning empty table.\n")
          return(tibble())
        }
        
        # Create points from coordinates
        points <- rgee::ee$FeatureCollection(
          purrr::map2(
            coords_all$longitude_e, 
            coords_all$latitude_n,
            function(lon, lat) {
              rgee::ee$Feature(
                rgee::ee$Geometry$Point(c(lon, lat)),
                list(longitude_e = lon, latitude_n = lat)
              )
            }
          )
        )
        
        # Extract values at points using low-level getInfo (avoids all geojsonio dependencies)
        extracted <- gsl_image$sampleRegions(
          collection = points,
          scale = 500,
          geometries = FALSE
        )
        
        # Get raw data using getInfo
        extracted_info <- extracted$getInfo()
        
        # Convert to data frame manually
        if (length(extracted_info$features) == 0) {
          cat("No features returned from sampleRegions\n")
          return(tibble())
        }
        
        # Extract coordinates and values (handle NULL geometry)
        coords_list <- lapply(extracted_info$features, function(f) {
          # Use coordinates from properties if geometry is NULL
          if (is.null(f$geometry) || is.null(f$geometry$coordinates)) {
            lon <- f$properties$longitude_e
            lat <- f$properties$latitude_n
          } else {
            coords <- f$geometry$coordinates
            lon <- coords[1]
            lat <- coords[2]
          }
          gsl_val <- f$properties$growing_season_length
          data.frame(
            longitude_e = lon,
            latitude_n = lat, 
            growing_season_length = gsl_val
          )
        })
        
        extracted_df <- do.call(rbind, coords_list)
        
        if (nrow(extracted_df) == 0) {
          cat("No values returned by sampleRegions (0 rows). Returning empty table.\n")
          return(tibble())
        }
        
        # Process the results
        result <- extracted_df |>
          as_tibble() |>
          # Join back with all_coordinates to get site information
          left_join(all_coordinates, by = c("longitude_e", "latitude_n")) |>
          select(country, region, gradient, site, plot_id, elevation_m, longitude_e, latitude_n, ecosystem, growing_season_length)
        
        # Clean the data
        result <- result |>
          mutate(
            growing_season_length = ifelse(
              growing_season_length < 0 | growing_season_length > 365 | is.na(growing_season_length),
              NA,
              growing_season_length
            )
          )
        
        cat("After cleaning, non-NA GSL values:", sum(!is.na(result$growing_season_length)), "\n")
        
        # Print summary
        cat("GEE Growing Season Length Summary:\n")
        cat("Total sites:", nrow(result), "\n")
        cat("Sites with valid data:", sum(!is.na(result$growing_season_length)), "\n")
        if (sum(!is.na(result$growing_season_length)) > 0) {
          cat("GSL range:", round(range(result$growing_season_length, na.rm = TRUE)), "days\n")
          cat("Mean GSL:", round(mean(result$growing_season_length, na.rm = TRUE)), "days\n")
        }
        
        return(result)
        
      }, error = function(e) {
        cat("Error extracting GEE data:", e$message, "\n")
        return(tibble())
      })
    }
  ),

  # Compare GEE growing season length with CHELSA
  tar_target(
    name = gee_chelsa_gsl_comparison,
    command = {
      if (nrow(gee_growing_season_length) == 0 || nrow(chelsa_extracted) == 0) {
        return(tibble())
      }
      
      # Join GEE and CHELSA growing season length data
      comparison_data <- gee_growing_season_length |>
        select(country, region, site, plot_id, longitude_e, latitude_n, 
               gee_gsl = growing_season_length) |>
        left_join(
          chelsa_extracted |>
            select(country, region, site, plot_id, longitude_e, latitude_n,
                   chelsa_gsl = growing_season_length),
          by = c("country", "region", "site", "plot_id", "longitude_e", "latitude_n")
        ) |>
        filter(!is.na(gee_gsl), !is.na(chelsa_gsl)) |>
        mutate(
          region_label = case_when(
            region == "ch" ~ "China",
            region == "co" ~ "Colorado", 
            region == "no" ~ "Norway",
            region == "pe" ~ "Peru",
            region == "sa" ~ "South Africa",
            region == "sv" ~ "Svalbard",
            TRUE ~ region
          )
        )
      
      if (nrow(comparison_data) == 0) {
        cat("No overlapping data between GEE and CHELSA\n")
        return(tibble())
      }
      
      cat("GEE vs CHELSA GSL comparison:\n")
      cat("Sites with both datasets:", nrow(comparison_data), "\n")
      
      return(comparison_data)
    }
  )

)
