gee_plan <- list(

  # =============================================================================
  # GOOGLE EARTH ENGINE DATA EXTRACTION PLAN
  # =============================================================================
  # This plan extracts phenology windows and growing-season climate metrics
  # from Google Earth Engine (GEE), and derives site-level summaries.
  #
  # WHAT WE DOWNLOAD (from GEE):
  # - NOAA/VIIRS/001/VNP22Q2 (phenology, ~500 m):
  #     * Onset_Greenness_Increase_1 (DOY)
  #     * Onset_Greenness_Decrease_1 (DOY)
  #     * Optionally: Growing_Season_Length_1 (days) when available
  # - MODIS/006/MOD11A2 (LST, 8-day, 1 km):
  #     * LST_Day_1km (K, scale 0.02)
  #     * LST_Night_1km (K, scale 0.02)
  # - MODIS/006/MOD16A2 (ET/PET, 8-day, 500 m):
  #     * PET (units as provided by MOD16A2; 8-day composite)
  #
  # WHAT WE CALCULATE (per site):
  # - Growing-season window (from VIIRS):
  #     * gs_start_doy, gs_end_doy (DOY)
  #     * gs_length_days (days)
  #     * gs_start_month, gs_end_month (approx. month, 1–12)
  # - Growing-season metrics from MODIS time series (limited to year 2020 to
  #   respect GEE client transfer caps):
  #     * growing_season_temperature (°C): mean of ((LST_Day + LST_Night)/2) across GS window
  #       - Conversion: degC = (value * 0.02) - 273.15
  #     * growing_season_diurnal_range (°C): mean of (LST_Day - LST_Night) across GS window
  #     * potential_evapotranspiration: sum of PET across GS window
  #
  # TEMPORAL RESOLUTION AND PERIODS USED HERE:
  # - VIIRS VNP22Q2 phenology: annual metrics (per year) available from ~2012–present.
  #   In this workflow we compute a median composite over 2018–2023 to derive a
  #   stable GS window (start/end DOY and length) per site.
  # - MODIS MOD11A2 LST: 8-day composites from ~2000–present. We currently
  #   extract only the year 2020 to stay within GEE transfer limits. LST values
  #   are converted to °C using: degC = (value * 0.02) - 273.15.
  # - MODIS MOD16A2 PET: 8-day composites from ~2000–present. We currently
  #   extract only the year 2020. PET is reported per 8-day period (effectively
  #   mm per 8-day, equivalent to kg/m^2 per 8-day); we sum over the GS window
  #   to obtain growing-season PET.
  #
  # NOTES:
  # - We currently restrict MODIS time series to 2020 to avoid the ~5000-element
  #   client transfer limit. A multi-year PET/LST extraction is on the TODO list.
  # - Some MODIS collections used here are marked deprecated by GEE; we keep them
  #   for compatibility and will upgrade in a future iteration.
  # - All targets initialize GEE within the target process to work under `targets`.
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

  # Explore available GEE datasets for additional bioclim variables
  tar_target(
    name = gee_dataset_exploration,
    command = {
      # Ensure GEE is initialized
      if (!requireNamespace("rgee", quietly = TRUE)) {
        stop("Package rgee is not installed.")
      }
      ok <- tryCatch({ rgee::ee$Date$now()$getInfo(); TRUE }, error = function(e) FALSE)
      if (!ok) {
        tryCatch({
          rgee::ee_Initialize(drive = FALSE, gcs = FALSE)
        }, error = function(e) {
          stop("Google Earth Engine is not initialized.")
        })
      }
      
      # Search for datasets related to our variables
      datasets_to_check <- list(
        "Growing Season Temperature" = c(
          "MODIS/006/MOD11A2",  # Land Surface Temperature
          "MODIS/006/MOD21A2",  # Land Surface Temperature (newer)
          "ECMWF/ERA5_LAND/MONTHLY",  # ERA5-Land monthly
          "IDAHO_EPSCOR/TERRACLIMATE"  # TerraClimate
        ),
        "Growing Season Precipitation" = c(
          "MODIS/006/MOD16A2",  # Evapotranspiration
          "ECMWF/ERA5_LAND/MONTHLY",  # ERA5-Land monthly
          "IDAHO_EPSCOR/TERRACLIMATE",  # TerraClimate
          "UCSB-CHG/CHIRPS/DAILY"  # CHIRPS precipitation
        ),
        "Potential Evapotranspiration" = c(
          "MODIS/006/MOD16A2",  # Evapotranspiration
          "ECMWF/ERA5_LAND/MONTHLY",  # ERA5-Land monthly
          "IDAHO_EPSCOR/TERRACLIMATE"  # TerraClimate
        ),
        "Vapor Pressure Deficit" = c(
          "ECMWF/ERA5_LAND/MONTHLY",  # ERA5-Land monthly
          "IDAHO_EPSCOR/TERRACLIMATE"  # TerraClimate
        ),
        "Diurnal Range" = c(
          "MODIS/006/MOD11A2",  # Land Surface Temperature
          "ECMWF/ERA5_LAND/MONTHLY",  # ERA5-Land monthly
          "IDAHO_EPSCOR/TERRACLIMATE"  # TerraClimate
        )
      )
      
      # Function to check dataset availability and bands
      check_dataset <- function(dataset_id) {
        tryCatch({
          # Try as ImageCollection first (most common)
          collection <- rgee::ee$ImageCollection(dataset_id)
          first_image <- collection$first()
          bands <- first_image$bandNames()$getInfo()
          return(list(
            dataset = dataset_id,
            type = "ImageCollection",
            bands = bands,
            available = TRUE
          ))
        }, error = function(e1) {
          tryCatch({
            # Try as Image if ImageCollection fails
            image <- rgee::ee$Image(dataset_id)
            bands <- image$bandNames()$getInfo()
            return(list(
              dataset = dataset_id,
              type = "Image",
              bands = bands,
              available = TRUE
            ))
          }, error = function(e2) {
            return(list(
              dataset = dataset_id,
              type = "Unknown",
              bands = character(0),
              available = FALSE,
              error = paste("ImageCollection:", e1$message, "| Image:", e2$message)
            ))
          })
        })
      }
      
      # Check all datasets
      results <- list()
      for (variable in names(datasets_to_check)) {
        cat("Checking datasets for", variable, ":\n")
        variable_results <- list()
        for (dataset in datasets_to_check[[variable]]) {
          result <- check_dataset(dataset)
          variable_results[[dataset]] <- result
          if (result$available) {
            cat("  ✓", dataset, "- Bands:", paste(result$bands, collapse = ", "), "\n")
          } else {
            cat("  ✗", dataset, "- Error:", result$error, "\n")
          }
        }
        results[[variable]] <- variable_results
      }
      
      return(results)
    }
  ),

  # Growing-season window (start/end DOY and months) per site from VIIRS phenology
  tar_target(
    name = gee_gs_window,
    command = {
      if (!requireNamespace("rgee", quietly = TRUE)) stop("rgee not installed")
      ok <- tryCatch({ rgee::ee$Date$now()$getInfo(); TRUE }, error = function(e) FALSE)
      if (!ok) rgee::ee_Initialize(drive = FALSE, gcs = FALSE)

      coords_all <- all_coordinates |>
        dplyr::distinct(longitude_e, latitude_n) |>
        dplyr::filter(!is.na(longitude_e), !is.na(latitude_n))
      if (nrow(coords_all) == 0) return(tibble())

      pts <- rgee::ee$FeatureCollection(
        purrr::map2(coords_all$longitude_e, coords_all$latitude_n, function(lon, lat) {
          rgee::ee$Feature(rgee::ee$Geometry$Point(c(lon, lat)), list(longitude_e = lon, latitude_n = lat))
        })
      )

      viirs <- rgee::ee$ImageCollection("NOAA/VIIRS/001/VNP22Q2")$filterDate("2018-01-01", "2023-12-31")$median()
      bands <- viirs$bandNames()$getInfo()
      if (!all(c("Onset_Greenness_Increase_1","Onset_Greenness_Decrease_1") %in% bands)) return(tibble())

      img <- viirs$select(c("Onset_Greenness_Increase_1","Onset_Greenness_Decrease_1"))
      samp <- img$sampleRegions(collection = pts, scale = 500, geometries = FALSE)
      info <- samp$getInfo()
      if (length(info$features) == 0) return(tibble())
      rows <- lapply(info$features, function(f){
        p <- f$properties
        data.frame(
          longitude_e = p$longitude_e,
          latitude_n = p$latitude_n,
          gs_start_doy = as.numeric(p$Onset_Greenness_Increase_1),
          gs_end_doy = as.numeric(p$Onset_Greenness_Decrease_1)
        )
      })
      out <- dplyr::bind_rows(rows) |>
        dplyr::mutate(
          gs_length_days = (gs_end_doy - gs_start_doy) %% 366,
          gs_start_month = floor((pmax(1, gs_start_doy) - 1)/30.5) + 1,
          gs_end_month = floor((pmax(1, gs_end_doy) - 1)/30.5) + 1,
          gs_start_month = pmin(pmax(gs_start_month,1),12),
          gs_end_month = pmin(pmax(gs_end_month,1),12)
        ) |>
        dplyr::left_join(all_coordinates, by = c("longitude_e","latitude_n")) |>
        dplyr::select(country, region, gradient, site, plot_id, elevation_m, longitude_e, latitude_n, ecosystem,
                      gs_start_doy, gs_end_doy, gs_length_days, gs_start_month, gs_end_month)
      out
    }
  ),

  # MODIS LST time series (8-day composites) extracted at sites
  tar_target(
    name = gee_modis_lst_timeseries,
    command = {
      if (!requireNamespace("rgee", quietly = TRUE)) stop("rgee not installed")
      ok <- tryCatch({ rgee::ee$Date$now()$getInfo(); TRUE }, error = function(e) FALSE)
      if (!ok) rgee::ee_Initialize(drive = FALSE, gcs = FALSE)

      coords_all <- all_coordinates |>
        dplyr::distinct(longitude_e, latitude_n) |>
        dplyr::filter(!is.na(longitude_e), !is.na(latitude_n))
      if (nrow(coords_all) == 0) return(tibble())

      pts <- rgee::ee$FeatureCollection(
        purrr::map2(coords_all$longitude_e, coords_all$latitude_n, function(lon, lat) {
          rgee::ee$Feature(rgee::ee$Geometry$Point(c(lon, lat)), list(longitude_e = lon, latitude_n = lat))
        })
      )

      # Try to use the deprecated MODIS/006/MOD11A2 dataset with error handling
      tryCatch({
        # Limit to a single representative year to avoid client transfer limits
        col <- rgee::ee$ImageCollection("MODIS/006/MOD11A2")$select(c("LST_Day_1km","LST_Night_1km"))$filterDate("2020-01-01","2020-12-31")
        # Add date and DOY properties
        col <- col$map(function(img){
          img$set(list(
            date = img$date()$format("YYYY-MM-dd"),
            doy = img$date()$getRelative('day', 'year')
          ))
        })
        samp <- col$map(function(img){
          img$sampleRegions(collection = pts, scale = 1000, geometries = FALSE)
        })$flatten()
        info <- samp$getInfo()
        if (length(info$features) == 0) return(tibble())
        
        # Process features with error handling
        rows <- lapply(info$features, function(f){
          p <- f$properties
          # Handle missing or invalid properties
          if (is.null(p) || length(p) == 0) return(NULL)
          data.frame(
            longitude_e = ifelse(is.null(p$longitude_e), NA, p$longitude_e),
            latitude_n = ifelse(is.null(p$latitude_n), NA, p$latitude_n),
            date = ifelse(is.null(p$date), NA, as.character(p$date)),
            doy = ifelse(is.null(p$doy), NA, as.numeric(p$doy)),
            LST_Day_1km = ifelse(is.null(p$LST_Day_1km), NA, as.numeric(p$LST_Day_1km)),
            LST_Night_1km = ifelse(is.null(p$LST_Night_1km), NA, as.numeric(p$LST_Night_1km))
          )
        })
        
        # Remove NULL entries and bind rows
        rows <- rows[!sapply(rows, is.null)]
        if (length(rows) == 0) return(tibble())
        
        dplyr::bind_rows(rows)
        
      }, error = function(e) {
        cat("Error extracting MODIS LST data:", e$message, "\n")
        cat("Returning empty tibble\n")
        return(tibble())
      })
    }
  ),

  # MODIS PET time series (8-day composites) extracted at sites
  tar_target(
    name = gee_modis_pet_timeseries,
    command = {
      if (!requireNamespace("rgee", quietly = TRUE)) stop("rgee not installed")
      ok <- tryCatch({ rgee::ee$Date$now()$getInfo(); TRUE }, error = function(e) FALSE)
      if (!ok) rgee::ee_Initialize(drive = FALSE, gcs = FALSE)

      coords_all <- all_coordinates |>
        dplyr::distinct(longitude_e, latitude_n) |>
        dplyr::filter(!is.na(longitude_e), !is.na(latitude_n))
      if (nrow(coords_all) == 0) return(tibble())

      pts <- rgee::ee$FeatureCollection(
        purrr::map2(coords_all$longitude_e, coords_all$latitude_n, function(lon, lat) {
          rgee::ee$Feature(rgee::ee$Geometry$Point(c(lon, lat)), list(longitude_e = lon, latitude_n = lat))
        })
      )

      # Try to use the deprecated MODIS/006/MOD16A2 dataset with error handling
      tryCatch({
        # Limit to a single representative year to avoid client transfer limits
        col <- rgee::ee$ImageCollection("MODIS/006/MOD16A2")$select(c("PET"))$filterDate("2020-01-01","2020-12-31")
        col <- col$map(function(img){
          img$set(list(
            date = img$date()$format("YYYY-MM-dd"),
            doy = img$date()$getRelative('day', 'year')
          ))
        })
        samp <- col$map(function(img){ img$sampleRegions(collection = pts, scale = 500, geometries = FALSE) })$flatten()
        info <- samp$getInfo()
        if (length(info$features) == 0) return(tibble())
        
        # Process features with error handling
        rows <- lapply(info$features, function(f){
          p <- f$properties
          # Handle missing or invalid properties
          if (is.null(p) || length(p) == 0) return(NULL)
          data.frame(
            longitude_e = ifelse(is.null(p$longitude_e), NA, p$longitude_e),
            latitude_n = ifelse(is.null(p$latitude_n), NA, p$latitude_n),
            date = ifelse(is.null(p$date), NA, as.character(p$date)),
            doy = ifelse(is.null(p$doy), NA, as.numeric(p$doy)),
            PET = ifelse(is.null(p$PET), NA, as.numeric(p$PET))
          )
        })
        
        # Remove NULL entries and bind rows
        rows <- rows[!sapply(rows, is.null)]
        if (length(rows) == 0) return(tibble())
        
        dplyr::bind_rows(rows)
        
      }, error = function(e) {
        cat("Error extracting MODIS PET data:", e$message, "\n")
        cat("Returning empty tibble\n")
        return(tibble())
      })
    }
  ),

  # Growing-season metrics from MODIS timeseries + VIIRS window
  tar_target(
    name = gee_gs_temp_diurnal,
    command = {
      if (nrow(gee_gs_window) == 0 || nrow(gee_modis_lst_timeseries) == 0) return(tibble())
      # Convert LST to degC; scale factor 0.02 and Kelvin to Celsius
      lst <- gee_modis_lst_timeseries |>
        dplyr::mutate(
          lst_day_c = LST_Day_1km * 0.02 - 273.15,
          lst_night_c = LST_Night_1km * 0.02 - 273.15,
          temp_mean_c = (lst_day_c + lst_night_c)/2,
          diurnal_range_c = (lst_day_c - lst_night_c)
        )
      # Join GS window and keep records whose DOY falls within start/end (wrap-around safe)
      dat <- lst |>
        dplyr::left_join(gee_gs_window[,c("longitude_e","latitude_n","gs_start_doy","gs_end_doy")], by = c("longitude_e","latitude_n")) |>
        dplyr::mutate(in_gs = ifelse(gs_end_doy >= gs_start_doy,
                                     doy >= gs_start_doy & doy <= gs_end_doy,
                                     doy >= gs_start_doy | doy <= gs_end_doy)) |>
        dplyr::filter(in_gs)
      if (nrow(dat) == 0) return(tibble())
      dat |>
        dplyr::group_by(longitude_e, latitude_n) |>
        dplyr::summarise(
          growing_season_temperature = mean(temp_mean_c, na.rm = TRUE),
          growing_season_diurnal_range = mean(diurnal_range_c, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::left_join(all_coordinates, by = c("longitude_e","latitude_n")) |>
        dplyr::select(country, region, gradient, site, plot_id, elevation_m, longitude_e, latitude_n, ecosystem,
                      growing_season_temperature, growing_season_diurnal_range)
    }
  ),

  tar_target(
    name = gee_gs_pet,
    command = {
      if (nrow(gee_gs_window) == 0 || nrow(gee_modis_pet_timeseries) == 0) return(tibble())
      dat <- gee_modis_pet_timeseries |>
        dplyr::left_join(gee_gs_window[,c("longitude_e","latitude_n","gs_start_doy","gs_end_doy")], by = c("longitude_e","latitude_n")) |>
        dplyr::mutate(in_gs = ifelse(gs_end_doy >= gs_start_doy,
                                     doy >= gs_start_doy & doy <= gs_end_doy,
                                     doy >= gs_start_doy | doy <= gs_end_doy)) |>
        dplyr::filter(in_gs)
      if (nrow(dat) == 0) return(tibble())
      dat |>
        dplyr::group_by(longitude_e, latitude_n) |>
        dplyr::summarise(
          potential_evapotranspiration = sum(PET, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::left_join(all_coordinates, by = c("longitude_e","latitude_n")) |>
        dplyr::select(country, region, gradient, site, plot_id, elevation_m, longitude_e, latitude_n, ecosystem,
                      potential_evapotranspiration)
    }
  ),

  # Elevation at sites (NASADEM 30 m; fallback to SRTM if needed)
  tar_target(
    name = gee_elevation,
    command = {
      if (!requireNamespace("rgee", quietly = TRUE)) stop("rgee not installed")
      ok <- tryCatch({ rgee::ee$Date$now()$getInfo(); TRUE }, error = function(e) FALSE)
      if (!ok) rgee::ee_Initialize(drive = FALSE, gcs = FALSE)

      coords_all <- all_coordinates |>
        dplyr::distinct(longitude_e, latitude_n) |>
        dplyr::filter(!is.na(longitude_e), !is.na(latitude_n))
      if (nrow(coords_all) == 0) return(tibble())

      pts <- rgee::ee$FeatureCollection(
        purrr::map2(coords_all$longitude_e, coords_all$latitude_n, function(lon, lat) {
          rgee::ee$Feature(rgee::ee$Geometry$Point(c(lon, lat)), list(longitude_e = lon, latitude_n = lat))
        })
      )

      # Prefer NASADEM; fallback to SRTM if NASADEM unavailable
      elev_img <- tryCatch({
        rgee::ee$Image("NASA/NASADEM_HGT/001")$select("elevation")
      }, error = function(e) {
        rgee::ee$Image("USGS/SRTMGL1_003")$select("elevation")
      })

      samp <- elev_img$sampleRegions(collection = pts, scale = 30, geometries = FALSE)
      info <- samp$getInfo()
      if (length(info$features) == 0) return(tibble())

      rows <- lapply(info$features, function(f){
        p <- f$properties
        if (is.null(p) || length(p) == 0) return(NULL)
        data.frame(
          longitude_e = p$longitude_e,
          latitude_n = p$latitude_n,
          elevation_m_gee = as.numeric(p$elevation)
        )
      })
      rows <- rows[!sapply(rows, is.null)]
      if (length(rows) == 0) return(tibble())

      dplyr::bind_rows(rows) |>
        dplyr::left_join(all_coordinates, by = c("longitude_e","latitude_n")) |>
        dplyr::select(country, region, gradient, site, plot_id, elevation_m, elevation_m_gee,
                      longitude_e, latitude_n, ecosystem)
    }
  )

)
