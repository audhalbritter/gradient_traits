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

  # Growing-season window (start/end DOY) per site using multi-year daily ERA5 temperature data
  tar_target(
    name = gee_gs_window,
    command = {
      if (!requireNamespace("rgee", quietly = TRUE)) stop("rgee not installed")
      ok <- tryCatch({ rgee::ee$Date$now()$getInfo(); TRUE }, error = function(e) FALSE)
      if (!ok) rgee::ee_Initialize(drive = FALSE, gcs = FALSE)

      coords_all <- all_coordinates |>
        dplyr::distinct(longitude_e, latitude_n, country, region, gradient, site, plot_id, elevation_m, ecosystem) |>
        dplyr::filter(!is.na(longitude_e), !is.na(latitude_n))
      if (nrow(coords_all) == 0) return(tibble())

      pts <- rgee::ee$FeatureCollection(
        purrr::map2(coords_all$longitude_e, coords_all$latitude_n, function(lon, lat) {
          rgee::ee$Feature(rgee::ee$Geometry$Point(c(lon, lat)), list(longitude_e = lon, latitude_n = lat))
        })
      )

      # Use monthly ERA5 data for multiple years (reliable approach)
      # This works within GEE transfer limits and provides good growing season estimates
      cat("Calculating growing season using multi-year monthly ERA5 data...\n")
      
      # Process multiple years of monthly data
      years <- 2015:2022
      all_temp_data <- list()
      
      for (year in years) {
        cat("Processing year", year, "\n")
        start_date <- paste0(year, "-01-01")
        end_date <- paste0(year, "-12-31")
        
        # Get monthly ERA5 data
        era5_monthly <- rgee::ee$ImageCollection("ECMWF/ERA5/MONTHLY")$filterDate(start_date, end_date)$select("mean_2m_air_temperature")
        
        # Process each month separately to get proper month information
        temp_data_year <- list()
        
        for (month in 1:12) {
          month_start <- paste0(year, "-", sprintf("%02d", month), "-01")
          if (month == 12) {
            month_end <- paste0(year, "-12-31")
          } else {
            month_end <- paste0(year, "-", sprintf("%02d", month + 1), "-01")
          }
          
          # Get data for this specific month
          month_collection <- era5_monthly$filterDate(month_start, month_end)
          
          if (month_collection$size()$getInfo() == 0) {
            cat("  No data for month", month, "\n")
            next
          }
          
          # Sample temperature data for this month
          samp <- month_collection$map(function(img){
            img$sampleRegions(collection = pts, scale = 25000, geometries = FALSE)
          })$flatten()
          
          info <- samp$getInfo()
          if (length(info$features) == 0) {
            cat("  No features for month", month, "\n")
            next
          }
          
          # Process temperature data for this month
          month_data <- lapply(info$features, function(f){
            p <- f$properties
            if (is.null(p) || length(p) == 0) return(NULL)
            
            # Create date for the middle of the month
            date_val <- paste0(year, "-", sprintf("%02d", month), "-15")
            
            data.frame(
              longitude_e = ifelse(is.null(p$longitude_e), NA, p$longitude_e),
              latitude_n = ifelse(is.null(p$latitude_n), NA, p$latitude_n),
              temp_c = ifelse(is.null(p$mean_2m_air_temperature), NA, as.numeric(p$mean_2m_air_temperature) - 273.15),
              date = date_val,
              year = year,
              month = month
            )
          })
          
          month_data <- month_data[!sapply(month_data, is.null)]
          if (length(month_data) > 0) {
            temp_data_year[[as.character(month)]] <- dplyr::bind_rows(month_data)
          }
        }
        
        temp_data_year <- temp_data_year[!sapply(temp_data_year, is.null)]
        if (length(temp_data_year) > 0) {
          all_temp_data[[as.character(year)]] <- dplyr::bind_rows(temp_data_year)
        }
      }
      
      if (length(all_temp_data) == 0) return(tibble())
      
      # Combine all years
      temp_data <- dplyr::bind_rows(all_temp_data)
      cat("Total monthly temperature records:", nrow(temp_data), "\n")
      
      # Debug: Check temperature ranges and date column
      if (nrow(temp_data) > 0) {
        cat("Temperature range:", range(temp_data$temp_c, na.rm = TRUE), "°C\n")
        cat("Sites with temp > 0°C:", sum(temp_data$temp_c > 0, na.rm = TRUE), "records\n")
        cat("Sites with temp > 5°C:", sum(temp_data$temp_c > 5, na.rm = TRUE), "records\n")
        cat("Date column info:\n")
        cat("  Non-NA dates:", sum(!is.na(temp_data$date)), "\n")
        cat("  Sample dates:", head(temp_data$date, 3), "\n")
        cat("  Date class:", class(temp_data$date), "\n")
      }
      
      # Calculate growing season using monthly data with improved algorithm
      gs_data <- temp_data |>
        dplyr::filter(!is.na(temp_c), !is.na(date)) |>
        dplyr::mutate(
          date = as.Date(date),
          month = as.numeric(format(date, "%m")),
          year = as.numeric(format(date, "%Y"))
        )
      
      cat("After filtering and processing:", nrow(gs_data), "records\n")
      cat("Unique sites:", length(unique(paste(gs_data$longitude_e, gs_data$latitude_n))), "\n")
      
      # Group by site and year, then calculate growing season per year
      yearly_gs <- gs_data |>
        dplyr::group_by(longitude_e, latitude_n, year) |>
        dplyr::slice_head(n = 12) |>  # Ensure we only have 12 records per site per year
        dplyr::arrange(month) |>
        dplyr::reframe(
          # Use 0°C threshold (more appropriate for all ecosystems including high elevation/polar)
          first_above = ifelse(any(temp_c > 0), min(month[temp_c > 0]), NA),
          last_above = ifelse(any(temp_c > 0), max(month[temp_c > 0]), NA),
          # Find the month with the highest temperature (likely peak growing season)
          peak_month = month[which.max(temp_c)],
          # Calculate mean temperature during growing months
          mean_growing_temp = mean(temp_c[temp_c > 0], na.rm = TRUE),
          # Count months above threshold
          months_above_threshold = sum(temp_c > 0, na.rm = TRUE),
          # Temperature info
          min_temp = min(temp_c, na.rm = TRUE),
          max_temp = max(temp_c, na.rm = TRUE)
        )
      
      cat("After yearly processing:", nrow(yearly_gs), "records\n")
      cat("Sites with valid growing season:", sum(!is.na(yearly_gs$first_above)), "\n")
      
      # Group by site and calculate median across years
      site_gs <- yearly_gs |>
        dplyr::group_by(longitude_e, latitude_n) |>
        dplyr::summarise(
          # Calculate median growing season across all years for stability
          gs_start_month = median(first_above, na.rm = TRUE),
          gs_end_month = median(last_above, na.rm = TRUE),
          peak_month = median(peak_month, na.rm = TRUE),
          mean_growing_temp = mean(mean_growing_temp, na.rm = TRUE),
          months_above_threshold = mean(months_above_threshold, na.rm = TRUE),
          min_temp = mean(min_temp, na.rm = TRUE),
          max_temp = mean(max_temp, na.rm = TRUE),
          years_with_data = sum(!is.na(first_above)),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          # Convert months to DOY (middle of month) - more accurate
          gs_start_doy = ifelse(!is.na(gs_start_month), gs_start_month * 30.44 - 15, NA),
          gs_end_doy = ifelse(!is.na(gs_end_month), gs_end_month * 30.44 - 15, NA),
          # Calculate growing season length in days
          gs_length_days = ifelse(!is.na(gs_start_month) & !is.na(gs_end_month), 
                                 (gs_end_month - gs_start_month + 1) * 30.44, NA),
          # Ensure months are in valid range
          gs_start_month = pmax(1, pmin(12, gs_start_month)),
          gs_end_month = pmax(1, pmin(12, gs_end_month))
        ) |>
        dplyr::left_join(all_coordinates |> dplyr::distinct(longitude_e, latitude_n, .keep_all = TRUE), by = c("longitude_e", "latitude_n")) |>
        dplyr::select(country, region, gradient, site, plot_id, elevation_m, longitude_e, latitude_n, ecosystem,
                      gs_start_doy, gs_end_doy, gs_length_days, gs_start_month, gs_end_month, mean_growing_temp, years_with_data)
      
      cat("Growing season calculation complete. Sites with valid data:", sum(!is.na(site_gs$gs_start_doy)), "out of", nrow(site_gs), "\n")
      site_gs
    }
  ),

  # Parameterized MODIS LST extraction function
  extract_modis_lst_year <- function(year, coords_data) {
    if (!requireNamespace("rgee", quietly = TRUE)) stop("rgee not installed")
    ok <- tryCatch({ rgee::ee$Date$now()$getInfo(); TRUE }, error = function(e) FALSE)
    if (!ok) rgee::ee_Initialize(drive = FALSE, gcs = FALSE)

    coords_all <- coords_data |>
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
      # Extract data for the specified year
      start_date <- paste0(year, "-01-01")
      end_date <- paste0(year, "-12-31")
      cat("Extracting MODIS LST data for year", year, "(", start_date, "to", end_date, ")\n")
      
      col <- rgee::ee$ImageCollection("MODIS/006/MOD11A2")$select(c("LST_Day_1km","LST_Night_1km"))$filterDate(start_date, end_date)
      
      # Process each image individually and track dates
      # Get the collection as a list to process each image separately
      col_list <- col$toList(col$size())
      col_size <- col$size()$getInfo()
      
      cat("Processing", col_size, "images for year", year, "\n")
      
      # Process each image individually and track dates
      all_results <- list()
      image_dates <- list()
      
      for (i in 1:col_size) {
        img <- rgee::ee$Image(col_list$get(i-1))
        
        # Get the date for this image
        img_date <- img$date()$format("YYYY-MM-dd")$getInfo()
        img_doy <- img$date()$getRelative('day', 'year')$getInfo()
        
        # Sample regions for this image
        samp_img <- img$sampleRegions(collection = pts, scale = 1000, geometries = FALSE)
        info_img <- samp_img$getInfo()
        
        if (length(info_img$features) > 0) {
          # Add date information to each feature
          for (j in 1:length(info_img$features)) {
            info_img$features[[j]]$properties$date <- img_date
            info_img$features[[j]]$properties$doy <- img_doy
            info_img$features[[j]]$properties$year <- year
          }
          all_results[[i]] <- info_img$features
        }
      }
      
      # Combine all results
      info <- list(features = unlist(all_results, recursive = FALSE))
      if (length(info$features) == 0) {
        cat("No features returned for year", year, "\n")
        return(tibble())
      }
      
      # Date extraction successful - no debug needed
      
      # Process features with error handling
      rows <- lapply(info$features, function(f){
        p <- f$properties
        # Handle missing or invalid properties
        if (is.null(p) || length(p) == 0) return(NULL)
        
        # Extract date from system time if not available in properties
        date_val <- ifelse(is.null(p$date), NA, as.character(p$date))
        doy_val <- ifelse(is.null(p$doy), NA, as.numeric(p$doy))
        
        # If date is still NA, try to extract from system time
        if (is.na(date_val) && !is.null(p$`system:time_start`)) {
          date_val <- as.character(as.Date(as.numeric(p$`system:time_start`) / 1000, origin = "1970-01-01"))
          doy_val <- as.numeric(format(as.Date(date_val), "%j"))
        }
        
        data.frame(
          longitude_e = ifelse(is.null(p$longitude_e), NA, p$longitude_e),
          latitude_n = ifelse(is.null(p$latitude_n), NA, p$latitude_n),
          date = date_val,
          doy = doy_val,
          year = ifelse(is.null(p$year), year, as.numeric(p$year)),
          LST_Day_1km = ifelse(is.null(p$LST_Day_1km), NA, as.numeric(p$LST_Day_1km)),
          LST_Night_1km = ifelse(is.null(p$LST_Night_1km), NA, as.numeric(p$LST_Night_1km))
        )
      })
      
      # Remove NULL entries and bind rows
      rows <- rows[!sapply(rows, is.null)]
      if (length(rows) == 0) {
        cat("No valid rows for year", year, "\n")
        return(tibble())
      }
      
      result <- dplyr::bind_rows(rows)
      cat("Successfully extracted", nrow(result), "records for year", year, "\n")
      return(result)
      
    }, error = function(e) {
      cat("Error extracting MODIS LST data for year", year, ":", e$message, "\n")
      cat("Returning empty tibble\n")
      return(tibble())
    })
  },

  # Create individual targets for each year
  tar_target(
    name = gee_modis_lst_year_2015,
    command = extract_modis_lst_year(2015, all_coordinates)
  ),
  tar_target(
    name = gee_modis_lst_year_2016,
    command = extract_modis_lst_year(2016, all_coordinates)
  ),
  tar_target(
    name = gee_modis_lst_year_2017,
    command = extract_modis_lst_year(2017, all_coordinates)
  ),
  tar_target(
    name = gee_modis_lst_year_2018,
    command = extract_modis_lst_year(2018, all_coordinates)
  ),
  tar_target(
    name = gee_modis_lst_year_2019,
    command = extract_modis_lst_year(2019, all_coordinates)
  ),
  tar_target(
    name = gee_modis_lst_year_2020,
    command = extract_modis_lst_year(2020, all_coordinates)
  ),
  tar_target(
    name = gee_modis_lst_year_2021,
    command = extract_modis_lst_year(2021, all_coordinates)
  ),
  tar_target(
    name = gee_modis_lst_year_2022,
    command = extract_modis_lst_year(2022, all_coordinates)
  ),
  tar_target(
    name = gee_modis_lst_year_2023,
    command = extract_modis_lst_year(2023, all_coordinates)
  ),

  # Combine all years of MODIS LST data
  tar_target(
    name = gee_modis_lst_timeseries,
    command = {
      # Get all the year-specific targets
      year_targets <- list(
        gee_modis_lst_year_2015,
        gee_modis_lst_year_2016,
        gee_modis_lst_year_2017,
        gee_modis_lst_year_2018,
        gee_modis_lst_year_2019,
        gee_modis_lst_year_2020,
        gee_modis_lst_year_2021,
        gee_modis_lst_year_2022,
        gee_modis_lst_year_2023
      )
      
      # Combine all years
      combined_data <- dplyr::bind_rows(year_targets)
      
      cat("Combined MODIS LST data summary:\n")
      cat("Total records:", nrow(combined_data), "\n")
      cat("Years covered:", paste(sort(unique(combined_data$year)), collapse = ", "), "\n")
      cat("Unique sites:", length(unique(paste(combined_data$longitude_e, combined_data$latitude_n))), "\n")
      
      return(combined_data)
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
