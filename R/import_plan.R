import_plan <- list(

  # Svalbard Data
  # trait data
  tar_target(
    name = raw_traits_sv,
    command = read_csv(download_traits_sv)
  ),

  # community data
  tar_target(
    name = raw_community_sv,
    command = read_csv(download_community_sv)
  ),

  # Peru Data
  # trait data
  tar_target(
    name = raw_traits_pe,
    command = read_csv(download_traits_pe)
  ),

  # community data
  tar_target(
    name = raw_community_pe,
    command = read_csv(download_community_pe)
  ),

  # China Data
  # trait data
  tar_target(
    name = raw_traits_leaf_ch,
    command = read_csv(download_traits_leaf_ch)
  ),
  tar_target(
    name = raw_traits_chem_ch,
    command = read_csv(download_traits_chem_ch)
  ),

  # community data
  # imported in cleaning code

  # meta
  tar_target(
    name = raw_meta_ch,
    command = read_delim(download_meta_ch) |>
      clean_names() |>
      select(-country, -gradient)
  ),

  # Norway Data
  # community
  tar_target(
    name = raw_community_no,
    command = read_csv(download_community_no)
  ),

  # sp list
  tar_target(
    name = sp_list_no,
    command = read_csv(download_sp_no)
  ),

  # traits
  tar_target(
    name = raw_traits_no,
    command = read_csv(download_traits_no)
  ),

  # Colorado Data

  # community
  tar_target(
    name = raw_community_co,
    command = read_csv("data/Abundance_Data_2016_final.csv")
  ),

  # sp
  tar_target(
    name = raw_sp_co,
    command = read_csv("data/Lorah_scrubbed_notes.csv")
  ),

  # trait
  tar_target(
    name = raw_trait_co,
    command = read_csv("data/rmbl_trait_data_master.csv")
  ),

  # meta
  tar_target(
    name = coords_co,
    command = {
      plot_id <- 1:5
      meta <- tibble(
        site = c("CBT", "Road", "Pfeiler", "Cinnamon", "Almont"),
        elevation_m = c(2699, 2810, 3171, 3458, 2475),
        latitude_n = c(38.8817327, 38.89680392, 38.96044646, 38.99159151, 38.655455),
        longitude_e = c(-106.9799521, -106.978584, -107.0323667, -107.0663821, -106.861632)
      )
      crossing(meta, plot_id) |>
        mutate(
          country = "co",
          gradient = "C",
          site = paste0(country, "_", site),
          plot_id = paste0(site, "_", plot_id)
        )
    }
  ),

  # South Africa Data
  # community data
  tar_target(
    name = raw_community_sa,
    command = read_csv(download_community_sa)
  ),

  # trait data
  tar_target(
    name = raw_traits_sa,
    command = read_csv(download_traits_sa)
  ),

  # meta data
  tar_target(
    name = raw_meta_sa,
    command = read_csv(download_meta_sa) |>
      filter(site_id != 6)
  ),

  # meta data extended - needed to provide coordinates for all plots (1-5)
  tar_target(
    name = raw_meta_sa_extended,
    command = {
      # Get the existing plots 1 and 5 data
      plots_1_5 <- raw_meta_sa
      
      # Create lookup tables for plot 1 and plot 5 coordinates
      plot_1_coords <- plots_1_5 |>
        filter(plot_id == 1) |>
        select(site_id, aspect, latitude_plot1 = latitude, longitude_plot1 = longitude)
      
      plot_5_coords <- plots_1_5 |>
        filter(plot_id == 5) |>
        select(site_id, aspect, latitude_plot5 = latitude, longitude_plot5 = longitude)
      
      # Identify which plots are missing for each site and aspect
      existing_plots <- plots_1_5 |>
        select(site_id, aspect, plot_id) |>
        distinct()
      
      # Create all possible plot combinations (1-5) for each site and aspect
      all_plots <- plots_1_5 |>
        select(site_id, aspect, elevation_m_asl) |>
        distinct() |>
        crossing(plot_id = 1:5)
      
      # Find missing plots (plots that don't exist in the original data)
      missing_plots <- all_plots |>
        anti_join(existing_plots, by = c("site_id", "aspect", "plot_id")) |>
        # Join back with the original data to get all other columns (take first row to avoid duplicates)
        left_join(
          plots_1_5 |> 
            select(-plot_id, -latitude, -longitude) |>
            distinct(site_id, aspect, elevation_m_asl, .keep_all = TRUE),
          by = c("site_id", "aspect", "elevation_m_asl")
        ) |>
        # Join with coordinate lookup tables
        left_join(plot_1_coords, by = c("site_id", "aspect")) |>
        left_join(plot_5_coords, by = c("site_id", "aspect")) |>
        # Fill in latitude and longitude based on plot mapping rules
        mutate(
          latitude = case_when(
            plot_id %in% c(2, 3) ~ latitude_plot1,
            plot_id == 4 ~ latitude_plot5,
            TRUE ~ NA_real_
          ),
          longitude = case_when(
            plot_id %in% c(2, 3) ~ longitude_plot1,
            plot_id == 4 ~ longitude_plot5,
            TRUE ~ NA_real_
          )
        ) |>
        # Remove the temporary coordinate columns
        select(-latitude_plot1, -longitude_plot1, -latitude_plot5, -longitude_plot5)
      
      # Combine original data with missing plots
      bind_rows(plots_1_5, missing_plots) |>
        arrange(site_id, aspect, plot_id)
    }
  ),

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
        distinct(longitude_e, latitude_n)
      
      if (nrow(coords_all) == 0) {
        return(NULL)
      }
      
      bioclim_raster <- terra::rast(worldclim_files)
      coords_df <- data.frame(lon = coords_all$longitude_e, lat = coords_all$latitude_n)
      
      extracted <- terra::extract(bioclim_raster, coords_df)
      
      # Combine coordinates with extracted values
      bind_cols(coords_all, extracted)
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
        rename(
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
        select(country, gradient, site, plot_id, elevation_m, longitude_e, latitude_n, ecosystem, everything())
    }
  ),

  # # climate - combine all bioclim data
  # tar_target(
  #   name = bioclim,
  #   command = {
  #     # Use the newly extracted bioclim data from WorldClim
  #     # This gives us fresh, consistent data for all coordinates
  #     bioclim_all_values
  #   }
  # ),

  # Extended Bioclim+ variables from envidatS3paths.txt
  tar_target(
    name = bioclim_envidat_urls,
    command = chelsa_read_txt_urls("data/envidatS3paths.txt")
  ),

  # Show coordinate ranges for tile selection
  tar_target(
    name = study_area_bounds,
    command = {
      if (!exists("all_coordinates")) {
        return("Coordinates not available yet")
      }
      
      # Use the new country-specific analysis with actual URLs
      tile_analysis <- chelsa_analyze_country_tiles(bioclim_envidat_urls, all_coordinates)
      
      # Also show overall summary
      overall_bounds <- all_coordinates |>
        summarise(
          min_lon = min(longitude_e, na.rm = TRUE),
          max_lon = max(longitude_e, na.rm = TRUE),
          min_lat = min(latitude_n, na.rm = TRUE),
          max_lat = max(latitude_n, na.rm = TRUE),
          n_sites = n_distinct(paste(country, site, plot_id)),
          n_countries = n_distinct(country)
        )
      
      cat("\nOverall study area summary:\n")
      cat("==========================\n")
      cat("Countries:", overall_bounds$n_countries, "\n")
      cat("Total sites:", overall_bounds$n_sites, "\n")
      cat("Global range: Lon", overall_bounds$min_lon, "to", overall_bounds$max_lon, "\n")
      cat("Global range: Lat", overall_bounds$min_lat, "to", overall_bounds$max_lat, "\n")
      
      list(
        tile_analysis = tile_analysis,
        overall_bounds = overall_bounds
      )
    }
  ),

  tar_target(
    name = bioclim_envidat_files,
    command = {
      if (length(bioclim_envidat_urls) == 0) return(character())
      # Use tile filtering to only download what we need
      chelsa_download_study_area_tiles(bioclim_envidat_urls, all_coordinates)
    }
  ),

  tar_target(
    name = bioclim_envidat_values,
    command = {
      if (length(bioclim_envidat_files) == 0) return(tibble())
      chelsa_extract_from_nc_files(bioclim_envidat_files, all_coordinates)
    }
  )

)
