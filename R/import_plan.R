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

  # download WorldClim data
  tar_target(
    name = worldclim_data,
    command = {
      tryCatch({
        bioclim_data <- geodata::worldclim_global(
          var = "bio",
          res = 0.5,
          path = "WorldClimData",
          download = TRUE
        )
        
        if (is.null(bioclim_data)) {
          stop("WorldClim data download failed")
        }
        
        return(bioclim_data)
        
      }, error = function(e) {
        message("WorldClim download failed: ", e$message)
        return(NULL)
      })
    }
  ),

  # extract bioclim values for South Africa coordinates
  tar_target(
    name = bioclim_sa_extracted,
    command = {
      coords_sa <- raw_meta_sa |>
        distinct(latitude, longitude) |>
        rename(lat = latitude, lon = longitude)
      
      if (is.null(worldclim_data)) {
        return(NULL)
      }
      
      bioclim_files <- list.files("WorldClimData/climate/wc2.1_30s", pattern = "wc2.1_30s_bio_.*tif$", full.names = TRUE)
      
      if (length(bioclim_files) == 0) {
        return(NULL)
      }
      
      bioclim_raster <- terra::rast(bioclim_files)
      coords_df <- data.frame(lon = coords_sa$lon, lat = coords_sa$lat)
      
      terra::extract(bioclim_raster, coords_df) |>
        bind_cols(coords_sa)
    }
  ),

  # format bioclim data for South Africa
  tar_target(
    name = bioclim_sa,
    command = {
      if (is.null(bioclim_sa_extracted)) {
        # if extraction failed, create NA values for all rows in raw_meta_sa
        raw_meta_sa |>
          mutate(
            annual_temperature = NA_real_,
            diurnal_range = NA_real_,
            isothermality = NA_real_,
            temperature_seasonality = NA_real_,
            max_temperture_warmest_month = NA_real_,
            min_temperture_coldest_month = NA_real_,
            temperature_annual_range = NA_real_,
            mean_temperture_wettest_quarter = NA_real_,
            mean_temperture_driest_quarter = NA_real_,
            mean_temperture_warmest_quarter = NA_real_,
            mean_temperture_coldest_quarter = NA_real_,
            annual_precipitation = NA_real_,
            precipitation_wettest_month = NA_real_,
            precipitation_driest_month = NA_real_,
            precipitation_seasonality = NA_real_,
            precipitation_wettest_quarter = NA_real_,
            precipitation_driest_quarter = NA_real_,
            precipitation_warmest_quarter = NA_real_,
            precipitation_coldest_quarter = NA_real_
          )
      } else {
        # join bioclim data to all rows in raw_meta_sa
        raw_meta_sa |>
          left_join(
            bioclim_sa_extracted |>
              rename(
                latitude = lat,
                longitude = lon,
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
              select(-ID),
            by = c("latitude", "longitude")
          )
      } |>
        mutate(
          country = "sa",
          gradient = "C",
          site = paste0(country, "_", site_id),
          plot_id = paste0(site, "_", plot_id),
          elevation_m = elevation_m_asl,
          longitude_e = longitude,
          latitude_n = latitude
        ) |>
        select(
          country, gradient, site, plot_id, elevation_m, longitude_e, latitude_n,
          annual_temperature, diurnal_range, isothermality, temperature_seasonality,
          max_temperture_warmest_month, min_temperture_coldest_month, temperature_annual_range,
          mean_temperture_wettest_quarter, mean_temperture_driest_quarter,
          mean_temperture_warmest_quarter, mean_temperture_coldest_quarter,
          annual_precipitation, precipitation_wettest_month, precipitation_driest_month,
          precipitation_seasonality, precipitation_wettest_quarter, precipitation_driest_quarter,
          precipitation_warmest_quarter, precipitation_coldest_quarter
        )
    }
  ),

  # climate
  tar_target(
    name = bioclim,
    command = bind_rows(
      read_csv("data/bioclim_new.csv") |>
        rename(
          "annual_temperature" = bio_1,
          "diurnal_range" = bio_2,
          "isothermality" = bio_3,
          "temperature_seasonality" = bio_4,
          "max_temperture_warmest_month" = bio_5,
          "min_temperture_coldest_month" = bio_6,
          "temperature_annual_range" = bio_7,
          "mean_temperture_wettest_quarter" = bio_8,
          "mean_temperture_driest_quarter" = bio_9,
          "mean_temperture_warmest_quarter" = bio_10,
          "mean_temperture_coldest_quarter" = bio_11,
          "annual_precipitation" = bio_12,
          "precipitation_wettest_month" = bio_13,
          "precipitation_driest_month" = bio_14,
          "precipitation_seasonality" = bio_15,
          "precipitation_wettest_quarter" = bio_16,
          "precipitation_driest_quarter" = bio_17,
          "precipitation_warmest_quarter" = bio_18,
          "precipitation_coldest_quarter" = bio_19
        ),
      bioclim_sa
    )
  )
)
