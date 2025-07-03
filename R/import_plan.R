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
  # meta
  # tar_target(
  #   name = raw_meta_co,
  #   command = load("data/metaCommunityCO_2016.Rdata")
  # ),

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

  # trait
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


  # climate
  tar_target(
    name = bioclim,
    command = read_csv("data/bioclim_new.csv") |>
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
      )
  )
)
