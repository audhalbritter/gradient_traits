figure_plan <- list(

  # trait ordination
  tar_target(
    name = trait_pca_fig,
    command = make_pca_plot(trait_pca)
  ),

    tar_target(
    name = trait_pca_full_fig,
    command = make_pca_plot(trait_pca_full)
  ),

    # Archambault color palette for regions
  tar_target(
    name = archambault_palette,
    command = met.brewer("Archambault", n = 6)
  ),

  # diversity vs elevation plot
  tar_target(
    name = diversity_fig,
    command = diversity_predictions %>%
        make_diversity_plot(.)
  ),

  # diversity vs annual mean temperature (WorldClim bioclim)
  tar_target(
    name = diversity_temp_annual_fig,
    command = diversity_predictions_temp_annual %>%
        make_diversity_temp_annual_plot(.)
  ),

  # world map of regions
  tar_target(
    name = regions_world_map,
    command = make_region_world_map(all_coordinates)
  ),

  # # Trait vs predictor plots
  # Growing Season Length (GEE)
  tar_target(
    name = trait_climate_gsl_fig,
    command = trait_models_output |>
        unnest(predictions) |>
        make_trait_climate_plot(climate_variable = "growing_season_length", data_source = "GEE", x_label = "Growing Season Length (days)")
  ),

  # Growing Season Length (CHELSA)
  tar_target(
    name = trait_climate_gsl_chelsa_fig,
    command = trait_models_output |>
        unnest(predictions) |>
        make_trait_climate_plot(climate_variable = "gsl_1981-2010_chelsa", data_source = "CHELSA", x_label = "Growing Season Length (days)")
  ),

  # Growing Season Temperature (CHELSA)
  tar_target(
    name = trait_climate_gst_fig,
    command = trait_models_output |>
        unnest(predictions) |>
        make_trait_climate_plot(climate_variable = "gst_1981-2010_chelsa", data_source = "CHELSA", x_label = "Growing Season Temperature (°C)")
  ),

  # Growing Season Precipitation (CHELSA)
  tar_target(
    name = trait_climate_gsp_fig,
    command = trait_models_output |>
        unnest(predictions) |>
        make_trait_climate_plot(climate_variable = "gsp_1981-2010_chelsa", data_source = "CHELSA", x_label = "Growing Season Precipitation (mm)")
  ),

  # Potential Evapotranspiration (CHELSA)
  tar_target(
    name = trait_climate_pet_fig,
    command = trait_models_output |>
        unnest(predictions) |>
        make_trait_climate_plot(climate_variable = "pet_penman_mean_1981-2010_chelsa", data_source = "CHELSA", x_label = "Potential Evapotranspiration (mm/month)")
  ),

  # Mean Temperature Warmest Quarter (WorldClim)
  tar_target(
    name = trait_climate_temp_warm_fig,
    command = trait_models_output |>
        unnest(predictions) |>
        make_trait_climate_plot(climate_variable = "mean_temperture_warmest_quarter_bioclim", data_source = "WorldClim", x_label = "Mean Temperature Warmest Quarter (°C)")
  ),

  # Precipitation Warmest Quarter (WorldClim)
  tar_target(
    name = trait_climate_precip_warm_fig,
    command = trait_models_output |>
        unnest(predictions) |>
        make_trait_climate_plot(climate_variable = "precipitation_warmest_quarter_bioclim", data_source = "WorldClim", x_label = "Precipitation Warmest Quarter (mm)")
  ),

  # Mean Diurnal Range (WorldClim)
  tar_target(
    name = trait_climate_diurnal_fig,
    command = trait_models_output |>
        unnest(predictions) |>
        make_trait_climate_plot(climate_variable = "diurnal_range_bioclim", data_source = "WorldClim", x_label = "Mean Diurnal Range (°C)")
  ),

  # Annual Temperature (WorldClim)
  tar_target(
    name = trait_climate_annual_temp_fig,
    command = trait_models_output |>
        unnest(predictions) |>
        make_trait_climate_plot(climate_variable = "annual_temperature_bioclim", data_source = "WorldClim", x_label = "Annual Temperature (°C)")
  )

)