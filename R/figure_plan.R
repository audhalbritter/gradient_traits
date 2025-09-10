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
  tar_target(
    name = trait_lat_fig,
    command = trait_predictions |>
        make_trait_predictor_plot(predictor = "lat", x_label = "Latitude (°N)")
  ),

  tar_target(
    name = trait_elev_fig,
    command = trait_predictions |>
        make_trait_predictor_plot(predictor = "elev", x_label = "Elevation (m a.s.l.)")
  ),

  tar_target(
    name = trait_gsl_fig,
    command = trait_predictions |>
        make_trait_predictor_plot(predictor = "gsl_gee", x_label = "Growing Season Length (days)")
  ),

  tar_target(
    name = trait_gst_fig,
    command = trait_predictions |>
        make_trait_predictor_plot(predictor = "gst_chelsa", x_label = "Growing Season Temperature (°C)")
  ),

  tar_target(
    name = trait_gsp_fig,
    command = trait_predictions |>
        make_trait_predictor_plot(predictor = "gsp_chelsa", x_label = "Growing Season Precipitation (mm)")
  ),

  tar_target(
    name = trait_pet_fig,
    command = trait_predictions |>
        make_trait_predictor_plot(predictor = "pet_chelsa", x_label = "Potential Evapotranspiration (mm/month)")
  ),

  tar_target(
    name = trait_gsl_chelsa_fig,
    command = trait_predictions |>
        make_trait_predictor_plot(predictor = "gsl_chelsa", x_label = "Growing Season Length (days)")
  ),

  tar_target(
    name = trait_temp_warm_fig,
    command = trait_predictions |>
        make_trait_predictor_plot(predictor = "temp_warm_bioclim", x_label = "Mean Temperature Warmest Quarter (°C)")
  ),

  tar_target(
    name = trait_precip_warm_fig,
    command = trait_predictions |>
        make_trait_predictor_plot(predictor = "precip_warm_bioclim", x_label = "Precipitation Warmest Quarter (mm)")
  ),

  tar_target(
    name = trait_diurnal_fig,
    command = trait_predictions |>
        make_trait_predictor_plot(predictor = "diurnal_bioclim", x_label = "Mean Diurnal Range (°C)")
  )

)