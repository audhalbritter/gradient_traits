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
    name = diversity_elev_fig,
    command = diversity_predictions %>%
        make_diversity_plot(.)
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
  )

  # tar_target(
  #   name = trait_gst_fig,
  #   command = trait_predictions |>
  #       unnest(prediction) |>
  #       make_trait_predictor_plot(predictor = "gst", x_label = "Growing Season Temperature (°C)")
  # ),

  # tar_target(
  #   name = trait_pet_fig,
  #   command = trait_predictions |>
  #       unnest(prediction) |>
  #       make_trait_predictor_plot(predictor = "pet", x_label = "Potential Evapotranspiration (mm/month)")
  # ),

  # tar_target(
  #   name = trait_diurnal_fig,
  #   command = trait_predictions |>
  #       unnest(prediction) |>
  #       make_trait_predictor_plot(predictor = "diurnal", x_label = "Mean Diurnal Range (°C)")
  # )
  
  )
