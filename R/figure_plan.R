figure_plan <- list(


  # bioclim correlation plot
  tar_target(
    name = bioclim_correlation_fig,
    command = make_bioclim_correlation_plot(bioclim)
  ),

    tar_target(
    name = bioclim_pca_fig,
    command = make_bioclim_pca_plot(bioclim_pca)
  ),

  # trait ordination
  tar_target(
    name = trait_pca_fig,
    command = make_pca_plot(trait_pca)
  ),

    tar_target(
    name = trait_pca_full_fig,
    command = make_pca_plot(trait_pca_full)
  ),

    # Archambault color palette for diversity plots
  tar_target(
    name = archambault_palette,
    command = met.brewer("Archambault", n = 6)
  ),

  # diversity vs latitude plot
  tar_target(
    name = diversity_lat_fig,
    command = diversity_predictions |>
        unnest(prediction) |>
        make_diversity_predictor_plot(predictor = "elev", x_label = "Elevation (m a.s.l.)")
  ),

  # diversity vs annual temperature plot
  tar_target(
    name = diversity_anntemp_fig,
    command = diversity_predictions |>
        unnest(prediction) |>
        make_diversity_predictor_plot(predictor = "anntemp", x_label = "Annual Temperature (°C)")
  ),

  # diversity vs temperature range plot
  tar_target(
    name = diversity_temprange_fig,
    command = diversity_predictions |>
        unnest(prediction) |>
        make_diversity_predictor_plot(predictor = "temprange", x_label = "Temperature Annual Range (°C)")
  ),

  # world map of regions
  tar_target(
    name = regions_world_map,
    command = make_region_world_map(all_coordinates)
  ),

  # Trait vs predictor plots
  tar_target(
    name = trait_lat_fig,
    command = trait_predictions |>
        unnest(prediction) |>
        make_trait_predictor_plot(predictor = "elev", x_label = "Elevation (m a.s.l.)")
  ),

  tar_target(
    name = trait_anntemp_fig,
    command = trait_predictions |>
        unnest(prediction) |>
        make_trait_predictor_plot(predictor = "anntemp", x_label = "Annual Temperature (°C)")
  ),

  tar_target(
    name = trait_temprange_fig,
    command = trait_predictions |>
        unnest(prediction) |>
        make_trait_predictor_plot(predictor = "temprange", x_label = "Temperature Annual Range (°C)")
  )
)
