figure_plan <- list(

  # Archambault color palette for regions
  tar_target(
    name = archambault_palette,
    command = met.brewer("Archambault", n = 6)
  ),

  # world map of regions
  tar_target(
    name = regions_world_map,
    command = make_region_world_map(all_coordinates)
  ),

  # Growing-season climate variables vs site latitude (same region colours as other figures)
  tar_target(
    name = climate_latitude_fig,
    command = {
      site_lat <- community |>
        dplyr::filter(!is.na(site)) |>
        dplyr::group_by(country, gradient, site) |>
        dplyr::summarise(
          latitude_n = mean(latitude_n, na.rm = TRUE),
          elevation_m = mean(elevation_m, na.rm = TRUE),
          region = dplyr::first(region),
          .groups = "drop"
        ) |>
        dplyr::inner_join(
          growing_season_climate_site,
          by = dplyr::join_by(country, gradient, site)
        ) |>
        dplyr::filter(!is.na(latitude_n))
      make_climate_latitude_plot(site_lat)
    }
  ),

  # Shannon diversity: latitude, downscaled T2m, VPD (single composite figure)
  tar_target(
    name = diversity_three_panel_fig,
    command = make_diversity_three_panel_plot(
      lat_predictions = diversity_predictions,
      climate_predictions = list(
        global = diversity_predictions_ds_climate,
        region = diversity_predictions_region_ds_climate
      )
    )
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

  # Trait distribution ridgeline plot
  tar_target(
    name = trait_distribution_ridgeline_fig,
    command = make_trait_ridgeline_plot(trait_mean_long)
  ),

  # Trait vs growing-season climate (mean traits only)
  tar_target(
    name = trait_climate_gs_temperature_fig,
    command = make_trait_comparison_plot(
      trait_models_region_output, trait_models_output, trait_mean_long,
      "gs_temperature", "Growing season temperature (°C)"
    )
  ),

  tar_target(
    name = trait_climate_gs_vpd_fig,
    command = make_trait_comparison_plot(
      trait_models_region_output, trait_models_output, trait_mean_long,
      "gs_vpd", "Growing season VPD"
    )
  ),

  # Trait sampling coverage diagnostic from traitstrap fill levels
  tar_target(
    name = trait_coverage_fig,
    command = make_trait_coverage_plot(trait_imputed)
  )
)
