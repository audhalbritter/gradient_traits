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

  # Downscaled T2m vs site latitude (same region colours as diversity/trait figures)
  tar_target(
    name = downscaled_t2m_latitude_fig,
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
          downscaled_climate |> dplyr::select(country, gradient, site, T2m),
          by = dplyr::join_by(country, gradient, site)
        ) |>
        dplyr::filter(!is.na(T2m), !is.na(latitude_n))
      make_downscaled_t2m_latitude_plot(site_lat)
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

  # Trait vs downscaled climate (mean traits only)
  tar_target(
    name = trait_climate_ds_t2m_fig,
    command = make_trait_comparison_plot(
      trait_models_region_output, trait_models_output, trait_mean_long,
      "ds_t2m", "Mean annual temperature (°C)"
    )
  ),

  tar_target(
    name = trait_climate_ds_vpd_fig,
    command = make_trait_comparison_plot(
      trait_models_region_output, trait_models_output, trait_mean_long,
      "ds_vpd", "Vapour pressure deficit"
    )
  ),

  # Trait sampling coverage diagnostic from traitstrap fill levels
  tar_target(
    name = trait_coverage_fig,
    command = make_trait_coverage_plot(trait_imputed)
  )
)
