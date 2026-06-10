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

  # Mean daily temperature seasonality per country, with the growing-season window
  tar_target(
    name = climate_seasonality_fig,
    command = make_climate_seasonality_plot(daily_climate, growing_season)
  ),

  # Shannon diversity: latitude, growing-season temperature, VPD (single composite figure)
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

  # Shannon diversity vs all five growing-season climate variables
  tar_target(
    name = diversity_climate_five_panel_fig,
    command = make_diversity_climate_five_panel_plot(
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

  tar_target(
    name = trait_pca_scree_fig,
    command = make_pca_scree_plot(list(
      "All countries (no P, N:P, height)" = trait_pca_full,
      "P/N:P countries (no Norway, SA)" = trait_pca
    ))
  ),

  # PCA axis vs growing-season climate — trait_pca_full sites, one figure per climate variable
  tar_target(
    name = trait_pca_full_climate_figs,
    command = {
      labels <- climate_variable_labels()
      purrr::imap(labels, function(lab, var) {
        make_pca_climate_comparison_plot(
          trait_pca_full_climate_models_region_output,
          trait_pca_full_climate_models_output,
          trait_pca_full_long,
          var,
          lab,
          trait_pca_full_variance
        )
      })
    }
  ),

  # Trait distribution ridgeline plot
  tar_target(
    name = trait_distribution_ridgeline_fig,
    command = make_trait_ridgeline_plot(trait_mean_long)
  ),

  # Trait vs growing-season climate — one figure per climate variable (mean traits only)
  tar_target(
    name = trait_climate_figs,
    command = {
      labels <- climate_variable_labels()
      purrr::imap(labels, function(lab, var) {
        make_trait_comparison_plot(
          trait_models_region_output,
          trait_models_output,
          trait_mean_long,
          var,
          lab
        )
      })
    }
  ),

  # Trait sampling coverage diagnostic from traitstrap fill levels
  tar_target(
    name = trait_coverage_fig,
    command = make_trait_coverage_plot(trait_imputed)
  )
)
