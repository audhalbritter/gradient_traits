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
  ),

  # CHELSA bioclim+ density plots by region
  tar_target(
    name = chelsa_density_fig,
    command = {

      
      # Convert to long format
      chelsa_long <- chelsa_extracted |>
        pivot_longer(cols = -c(country:ecosystem), names_to = "variable", values_to = "value") |>
        filter(!is.na(value), is.finite(value)) |>
        # Add proper labels and units
        mutate(
          variable_label = case_when(
            variable == "soil_water_balance" ~ "Soil Water Balance (mm)",
            variable == "growing_season_length" ~ "Growing Season Length (days)",
            variable == "growing_season_precipitation" ~ "Growing Season Precipitation (mm)",
            variable == "growing_season_temperature" ~ "Growing Season Temperature (°C)",
            variable == "potential_evapotranspiration" ~ "Potential Evapotranspiration (mm/month)",
            variable == "vapor_pressure_deficit" ~ "Vapor Pressure Deficit (Pa)",
            variable == "mean_diurnal_range_chelsa" ~ "Mean Diurnal Range (°C)",
            TRUE ~ variable
          )
        )
      
      # Create the plot
      chelsa_long |>
      filter(region != "Svalbard") |>
        ggplot(aes(x = value, fill = region)) +
        geom_density(alpha = 0.7, linewidth = 0.3) +
        facet_wrap(~ variable_label, scales = "free", ncol = 2) +
        labs(
          x = "Value",
          y = "Density"
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12),
          strip.text = element_text(size = 10, face = "bold"),
          legend.position = "bottom",
          legend.title = element_text(size = 11, face = "bold"),
          axis.text = element_text(size = 9),
          axis.title = element_text(size = 11)
        ) +
        guides(fill = guide_legend(nrow = 1))
    }
  )
  
  )
