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
        make_diversity_predictor_plot(predictor = "lat", x_label = "Latitude (°N)")
  ),

  # diversity vs elevation plot
  tar_target(
    name = diversity_elev_fig,
    command = diversity_predictions |>
        unnest(prediction) |>
        make_diversity_predictor_plot(predictor = "elev", x_label = "Elevation (m a.s.l.)")
  ),

  # diversity vs growing season length plot
  tar_target(
    name = diversity_gsl_fig,
    command = diversity_predictions |>
        unnest(prediction) |>
        make_diversity_predictor_plot(predictor = "gsl", x_label = "Growing Season Length (days)")
  ),

  # diversity vs growing season temperature plot
  tar_target(
    name = diversity_gst_fig,
    command = diversity_predictions |>
        unnest(prediction) |>
        make_diversity_predictor_plot(predictor = "gst", x_label = "Growing Season Temperature (°C)")
  ),

  # diversity vs potential evapotranspiration plot
  tar_target(
    name = diversity_pet_fig,
    command = diversity_predictions |>
        unnest(prediction) |>
        make_diversity_predictor_plot(predictor = "pet", x_label = "Potential Evapotranspiration (mm/month)")
  ),

  # diversity vs diurnal range plot
  tar_target(
    name = diversity_diurnal_fig,
    command = diversity_predictions |>
        unnest(prediction) |>
        make_diversity_predictor_plot(predictor = "diurnal", x_label = "Mean Diurnal Range (°C)")
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
        make_trait_predictor_plot(predictor = "lat", x_label = "Latitude (°N)")
  ),

  tar_target(
    name = trait_elev_fig,
    command = trait_predictions |>
        unnest(prediction) |>
        make_trait_predictor_plot(predictor = "elev", x_label = "Elevation (m a.s.l.)")
  ),

  tar_target(
    name = trait_gsl_fig,
    command = trait_predictions |>
        unnest(prediction) |>
        make_trait_predictor_plot(predictor = "gsl", x_label = "Growing Season Length (days)")
  ),

  tar_target(
    name = trait_gst_fig,
    command = trait_predictions |>
        unnest(prediction) |>
        make_trait_predictor_plot(predictor = "gst", x_label = "Growing Season Temperature (°C)")
  ),

  tar_target(
    name = trait_pet_fig,
    command = trait_predictions |>
        unnest(prediction) |>
        make_trait_predictor_plot(predictor = "pet", x_label = "Potential Evapotranspiration (mm/month)")
  ),

  tar_target(
    name = trait_diurnal_fig,
    command = trait_predictions |>
        unnest(prediction) |>
        make_trait_predictor_plot(predictor = "diurnal", x_label = "Mean Diurnal Range (°C)")
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
