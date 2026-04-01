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

  # climate variables
  tar_target(
    name = climate_variable_fig,
    command = trait_mean_long |>
      filter(trait_trans == "leaf_area_cm2_log") |>
      ggplot(aes(x = climate_value, y = trait_value, colour = region)) +
      geom_point(alpha = 0.6, size = 2) +
      facet_wrap(~climate_variable_clean, scales = "free_x") +
      scale_color_manual(values = create_region_color_mapping(), name = "Region") +
      labs(
        x = "Climate Variable",
        y = "Trait mean"
      ) +
      theme_bw() +
      theme(
        legend.position = "top",
        strip.text = element_text(size = 10),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)
      )
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

  # trait ordination
  tar_target(
    name = trait_pca_fig,
    command = make_pca_plot(trait_pca)
  ),
  tar_target(
    name = trait_pca_full_fig,
    command = make_pca_plot(trait_pca_full)
  ),

  # # Trait vs predictor plots

  # Growing Season Length (CHELSA)
  tar_target(
    name = trait_climate_gsl_chelsa_fig,
    command = make_trait_comparison_plot(trait_models_region_output, trait_models_output, trait_mean_long, 
                                        "gsl_1981-2010_chelsa", "Growing Season Length (days)")
  ),

  # Growing Season Temperature (CHELSA)
  tar_target(
    name = trait_climate_gst_fig,
    command = make_trait_comparison_plot(trait_models_region_output, trait_models_output, trait_mean_long, 
                                        "gst_1981-2010_chelsa", "Growing Season Temperature (°C)")
  ),

  # Growing Season Precipitation (CHELSA)
  tar_target(
    name = trait_climate_gsp_fig,
    command = make_trait_comparison_plot(trait_models_region_output, trait_models_output, trait_mean_long, 
                                        "gsp_1981-2010_chelsa", "Growing Season Precipitation (mm)")
  ),

  # Potential Evapotranspiration (CHELSA)
  tar_target(
    name = trait_climate_pet_fig,
    command = make_trait_comparison_plot(trait_models_region_output, trait_models_output, trait_mean_long, 
                                        "pet_penman_mean_1981-2010_chelsa", "Potential Evapotranspiration (mm/month)")
  ),

  # Mean Temperature Warmest Quarter (WorldClim)
  tar_target(
    name = trait_climate_temp_warm_fig,
    command = make_trait_comparison_plot(trait_models_region_output, trait_models_output, trait_mean_long, 
                                        "mean_temperture_warmest_quarter_bioclim", "Mean Temperature Warmest Quarter (°C)")
  ),

  # Precipitation Warmest Quarter (WorldClim)
  tar_target(
    name = trait_climate_precip_warm_fig,
    command = make_trait_comparison_plot(trait_models_region_output, trait_models_output, trait_mean_long, 
                                        "precipitation_warmest_quarter_bioclim", "Precipitation Warmest Quarter (mm)")
  ),

  # Mean Diurnal Range (WorldClim)
  tar_target(
    name = trait_climate_diurnal_fig,
    command = make_trait_comparison_plot(trait_models_region_output, trait_models_output, trait_mean_long, 
                                        "diurnal_range_bioclim", "Mean Diurnal Range (°C)")
  ),

  # Annual Temperature (WorldClim)
  tar_target(
    name = trait_climate_annual_temp_fig,
    command = make_trait_comparison_plot(trait_models_region_output, trait_models_output, trait_mean_long, 
                                        "annual_temperature_bioclim", "Annual Mean Temperature (°C)")
  ),

  # Vapour Pressure Deficit (CHELSA)
  tar_target(
    name = trait_climate_vpd_fig,
    command = make_trait_comparison_plot(trait_models_region_output, trait_models_output, trait_mean_long, 
                                        "vpd_mean_1981-2010_chelsa", "Vapour Pressure Deficit (Pa)")
  ),

  # Trait variance vs annual temperature figure
  tar_target(
    name = trait_variance_annual_temp_fig,
    command = {
      trait_variance_output |>
        unnest(predictions) |>
        # Add trait fancy names for plotting
        fancy_trait_name_dictionary() |>
        # Create the plot
        ggplot(aes(x = climate_value, y = trait_value, color = region)) +
        geom_point(alpha = 0.6, size = 2) +
        # Add prediction line with different line types based on significance
        geom_line(aes(y = .fitted, linetype = is_significant),
          linewidth = 1, color = "grey40", show.legend = FALSE
        ) +
        # Add confidence intervals
        geom_ribbon(aes(ymin = plo, ymax = phi),
          alpha = 0.2, color = NA, fill = "grey40"
        ) +
        facet_wrap(~trait_fancy, scales = "free_y") +
        scale_color_manual(values = create_region_color_mapping(), name = "Region") +
        scale_linetype_manual(values = c("FALSE" = "dashed", "TRUE" = "solid")) +
        labs(
          x = "Annual Mean Temperature (°C)",
          y = "Trait Variance",
        ) +
        theme_bw() +
        theme(
          legend.position = "top",
          strip.text = element_text(size = 10),
          axis.text = element_text(size = 9),
          axis.title = element_text(size = 11)
        )
    }),
  
  # Regional vs Global comparison plot for Annual Temperature
  tar_target(
    name = trait_region_annual_temp_fig,
    command = {
      # Prepare regional data
      reg_data <- trait_models_region_output |>
        filter(climate_variable == "annual_temperature_bioclim") |>
        # Unnest the smooth predictions
        select(trait_trans, is_significant, predictions) |>
        unnest(predictions)
      
      # Prepare global data
      glob_data <- trait_models_output |>
        filter(climate_variable == "annual_temperature_bioclim") |>
        select(trait_trans, is_significant, predictions) |>
        unnest(predictions)
        
      # Prepare raw points
      raw_data <- trait_mean_long |>
        filter(climate_variable == "annual_temperature_bioclim") |>
        filter(trait_trans %in% unique(reg_data$trait_trans))
        
      make_trait_region_climate_plot(
        data = raw_data,
        prediction_region = reg_data,
        prediction_global = glob_data,
        x_label = "Annual Mean Temperature (°C)"
      )
    }
  )
)
