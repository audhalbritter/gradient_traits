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
    command = {
      # Get the latitude predictions from diversity_predictions
      diversity_predictions |>
        unnest(prediction) |>
        make_diversity_predictor_plot(predictor = "lat", color_palette = archambault_palette, x_label = "Latitude (°N)")
    }
  )

  # # diversity latitude plot
  # tar_target(
  #   name = diversity_lat_fig,
  #   command = {
  #     dat <- diversity_lat_model |>
  #       unnest(data)

  #     res <- diversity_lat_model |>
  #       select(result) |>
  #       unnest(result) |>
  #       filter(term == "annual_temperature") |>
  #       mutate(sign = if_else(p.value <= 0.05, "sign", "non-sign"))

  #     pred <- diversity_lat_model |>
  #       select(diversity_index, output) |>
  #       unnest(output) |>
  #       rename(prediction = fit) |>
  #       left_join(res)

  #     ggplot(dat, aes(x = annual_temperature, y = value)) +
  #       # CI from prediction
  #       geom_ribbon(
  #         data = pred, aes(
  #           y = prediction, ymin = lwr,
  #           ymax = upr
  #         ),
  #         alpha = 0.1,
  #         linetype = 0
  #       ) +
  #       geom_point(alpha = 0.5, aes(colour = ecosystem)) +
  #       # prediction line
  #       geom_line(data = pred, aes(y = prediction, linetype = sign), colour = "grey50", linewidth = 0.5) +
  #       scale_colour_viridis_d(option = "plasma", end = 0.8, name = "") +
  #       # scale_fill_viridis_d(option = "plasma", end = 0.8, name = "") +
  #       # scale_shape_manual(values = c(15, 16, 17)) +
  #       scale_linetype_manual(values = c("solid", "dashed"), name = "") +
  #       labs(
  #         x = "Mean annual temperature in °C",
  #         y = "Diversity index"
  #       ) +
  #       facet_wrap(~diversity_index, scales = "free", labeller = label_parsed) +
  #       theme_bw() +
  #       theme(
  #         legend.position = "top",
  #         legend.box = "vertical"
  #       )
  #   }
  # ),


  # # diversity plot
  # tar_target(
  #   name = diversity_fig,
  #   command = {
  #     dat <- diversity_model |>
  #       unnest(data)

  #     res <- diversity_model |>
  #       select(result) |>
  #       unnest(result) |>
  #       filter(term == "annual_temperature") |>
  #       mutate(sign = if_else(p.value <= 0.05, "sign", "non-sign"))

  #     pred <- diversity_model |>
  #       select(country, gradient, diversity_index, ecosystem, output) |>
  #       unnest(output) |>
  #       rename(prediction = fit) |>
  #       left_join(res)

  #     ggplot(dat, aes(x = annual_temperature, y = value, colour = ecosystem, shape = gradient)) +
  #       # CI from prediction
  #       geom_ribbon(
  #         data = pred, aes(
  #           y = prediction, ymin = lwr,
  #           ymax = upr,
  #           fill = ecosystem
  #         ),
  #         alpha = 0.1,
  #         linetype = 0
  #       ) +
  #       geom_point(alpha = 0.5) +
  #       # prediction line
  #       geom_line(data = pred, aes(y = prediction, linetype = sign), linewidth = 0.5) +
  #       scale_colour_viridis_d(option = "plasma", end = 0.8, name = "") +
  #       scale_fill_viridis_d(option = "plasma", end = 0.8, name = "") +
  #       scale_shape_manual(values = c(15, 16, 17)) +
  #       scale_linetype_manual(values = c("dashed", "solid"), name = "") +
  #       labs(
  #         x = "Mean annual temperature in °C",
  #         y = "Diversity index"
  #       ) +
  #       facet_wrap(~diversity_index, scales = "free", labeller = label_parsed) +
  #       theme_bw() +
  #       theme(
  #         legend.position = "top",
  #         legend.box = "vertical"
  #       )
  #   }
  # ),

  # # trait mean latitude plot
  # tar_target(
  #   name = trait_lat_fig,
  #   command = {
  #     dat <- latitude_model |>
  #       unnest(data)

  #     res <- latitude_model |>
  #       select(result) |>
  #       unnest(result) |>
  #       filter(term == "annual_temperature") |>
  #       mutate(sign = if_else(p.value <= 0.05, "sign", "non-sign"))

  #     pred <- latitude_model |>
  #       select(trait_trans, trait_fancy, output) |>
  #       unnest(output) |>
  #       rename(prediction = fit) |>
  #       left_join(res)

  #     ggplot(dat, aes(x = annual_temperature, y = mean)) +
  #       # CI from prediction
  #       geom_ribbon(
  #         data = pred, aes(
  #           y = prediction, ymin = lwr,
  #           ymax = upr
  #         ),
  #         alpha = 0.1,
  #         linetype = 0
  #       ) +
  #       geom_point(alpha = 0.5, aes(colour = ecosystem)) +
  #       # prediction line
  #       geom_line(data = pred, aes(y = prediction, linetype = sign), colour = "grey50", linewidth = 0.5) +
  #       scale_colour_viridis_d(option = "plasma", end = 0.8, name = "") +
  #       scale_linetype_manual(values = c("solid", "dashed"), name = "") +
  #       labs(
  #         x = "Mean annual temperature in °C",
  #         y = "Bootstrapped trait mean"
  #       ) +
  #       facet_wrap(~figure_names, scales = "free", labeller = label_parsed) +
  #       theme_bw() +
  #       theme(
  #         legend.position = "top",
  #         legend.box = "vertical"
  #       )
  #   }
  # ),

  # # trait mean plot
  # tar_target(
  #   name = trait_mean_fig,
  #   command = {
  #     dat <- trait_mean_model |>
  #       unnest(data)

  #     res <- trait_mean_model |>
  #       select(result) |>
  #       unnest(result) |>
  #       filter(term == "annual_temperature") |>
  #       mutate(sign = if_else(p.value <= 0.05, "sign", "non-sign"))

  #     pred <- trait_mean_model |>
  #       select(country, gradient, trait_trans, trait_fancy, ecosystem, output) |>
  #       unnest(output) |>
  #       rename(prediction = fit) |>
  #       left_join(res)

  #     ggplot(dat, aes(x = annual_temperature, y = mean, colour = ecosystem, shape = gradient)) +
  #       # CI from prediction
  #       geom_ribbon(
  #         data = pred, aes(
  #           y = prediction, ymin = lwr,
  #           ymax = upr,
  #           fill = ecosystem
  #         ),
  #         alpha = 0.1,
  #         linetype = 0
  #       ) +
  #       geom_point(alpha = 0.5) +
  #       # prediction line
  #       geom_line(data = pred, aes(y = prediction, linetype = sign), linewidth = 0.5) +
  #       scale_colour_viridis_d(option = "plasma", end = 0.8, name = "") +
  #       scale_fill_viridis_d(option = "plasma", end = 0.8, name = "") +
  #       scale_shape_manual(values = c(15, 16, 17)) +
  #       scale_linetype_manual(values = c("dashed", "solid"), name = "") +
  #       labs(
  #         x = "Mean annual temperature in °C",
  #         y = "Bootstrapped trait mean"
  #       ) +
  #       facet_wrap(~figure_names, scales = "free", labeller = label_parsed) +
  #       theme_bw() +
  #       theme(
  #         legend.position = "top",
  #         legend.box = "vertical"
  #       )
  #   }
  # ),


  # # trait variance plot
  # tar_target(
  #   name = trait_var_fig,
  #   command = trait_mean |>
  #     ggplot(aes(x = annual_temperature, y = var, colour = ecosystem)) +
  #     geom_point(alpha = 0.5) +
  #     geom_smooth(method = "lm") +
  #     scale_colour_viridis_d(option = "plasma", end = 0.8) +
  #     labs(
  #       x = "Mean annual temperature in °C",
  #       y = "Bootstrapped trait variance"
  #     ) +
  #     facet_wrap(~figure_names, scales = "free", labeller = label_parsed) +
  #     theme_bw() +
  #     theme(
  #       legend.position = "top",
  #       legend.box = "vertical"
  #     )
  # ),

  # # trait variance plot
  # tar_target(
  #   name = trait_higher_moments_fig,
  #   command = trait_mean |>
  #     ggplot(aes(x = skew, y = kurt, colour = ecosystem)) +
  #     geom_point(alpha = 0.5) +
  #     scale_colour_viridis_d(option = "plasma", end = 0.8) +
  #     labs(
  #       y = "Bootstrapped trait kurtosis",
  #       x = "Bootstrapped trait skewness"
  #     ) +
  #     facet_wrap(~figure_names, scales = "free", labeller = label_parsed) +
  #     theme_bw() +
  #     theme(
  #       legend.position = "top",
  #       legend.box = "vertical"
  #     )
  # )
)
